library(dplyr)
library(glue)
library(igraph)
library(shiny)
library(stringr)
library(tools)
library(tidytext)
library(visNetwork)

# TODO
# - package description for selected package
# - perhaps also tooltip of title?
# - bugfix: text input to update when related/node is clicked
# - bugfix: should cope when e.g. dplyr is clicked
# - maybe just trim the most networked packages...
#   - this is better than current solution of trimming *everything* if
#     you're next to dplyr
# - different networks visible, selected by dropdown
#   - allow for tree display of the CRAN dependency network
#   - though, is it a tree?
# - don't scroll text select
# - choose different node colors (related, community)
#   - and is that also reflected in the Related box?
# - add a writeup somewhere
# - R-universe data for packages also?

# Function to process data and save cache
process_and_cache_data <- function(cache_file) {
  message('Processing data...')
  
  cran <- tools::CRAN_package_db() |> 
    as_tibble() |> 
    filter(!duplicated(Package))
  
  cran$long_desc <- paste(cran$Title, "", cran$Description, sep = "\n")
  
  cran_tidy <- cran |>
    select(Package, long_desc) |>
    unnest_tokens(word, long_desc) |>
    anti_join(stop_words, by = "word")
  
  cran_counts <- count(cran_tidy, Package, word) |>
    bind_tf_idf(term = word, document = Package, n = n) |>
    filter(tf_idf > 0.1, n >= 2)
  
  cran_dtm <- cast_dtm(cran_counts, document = Package, term = word, value = n)
  set.seed(123)  # for reproducibility
  cran_lda <- topicmodels::LDA(cran_dtm, k = 300, 
                               method = "Gibbs", 
                               control=list(iter = 1000, verbose = 10))
  
  cran_lda_topics <- topics(cran_lda, 1)
  cran_lda_topics <- tibble(
    Package = names(cran_lda_topics), 
    lda_topic = cran_lda_topics
  )
  
  cran <- left_join(cran, cran_lda_topics, 
                    by = "Package", unmatched = "error")  
  
  cran_deps <- cran |> 
    select(Package, Imports) |>           
    separate_longer_delim(Imports, delim = regex(",\\s*")) |> 
    filter(! is.na(Imports), Imports != "") |> 
    mutate(Imports = str_remove(Imports, regex("\\s+.*", dotall = TRUE))) |> 
    filter(Imports %in% cran$Package)
  
  base_packages <- rownames(installed.packages(priority="base"))
  cran_deps <- cran_deps |> 
    filter(! Imports %in% base_packages)
  
  # Create full graph
  full_graph <- graph_from_data_frame(cran_deps, 
                                      directed = TRUE, 
                                      vertices = cran$Package)
  full_graph <- full_graph |> 
    as_tbl_graph() |> 
    activate(nodes) |> 
    mutate(
      n_nbrs = local_size()
    ) |> 
    as.igraph()
  
  # Save processed data to cache file
  save(cran, cran_deps, full_graph, file = cache_file)
  message('Data processed and cached.')
}

cache_file <- "cran_network_data_cache.RData"

if (! file.exists(cache_file)) {
  process_and_cache_data(cache_file)
}

load(cache_file)

get_subgraph <- function(package, full_graph, degree) {
  # Get nodes within degree of the selected package
  nodes <- ego(full_graph, order = degree, nodes = package, 
               mode = "all")[[1]]
  subgraph <- induced_subgraph(full_graph, nodes)
  
  subgraph <- tidygraph::as_tbl_graph(subgraph) |> 
    activate(edges) |> 
    filter(
      .N()$n_nbrs[to] <= 100 | .N()$name[from] == package
    ) |> 
    tidygraph::as.igraph()
    
  
  return(subgraph)
}


ui <- fluidPage(
  tags$head(
    tags$style("#related_packages {overflow: auto;}")
  ),
  titlePanel("R Package Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      selectizeInput("package", "Select a package:", 
                     choices = NULL, 
                     options = list(maxOptions = 1000)),
      h4("Related Packages:"),
      uiOutput("related_packages"),
      width = 3,
    ),
    mainPanel(
      visNetworkOutput("network", height = "800px"),
      width = 9
    )
  )
)

MESSAGE <- message
palette("Alphabet")

# Server
server <- function(input, output, session) {

  # Update selectize input with package names using server-side selection
  updateSelectizeInput(session, "package", 
                       choices = V(full_graph)$name,
                       selected = "tidygraph",
                       server = TRUE,
                       options = list(maxOptions = 1000))
  
  selected_package <- reactiveVal("tidygraph")

  observeEvent(input$package, {
    selected_package(input$package)
  })
  
  # Reactive subgraph
  subgraph <- reactive({
    req(selected_package())
    get_subgraph(selected_package(), full_graph, degree = 2L)
  })
  
  # Render the network
  output$network <- renderVisNetwork({
    req(subgraph())
  
    sg <- subgraph()
    topics <- cran$lda_topic[match(V(sg)$name, cran$Package)]
    pal_cols <- palette.colors(n_distinct(cran$lda_topic), recycle = TRUE)
    colors <- pal_cols[match(topics, unique(cran$lda_topic))]
    nodes <- data.frame(
      id = V(sg)$name,
      label = V(sg)$name,
      title = V(sg)$name,  # for tooltips
      color.background = colors,
      color.border = "#D2E5FF",
      shape = "ellipse"
    )
    nodes$shape[nodes$id == selected_package()] <- "box"
    nodes$color.border[nodes$id == selected_package()] <- "yellow"
    edges <- igraph::as_data_frame(sg, what = "edges")

    visNetwork(nodes, edges) |>
      visIgraphLayout() |>
      visOptions(nodesIdSelection = TRUE) |>
      visPhysics(stabilization = FALSE) |> 
      visEdges(smooth = FALSE, color = "grey85") |> 
      visEvents(click = "function(nodes) {
        Shiny.setInputValue('clicked_node', nodes.nodes[0]);
      }")
  })
  
  # Observer for node clicks
  observeEvent(input$clicked_node, {
    clicked_package <- input$clicked_node
    if (!is.null(clicked_package) && clicked_package != "") {
      selected_package(clicked_package)
    }
  })
  
  # For clicks on the "related packages"
  observeEvent(input$clicked_related, {
    clicked_package <- input$clicked_related
    if (!is.null(clicked_package) && clicked_package != "") {
MESSAGE("clicked_related ", clicked_package)
      selected_package(clicked_package)
    }
  })
  
  # Observe package selection and focus on it
#   observe({
#     req(selected_package())
# message("observe 171")
#     # visNetworkProxy("network") |>
#     #   visFocus(id = selected_package())
#     # updateSelectizeInput(session, "package", 
#     #                      selected = selected_package())
#   })
  
  # Render related packages
  output$related_packages <- renderUI({
    req(selected_package())
MESSAGE("related_packages")
    selected_topic <- cran$lda_topic[cran$Package == selected_package()]
    related_pkgs <- cran |>
      filter(lda_topic == selected_topic, Package != selected_package()) |>
      select(Package, Title)
    
    tagList(
      lapply(seq_len(nrow(related_pkgs)), function(i) {
        pkg <- related_pkgs[i, ]
        actionLink(
          inputId = glue("related_{pkg$Package}"),
          label = HTML(glue("<strong>{pkg$Package}</strong><br>{pkg$Title}")),
          onClick = glue("Shiny.setInputValue('clicked_related', 
                                              '{pkg$Package}');")
        )
      }),
      tags$style(HTML("
        .action-button { display: block; margin-bottom: 10px; text-align: left; }
      "))
    )
  })
}

# Run the app
shinyApp(ui, server)
