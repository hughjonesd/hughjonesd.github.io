
# == setup and data ====
library(stringr)
library(tidygraph)
library(tidyr)
library(tidytext)
library(tidyverse)
library(topicdoc)
library(topicmodels)


cran <- tools::CRAN_package_db() |> 
  as_tibble() |> 
  filter(! duplicated(Package))
cran$long_desc <- paste(cran$Title, "", cran$Description, sep = "\n")

cran_tidy <- cran |> 
  select(Package, long_desc) |> 
  tidytext::unnest_tokens(word, long_desc) |> 
  anti_join(tidytext::stop_words)

cran_counts <- count(cran_tidy, Package, word)

cran_counts <- cran_counts |> 
  tidytext::bind_tf_idf(term = word, document = Package, n = n) |> 
  filter(tf_idf > 0.1)

cran_counts <- cran_counts |> filter(n >= 2)

cran_dtm <- tidytext::cast_dtm(cran_counts, 
                               document = Package, 
                               term = word, 
                               value = n)

# == models ====

cran_lda <- topicmodels::LDA(cran_dtm, k = 300, 
                             method = "Gibbs", 
                             control=list(iter = 1000, verbose = 10))

cran_lda_topics <- topics(cran_lda, 1)
cran_lda_topics <- tibble(
  Package = names(cran_lda_topics), 
  lda_topic = cran_lda_topics)
cran <- left_join(cran, cran_lda_topics, by = "Package", unmatched = "error")


# way way too slow
# cran_ctm <- topicmodels::CTM(cran_dtm, k = 10, control = list(verbose = 1))

# == diagnostics ====

# diagnostics <- topicdoc::topic_diagnostics(cran_lda, cran_dtm) |> 
#   as_tibble()

cran_lda_post <- posterior(cran_lda)$topics

topic_packages <- function (pkg) {
  topic <- cran$lda_topic[cran$Package==pkg]
  cran |> 
    filter(lda_topic == topic) |> 
    select(Package, Title, Description)
}

topic_packages("ggplot2") |> View()
topic_packages("data.table") |> View()
topic_packages("igraph") |> View()
topic_packages("sf") |> View()
topic_packages("tidytext") |> View()

# == networks ====

cran_deps <- cran |> 
  select(Package, Imports) |>           
  # Split on comma + whitespace:
  tidyr::separate_longer_delim(Imports, delim = regex(",\\s*")) |> 
  filter(! is.na(Imports)) |> 
  mutate(                                  
    # Remove version requirements like " (>= x.y.z)"
    Imports = stringr::str_remove(Imports, regex("\\s+.*", dotall = TRUE))
  )


same_topic <- cran_deps |> 
  left_join(cran_lda_topics, by = "Package") |> 
  left_join(cran_lda_topics, by = c("Imports" = "Package")) |> 
  summarise(
    same_topic = mean(lda_topic.x == lda_topic.y, na.rm = TRUE)
  ) |> 
  pull(same_topic)

# Test by permuting packages
same_topic_null_dist <- replicate(200, {
  imports <- unique(cran_deps$Imports)
  import_resample <- setNames(sample(imports), imports)
  cran_deps |> 
    mutate(
      Imports = import_resample[Imports]
    ) |> 
    left_join(cran_lda_topics, by = "Package") |> 
    left_join(cran_lda_topics, by = c("Imports" = "Package")) |> 
    summarise(
      same_topic = mean(lda_topic.x == lda_topic.y, na.rm = TRUE)
    ) |> 
    pull(same_topic)
})

mean(same_topic_null_dist < same_topic)
summary(same_topic_null_dist)

cran_graph <- cran_deps |> 
  tidygraph::as_tbl_graph(directed = TRUE)

cran_dists <- igraph::distances(cran_graph, mode = "all")
diag(cran_dists) <- NA
dist_sample <- sample(cran_dists, 1e6)
dist_sample <- dist_sample[is.finite(dist_sample)]
t.test(dist_sample)$conf

dist_within_topic <- cran_lda_topics |> 
  filter(Package %in% rownames(cran_dists)) |> 
  summarize(.by = lda_topic,
    mean_dist = {
      cd <- cran_dists[Package, Package]  
      cd <- cd[is.finite(cd)]
      mean(cd)
    }
  )

mean_dist <- mean(dist_sample)
topic_dists <- sort(dist_within_topic$mean_dist)
plot(topic_dists, 
     col = ifelse(topic_dists < mean_dist, "grey60", "grey30"))
                  
abline(h = mean(dist_sample), col = "red")
abline(h = mean(topic_dists), col = "blue")
abline(h = t.test(topic_dists)$conf.int, col = "blue", lty = 2)
