
library(rlang)
library(purrr)
library(dplyr)
library(codetools)
library(codesamples)

handler <- function(e, w) {
  func <- e[[1]]
  funcName <- as.character(func)
  if (funcName %in% c('<-', '<<-', '=')) {
    return(check_pipeable_assign)
  } else {
    return(NULL)
  }
}

# assignments are:
#   "simple" if they are like x <- f(x, ...)
#   "complex" if they are like x$y <- f(x$y, ...), i.e. lhs is not just a name
#   "no" otherwise
check_pipeable_assign <- function (expr, walker) {
  if (is.character(expr)) return(NULL)
  args <- as.list(expr[-1])
  # we're interested in cases of the form a <- foo(a, ...)
  # this works even if lhs is "names(a)" or such:
  lhs <- deparse1(args[[1]])
  complex_lhs <- is.call(args[[1]])
  pipeable <- if (class(args[[2]]) == "call" && length(args[[2]]) > 1) {
               rhs_first <- deparse1(args[[2]][[2]])
               lhs == rhs_first
             } else {
               FALSE
             }
  
  ass <- list(
    complex = complex_lhs,
    pipeable = pipeable,
    expr = tryCatch(deparse1(expr), error = \(e) "!UNPARSEABLE!")
  )
  walker$collect(ass)
  # walkCode(args[[1]], walker) # don't need to check LHS of assignment
  walkCode(args[[2]], walker)
}


find_assignments <- function (snip) {
  map(snip$snippet, .progress = TRUE,
    function (x) {
       env <- new.env()
       assign("assignments", list(), envir = env)
       cw <- codetools::makeCodeWalker(
         handler = handler,
         leaf = function (e, w) {},
         collect = function (assignment) {
           a <- get("assignments", envir = env)
           assign("assignments", c(a, list(assignment)), envir = env)
         }
       )
       
       exprs <- try(rlang::parse_exprs(x), silent = TRUE)
       if (inherits(exprs, "try-error")) return(NULL)
       map(exprs, \(ex) codetools::walkCode(ex, cw))
       
       get("assignments", envir = env)
    })
}

gh_ass <- find_assignments(github_data)
so_ass <- find_assignments(so_questions)
ex_ass <- find_assignments(package_examples)

make_stats <- function (ass) {
  n_assignments <- lengths(ass)
  n_complex <- map_dbl(ass, \(x) sum(map_dbl(x, "complex")))
  n_pipeable <- map_dbl(ass, \(x) sum(map_dbl(x, "pipeable")))
  n_both  <- map_dbl(ass, \(x) sum(map_dbl(x, \(x) x$pipeable && x$complex)))
  data.frame(
    n_assignments, n_complex, n_pipeable, n_both
  )
}

gh_stats <- make_stats(gh_ass)
so_stats <- make_stats(so_ass)
ex_stats <- make_stats(ex_ass)

github_data <- cbind(github_data, gh_stats)
so_questions <- cbind(so_questions, so_stats)
package_examples <- cbind(package_examples, ex_stats)

github_data <- as_tibble(github_data)
so_questions <- as_tibble(so_questions)
package_examples <- as_tibble(package_examples)

summarize_stats <- function (data) {
  summarize(data,
    n = nrow(data),
    prop_pipeable = sum(n_pipeable)/sum(n_assignments),
    prop_both = sum(n_both)/sum(n_assignments),
  ) 
}

gh_summary <- summarize_stats(github_data)
so_summary <- summarize_stats(so_questions)
ex_summary <- summarize_stats(package_examples)

