# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# One Pizza
source("../problem/one-pizza-incremental.R")

solve.problem <- function(file, p, random_acttions) {
  problem <- initialize.problem(file, p, random_acttions)

  bfs_ts   <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000)
  bfs_gs   <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  dfs_ts   <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
  dfs_gs   <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  dls20_ts <- depth.limited.search(problem, max_iterations = 2500, depth_limit = 20, count_print = 1000)
  dls20_gs <- depth.limited.search(problem, max_iterations = 2500, depth_limit = 20, count_print = 1000, graph_search = TRUE)
  ids_ts   <- iterative.deepening.search(problem, max_iterations = 2500, max_depth = 20, count_print = 1000)
  ids_gs   <- iterative.deepening.search(problem, max_iterations = 2500, max_depth = 20, count_print = 1000, graph_search = TRUE)
  ucs_ts   <- uniform.cost.search(problem, max_iterations = 2500, count_print = 1000)
  ucs_gs   <- uniform.cost.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  gbfs_ts  <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000)
  gbfs_gs  <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  a_star_gs <- a.star.search(problem, max_iterations = 2500, count_print = 1000)
  a_star_ts <- a.star.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

  results <- analyze.results(list(bfs_ts, bfs_gs,
                                  dfs_ts, dfs_gs,
                                  dls20_ts, dls20_gs,
                                  ids_ts, ids_gs,
                                  ucs_ts, ucs_gs,
                                  gbfs_ts, gbfs_gs,
                                  a_star_gs, a_star_ts), problem)

  # Print results in an HTML Table
  kable_material(kbl(results, caption = problem$name), c("striped", "hover", "condensed", "responsive"))
}

file <- "../data/one-pizza/a_an_example.in.txt";
solve.problem(file, 0.3, FALSE)

file <- "../data/one-pizza/b_basic.in.txt"
solve.problem(file, 0.3, FALSE)

file <- "../data/one-pizza/c_coarse.in.txt"
solve.problem(file, 0.3, FALSE)

file <- "../data/one-pizza/s_spanish.in.txt"
solve.problem(file, 0.3, FALSE)
