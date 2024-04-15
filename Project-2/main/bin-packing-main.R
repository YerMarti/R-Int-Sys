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
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/stochastic-hill-climbing.R")
source("../algorithms/informed/random-restart-hill-climbing.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/bin-packing.R")

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  return(hill.climbing.search(problem = problem))
}

execute.stochastic.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  return(stochastic.hill.climbing(problem = problem))
}

execute.random.restart.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  return(random.restart.hill.climbing(problem = problem))
}

# Chooses what algorithm to execute
choose.algorithm <- function(algorithm) {
  switch(algorithm,
         "hill.climbing" = execute.hill.climbing,
         "stochastic.hill.climbing" = execute.stochastic.hill.climbing,
         "random.restart.hill.climbing" = execute.random.restart.hill.climbing,
         NULL)
}

# Execute an algorithm several times and analyze results
test.algorithm <- function(filename, times, algorithms) {
  results     <- vector(mode = "list", length = times * length(algorithms))
  result_pos  <- 1;
  
  #Execute each algorithm N times
  for (algorithm in algorithms) {
    # Initialize a problem instance for the analysis
    problem <- initialize.problem(filename)
    
    print(paste0("Executing: '", algorithm, ", ", times, " times to '",
                 problem$name, "'"), quote = FALSE)
    
    algorithm_function <- choose.algorithm(algorithm)
    
    for (i in 1:times) {
      results[[result_pos]] <- algorithm_function(filename)
      result_pos <- result_pos + 1
    }
  }
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2),
               " - Mean: ", round(mean(results_df$Evaluation), 2),
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2),
               " - Mean: ", round(mean(results_df$Runtime), 2),
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  # Print results in an HTML Table
  kable_material(kbl(results_df, caption = paste(problem$name, algorithm, sep = " - ")),
                 c("striped", "hover", "condensed", "responsive"))
}

# Clear console
cat("\014")
graphics.off()

algorithms    <- vector(mode = "list")
algorithms[1] <- "hill.climbing"
algorithms[2] <- "stochastic.hill.climbing"
algorithms[3] <- "random.restart.hill.climbing"

filenames     <- vector(mode = "list")
filenames[1]  <- "../data/bin-packing/bin-packing-5.txt"
filenames[2]  <- "../data/bin-packing/bin-packing-10.txt"
filenames[3]  <- "../data/bin-packing/bin-packing-15.txt"
filenames[4]  <- "../data/bin-packing/bin-packing-100.txt"

# Number of times to execute each algorithm
times       <- 5
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[1]], algorithms = algorithms, times = times)
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[2]], algorithms = algorithms, times = times)
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[3]], algorithms = algorithms, times = times)
# Execute several times the algorithms for a given problem
test.algorithm(filename = filenames[[4]], algorithms = algorithms, times = times)
