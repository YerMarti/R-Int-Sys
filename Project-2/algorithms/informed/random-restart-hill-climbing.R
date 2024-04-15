compare.results <- function(new, previous) {
  
  if (previous == 0) {
    return(1)
  }
  
  if (new$state$evaluation > previous$state$evaluation) {
    return(1)
  } else {
    return(0)
  }
}

# Basicamente es el Hill Climbing pero con un bucle que se repite n veces
random.restart.hill.climbing = function(problem,
                                        restarts = 5, 
                                        max_iterations = 50, 
                                        count_print = 10, 
                                        trace = FALSE) {
  
  name_method      <- paste0("Random Restart Hill Climbing")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  print(paste0("* START: ", name_method), quote = F)
  start_time       <- Sys.time()
  
  # Create result container
  best_result <- 0
  
  count <- 1
  end_reason <- 0
  
  # Loop for restarts
  for (restart in restarts){
    
    restart_result <- hill.climbing(problem = problem, 
                                    max_iterations = max_iterations, 
                                    count_print = count_print, 
                                    trace = trace)
    
    # Check if the result is better than the best result
    if (compare.results(new = restart_result, previous = best_result) == 1) {
      best_result <- restart_result
    }
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  print(paste0("Final State: ", to.string(state = best_result$state, problem = problem)), quote = FALSE)
  
  result$state_final <- best_result
  result$report      <- report
  print(paste0("* END: ", name_method), quote = F)
  
  return(result)
}