local.beam.search = function(problem,
                                max_iterations = 50,
                                count_print = 10,
                                beams = 5,
                                trace = FALSE) {
  
  name_method      <- paste0("Local Beam Search")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  print(paste0("* START: ", name_method), quote = F)
  start_time       <- Sys.time()
  
  current_nodes_list <- list()
  
  for(i in 1:beams){
    node_current <- list(parent = c(),
                         state = state_initial,
                         actions = c(),
                         depth = 1,
                         cost = get.cost(state = state_initial, problem = problem),
                         evaluation = get.evaluation(state_initial, problem))
    
    current_nodes_list <- append(current_nodes_list, list(node_current))
  }
  
  count <- 1
  end_reason <- 0
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       depth_of_expanded = numeric())
  
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  while (count <= max_iterations) {
    # Print a search trace for each "count_print" iteration
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", evaluation=", node_current$evaluation, " / cost=", node_current$cost), quote = FALSE)
    }
    
    #If "trace" is on, the information of current node is displayed
    if (trace) {
      print(paste0("Iteration: ", count, ", evaluation=", node_current$evaluation, " / cost=", node_current$cost), quote = FALSE)
      to.string(state = node_current$state, problem = problem)
    }
    
    # Recorrer los beams
    node_candidates <- list()
    for (node in current_nodes_list) {
      # Add all parent nodes
      
      
      node_candidates <- append(node_candidates, list(node))
      
      # Current node is expanded
      successor_nodes <- local.expand.node(node, actions_possible, problem)
      
      for (node in successor_nodes) {
        if (node$evaluation <= node_current$evaluation){
          node_candidates <- append(node_candidates, list(node))
        }
      }
      
    }
    
    # Successor nodes are sorted ascending order of the evaluation function
    node_candidates <- node_candidates[order(sapply(node_candidates,function (x) x$evaluation))]
    
    for (i in 1:beams){
      current_nodes_list[i] <- node_candidates[i]
    }
    
    #Add of information for further analysis
    report <- rbind(report, data.frame(iteration = count,
                                       depth_of_expanded = current_nodes_list[[1]]$depth))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  print(paste0("Final State: ", to.string(state = current_nodes_list[[1]]$state, problem = problem)), quote = FALSE)
  
  result$state_final <- current_nodes_list[[1]]
  result$report      <- report
  print(paste0("* END: ", name_method), quote = F)
  
  return(result)
}
