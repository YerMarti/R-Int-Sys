# =========================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =========================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(initial_pos=vec(1, 1),
                               final_pos=vec(1, 1),
                               map=matrix(0, nrow=1, ncol=1)) {
  problem <- list() # Default value is an empty list.
  
  ## TO ASK - Cómo se inicializa el problema? Carga del fichero. Funciones eval y cost.
  
  # This attributes are compulsory
  problem$name <- paste0("Multimodal Planner ( Initial position:", initial_pos, ", Final position:", final_pos, ")")
  problem$state_initial <- initial_pos
  problem$state_final <- final_pos
  problem$actions_possible <- list("north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest")
  
  # You can add additional attributes
  problem$map <- map
  problem$actual_pos <- initial_pos # Positions are managed as (x, y) coordinates
  problem$time <- 0
  problem$cost <- 0
  problem$mode <- "W" # Default value is "W" (Walking), "M" (Metro), "B" (Bus), "T" (Tram)
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  switch(action,
         "north" = {
           return(actual_pos[2] > 1)
         },
         "northeast" = {
           return(actual_pos[2] > 1) && (actual_pos[1] < ncol(map))
         },
         "east" = {
           return (actual_pos[1] < ncol(map))
         },
         "southeast" = {
           return(actual_pos[2] < nrow(map)) && (actual_pos[1] < ncol(map))
         },
         "south" = {
           return(actual_pos[2] < nrow(map))
         },
         "southwest" = {
           return(actual_pos[2] < nrow(map)) && (actual_pos[1] > 1)
         },
         "west" = {
           return(actual_pos[1] > 1)
         },
         "northwest" = {
           return(actual_pos[2] > 1) && (actual_pos[1] > 1)
         },
         
         return(FALSE)
  )
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>
  switch(action,
         "north" = {
           actual_pos[2] <- actual_pos[2] - 1
         },
         "northeast" = {
           actual_pos[2] <- actual_pos[2] - 1
           actual_pos[1] <- actual_pos[1] + 1
         },
         "east" = {
           actual_pos[1] <- actual_pos[1] + 1
         },
         "southeast" = {
           actual_pos[2] <- actual_pos[2] + 1
           actual_pos[1] <- actual_pos[1] + 1
         },
         "south" = {
           actual_pos[2] <- actual_pos[2] + 1
         },
         "southwest" = {
           actual_pos[2] <- actual_pos[2] + 1
           actual_pos[1] <- actual_pos[1] - 1
         },
         "west" = {
           actual_pos[1] <- actual_pos[1] - 1
         },
         "northwest" = {
           actual_pos[2] <- actual_pos[2] - 1
           actual_pos[1] <- actual_pos[1] - 1
         }
  )
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT>
  result <- (state == final_state)
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE>
  return(paste0("Actual State ( Actual position:", problem$actual_pos, ", Final position:", problem$final_pos, "Time spent:", problem$time, "Actual mode:", problem$mode, "Cost sum:", problem$cost, " )"))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE>
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}
