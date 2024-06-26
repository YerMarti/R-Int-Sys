# =========================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =========================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(file, random = FALSE) {
  
  problem <- list() # Default value is an empty list.
  
  # Load CSV file
  data <- load.from.csv(file)
  
  problem$initial_pos <- c(data$initial_pos[[1]], data$initial_pos[[2]]) # (x, y)
  problem$final_pos <- c(data$final_pos[[1]], data$final_pos[[2]]) # (x, y)
  problem$map <- data$map
  problem$transport <- data$df  # Default value is "W" (Walking), "M" (Metro), "B" (Bus), "T" (Tram)
  
  vec_temp <- c("north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest")
  transports_used <- c()
  
  for (i in 1:nrow(problem$transport)) {
    if (problem$transport$mode[[i]] != "E") {
      new_action <- paste0("X", problem$transport$mode[[i]])
      vec_temp <- append(vec_temp, new_action)
      transports_used <- append(transports_used, list(name = problem$transport$mode[[i]], value = 0))
    }
  }
  
  problem$min_cost <- min(problem$transport$time_cost)
  
  problem$actions_possible <- data.frame(actions = vec_temp, stringsAsFactors = FALSE)
  
  # This attributes are compulsory
  problem$name <- paste0("Multimodal Planner ( File: ", file, " | Initial position:", problem$initial_pos[[1]], ",", problem$initial_pos[[2]], " | Final position:", problem$final_pos[[1]], ",", problem$final_pos[[2]], " )")
  problem$state_initial <- list(actual_pos = problem$initial_pos,
                                time = 0, 
                                money = 0, 
                                mode = "W",
                                transports_used = transports_used)
  problem$state_final <- list(actual_pos = problem$final_pos,
                              time = 0, 
                              money = 0, 
                              mode = "W",
                              transports_used = transports_used)
  
  return(problem)
}

# Factorized code to check if two stations are consecutive
is.consecutive <- function (actual_pos, next_pos, state, problem) {
  if (next_pos[1] < 1 | next_pos[1] > ncol(problem$map) | next_pos[2] < 1 | next_pos[2] > nrow(problem$map))
    return (FALSE)
  
  next_idx <- which(problem$map[next_pos[2], next_pos[1]][[1]] == state$mode)
  
  if (length(next_idx) == 0 & state$mode != "W")
    return (FALSE)
  
  if (state$mode == "W")
    return (TRUE)
  
  idx <- which(problem$map[actual_pos[2], actual_pos[1]][[1]] == state$mode)
  actual_num_station <- as.integer(problem$map[actual_pos[2], actual_pos[1]][[1]][idx+1])
  next_num_station <- as.integer(problem$map[next_pos[2], next_pos[1]][[1]][next_idx+1])
  is_consecutive <- ((next_num_station == actual_num_station + 1) | (next_num_station == actual_num_station - 1))
  return (is_consecutive)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  switch(action,
         "north" = {
           next_pos <- state$actual_pos
           next_pos[2] <- next_pos[2] - 1
           if (state$actual_pos[2] > 1)
              return (is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "northeast" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] + 1
           next_pos[2] <- next_pos[2] - 1
           if ((state$actual_pos[2] > 1) & (state$actual_pos[1] < ncol(problem$map)))
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "east" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] + 1
           if (state$actual_pos[1] < ncol(problem$map))
              return (is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "southeast" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] + 1
           next_pos[2] <- next_pos[2] + 1
           if ((state$actual_pos[2] < nrow(problem$map)+1) & (state$actual_pos[1] < ncol(problem$map)))
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "south" = {
           next_pos <- state$actual_pos
           next_pos[2] <- next_pos[2] + 1
           if (state$actual_pos[2] < nrow(problem$map)+1)
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "southwest" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] - 1
           next_pos[2] <- next_pos[2] + 1
           if ((state$actual_pos[2] < nrow(problem$map)+1) & (state$actual_pos[1] > 1))
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "west" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] - 1
           if (state$actual_pos[1] > 1)
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         "northwest" = {
           next_pos <- state$actual_pos
           next_pos[1] <- next_pos[1] - 1
           next_pos[2] <- next_pos[2] - 1
           if ((state$actual_pos[2] > 1) & (state$actual_pos[1] > 1))
              return(is.consecutive(state$actual_pos, next_pos, state, problem))
         },
         {
           new_mode <- strsplit(action, "X")[[1]][2]
           if (new_mode != state$mode) {
             idx <- which(problem$map[state$actual_pos[2], state$actual_pos[1]][[1]] == new_mode)
             if (new_mode == "W"){
               return (TRUE)
             }
             if (length(idx) == 0){
               return (FALSE)
             }
             return (TRUE)
           }
           return (FALSE) 
          }
  )
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  ex_action <- FALSE
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>
  switch(action,
         "north" = {
           state$actual_pos[2] <- state$actual_pos[2] - 1
         },
         "northeast" = {
           state$actual_pos[2] <- state$actual_pos[2] - 1
           state$actual_pos[1] <- state$actual_pos[1] + 1
         },
         "east" = {
           state$actual_pos[1] <- state$actual_pos[1] + 1
         },
         "southeast" = {
           state$actual_pos[2] <- state$actual_pos[2] + 1
           state$actual_pos[1] <- state$actual_pos[1] + 1
         },
         "south" = {
           state$actual_pos[2] <- state$actual_pos[2] + 1
         },
         "southwest" = {
           state$actual_pos[2] <- state$actual_pos[2] + 1
           state$actual_pos[1] <- state$actual_pos[1] - 1
         },
         "west" = {
           state$actual_pos[1] <- state$actual_pos[1] - 1
         },
         "northwest" = {
           state$actual_pos[2] <- state$actual_pos[2] - 1
           state$actual_pos[1] <- state$actual_pos[1] - 1
         },
         {
           ex_action <- TRUE
           
           new_mode <- strsplit(action, "X")[[1]][2]
           state$mode <- new_mode
           idx <- which(problem$transport$mode == "E")
           state$time <- state$time + problem$transport$time_cost[idx]
           
           if (new_mode == "W"){
             return(state)
           }
           
           idx <- which(state$transports_used == new_mode)
           if (state$transports_used[idx+1] == 0){
             state$transports_used[idx+1] <- 1
             idx <- which(problem$transport$mode == new_mode)
             state$money <- state$money + problem$transport$money_cost[idx]
           }
         }
  )
  
  if (ex_action == FALSE){
    idx <- which(problem$transport$mode == state$mode)
    state$time <- state$time + problem$transport$time_cost[idx]
  }
  
  return(state)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT>
  result <- ((state$actual_pos[1] == problem$final_pos[1]) & (state$actual_pos[2] == problem$final_pos[2]))
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem=NULL) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE>
  return(paste0("Actual State ( Actual position: ", state$actual_pos[[1]], ",", state$actual_pos[[2]], " | Time spent: ", state$time, " | Actual mode: ", state$mode, " | Money sum: ", state$money, " )"))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE>
  new_mode <- strsplit(action, "X")[[1]][2]

  if (!is.na(new_mode)){
    idx <- which(problem$transport$mode == state$mode)
    return(problem$transport$time_cost[idx])
  }

  idx <- which(problem$transport$mode == "E")

  return(problem$transport$time_cost[idx])
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  return (sqrt(sum((state$actual_pos - problem$final_pos)^2)) * problem$min_cost)
}

### LOAD MAP FROM CSV FUNCTION
load.from.csv <- function(file){
  # Read the content of the file
  file_content <- readLines(file)
  
  # Extracting rows, cols, initial_pos, and final_pos
  map_dimensions <- strsplit(file_content[1], ",")[[1]]
  rows <- as.integer(map_dimensions[1])
  cols <- as.integer(map_dimensions[2])
  
  initial_pos <- c(as.integer(strsplit(file_content[2], ",")[[1]][[1]]), as.integer(strsplit(file_content[2], ",")[[1]][[2]]))
  final_pos <- c(as.integer(strsplit(file_content[3], ",")[[1]][[1]]), as.integer(strsplit(file_content[3], ",")[[1]][[2]]))
  
  # Creating an empty data frame
  df <- data.frame(
    mode = character(),
    time_cost = integer(),
    money_cost = double()
  )
  
  # Create map
  map = matrix(list(), nrow = rows, ncol = cols)
  
  # Parsing each line and adding it to the data frame
  for (i in 4:length(file_content)) {
    line <- file_content[i]
    parts <- strsplit(line, ";")[[1]]
    
    mode <- substr(parts[1], 1, 1)
    time_cost <- as.double(substr(parts[1], 3, nchar(parts[1])))
    money_cost <- as.double(parts[2])
    
    if (length(parts) > 2) {
      for (j in 3:length(parts)) {
        station_list <- list(name = mode, number = j - 2)
        station_pos <- c(as.integer(strsplit(parts[j], ",")[[1]][[1]]), as.integer(strsplit(parts[j], ",")[[1]][[2]]))
        map[station_pos[2], station_pos[1]][[1]] <- append(map[station_pos[2], station_pos[1]][[1]], station_list)
      }
    }
    
    df <- rbind(df, data.frame(mode, time_cost, money_cost))
  }
  
  # Return the data
  return(list(initial_pos = initial_pos,
              final_pos = final_pos,
              map = map,
              df = df))
}
