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
  data <- load.from.csv.v2("../data/multimodal-planner/map0.txt")
  map <- data$map
  map
  
  problem$initial_pos <- c(data$V2[[2]], data$V1[[2]])
  problem$final_pos <- c(data$V2[[3]], data$V1[[3]])
  
  # This attributes are compulsory
  problem$name <- paste0("Multimodal Planner ( Initial position:", problem$initial_pos, ", Final position:", problem$final_pos, ")")
  problem$state_initial <- list(actual_pos = problem$initial_pos,
                                time = 0, 
                                cost = 0, 
                                mode = "W") # Default value is "W" (Walking), "M" (Metro), "B" (Bus), "T" (Tram)
  problem$state_final <- list(actual_pos = problem$final_pos,
                              time = 0, 
                              cost = 0, 
                              mode = "W")
  problem$actions_possible <- data.frame(c("north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest"), stringsAsFactors = FALSE)
  
  # You can add additional attributes
  problem$map <- matrix(list(), nrow = strtoi(data$V1[[1]]), ncol = strtoi(data$V2[[1]])) # Matriz de listas de transportes (caminar es el default /lista vacia/)
  problem$time_cost_list <- list(w=strtoi(data$time_cost[[4]]), e=strtoi(data$time_cost[[5]])) # costes de tiempo cada modo de transporte
  problem$cost_list <- list(w=strtoi(data$cost[[4]]), e=strtoi(data$cost[[5]])) # costes de cada modo de transporte
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  print(action[1])
  
  # <INSERT CODE HERE TO CHECK THE APPLICABILITY OF EACH ACTION>
  switch(action,
         "north" = {
           print(paste0(state$actual_pos[2]))
           return(state$actual_pos[2] > 1)
         },
         "northeast" = {
           return(state$actual_pos[2] > 1) && (state$actual_pos[1] < ncol(problem$map))
         },
         "east" = {
           return (state$actual_pos[1] < ncol(problem$map))
         },
         "southeast" = {
           return(state$actual_pos[2] < nrow(problem$map)) && (state$actual_pos[1] < ncol(problem$map))
         },
         "south" = {
           return(state$actual_pos[2] < nrow(problem$map))
         },
         "southwest" = {
           return(state$actual_pos[2] < nrow(problem$map)) && (state$actual_pos[1] > 1)
         },
         "west" = {
           return(state$actual_pos[1] > 1)
         },
         "northwest" = {
           return(state$actual_pos[2] > 1) && (state$actual_pos[1] > 1)
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
         }
  )
  
  switch(state$mode,
         "W" = {
           state$time <- state$time + problem$time_cost_list$w
           state$cost <- state$cost + problem$cost_list$w
         })
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT>
  result <- identical(state$actual_pos, final_state$actual_pos)
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE>
  return(paste0("Actual State ( Actual position:", state$actual_pos, ", Final position:", problem$final_pos, "Time spent:", state$time, "Actual mode:", state$mode, "Cost sum:", state$cost, " )"))
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

### LOAD MAP FROM CSV FUNCTION
load.from.csv <- function(file) {
  data <- read.csv(file, header=FALSE) # Read a CSV file
  
  splitted_txt4 <- str_split(data$V1[[4]], ":")
  values4 <- str_split(splitted_txt4[[1]][2], ";")
  
  splitted_txt5 <- str_split(data$V1[[5]], ":")
  values5 <- str_split(splitted_txt5[[1]][2], ";")
  
  modes <- c(0, 0, 0, splitted_txt4[[1]][1], splitted_txt5[[1]][1])
  data$mode <- modes
  
  time_costs <- c(0, 0, 0, values4[[1]][1], values5[[1]][1])
  data$time_cost <- time_costs
  
  costs <- c(0, 0, 0, values4[[1]][2], values5[[1]][2])
  data$cost <- costs
  
  return(data)
}

load.from.csv.v2 <- function(file){
  # Read the content of the file
  file_content <- readLines("../data/multimodal-planner/map4.txt")
  
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
        print("-----------")
        print(paste0("Iteration: ", j))
        station_list <- list(name = mode, number = as.character(j - 2))
        station_pos <- c(as.integer(strsplit(parts[j], ",")[[1]][[1]]), as.integer(strsplit(parts[j], ",")[[1]][[2]]))
        print(paste0("Station pos (x, y): ", station_pos[1], ", ", station_pos[2]))
        map[station_pos[2], station_pos[1]][[1]] <- append(map[station_pos[2], station_pos[1]][[1]], station_list)
        print(map[station_pos[2], station_pos[1]][[1]])
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

# Debugg
data <- load.from.csv.v2("../data/multimodal-planner/map4.txt")

print(data$map)

print(data$map[8, 5][[1]][[3]])
