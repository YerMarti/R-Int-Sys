# -----------------------------------------------------------------------------
# This code snippet was generated for educational purposes as part of the
# Intelligent Systems course at the University of Deusto. The code has been
# created with assistance from ChatGPT version 3.5 and GitHub Copilot.
#
# The code is released under the Creative Commons License and is provided
# for free use and modification by the programming and development community.
#
# This script was generated in April 2024, the year when the Athletic Club de
# Bilbao won the 25th King's Cup.
# -----------------------------------------------------------------------------

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load custom functions
source("decision-tree-learning-utils.R")

#-----------------------
# LOAD AND PREPARE DATA
#-----------------------
# Read train data from CSV
filename = "../data/2024_clients-train.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

# Read test data from CSV
filename = "../data/2024_clients-test.csv"
data_test <- read.csv(file = filename, sep =",", header = TRUE)


# Remove rows with NA values
data <- na.omit(data)
data_test <- na.omit(data_test)

# Remove rows with empty string values
data <- data[!(data$Profession=="" | data$Ever_Married=="" | data$Graduated=="" | data$Var_1==""), ]
data_test <- data_test[!(data_test$Profession=="" | data_test$Ever_Married=="" | data_test$Graduated=="" | data_test$Var_1==""), ]

# Zona de visualizacion o analisis lo que sea
ggplot(data, aes(x=Age, y=Segmentation)) + geom_point()
ggplot(data, aes(x=Work_Experience, y=Segmentation)) + geom_point()
ggplot(data, aes(x=Family_Size, y=Segmentation)) + geom_point()

ggplot(data, aes(x=Age, fill=Age)) + geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25) +
  labs(x = "Age", y = "Frequency")

ggplot(data, aes(x=Work_Experience, fill=Work_Experience)) + geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25) +
  labs(x = "Work_Experience", y = "Frequency")

ggplot(data, aes(x=Family_Size, fill=Family_Size)) + geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25) +
  labs(x = "Family_Size", y = "Frequency")


#------------------------
# PLOT DATA DISTRIBUTION
#------------------------
# Folder to save images
images_folder <- "../data/images/2024-client-segmentation/"
# Plot the relation between the target variable and the rest of the columns
plot.data.distribution(data, target = "Segmentation", folder=images_folder)

#----------------------------------------
# TRANSFORM NUMERIC COLUMNS INTO FACTORS
#----------------------------------------
# Create 4 bims for Age (based on lowest and highest values)
# bin_width <- round((max(data$Age) - min(data$Age)) / 4)
# labels <- paste0(min(data$Age) + (1:4 - 1) * bin_width, " <-> ", min(data$Age) + 1:4 * bin_width)
# data$Age <- cut(data$Age, breaks = 4, labels = labels)
# data_test$Age <- cut(data_test$Age, breaks = 4, labels = labels)

# Create 4 bims for Age (based on quartiles)
data_ordenada <- arrange(data, Age)
cuartiles <- quantile(data_ordenada$Age, probs = c(0.25, 0.5, 0.75))
min_age <- min(data_ordenada$Age)
max_age <- max(data_ordenada$Age)
data$Age <- cut(data$Age, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_age, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_age)
))
data_test$Age <- cut(data_test$Age, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_age, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_age)
))


# Create 4 bims for Work_Experience (based on lowest and highest values)
# bin_width <- round((max(data$Work_Experience) - min(data$Work_Experience)) / 4)
# labels <- paste0(min(data$Work_Experience) + (1:4 - 1) * bin_width, " <-> ", min(data$Work_Experience) + 1:4 * bin_width)
# data$Work_Experience <- cut(data$Work_Experience, breaks = 4, labels = labels)
# data_test$Work_Experience <- cut(data_test$Work_Experience, breaks = 4, labels = labels)

# Create 4 bims for Work_Experience (based on quartiles)
data_ordenada <- arrange(data, Work_Experience)
cuartiles <- quantile(data_ordenada$Work_Experience, probs = c(0.25, 0.5, 0.75))
min_exp <- min(data_ordenada$Work_Experience)
max_exp <- max(data_ordenada$Work_Experience)
data$Work_Experience <- cut(data$Work_Experience, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_exp, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_exp)
))
data_test$Work_Experience <- cut(data_test$Work_Experience, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_exp, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_exp)
))


# Create 4 bims for Family_Size (based on lowest and highest values)
# bin_width <- round((max(data$Family_Size) - min(data$Family_Size)) / 4)
# labels <- paste0(min(data$Family_Size) + (1:4 - 1) * bin_width, " <-> ", min(data$Family_Size) + 1:4 * bin_width)
# data$Family_Size <- cut(data$Family_Size, breaks = 4, labels = labels)
# data_test$Family_Size <- cut(data_test$Family_Size, breaks = 4, labels = labels)

# Create 4 bims for Family_Size (based on quartiles)
data_ordenada <- arrange(data, Family_Size)
cuartiles <- quantile(data_ordenada$Family_Size, probs = c(0.25, 0.5, 0.75))

cuartiles[2] <- cuartiles[2] + 1
min_fam <- min(data_ordenada$Family_Size)
max_fam <- max(data_ordenada$Family_Size)

data$Family_Size <- cut(data$Family_Size, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_fam, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_fam)
))
data_test$Family_Size <- cut(data_test$Family_Size, breaks = c(-Inf, cuartiles[1], cuartiles[2], cuartiles[3], Inf), labels = c(
  paste0(min_fam, " <-> ", cuartiles[1]),
  paste0(cuartiles[1], " <-> ", cuartiles[2]),
  paste0(cuartiles[2]+1, " <-> ", cuartiles[3]),
  paste0(cuartiles[3]+1, " <-> ", max_fam)
))


#---------------------
# PLOT DATA FREQUENCY
#---------------------
# Plot the frequency of each attribute
plot.data.frequency(data, folder=images_folder)

#---------------------------------------------------
# GENERATE AND ANALYZE DECISION TREE LEARNING MODEL
#---------------------------------------------------

best_model <- NA
best_accuracy <- 0

for(i in 1:10) {
  # Use the whole file as training data
  training_data <- data
  test_data <- data_test
  
  # Create Linear Model using training data. Formula = all the columns except Salary
  model <- rpart(formula = Segmentation~., data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$Segmentation, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  # Get attribute names order by importance
  attr <- names(model$variable.importance)
  
  # Print accuracy and top 5 attributes
  cat(paste0(i, ".- Accuracy = ", round(100*accuracy, digits = 2)),
      "%. Top 5 attributes:\n")
  # Print top 5 attributes
  for (j in 1:5) {
    print(paste0("    Attribute-", j, " = ", attr[j]), quote = FALSE)
  }
  
  # Update best model if accuracy is better
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
  }
}

# Print accurcy and attributes in order of relevance
attrs <- names(best_model$variable.importance)
cat(paste0("Accuracy = ", round(100*best_accuracy, digits = 2)),
    "%\nAttributes ordered by relevance:", paste0("\n  ", attrs))

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(best_model,
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "BuGn",
           shadow.col = "darkgray",
           main = "Client Segmentation Decision Tree",
           sub = paste0("Accuracy = ", round(100*best_accuracy, digits = 2), "%"))

# Print the rules that represent the Decision Tree
rpart.rules(best_model,
            style="tall",
            trace = 0,
            cover = TRUE,
            eq = "==",
            when = "IF",
            and = "&&",
            extra = 4)

#------------------------------
# ANSWER THEORETICAL QUESTIONS
#------------------------------

prediction_best <- predict(best_model, data_test, type = "class")

# Obtén la información de las personas cuya categoría cambie si se modifican sus 
# estudios superiores a “yes” (incluye tanto la categoría previa como la nueva).

data_new_studies <- data_test
data_new_studies$Graduated <- "Yes"
prediction_new_studies <- predict(best_model, data_new_studies, type = "class")

results_new_studies <- data.frame(
  ID = data_new_studies$ID,
  Previous_Segmentation = prediction_best,
  New_Segmentation = prediction_new_studies
)

# Filtrar filas donde Previous_Segmentation != New_Segmentation
results_new_studies <- results_new_studies[results_new_studies$Previous_Segmentation != results_new_studies$New_Segmentation, ]

# Obtén la información de las personas cuya categoría cambie si se modifica su 
# estado matrimonial a “yes” (incluye tanto la categoría previa como la nueva).

data_new_married <- data_test
data_new_married$Ever_Married <- "Yes"
prediction_new_married <- predict(best_model, data_new_married, type = "class")

results_new_married <- data.frame(
  ID = data_new_married$ID,
  Previous_Segmentation = prediction_best,
  New_Segmentation = prediction_new_married
)

# Filtrar filas donde Previous_Segmentation != New_Segmentation
results_new_married <- results_new_married[results_new_married$Previous_Segmentation != results_new_married$New_Segmentation, ]

