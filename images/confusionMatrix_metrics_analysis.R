library(scales)
library(stringr)

city = "hagadera"    # cologne or hagadera
model_prob = "multiclass"     # multiclass or binary
invariance = "scale"     # scale or shape

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","saved_models/",city,"/", model_prob,"/",invariance))

# ==========================================================================================================

# Get the list of file names in the directory
file_names <- list.files(getwd())

# Iterate over each file name
for (file_name in file_names) {
  # Check if the file name contains a "+"
  if (str_detect(file_name, "\\+")) {
    # Create the new file name by replacing "+" with "_"
    new_file_name <- str_replace_all(file_name, "\\+", "_")
    
    # Rename the file
    file.rename(
      from = file.path(getwd(), file_name),
      to = file.path(getwd(), new_file_name)
    )
    
    # Print the renaming action
    cat(sprintf("Renamed: '%s' -> '%s'\n", file_name, new_file_name))
  }
}

# Completion message
cat("File renaming completed.\n")


# ==========================================================================================================




# List all files in the current directory
all_files <- list.files(path = getwd())

# Filter files that contain "confusionMatrix" after the date
filtered_files <- all_files[grep("^\\d{8}_confusionMatrix", all_files)]

# Extract the model name from the file names (remove date and seed)
model_names <- unique(gsub("^\\d{8}_confusionMatrix_(.+)_\\d+seed\\.rds$", "\\1", filtered_files))

# Create a list to store grouped files by model name
confusionMatrix_list <- list()

# Group files by model and store in a named list
for (model in model_names) {
  model_files <- filtered_files[grep(paste0("_confusionMatrix_", model, "_"), filtered_files)]
  # Store the model files in the list using the model name as the key
  confusionMatrix_list[[model]] <- model_files
}

# Assign the list to variables named after the model names
for (model in model_names) {
  assign(model, confusionMatrix_list[[model]])
}

# Print the variables created
# print(ls(pattern = paste(model_names, collapse = "|")))



# Function to extract metrics from confusion matrix
extract_metrics <- function(confusion_matrix) {
  # Initialize metrics
  metrics <- list()
  
  # Extract Overall Accuracy and Kappa
  if (!is.null(confusion_matrix$overall)) {
    metrics$OA <- confusion_matrix$overall["Accuracy"]
    metrics$Kappa <- confusion_matrix$overall["Kappa"]
  } else {
    metrics$OA <- NA
    metrics$Kappa <- NA
  }
  
  # Extract class-wise metrics (F1 and Balanced Accuracy)
  if (!is.null(confusion_matrix$byClass)) {
    metrics$F1 <- colMeans(confusion_matrix$byClass[, "F1", drop = FALSE], na.rm = TRUE) # Mean F1 for all classes
    metrics$Balanced_Accuracy <- colMeans(confusion_matrix$byClass[, "Balanced Accuracy", drop = FALSE], na.rm = TRUE) # Mean BA for all classes
    metrics$class_F1 <- confusion_matrix$byClass[, "F1"] # F1 for each class
    metrics$class_BA <- confusion_matrix$byClass[, "Balanced Accuracy"] # BA for each class
  } else {
    metrics$F1 <- NA
    metrics$Balanced_Accuracy <- NA
    metrics$class_F1 <- NA
    metrics$class_BA <- NA
  }
  
  return(metrics)
}

# Dictionary to store results
results <- list()

closest_metrics_files <- list()

# Initialize the maximum metrics to compare across models
max_OA <- -Inf
max_F1 <- -Inf
max_Kappa <- -Inf

# Process each model
for (model in names(confusionMatrix_list)) {
  model_files <- confusionMatrix_list[[model]]
  
  # Store metrics for each file
  OAs <- c()
  Kappas <- c()
  F1s <- c()
  Balanced_Accuracies <- c()
  class_F1s <- list()
  class_BAs <- list()
  
  metrics_to_files <- list(OA = list(), F1 = list(), Kappa = list())
  
  for (file in model_files) {
    # Load the confusion matrix from the file
    confusion_matrix <- readRDS(file)
    
    # Extract metrics
    metrics <- extract_metrics(confusion_matrix)
    OAs <- c(OAs, metrics$OA)
    Kappas <- c(Kappas, metrics$Kappa)
    F1s <- c(F1s, metrics$F1)
    Balanced_Accuracies <- c(Balanced_Accuracies, metrics$Balanced_Accuracy)
    
    # Map metrics to files
    metrics_to_files$OA[[file]] <- metrics$OA
    metrics_to_files$F1[[file]] <- metrics$F1
    metrics_to_files$Kappa[[file]] <- metrics$Kappa
    
    # Aggregate per-class metrics
    for (class_name in names(metrics$class_F1)) {
      class_F1s[[class_name]] <- c(class_F1s[[class_name]], metrics$class_F1[class_name])
      class_BAs[[class_name]] <- c(class_BAs[[class_name]], metrics$class_BA[class_name])
    }
  }
  
  # # Identify the files corresponding to the max values
  # max_OA_file <- names(which.max(metrics_to_files$OA))
  # max_F1_file <- names(which.max(metrics_to_files$F1))
  # max_Kappa_file <- names(which.max(metrics_to_files$Kappa))
  
  # Compute mean and standard deviation for each metric
  results[[model]] <- list(
    mean_OA = mean(OAs, na.rm = TRUE),
    sd_OA = sd(OAs, na.rm = TRUE),
    mean_Kappa = mean(Kappas, na.rm = TRUE),
    sd_Kappa = sd(Kappas, na.rm = TRUE),
    mean_F1 = mean(F1s, na.rm = TRUE),
    sd_F1 = sd(F1s, na.rm = TRUE),
    mean_BA = mean(Balanced_Accuracies, na.rm = TRUE),
    sd_BA = sd(Balanced_Accuracies, na.rm = TRUE),
    class_F1 = lapply(class_F1s, function(x) list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))),
    class_BA = lapply(class_BAs, function(x) list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))),
    files = model_files#,
    # max_OA = max(OAs, na.rm = TRUE),
    # max_OA_file = max_OA_file,
    # max_F1 = max(F1s, na.rm = TRUE),
    # max_F1_file = max_F1_file,
    # max_Kappa = max(Kappas, na.rm = TRUE),
    # max_Kappa_file = max_Kappa_file
  )
  
  # Calculate the mean metrics for the model
  mean_OA <- mean(OAs, na.rm = TRUE)
  mean_F1 <- mean(F1s, na.rm = TRUE)
  mean_Kappa <- mean(Kappas, na.rm = TRUE)
  
  # Find the file with the closest metrics to the means
  abs_diff_OA <- abs(unlist(metrics_to_files$OA) - mean_OA)
  # abs_diff_F1 <- abs(unlist(metrics_to_files$F1) - mean_F1)
  # abs_diff_Kappa <- abs(unlist(metrics_to_files$Kappa) - mean_Kappa)
  
  closest_OA_file <- sub("\\.Accuracy$", "", names(which.min(abs_diff_OA)))  # Remove ".Accuracy" from the file name
  # closest_F1_file <- names(which.min(abs_diff_F1))
  # closest_Kappa_file <- names(which.min(abs_diff_Kappa))
  
  # Store the results
  closest_metrics_files[[model]] <- list(
    closest_OA_file = closest_OA_file,
    closest_OA = metrics_to_files$OA[[closest_OA_file]]#,
    
    # mean_F1 = mean_F1,
    # closest_F1_file = closest_F1_file,
    # closest_F1 = metrics_to_files$F1[[closest_F1_file]],
    # 
    # mean_Kappa = mean_Kappa,
    # closest_Kappa_file = closest_Kappa_file,
    # closest_Kappa = metrics_to_files$Kappa[[closest_Kappa_file]]
  )
  
  
}

# Print results for each model
for (model in names(results)) {
  cat(sprintf("Model: %s\n", model))
  cat(sprintf(" OA: %.2f ± %.2f\n", results[[model]]$mean_OA * 100, results[[model]]$sd_OA * 100))
  cat(sprintf(" Kappa: %.2f ± %.2f\n", results[[model]]$mean_Kappa * 100, results[[model]]$sd_Kappa * 100))
  cat(sprintf(" F1 (All Classes): %.2f ± %.2f\n", results[[model]]$mean_F1 * 100, results[[model]]$sd_F1 * 100))
  cat(sprintf(" Balanced Accuracy (All Classes): %.2f ± %.2f\n", results[[model]]$mean_BA * 100, results[[model]]$sd_BA * 100))
  cat("  Per-Class Metrics:\n")
  
  for (class_name in names(results[[model]]$class_F1)) {
    class_metrics <- results[[model]]$class_F1[[class_name]]
    class_BA <- results[[model]]$class_BA[[class_name]]
    cat(sprintf("  %s\n", class_name))
    cat(sprintf("        F1: %.2f ± %.2f\n", class_metrics$mean * 100, class_metrics$sd * 100))
    cat(sprintf("        Balanced Accuracy: %.2f ± %.2f\n", class_BA$mean * 100, class_BA$sd * 100))
  }
  # cat(sprintf("Max OA: %.2f (File: %s)\n", results[[model]]$max_OA, results[[model]]$max_OA_file))
  # cat(sprintf("Max F1: %.2f (File: %s)\n", results[[model]]$max_F1, results[[model]]$max_F1_file))
  # cat(sprintf("Max Kappa: %.2f (File: %s)\n", results[[model]]$max_Kappa, results[[model]]$max_Kappa_file))
  cat(sprintf("Closest OA file: %s\n Closest OA value: %.4f\n", 
              closest_metrics_files[[model]]$closest_OA_file, 
              closest_metrics_files[[model]]$closest_OA))
  
  # cat(sprintf("Closest F1 file: %s\n Closest F1 value: %.4f\n", 
  #             closest_metrics_files[[model]]$closest_F1_file, 
  #             closest_metrics_files[[model]]$closest_F1))
  # 
  # cat(sprintf("Closest Kappa file: %s\n Closest Kappa value: %.4f\n\n", 
  #             closest_metrics_files[[model]]$closest_Kappa_file, 
  #             closest_metrics_files[[model]]$closest_Kappa))
  
  cat("\n\n\n")
}





