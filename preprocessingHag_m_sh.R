script = "preproc"  # -> 
#####################################################  Libraries  ################################################
library(caret)
# library(kernlab)
# library(sampling)
library(data.table)
##### #############################################################################################################

city = c("hagadera")    # cologne or hagadera location
invariance = c("shape")   # scale or shape invariance
model_prob = c("binary")  # multiclass or binary problem

path = '/home/data1/Lorenzo/'
#####################################################  Utils  ####################################################
# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #
lgtS=TRUE

if(!dir.exists(path)){path = "D:/"}

classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}
##################################################################################################################

      
################################################@@  Preprocessing  #############################@@@@##############

# # To load them back:
setwd(paste0(path, "tunc_oz/apply_model/", "rds_data_r_import/",city,"/",invariance))
if (file.exists(paste0(city,"_",model_prob,"_",invariance,"_trainDataPoolAllLev.rds"))) {
  
  cat("loading",city,model_prob,invariance,"\n")
  trainDataPoolAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_trainDataPoolAllLev.rds"))
  testDataAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_testDataAllLev.rds"))
  validateDataAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_validateDataAllLev.rds"))
  
} else {
  
  cat("preprocessing",city,model_prob,invariance,"\n")
  
  if (city=="hagadera") {
    
    if (invariance=="shape") {
      # Constants
      numFeat <- 26
      objInfoNames <- c("Lx_g_comp", "Lx_g_elfi", "Lx_g_refi", "Lx_g_roun", "Lx_g_shin",
                        "Lx_m_cb", "Lx_m_bl", "Lx_m_gr", "Lx_m_y", "Lx_m_reg", "Lx_m_nir2", "Lx_m_ndvi", "Lx_m_nir", "Lx_m_re",
                        "Lx_s_cb", "Lx_s_bl", "Lx_s_gr", "Lx_s_y", "Lx_s_reg", "Lx_s_nir2", "Lx_s_ndvi", "Lx_s_nir", "Lx_s_re",
                        "Lx_t_diss", "Lx_t_hom", "Lx_t_mean", "label")
      
      sindexSVMDATA <- 1
      eindexSVMDATA <- sindexSVMDATA + numFeat - 1
      
      # Column classes for input files
      columnclass <- c("NULL", rep(NA, 26), "factor", "integer")
      columnclass2 <- c(NA, NA, "factor", rep(NA, 26), "factor")

      # Paths and file loading
      setwd(paste0(path, "tunc_oz/apply_model/csv_data_r_import/", city, "/shape1"))
      inputPath <- "base_level_complete.csv"
      
      # Load data using data.table for efficient processing
      cat("loading file:", inputPath, "\n")
      generalDataPool_scale <- fread(inputPath,header = T, sep = ";", colClasses = columnclass)
      
      setwd(paste0(path, "tunc_oz/apply_model/csv_data_r_import/", city, "/", invariance))
      files <- list.files(pattern = "hagadera_s25_.*_allclass_use.csv")
    
      # process_file <- function(file, col_classes) {
      #   data <- fread(file, sep = ";", colClasses = col_classes)
      #   cat(ncol(data))
      #   if (ncol(data) < 30) {
      #     stop(paste("File", file, "has fewer than 30 columns"))
      #   }
      #   data <- data[, .(1:2, 4:29, 3, 30)] # Reorder columns
      #   setnames(data, 3:30, c(objInfoNames[-length(objInfoNames)],"REF","use" ))
      #   data[, `:=`(
      #     S9C1T_DISS = as.numeric(S9C1T_DISS),
      #     S9C1T_HOM = as.numeric(S9C1T_HOM),
      #     S9C1T_MEA = as.numeric(S9C1T_MEA)
      #   )]
      #   data <- data[REF != "unclassified"]
      #   data[, REF := factor(REF)]
      #   return(data)
      # }
      
      process_file <- function(file, col_classes) {
        
        data <- fread(file, header = T, sep = ";", colClasses = col_classes)
        
        cat("loading file:", file," - number of columns:", ncol(data), "\n")

        # Check for expected number of columns
        if (ncol(data) != 30) {
          stop(paste("File", file, "has an unexpected number of columns:", ncol(data)))
        }

        # Reorder columns and rename
        # data <- data[, .(1:2, 4:29, 3, 30)]
        setcolorder(data, c(1, 2, 4:29, 3, 30))
        # setnames(data, 3:30, c(objInfoNames[-length(objInfoNames)], "REF", "use"))

        # Convert specific columns to numeric
        # data[, `:=`(
        #   S9C1T_DISS = as.numeric(S9C1T_DISS),
        #   S9C1T_HOM = as.numeric(S9C1T_HOM),
        #   S9C1T_MEA = as.numeric(S9C1T_MEA)
        # )]

        # Filter out unclassified rows
        data <- data[REF != "unclassified"]
        data[, REF := factor(REF)]

        return(data)
      }
      
      

      
      
      # Load and process data for each file
      processed_data_list <- lapply(files, process_file, col_classes = columnclass2)
      
      # Bind processed data
      generalDataPool <- rbindlist(list(
        setnames(generalDataPool_scale[, 1:28], c(objInfoNames[-length(objInfoNames)], "REF", "use")),
        rbindlist(lapply(processed_data_list, function(dt) setnames(dt[, 3:30], c(objInfoNames[-length(objInfoNames)], "REF", "use"))))
      ))  
      
      # Convert character columns to numeric
      char_columns <- which(sapply(generalDataPool[, 1:(ncol(generalDataPool) - 2)], class) == "character")
      generalDataPool[, (char_columns) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = char_columns]
      
      if (model_prob == "binary") {
        generalDataPool <- classificationProblem(generalDataPool)
      }
      
      # Scaling data
      normalizedFeat <- generalDataPool[, 1:(ncol(generalDataPool) - 2)]
      normalizedLabelUSE <- generalDataPool[, (ncol(generalDataPool) - 1):ncol(generalDataPool)]
      rm(generalDataPool)
      
      # Batch-wise normalization
      normalizedFeatBatches <- list()
      for (i in 1:numFeat) {
        batch <- normalizedFeat[, (i * numFeat + 1):((i + 1) * numFeat)]
        preProc <- preProcess(batch, method = "range")
        normalizedFeatBatches[[i]] <- predict(preProc, batch) 
      } 
      
      # Combine normalized features
      normalizedFeat <- do.call(cbind, normalizedFeatBatches)
      rm(normalizedFeatBatches)
      
      generalDataPoolfinal_shape <- cbind(normalizedFeat, normalizedLabelUSE)
      
      # Splitting and sampling
      splitdf <- split(generalDataPoolfinal_shape, generalDataPoolfinal_shape$use)
      trainDataPoolAllLev <- as.data.frame(splitdf[[1]])
      testDataAllLev <- as.data.frame(splitdf[[2]])
      validateDataAllLev <- as.data.frame(splitdf[[3]])
      rm(splitdf, generalDataPoolfinal_shape)
      
      # Save intermediate results to disk
      setwd(paste0(path, "tunc_oz/apply_model/rds_data_r_import/", city, "/", invariance))
      saveRDS(trainDataPoolAllLev, paste0(city, "_", model_prob, "_", invariance, "_trainDataPoolAllLev.rds"))
      saveRDS(testDataAllLev, paste0(city, "_", model_prob, "_", invariance, "_testDataAllLev.rds"))
      saveRDS(validateDataAllLev, paste0(city, "_", model_prob, "_", invariance, "_validateDataAllLev.rds"))
      
      # Final processing steps
      validateLabels <- validateDataAllLev[, ncol(validateDataAllLev)]
      validateFeatsub <- validateDataAllLev[, sindexSVMDATA:eindexSVMDATA]
      trainDataPoolAllLev <- trainDataPoolAllLev[order(trainDataPoolAllLev[, ncol(trainDataPoolAllLev)]), ]
      
      # Clean-up
      rm(validateDataAllLev, normalizedFeat, normalizedLabelUSE)
    }
  }
  cat("preprocessed data: stored ")
}
