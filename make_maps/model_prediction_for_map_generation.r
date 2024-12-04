script = "ALTSLmaps"  # ALTSL_Maps_Generation_and_Model_Application
#####################################################  Libraries  ################################################
library(caret)
library(kernlab)
library(sampling)
library(ggplot2)
library(dplyr)
##################################################################################################################

city = "hagadera"    # cologne or hagadera location
invariance = "scale"   # scale or shape invariance
model_prob = "multiclass"  # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
sampleSizePor = c(100)

path = "D:/" 
# if(!dir.exists(path)){path = '/home/data1/Lorenzo/'}
###############################################  Preprocessing  ############################################

# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #

cat("preprocessing",city,model_prob,invariance,"\n") #

if(city=="cologne"){ 
  sampleSizePor = c(120)} 
if(model_prob=="binary"){ 
  sampleSizePor = c(40)
}

classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}

plot_confusion_matrix <- function(cm, filename = "confusion_matrix.png") {
  # Extract confusion matrix as a table
  cm_table <- as.table(cm$table)
  
  # Convert table to data frame for ggplot
  cm_df <- as.data.frame(cm_table)
  colnames(cm_df) <- c("Reference", "Prediction", "Frequency")
  
  # Create the plot
  p <- ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue") +
    geom_text(aes(label = Frequency), vjust = 1) +
    labs(title = "Confusion Matrix", fill = "Frequency") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 15)
    )
  
  # Save the plot as a PNG file
  ggsave(filename = filename, plot = p, width = 7, height = 6, dpi = 300)
}

# # To load them back:
setwd(paste0(path, "tunc_oz/apply_model/", "rds_data_r_import/",city))
if (file.exists(paste0(city,"_",model_prob,"_normBaseLevData.rds"))) {
  
  cat("loading",city,model_prob,invariance,"dataset\n")
  
  # normBaseLevFeatMap <- readRDS(paste0(city,"_",model_prob,"_normBaseLevFeatMap.rds"))
  # datLabel <- readRDS(paste0(city,"_",model_prob,"_datLabel.rds"))
  
  normBaseLevData <- readRDS(paste0(city,"_",model_prob,"_normBaseLevData.rds"))
  normBaseLevFeatMap = normBaseLevData[,1:(ncol(normBaseLevData)-2)]
  dataLabelMap = normBaseLevData[,(ncol(normBaseLevData)-1)]
  rm(normBaseLevData)
  
  validateBaseLevData <- readRDS(paste0(city,"_",model_prob,"_validateBaseLevData.rds"))
  validateFeat = validateBaseLevData[,1:(ncol(validateBaseLevData)-2)]
  validateLabel = validateBaseLevData[,(ncol(validateBaseLevData)-1)]
  rm(validateBaseLevData)
  
} else {
  
if (city=="cologne") {
  
  ########################################  Input
  
  numFeat = 18                                            # number of features per level (dimensionality)
  
  #names to use in rbind() of VSV                         # 18 features names + 19.label
  objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                    "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                    "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
                    "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                    "label")
  
  sindexSVMDATA = 37        # start of baseline model with one segmentation scale data
  eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
  
  inputPath ="cologne_res_100_L2-L13.csv" 
  
  #import format; "NULL" for subset of data on only some level (speed up import)
  columnclass = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  "factor","factor")
  
  # import data
  setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/scale"))
  generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnclass)
  
  ########################################  Input  ########################################
  
  # exclude unclassified and delete level of factor
  # generalDataPool = subset(generalDataPool, REF != "unclassified")
  generalDataPool$REF <- factor(generalDataPool$REF)
  # generalDataPool <- na.omit(generalDataPool) 
  
  if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
    generalDataPool=classificationProblem(generalDataPool)
  }
  ###################################################  Scaling  ################################################
  
  dataFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
  dataLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]
  
  preProc = preProcess(setNames(dataFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
  # dataFeatBase = predict(preProc, setNames(dataFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # **************************************** data for map visualization ****************************************
  normBaseLevFeatMap = predict(preProc, setNames(generalDataPool[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # ************************************************************************************************************
  
  #########################################################################################
  
} else if (city=="hagadera") {

  ########################################  Input
  
  numFeat = 26                   # number of features per level (dimensionality)
  
  #names to use in rbind() of VSV                # 26 features names + 19.label
  objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                    "Lx_m_cb","Lx_m_bl","Lx_m_gr","Lx_m_y","Lx_m_reg","Lx_m_nir2","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                    "Lx_s_cb","Lx_s_bl","Lx_s_gr","Lx_s_y","Lx_s_reg","Lx_s_nir2","Lx_s_ndvi","Lx_s_nir","Lx_s_re",
                    "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                    "label")
  
  sindexSVMDATA = 53                                      # start of baseline model with one segmentation scale data
  eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
  
  inputPath ="hagadera_all_level_scale_specgeomtex.csv"  
  
  columnclass = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  "factor","integer")
  
  setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/scale"))
  
  # import data
  generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnclass)
  colnames(generalDataPool)[209] = "REF"
  
  # generalDataPool = generalDataPool[c(sindexSVMDATA:eindexSVMDATA, (ncol(generalDataPool)-1):(ncol(generalDataPool)))]
  # colnames(generalDataPool)[(ncol(generalDataPool)-1)] = "REF"
  
  # exclude unclassified and delete level of factor
  # generalDataPool = subset(generalDataPool, REF != "unclassified")
  generalDataPool$REF <- factor(generalDataPool$REF)
  # generalDataPool <- na.omit(generalDataPool)
  
  char_columns <- which(sapply(generalDataPool[,1:(ncol(generalDataPool)-1)], class) == "character")
  generalDataPool[char_columns] <- lapply(generalDataPool[char_columns], function(x) as.numeric(as.character(x)))
  unique(sapply(generalDataPool[,1:(ncol(generalDataPool)-1)], class))
  rm(char_columns)
  
  if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
    generalDataPool=classificationProblem(generalDataPool)
  }
  ###################################################  Scaling  ################################################

  dataFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
  dataLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]
  rm(generalDataPool)
  
  preProc = preProcess(setNames(dataFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
  # dataFeatBase = predict(preProc, setNames(dataFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # **************************************** data for map visualization ****************************************
  normBaseLevFeatMap = predict(preProc, setNames(dataFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # ************************************************************************************************************
  rm(dataFeat)
  
  #############################################################################################################
} else { 
  stop(paste("Wrong combination of city, invariance and classification problem")) 
}


############################################# Splitting & Sampling #############################################
# Split data in test, train and validate data
normalized_dataPool <- cbind(normBaseLevFeatMap , dataLabelUSE)

setwd(paste0(path, "tunc_oz/apply_model/", "rds_data_r_import/",city))

# saveRDS(normBaseLevFeatMap, paste0(city,"_",model_prob,"_normBaseLevFeatMap.rds"))
# saveRDS(dataLabelMap, paste0(city,"_",model_prob,"_dataLabelMap.rds"))

# saveRDS(cbind(normBaseLevFeatMap , dataLabelUSE), paste0(city,"_",model_prob,"_normBaseLevData.rds"))
saveRDS(normalized_dataPool, paste0(city,"_",model_prob,"_normBaseLevData.rds"))



normalized_dataPool = subset(normalized_dataPool, REF != "unclassified")
normalized_dataPool$REF <- factor(normalized_dataPool$REF)
splitdf <- split(normalized_dataPool, normalized_dataPool$USE)

validateBaseLevData = as.data.frame(splitdf[[3]])

saveRDS(validateBaseLevData, paste0(city,"_",model_prob,"_validateBaseLevData.rds"))

dataLabelMap <- dataLabelUSE[dataLabelUSE$REF != "unclassified", ]
dataLabelMap <- droplevels(dataLabelMap[,1])


validateFeat = validateBaseLevData[,1:(ncol(validateBaseLevData)-2)]
validateLabel = validateBaseLevData[,(ncol(validateBaseLevData)-1)]

rm(splitdf,normalized_dataPool,validateBaseLevData)

cat("preprocessed data: stored ")
}



# if(sum(is.na(normBaseLevFeatMap ))>0){
#   cat(sum(is.na(normBaseLevFeatMap )))
#   # Loop through all columns in normBaseLevFeatMap and apply mean imputation
#   for (col in names(normBaseLevFeatMap )) {
#     # Check if there are any missing values in the column
#     if (any(is.na(normBaseLevFeatMap [[col]]))) {
#       # Replace NA with the mean of the column (excluding NA values)
#       normBaseLevFeatMap [[col]][is.na(normBaseLevFeatMap [[col]])] <- mean(normBaseLevFeatMap [[col]], na.rm = TRUE)
#     }
#   }
#   cat(sum(is.na(normBaseLevFeatMap )))
# }






###############################################  Map Prediction  #################################################

# Define models and metadata in a list
models <- list(
  list(
    # model_file = "20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds",
    # model_file = "20241123SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_71seed.rds",
    model_file = "20241117SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_130seed.rds",
    model_name = "SVM"
  ),
  list(
    # model_file <- "20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds",
    # model_file = "20241123SVM_SLUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds",
    model_file = "20241117SVM_SLUn_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_130seed.rds",
    model_name = "SVM_SLUn"
  ),
  list(
    # model_file = "20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds",
    # model_file = "20241117VSVM_SL_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_55seed.rds",
    model_file = "20241117VSVM_SL_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_130seed.rds",
    model_name = "VSVM_SL"
  ),
  list(
    # model_file = "20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds",
    # model_file = "20241119VSVM_SLUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_65seed.rds",
    model_file = "20241117VSVM_SLUn_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_149seed.rds",
    model_name = "VSVM_SLUn"
  ),
  list(
    # model_file = "20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds),
    # model_file = "20241119VSVM_SLvUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_65seed.rds",
    model_file = "20241117VSVM_SLvUn_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_112seed.rds",
    model_name = "VSVM_SLvUn"
  ),
  list(
    # model_file = "20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds",
    # model_file = "20241123AL_MS_kmeans_Train_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_121seed.rds",
    model_file = "20241117AL_MS_kmeans_Train_SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_130seed.rds",
    model_name = "AL_MS+kmeans+Train_SVM"
  ),
  list(
    # model_file = "20241119AL_MS_tSNE_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds",
    # model_file = "20241123AL_MS_tSNE_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds",
    model_file = "20241117AL_MS_tSNE_SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_97seed.rds",
    model_name = "AL_MS+tSNE_SVM"
    ),
  list(

    # model_file = "20241121AL_MS_tSNE_SL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_69seed.rds",
    # model_file = "20241118AL_MS_tSNE_SL_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_147seed.rds",
    model_file = "20241117AL_MS_tSNE_SL_SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_65seed.rds",
    model_name = "AL_MS+tSNE+SL_SVM"
  ),
  list(
    # model_file = "20241121AL_MS_kmeans_semiSL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds",
    # model_file = "20241123AL_MS_kmeans_semiSL_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_53seed.rds",
    model_file = "20241117AL_MS_kmeans_semiSL_SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_65seed.rds",
    model_name = "AL_MS+kmeans+semiSL_SVM"
  ),
  list(
    # model_file = "20241119AL_MCLU_kmeans_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_113seed.rds",
    # model_file = "20241123AL_MCLU_kmeans_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds",
    model_file = "20241117AL_MCLU_kmeans_SVM_hagadera_multiclass_scale_ALTSLv3_100sampleSizePor_20Unl_realiz1_69seed.rds",
    model_name = "AL_MCLU+kmeans_SVM"
  )
)

# Iterate over the models
for (model_info in models) {
  # Load the model
  model_path <- paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/", city, "/", model_prob, "/", invariance, "/", model_info$model_file)
  tunedModel <- readRDS(model_path)
  
  # Perform prediction
  start.time <- Sys.time()
  predLabelMap <- predict(tunedModel, na.omit(normBaseLevFeatMap))
  cat("Execution time for", model_info$model_name, ":", round(as.numeric((Sys.time() - start.time), units = "secs"), 2), "sec\n")
  
  # Convert predictions to data frame
  predLabelMap <- as.data.frame(predLabelMap)
  
  # Validate predictions
  predValidateLabel <- predict(tunedModel, validateFeat)
  acc <- confusionMatrix(predValidateLabel, validateLabel)
  
  # Save confusion matrix plot
  setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/", city, "/", model_prob, "/", invariance))
  plot_confusion_matrix(acc, filename = paste(city, invariance, model_prob, model_info$model_name, "confusion_matrix.png", sep = "_"))
  
  # Save prediction results to CSV
  setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/", city))
  outputfile <- paste0(format(Sys.time(), "%Y%m%d"), model_info$model_name, "_", city, "_", model_prob, "_", invariance, "_", sampleSizePor[1], "Size_", b, "Unl_samples.csv")
  write.csv2(predLabelMap, file = outputfile, sep = ";", row.names = TRUE, col.names = FALSE)
  
  cat("Processed:", model_info$model_name, "\n")
}

cat("All models processed successfully.\n")
































# ############################################################
# ############################################################
# ##                      apply SVM                         ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
# 
# model_name = "SVM"
# 
# # tunedSVM <- readRDS("20240924SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedSVM <- readRDS("20240927SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedSVM <- readRDS("20240921SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedSVM <- readRDS("20240920SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# tunedSVM <- readRDS("20240926SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedSVM <- readRDS("20240925SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedSVM <- readRDS("20240924SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedSVM <- readRDS("20240927SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedSVM <- readRDS("20241123SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_71seed.rds")
# # tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# 
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedSVM, na.omit(normBaseLevFeatMap) )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accSVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# 
# predValidateLabel = predict(tunedSVM, validateFeat)
# accSVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accSVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accSVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# ############################################################
# ############################################################
# ##                      apply SVM_SLUn                   ##  
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "SVM_SLUn"
# 
# # tunedSVMUn <- readRDS("20240924SVM_SLUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedSVMUn <- readRDS("20240927SVM_SLUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedSVMUn <- readRDS("20240921SVM_SLUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedSVMUn <- readRDS("20240920SVM_SLUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedSVMUn <- readRDS("20240926SVM_SLUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedSVMUn <- readRDS("20240925SVM_SLUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedSVMUn <- readRDS("20240924SVM_SLUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedSVMUn <- readRDS("20240927SVM_SLUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedSVMUn <- readRDS("20241123SVM_SLUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedSVMUn, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")  
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accSVM_SLUn_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedSVMUn, validateFeat)
# accSVM_SLUn = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accSVM_SLUn_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accSVM_SLUn, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##                      apply VSVM_SL                     ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "VSVM_SL"
# 
# # tunedVSVMSL <- readRDS("20240924VSVM_SL_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSL <- readRDS("20240927VSVM_SL_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSL <- readRDS("20240921VSVM_SL_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSL <- readRDS("20240920VSVM_SL_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedVSVMSL <- readRDS("20240926VSVM_SL_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedVSVMSL <- readRDS("20240925VSVM_SL_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSL <- readRDS("20240924VSVM_SL_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedVSVMSL <- readRDS("20240927VSVM_SL_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedVSVMSL <- readRDS("20241117VSVM_SL_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_55seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedVSVMSL, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accVSVM_SL_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedVSVMSL, validateFeat)
# accVSVM_SL = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accVSVM_SL_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accVSVM_SL, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# 
# ############################################################
# ############################################################
# ##                      apply VSVM_SLUn                  ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "VSVM_SLUn"
# 
# # tunedVSVMSLUn <- readRDS("20240924VSVM_SLUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLUn <- readRDS("20240927VSVM_SLUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLUn <- readRDS("20240921VSVM_SLUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLUn <- readRDS("20240920VSVM_SLUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedVSVMSLUn <- readRDS("20240926VSVM_SLUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedVSVMSLUn <- readRDS("20240925VSVM_SLUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLUn <- readRDS("20240924VSVM_SLUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedVSVMSLUn <- readRDS("20240927VSVM_SLUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedVSVMSLUn <- readRDS("20241119VSVM_SLUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_65seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedVSVMSLUn, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accVSVM_SLUn_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedVSVMSLUn, validateFeat)
# accVSVM_SLUn = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accVSVM_SLUn_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accVSVM_SLUn, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# 
# ############################################################
# ############################################################
# ##                      apply VSVM_SLvUn                 ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "VSVM_SLvUn"
# 
# # tunedVSVMSLvUn <- readRDS("20240924VSVM_SLvUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240927VSVM_SLvUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240921VSVM_SLvUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240920VSVM_SLvUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240926VSVM_SLvUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240925VSVM_SLvUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240924VSVM_SLvUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedVSVMSLvUn <- readRDS("20240927VSVM_SLvUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedVSVMSLvUn <- readRDS("20241119VSVM_SLvUn_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_65seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedVSVMSLvUn, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accVSVM_SLvUn_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedVSVMSLvUn, validateFeat)
# accVSVM_SLvUn = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accVSVM_SLvUn_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accVSVM_SLvUn, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##              apply AL_MS+kmeans+Train_SVM              ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "AL_MS+kmeans+Train_SVM"
# 
# # tunedALTSVM <- readRDS("20240924AL_MS+kmeans+Train_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALTSVM <- readRDS("20240927AL_MS+kmeans+Train_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALTSVM <- readRDS("20240921AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALTSVM <- readRDS("20240920AL_MS+kmeans+Train_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedALTSVM <- readRDS("20240926AL_MS+kmeans+Train_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedALTSVM <- readRDS("20240925AL_MS+kmeans+Train_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALTSVM <- readRDS("20240924AL_MS+kmeans+Train_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedALTSVM <- readRDS("20240927AL_MS+kmeans+Train_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedALTSVM <- readRDS("20241123AL_MS_kmeans_Train_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_121seed.rds")
# tunedALTSVM <- readRDS("20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedALTSVM, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accALTSVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedALTSVM, validateFeat)
# accALTSVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accALTSVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accALTSVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##                      apply AL_MS+tSNE_SVM              ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "AL_MS+tSNE_SVM"
# 
# # tunedALtSNESVM <- readRDS("20240924AL_MS+tSNE_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESVM <- readRDS("20240927AL_MS+tSNE_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESVM <- readRDS("20240921AL_MS+tSNE_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESVM <- readRDS("20240920AL_MS+tSNE_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedALtSNESVM <- readRDS("20240926AL_MS+tSNE_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedALtSNESVM <- readRDS("20240925AL_MS+tSNE_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESVM <- readRDS("20240924AL_MS+tSNE_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedALtSNESVM <- readRDS("20240927AL_MS+tSNE_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedALtSNESVM <- readRDS("20241123AL_MS_tSNE_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds")
# tunedALtSNESVM <- readRDS("20241119AL_MS_tSNE_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedALTSVM <- readRDS("20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedALtSNESVM, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")  
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accALtSNESVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedALtSNESVM, validateFeat)
# accALtSNESVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accALtSNESVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accALtSNESVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##                      apply AL_MS+tSNE+SL_SVM           ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "AL_MS+tSNE+SL_SVM"
# model_name = "AL_MS+kmeans+semiSL_SVM"
# model_name = "AL_MCLU+kmeans_SVM"
# # tunedALtSNESLSVM <- readRDS("20240924AL_MS+tSNE+SL_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240927AL_MS+tSNE+SL_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240921AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240920AL_MS+tSNE+SL_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240926AL_MS+tSNE+SL_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240925AL_MS+tSNE+SL_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240924AL_MS+tSNE+SL_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedALtSNESLSVM <- readRDS("20240927AL_MS+tSNE+SL_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedALtSNESLSVM <- readRDS("20241118AL_MS_tSNE_SL_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_147seed.rds")
# tunedALtSNESLSVM <- readRDS("20241121AL_MS_tSNE_SL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_69seed.rds")
# tunedALtSNESVM <- readRDS("20241119AL_MS_tSNE_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedALTSVM <- readRDS("20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedALtSNESLSVM, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")     
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accALtSNESLSVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedALtSNESLSVM, validateFeat)
# accALtSNESLSVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accALtSNESLSVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accALtSNESLSVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##               apply AL_MS+kmeans+semiSL_SVM            ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "AL_MS+kmeans+semiSL_SVM"
# 
# # tunedALsemiSLSVM <- readRDS("20240924AL_MS+kmeans+semiSL_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240927AL_MS+kmeans+semiSL_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240921AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240920AL_MS+kmeans+semiSL_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240926AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240925AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240924AL_MS+kmeans+semiSL_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedALsemiSLSVM <- readRDS("20240927AL_MS+kmeans+semiSL_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedALsemiSLSVM <- readRDS("20241123AL_MS_kmeans_semiSL_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_53seed.rds")
# tunedALsemiSLSVM <- readRDS("20241121AL_MS_kmeans_semiSL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedALtSNESLSVM <- readRDS("20241121AL_MS_tSNE_SL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_69seed.rds")
# tunedALtSNESVM <- readRDS("20241119AL_MS_tSNE_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedALTSVM <- readRDS("20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedALsemiSLSVM, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accALsemiSLSVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedALsemiSLSVM, validateFeat)
# accALsemiSLSVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accALsemiSLSVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accALsemiSLSVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# ############################################################
# ##                  apply AL_MCLU+kmeans_SVM              ##
# ############################################################
# ############################################################
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/", model_prob,"/",invariance))
# 
# model_name = "AL_MCLU+kmeans_SVM"
# # name_prova = "20240924AL_MCLU+kmeans_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds"
# # name_prova2 = paste0("20240924",model_name,"_",city,"_",model_prob,"_",invariance,"_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# 
# # tunedALMCLUSVM <- readRDS("20240924AL_MCLU+kmeans_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALMCLUSVM <- readRDS("20240927AL_MCLU+kmeans_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALMCLUSVM <- readRDS("20240921AL_MCLU+kmeans_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
# # tunedALMCLUSVM <- readRDS("20240920AL_MCLU+kmeans_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
# # tunedALMCLUSVM <- readRDS("20240926AL_MCLU+kmeans_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
# # tunedALMCLUSVM <- readRDS("20240925AL_MCLU+kmeans_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
# # tunedALMCLUSVM <- readRDS("20240924AL_MCLU+kmeans_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
# # tunedALMCLUSVM <- readRDS("20240927AL_MCLU+kmeans_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
# 
# # tunedALMCLUSVM <- readRDS("20241123AL_MCLU_kmeans_SVM_cologne_multiclass_scale_ALTSLv3_120sampleSizePor_20Unl_realiz1_111seed.rds")
# tunedALMCLUSVM <- readRDS("20241119AL_MCLU_kmeans_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_113seed.rds")
# tunedALsemiSLSVM <- readRDS("20241121AL_MS_kmeans_semiSL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedALtSNESLSVM <- readRDS("20241121AL_MS_tSNE_SL_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_69seed.rds")
# tunedALtSNESVM <- readRDS("20241119AL_MS_tSNE_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedALTSVM <- readRDS("20241119AL_MS_kmeans_Train_SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# tunedVSVMSLvUn <- readRDS("20241123VSVM_SLvUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_119seed.rds")
# tunedVSVMSLUn <- readRDS("20241121VSVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedVSVMSL <- readRDS("20241121VSVM_SL_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_89seed.rds")
# tunedSVMUn <- readRDS("20241124SVM_SLUn_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_64seed.rds")
# tunedSVM <- readRDS("20241119SVM_cologne_multiclass_shape_ALTSLv3_120sampleSizePor_20Unl_realiz1_78seed.rds")
# 
# start.time <- Sys.time()
# predLabelMap = predict(tunedALMCLUSVM, normBaseLevFeatMap )
# cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")     
# 
# 
# predLabelMap <- as.data.frame(predLabelMap)
# # predLabels <- predLabelMap[dataLabelMap != "unclassified", ]
# # accALMCLUSVM_all = confusionMatrix(predLabels, dataLabelMap[dataLabelMap != "unclassified"])
# 
# predValidateLabel = predict(tunedALMCLUSVM, validateFeat)
# accALMCLUSVM = confusionMatrix(predValidateLabel, validateLabel)
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/images/maps/",city))
# # Plot confusion matrices
# # plot_confusion_matrix(accALMCLUSVM_all, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix_all.png", sep = "_"))
# plot_confusion_matrix(accALMCLUSVM, filename = paste(city,invariance,model_prob,model_name,"confusion_matrix.png", sep = "_"))
# 
# 
# setwd(paste0(path, "GitHub/active-learning-virtual-SVM/make_maps/",city))
# outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
# write.csv2(predLabelMap, file = outputfile, row.names = TRUE ,col.names = FALSE)
# 
# cat("performance results: acquired\n\n\n")
      