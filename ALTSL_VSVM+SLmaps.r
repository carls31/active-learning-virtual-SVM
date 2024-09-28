script = "ALTSLmaps"  # 
#####################################################  Libraries  ################################################
library(caret)
library(kernlab)
library(sampling)
# library(progress)   # progress bar visualization
# library(stats)      # k-means clustering
# library(foreach)    # parallel processing
# library(doParallel) # multiple CPU cores
# library(Rtsne)      # t-distributed stochastic neighbour embedding
##################################################################################################################

city = "hagadera"    # cologne or hagadera location
invariance = "shape"   # scale or shape invariance
model_prob = "binary"  # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
sampleSizePor = c(40)

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}
###############################################  Preprocessing  ############################################

# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #

cat("preprocessing",city,model_prob,invariance,"\n")

if(city=="cologne"){ 
  sampleSizePor = c(48)} 
if(model_prob=="binary"){ 
  sampleSizePor = c(14)
}

classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}

if (city=="cologne") {
  
  inputPath ="cologne_res_100_L2-L13.csv" 
  
  numFeat = 18                                            # number of features per level (dimensionality)
  
  #names to use in rbind() of VSV                         # 18 features names + 19.label
  objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                    "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                    "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
                    "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                    "label")
  
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
  
  sindexSVMDATA = 37        # start of baseline model with one segmentation scale data
  eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
  
  # exclude unclassified and delete level of factor
  # generalDataPool = subset(generalDataPool, REF != "unclassified")
  generalDataPool$REF <- factor(generalDataPool$REF)
  # generalDataPool <- na.omit(generalDataPool) 
  
  if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
    generalDataPool=classificationProblem(generalDataPool)
  }
  ###################################################  Scaling  ################################################
  
  normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
  normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]
  
  preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
  normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # **************************************** data for map visualization ****************************************
  normalized_data = predict(preProc, setNames(generalDataPool[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
  # ************************************************************************************************************
  
  #########################################################################################
  
} else {
  
  numFeat = 26                   # number of features per level (dimensionality)
  
  #names to use in rbind() of VSV                # 26 features names + 19.label
  objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                    "Lx_m_cb","Lx_m_bl","Lx_m_gr","Lx_m_y","Lx_m_reg","Lx_m_nir2","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                    "Lx_s_cb","Lx_s_bl","Lx_s_gr","Lx_s_y","Lx_s_reg","Lx_s_nir2","Lx_s_ndvi","Lx_s_nir","Lx_s_re",
                    "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                    "label")
  
  ########################################  Input  ########################################
  inputPath ="hagadera_all_level_scale_specgeomtex.csv"  
  
  sindexSVMDATA = 53                                      # start of baseline model with one segmentation scale data
  eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
  
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
  # colnames(generalDataPool)[209] = "REF"
  
  generalDataPool = generalDataPool[c(sindexSVMDATA:eindexSVMDATA, (ncol(generalDataPool)-1))]
  colnames(generalDataPool)[(ncol(generalDataPool))] = "REF"
  
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
  
  normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-1)]
  
  normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool))]
  rm(generalDataPool)
  preProc = preProcess(setNames(normalizedFeat,objInfoNames[-length(objInfoNames)]), method = "range")
  # *************************************** data for map visualization *****************************************
  normalized_data = predict(preProc, setNames(normalizedFeat,objInfoNames[-length(objInfoNames)]))
  # ************************************************************************************************************
  rm(normalizedFeat)
  #############################################################################################################
}
if(sum(is.na(normalized_data))>0){
  cat(sum(is.na(normalized_data)))
  # Loop through all columns in normalized_data and apply mean imputation
  for (col in names(normalized_data)) {
    # Check if there are any missing values in the column
    if (any(is.na(normalized_data[[col]]))) {
      # Replace NA with the mean of the column (excluding NA values)
      normalized_data[[col]][is.na(normalized_data[[col]])] <- mean(normalized_data[[col]], na.rm = TRUE)
    }
  }
}
cat(sum(is.na(normalized_data)))
###############################################  Map Prediction  #################################################

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

############################################################
############################################################
##                      apply SVM                         ##
############################################################
############################################################
model_name = "SVM"

tunedSVM <- readRDS("20240924SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedSVM <- readRDS("20240928SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedSVM <- readRDS("20240921SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedSVM <- readRDS("20240920SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedSVM <- readRDS("20240926SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedSVM <- readRDS("20240925SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedSVM <- readRDS("20240924SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedSVM <- readRDS("20240927SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply SVM_SLUn                   ##  
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "SVM_SLUn"

tunedSVMUn <- readRDS("20240924SVM_SLUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedSVMUn <- readRDS("20240928SVM_SLUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedSVMUn <- readRDS("20240921SVM_SLUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedSVMUn <- readRDS("20240920SVM_SLUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedSVMUn <- readRDS("20240926SVM_SLUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedSVMUn <- readRDS("20240925SVM_SLUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedSVMUn <- readRDS("20240924SVM_SLUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedSVMUn <- readRDS("20240927SVM_SLUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVMUn, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SL                     ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "VSVM_SL"

tunedVSVMSL <- readRDS("20240924VSVM_SL_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSL <- readRDS("20240928VSVM_SL_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedVSVMSL <- readRDS("20240921VSVM_SL_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSL <- readRDS("20240920VSVM_SL_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedVSVMSL <- readRDS("20240926VSVM_SL_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedVSVMSL <- readRDS("20240925VSVM_SL_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedVSVMSL <- readRDS("20240924VSVM_SL_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedVSVMSL <- readRDS("20240927VSVM_SL_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")
start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedVSVMSL, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SLUn                  ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "VSVM_SLUn"

tunedVSVMSLUn <- readRDS("20240924VSVM_SLUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLUn <- readRDS("20240928VSVM_SLUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedVSVMSLUn <- readRDS("20240921VSVM_SLUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLUn <- readRDS("20240920VSVM_SLUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedVSVMSLUn <- readRDS("20240926VSVM_SLUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedVSVMSLUn <- readRDS("20240925VSVM_SLUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLUn <- readRDS("20240924VSVM_SLUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedVSVMSLUn <- readRDS("20240927VSVM_SLUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedVSVMSLUn, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SLvUn                 ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "VSVM_SLvUn"

tunedVSVMSLvUn <- readRDS("20240924VSVM_SLvUn_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLvUn <- readRDS("20240928VSVM_SLvUn_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedVSVMSLvUn <- readRDS("20240921VSVM_SLvUn_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLvUn <- readRDS("20240920VSVM_SLvUn_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedVSVMSLvUn <- readRDS("20240926VSVM_SLvUn_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedVSVMSLvUn <- readRDS("20240925VSVM_SLvUn_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedVSVMSLvUn <- readRDS("20240924VSVM_SLvUn_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedVSVMSLvUn <- readRDS("20240927VSVM_SLvUn_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedVSVMSLvUn, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS+kmeans+Train_SVM                   ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "AL_MS+kmeans+Train_SVM"

tunedALTSVM <- readRDS("20240924AL_MS+kmeans+Train_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALTSVM <- readRDS("20240928AL_MS+kmeans+Train_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedALTSVM <- readRDS("20240921AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALTSVM <- readRDS("20240920AL_MS+kmeans+Train_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedALTSVM <- readRDS("20240926AL_MS+kmeans+Train_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedALTSVM <- readRDS("20240925AL_MS+kmeans+Train_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedALTSVM <- readRDS("20240924AL_MS+kmeans+Train_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedALTSVM <- readRDS("20240927AL_MS+kmeans+Train_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedALTSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS+tSNE_SVM              ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "AL_MS+tSNE_SVM"

tunedALtSNESVM <- readRDS("20240924AL_MS+tSNE_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALtSNESVM <- readRDS("20240928AL_MS+tSNE_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedALtSNESVM <- readRDS("20240921AL_MS+tSNE_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALtSNESVM <- readRDS("20240920AL_MS+tSNE_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedALtSNESVM <- readRDS("20240926AL_MS+tSNE_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedALtSNESVM <- readRDS("20240925AL_MS+tSNE_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedALtSNESVM <- readRDS("20240924AL_MS+tSNE_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedALtSNESVM <- readRDS("20240927AL_MS+tSNE_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedALtSNESVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS+tSNE+SL_SVM           ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "AL_MS+tSNE+SL_SVM"

tunedALtSNESLSVM <- readRDS("20240924AL_MS+tSNE+SL_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALtSNESLSVM <- readRDS("20240928AL_MS+tSNE+SL_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedALtSNESLSVM <- readRDS("20240921AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALtSNESLSVM <- readRDS("20240920AL_MS+tSNE+SL_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedALtSNESLSVM <- readRDS("20240926AL_MS+tSNE+SL_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedALtSNESLSVM <- readRDS("20240925AL_MS+tSNE+SL_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedALtSNESLSVM <- readRDS("20240924AL_MS+tSNE+SL_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedALtSNESLSVM <- readRDS("20240927AL_MS+tSNE+SL_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedALtSNESLSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS+kmeans+semiSL_SVM            ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "AL_MS+kmeans+semiSL_SVM"

tunedALsemiSLSVM <- readRDS("20240924AL_MS+kmeans+semiSL_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALsemiSLSVM <- readRDS("20240928AL_MS+kmeans+semiSL_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedALsemiSLSVM <- readRDS("20240921AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALsemiSLSVM <- readRDS("20240920AL_MS+kmeans+semiSL_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedALsemiSLSVM <- readRDS("20240926AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedALsemiSLSVM <- readRDS("20240925AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedALsemiSLSVM <- readRDS("20240924AL_MS+kmeans+semiSL_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedALsemiSLSVM <- readRDS("20240927AL_MS+kmeans+semiSL_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedALsemiSLSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MCLU+kmeans_SVM            ##
############################################################
############################################################
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))

model_name = "AL_MCLU+kmeans_SVM"
# name_prova = "20240924AL_MCLU+kmeans_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds"
# name_prova2 = paste0("20240924",model_name,"_",city,"_",model_prob,"_",invariance,"_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")

tunedALMCLUSVM <- readRDS("20240924AL_MCLU+kmeans_SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALMCLUSVM <- readRDS("20240928AL_MCLU+kmeans_SVM_cologne_binary_scale_ALTSLv1_14sampleSizePor_20Unl_191seed.rds")
tunedALMCLUSVM <- readRDS("20240921AL_MCLU+kmeans_SVM_cologne_multiclass_scale_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
tunedALMCLUSVM <- readRDS("20240920AL_MCLU+kmeans_SVM_cologne_binary_shape_ALTSLv1_14sampleSizePor_20Unl_342seed.rds")
tunedALMCLUSVM <- readRDS("20240926AL_MCLU+kmeans_SVM_hagadera_multiclass_scale_ALTSLv1_40sampleSizePor_20Unl_129seed.rds")
tunedALMCLUSVM <- readRDS("20240925AL_MCLU+kmeans_SVM_hagadera_multiclass_shape_ALTSLv1_14sampleSizePor_20Unl_140seed.rds")
tunedALMCLUSVM <- readRDS("20240924AL_MCLU+kmeans_SVM_hagadera_binary_shape_ALTSLv1_14sampleSizePor_20Unl_170seed.rds")
tunedALMCLUSVM <- readRDS("20240927AL_MCLU+kmeans_SVM_hagadera_binary_scale_ALTSLv1_14sampleSizePor_20Unl_142seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedALMCLUSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, row.names = TRUE ,col.names = FALSE)

cat("performance results: acquired\n\n\n")
      