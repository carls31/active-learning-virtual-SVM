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

city = "cologne"    # cologne or hagadera location
invariance = "shape"   # scale or shape invariance
model_prob = "multiclass"  # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
sampleSizePor = c(40)

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}
#####################################################  Utils  ####################################################
# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #


classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}
##################################################################################################################


cat("preprocessing",city,model_prob,invariance,"\n")
if(city=="cologne"){ 
  sampleSizePor = c(48)} 
if(model_prob=="binary"){ 
  sampleSizePor = c(14)
}
###############################################  Preprocessing  ############################################

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
  
  if (invariance=="scale") {
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
    ########################################  Input  ########################################
    sindexSVMDATA = 1   # start of baseline model with one segmentation scale data
    eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
    
    generalDataPool = cbind(generalDataPool[,37:54], generalDataPool[,(ncol(generalDataPool)-1):ncol(generalDataPool)])
    
    #exclude unclassified and delete level of factor
    # generalDataPool = subset(generalDataPool, REF != "unclassified")
    generalDataPool$REF = factor(generalDataPool$REF)
    
    recordCount_shape = nrow(generalDataPool)
    
    generalDataPool_shape = setNames(generalDataPool[,1:20],c(objInfoNames[-length(objInfoNames)],"REF","use" ))
    
    
    if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
      generalDataPool_shape=classificationProblem(generalDataPool_shape)
    }
    ########################################  Scaling  ########################################
    
    normalizedFeat = generalDataPool_shape[,sindexSVMDATA:eindexSVMDATA]
    normalizedLabelUSE = generalDataPool_shape[1:nrow(generalDataPool),19:20]
    rm(generalDataPool_shape)
    
    normalizedFeat = normalizedFeat[1:recordCount_shape,]
    
    # normalization of  data ("range" scales the data to the interval [0, 1]; c("center", "scale") centers and scales the input data)
    preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
    normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
    
    # **************************************** data for map visualization ****************************************
    normalized_data = predict(preProc, setNames(generalDataPool[1:18],objInfoNames[-length(objInfoNames)]))
    # ************************************************************************************************************
    
    #########################################################################################
  }  
} else {
  
  numFeat = 26                   # number of features per level (dimensionality)
  
  #names to use in rbind() of VSV                # 26 features names + 19.label
  objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                    "Lx_m_cb","Lx_m_bl","Lx_m_gr","Lx_m_y","Lx_m_reg","Lx_m_nir2","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                    "Lx_s_cb","Lx_s_bl","Lx_s_gr","Lx_s_y","Lx_s_reg","Lx_s_nir2","Lx_s_ndvi","Lx_s_nir","Lx_s_re",
                    "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                    "label")
  if (invariance=="scale") {
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
    
    setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/",invariance))
    
    # import data
    generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnclass)
    colnames(generalDataPool)[209] = "REF"
    
    # exclude unclassified and delete level of factor
    # generalDataPool = subset(generalDataPool, REF != "unclassified")
    generalDataPool$REF <- factor(generalDataPool$REF)
    generalDataPool <- na.omit(generalDataPool)
    
    char_columns <- which(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class) == "character")
    generalDataPool[char_columns] <- lapply(generalDataPool[char_columns], function(x) as.numeric(as.character(x)))
    unique(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class))
    
    if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
      generalDataPool=classificationProblem(generalDataPool)
    }
    ###################################################  Scaling  ################################################
    
    normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
    normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]
    
    preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
    # # *************************************** data for map visualization *****************************************
    normalized_data = predict(preProc, setNames(generalDataPool[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
    # # ************************************************************************************************************
    
    #############################################################################################################
  } else {
    ##################################################  Input  ##################################################
    
    sindexSVMDATA = 1                                       # start of baseline model with one segmentation scale data
    eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
    
    #import format; "NULL" for subset of data on only some level (speed up import)
    columnclass = c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"factor","integer")
    columnclass2 = c(NA,NA,"factor",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"factor")
    
    setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/shape1"))
    inputPath ="base_level_complete.csv"   
    generalDataPool_scale = read.csv2(inputPath,header = T, sep =";",colClasses =columnclass)
    
    setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/",invariance))
    
    
    
    #exclude unclassified and delete level of factor
    # generalDataPool_scale = subset(generalDataPool_scale, REF != "unclassified")
    generalDataPool_scale$REF = factor(generalDataPool_scale$REF)
    
   
    
    recordCount_shape = nrow(generalDataPool_scale)
    
    generalDataPool = setNames(generalDataPool_scale[,1:28],c(objInfoNames[-length(objInfoNames)],"REF","use" ))
    
    char_columns <- which(sapply(generalDataPool[,1:26], class) == "character")
    generalDataPool[char_columns] <- lapply(generalDataPool[char_columns], function(x) as.numeric(as.character(x)))
    unique(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class))
    
    if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
      generalDataPool=classificationProblem(generalDataPool)
    }
    ########################################  Scaling  ########################################
    
    normalizedFeat = generalDataPool[,sindexSVMDATA:eindexSVMDATA]
    normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):ncol(generalDataPool)]
    rm(generalDataPool)
    
    normalizedFeat = normalizedFeat[1:recordCount_shape,]
    
    #normalization of  data ("range" scales the data to the interval [0, 1]; c("center", "scale") centers and scales the input data)
    preProc = preProcess(setNames(normalizedFeat,objInfoNames[-length(objInfoNames)]), method = "range")
    
    # **************************************** data for map visualization ****************************************
    normalized_data = predict(preProc, setNames(generalDataPool_scale[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
    # ************************************************************************************************************
  }
}

###############################################  Map Prediction  #################################################

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))


############################################################
############################################################
##                      apply SVM                         ##
############################################################
############################################################
model_name = "SVM"

tunedSVM <- readRDS("20240924SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply SVM_SL_Un                     
############################################################
############################################################
model_name = "SVM_SL_Un"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SL                         ##
############################################################
############################################################
model_name = "VSVM_SL"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SL_Un                         ##
############################################################
############################################################
model_name = "VSVM_SL_Un"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply VSVM_SL_vUn                         ##
############################################################
############################################################
model_name = "VSVM_SL_vUn"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS_SVM                         ##
############################################################
############################################################
model_name = "AL_MS_SVM"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS_tSNE_SVM                         ##
############################################################
############################################################
model_name = "AL_MS_tSNE_SVM"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS_tSNE_SL_SVM                         ##
############################################################
############################################################
model_name = "AL_MS_tSNE_SL_SVM"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MS_semiSL_SVM                         ##
############################################################
############################################################
model_name = "AL_MS_semiSL_SVM"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

############################################################
############################################################
##                      apply AL_MCLU_tSNE_SVM                         ##
############################################################
############################################################
model_name = "AL_MCLU_tSNE_SVM"

tunedSVM <- readRDS("")

start.time <- Sys.time()
predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "secs"), 2),"sec\n")      

setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)

cat("performance results: acquired\n\n\n")
      