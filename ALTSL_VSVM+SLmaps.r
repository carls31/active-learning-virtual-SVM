script = "ALTSLmaps"  # 
#####################################################  Libraries  ################################################
library(caret)
library(kernlab)
library(sampling)
library(progress)   # progress bar visualization
library(stats)      # k-means clustering
library(foreach)    # parallel processing
library(doParallel) # multiple CPU cores
library(Rtsne)      # t-distributed stochastic neighbour embedding
##################################################################################################################

cities = c("cologne")     # cologne or hagadera location
invariances = c("shape")   # scale or shape invariance
model_probs = c("multiclass")  # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
sampleSizePor = c(40)

path = '/home/data1/Lorenzo/'
#####################################################  Utils  ####################################################
# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #
num_cores <- 54 # parallel::detectCores() # 48 # Numbers of CPU cores for parallel processing

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
for (model_prob in model_probs) { 
  for (invariance in invariances) {
    for (city in cities) {
      
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
          generalDataPool = subset(generalDataPool, REF != "unclassified")
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
          
          setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/",invariance))
          
          # import data
          generalDataPoolOrg_S09C01 = read.csv2("cologne_res_100cm_S09C01_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S07C03 = read.csv2("cologne_res_100cm_S07C03_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S05C07 = read.csv2("cologne_res_100cm_S05C07_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S05C05 = read.csv2("cologne_res_100cm_S05C05_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S05C03 = read.csv2("cologne_res_100cm_S05C03_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S03C07 = read.csv2("cologne_res_100cm_S03C07_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S03C05 = read.csv2("cologne_res_100cm_S03C05_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          generalDataPoolOrg_S01C09 = read.csv2("cologne_res_100cm_S01C09_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnclass,22))
          
          #exclude unclassified and delete level of factor
          generalDataPool = subset(generalDataPool, REF != "unclassified")
          generalDataPool$REF = factor(generalDataPool$REF)
          
          generalDataPoolOrg_S09C01 = subset(generalDataPoolOrg_S09C01, REF != "unclassified")
          generalDataPoolOrg_S09C01$REF <- factor(generalDataPoolOrg_S09C01$REF)
          use_label_S09C01 = generalDataPoolOrg_S09C01[,21:22]
          
          generalDataPoolOrg_S07C03 = subset(generalDataPoolOrg_S07C03, REF != "unclassified")
          generalDataPoolOrg_S07C03$REF <- factor(generalDataPoolOrg_S07C03$REF)
          use_label_S07C03 = generalDataPoolOrg_S07C03[,21:22]
          
          generalDataPoolOrg_S05C07 = subset(generalDataPoolOrg_S05C07, REF != "unclassified")
          generalDataPoolOrg_S05C07$REF <- factor(generalDataPoolOrg_S05C07$REF)
          use_label_S07C03 = generalDataPoolOrg_S07C03[,21:22]
          
          generalDataPoolOrg_S05C05 = subset(generalDataPoolOrg_S05C05, REF != "unclassified")
          generalDataPoolOrg_S05C05$REF <- factor(generalDataPoolOrg_S05C05$REF)
          use_label_S05C05 = generalDataPoolOrg_S05C05[,21:22]
          
          generalDataPoolOrg_S05C03 = subset(generalDataPoolOrg_S05C03, REF != "unclassified")
          generalDataPoolOrg_S05C03$REF <- factor(generalDataPoolOrg_S05C03$REF)
          use_label_S05C03 = generalDataPoolOrg_S05C03[,21:22]
          
          generalDataPoolOrg_S03C07 = subset(generalDataPoolOrg_S03C07, REF != "unclassified")
          generalDataPoolOrg_S03C07$REF <- factor(generalDataPoolOrg_S03C07$REF)
          use_label_S03C07 = generalDataPoolOrg_S03C07[,21:22]
          
          generalDataPoolOrg_S03C05 = subset(generalDataPoolOrg_S03C05, REF != "unclassified")
          generalDataPoolOrg_S03C05$REF <- factor(generalDataPoolOrg_S03C05$REF)
          use_label_S03C05 = generalDataPoolOrg_S03C05[,21:22]
          
          generalDataPoolOrg_S01C09 = subset(generalDataPoolOrg_S01C09, REF != "unclassified")
          generalDataPoolOrg_S01C09$REF <- factor(generalDataPoolOrg_S01C09$REF)
          use_label_S01C09 = generalDataPoolOrg_S01C09[,21:22]
          
          recordCount_shape = nrow(generalDataPoolOrg_S01C09)
          
          generalDataPool_shape = rbind(setNames(generalDataPool[,1:20],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S01C09[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )), 
                                        setNames(generalDataPoolOrg_S03C05[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S03C07[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S05C03[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S05C05[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S05C07[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S07C03[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                        setNames(generalDataPoolOrg_S09C01[,3:22],c(objInfoNames[-length(objInfoNames)],"REF","use" )))
          
          
          if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
            generalDataPool_shape=classificationProblem(generalDataPool_shape)
          }
          ########################################  Scaling  ########################################
          
          normalizedFeat = generalDataPool_shape[,1:(ncol(generalDataPool_shape)-2)]
          normalizedLabelUSE = generalDataPool_shape[1:nrow(generalDataPoolOrg_S09C01),19:20]
          rm(generalDataPool_shape)
          
          normalizedFeat = cbind(normalizedFeat[1:recordCount_shape,],
                                 normalizedFeat[(recordCount_shape+1):(2*recordCount_shape),],
                                 normalizedFeat[((2*recordCount_shape)+1):(3*recordCount_shape),],
                                 normalizedFeat[((3*recordCount_shape)+1):(4*recordCount_shape),],
                                 normalizedFeat[((4*recordCount_shape)+1):(5*recordCount_shape),], 
                                 normalizedFeat[((5*recordCount_shape)+1):(6*recordCount_shape),],
                                 normalizedFeat[((6*recordCount_shape)+1):(7*recordCount_shape),],
                                 normalizedFeat[((7*recordCount_shape)+1):(8*recordCount_shape),],
                                 normalizedFeat[((8*recordCount_shape)+1):(9*recordCount_shape),])
          
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
          generalDataPool = subset(generalDataPool, REF != "unclassified")
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
          
          # import data
          generalDataPoolOrg_S09C01 = read.csv2("hagadera_s25_S09C01_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S07C03 = read.csv2("hagadera_s25_S07C03_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C07 = read.csv2("hagadera_s25_S05C07_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C05 = read.csv2("hagadera_s25_S05C05_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C03 = read.csv2("hagadera_s25_S05C03_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S03C07 = read.csv2("hagadera_s25_S03C07_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S03C05 = read.csv2("hagadera_s25_S03C05_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          generalDataPoolOrg_S01C09 = read.csv2("hagadera_s25_S01C09_allclass_use.csv" ,header = T, sep =";",colClasses =columnclass2)#, nrows = 2000000)
          
          generalDataPoolOrg_S09C01 = cbind(generalDataPoolOrg_S09C01[,1:2],generalDataPoolOrg_S09C01[,4:29],generalDataPoolOrg_S09C01[,3],generalDataPoolOrg_S09C01[,30])
          generalDataPoolOrg_S07C03 = cbind(generalDataPoolOrg_S07C03[,1:2],generalDataPoolOrg_S07C03[,4:29],generalDataPoolOrg_S07C03[,3],generalDataPoolOrg_S07C03[,30])
          generalDataPoolOrg_S05C07 = cbind(generalDataPoolOrg_S05C07[,1:2],generalDataPoolOrg_S05C07[,4:29],generalDataPoolOrg_S05C07[,3],generalDataPoolOrg_S05C07[,30])
          generalDataPoolOrg_S05C05 = cbind(generalDataPoolOrg_S05C05[,1:2],generalDataPoolOrg_S05C05[,4:29],generalDataPoolOrg_S05C05[,3],generalDataPoolOrg_S05C05[,30])
          generalDataPoolOrg_S05C03 = cbind(generalDataPoolOrg_S05C03[,1:2],generalDataPoolOrg_S05C03[,4:29],generalDataPoolOrg_S05C03[,3],generalDataPoolOrg_S05C03[,30])
          generalDataPoolOrg_S03C07 = cbind(generalDataPoolOrg_S03C07[,1:2],generalDataPoolOrg_S03C07[,4:29],generalDataPoolOrg_S03C07[,3],generalDataPoolOrg_S03C07[,30])
          generalDataPoolOrg_S03C05 = cbind(generalDataPoolOrg_S03C05[,1:2],generalDataPoolOrg_S03C05[,4:29],generalDataPoolOrg_S03C05[,3],generalDataPoolOrg_S03C05[,30])
          generalDataPoolOrg_S01C09 = cbind(generalDataPoolOrg_S01C09[,1:2],generalDataPoolOrg_S01C09[,4:29],generalDataPoolOrg_S01C09[,3],generalDataPoolOrg_S01C09[,30])
          
          colnames(generalDataPoolOrg_S09C01)[29] <- "REF"
          colnames(generalDataPoolOrg_S09C01)[30] <- "USE"
          
          generalDataPoolOrg_S09C01$S9C1T_DISS=as.numeric(generalDataPoolOrg_S09C01$S9C1T_DISS)
          generalDataPoolOrg_S09C01$S9C1T_HOM=as.numeric(generalDataPoolOrg_S09C01$S9C1T_HOM)
          generalDataPoolOrg_S09C01$S9C1T_MEA=as.numeric(generalDataPoolOrg_S09C01$S9C1T_MEA)
          
          colnames(generalDataPoolOrg_S07C03)[29] <- "REF"
          colnames(generalDataPoolOrg_S07C03)[30] <- "USE"
          generalDataPoolOrg_S07C03$S7C3T_DISS=as.numeric(generalDataPoolOrg_S07C03$S7C3T_DISS)
          generalDataPoolOrg_S07C03$S7C3T_HOM=as.numeric(generalDataPoolOrg_S07C03$S7C3T_HOM)
          generalDataPoolOrg_S07C03$S7C3T_MEA=as.numeric(generalDataPoolOrg_S07C03$S7C3T_MEA)
          
          colnames(generalDataPoolOrg_S05C07)[29] <- "REF"
          colnames(generalDataPoolOrg_S05C07)[30] <- "USE"
          generalDataPoolOrg_S05C07$S5C7T_DISS=as.numeric(generalDataPoolOrg_S05C07$S5C7T_DISS)
          generalDataPoolOrg_S05C07$S5C7T_HOM=as.numeric(generalDataPoolOrg_S05C07$S5C7T_HO)
          generalDataPoolOrg_S05C07$S5C7T_MEA=as.numeric(generalDataPoolOrg_S05C07$S5C7T_MEA)
          
          colnames(generalDataPoolOrg_S05C05)[29] <- "REF"
          colnames(generalDataPoolOrg_S05C05)[30] <- "USE"
          generalDataPoolOrg_S05C05$S5C5T_DISS=as.numeric(generalDataPoolOrg_S05C05$S5C5T_DISS)
          generalDataPoolOrg_S05C05$S5C5T_HOM=as.numeric(generalDataPoolOrg_S05C05$S5C5T_HOM)
          generalDataPoolOrg_S05C05$S5C5T_MEA=as.numeric(generalDataPoolOrg_S05C05$S5C5T_MEA)
          
          colnames(generalDataPoolOrg_S05C03)[29] <- "REF"
          colnames(generalDataPoolOrg_S05C03)[30] <- "USE"
          generalDataPoolOrg_S05C03$S5C3T_DISS=as.numeric(generalDataPoolOrg_S05C03$S5C3T_DISS)
          generalDataPoolOrg_S05C03$S5C3T_HOM=as.numeric(generalDataPoolOrg_S05C03$S5C3T_HOM)
          generalDataPoolOrg_S05C03$S5C3T_MEA=as.numeric(generalDataPoolOrg_S05C03$S5C3T_MEA)
          
          colnames(generalDataPoolOrg_S03C07)[29] <- "REF"
          colnames(generalDataPoolOrg_S03C07)[30] <- "USE"
          generalDataPoolOrg_S03C07$S3C7T_DISS=as.numeric(generalDataPoolOrg_S03C07$S3C7T_DISS)
          generalDataPoolOrg_S03C07$S3C7T_HOM=as.numeric(generalDataPoolOrg_S03C07$S3C7T_HOM)
          generalDataPoolOrg_S03C07$S3C7T_MEA=as.numeric(generalDataPoolOrg_S03C07$S3C7T_MEA)
          
          colnames(generalDataPoolOrg_S03C05)[29] <- "REF"
          colnames(generalDataPoolOrg_S03C05)[30] <- "USE"
          generalDataPoolOrg_S03C05$S3C5T_DISS=as.numeric(generalDataPoolOrg_S03C05$S3C5T_DISS)
          generalDataPoolOrg_S03C05$S3C5T_HOM=as.numeric(generalDataPoolOrg_S03C05$S3C5T_HOM)
          generalDataPoolOrg_S03C05$S3C5T_MEA=as.numeric(generalDataPoolOrg_S03C05$S3C5T_MEA)
          
          colnames(generalDataPoolOrg_S01C09)[29] <- "REF"
          colnames(generalDataPoolOrg_S01C09)[30] <- "USE"
          generalDataPoolOrg_S01C09$S1C9T_DISS=as.numeric(generalDataPoolOrg_S01C09$S1C9T_DISS)
          generalDataPoolOrg_S01C09$S1C9T_HOM=as.numeric(generalDataPoolOrg_S01C09$S1C9T_HOM)
          generalDataPoolOrg_S01C09$S1C9T_MEA=as.numeric(generalDataPoolOrg_S01C09$S1C9T_MEA)
          
          #exclude unclassified and delete level of factor
          generalDataPool_scale = subset(generalDataPool_scale, REF != "unclassified")
          generalDataPool_scale$REF = factor(generalDataPool_scale$REF)
          
          generalDataPoolOrg_S09C01 = subset(generalDataPoolOrg_S09C01, REF != "unclassified")
          generalDataPoolOrg_S09C01$REF <- factor(generalDataPoolOrg_S09C01$REF)
          
          generalDataPoolOrg_S07C03 = subset(generalDataPoolOrg_S07C03, REF != "unclassified")
          generalDataPoolOrg_S07C03$REF <- factor(generalDataPoolOrg_S07C03$REF)
          
          generalDataPoolOrg_S05C07 = subset(generalDataPoolOrg_S05C07, REF != "unclassified")
          generalDataPoolOrg_S05C07$REF <- factor(generalDataPoolOrg_S05C07$REF)
          
          generalDataPoolOrg_S05C05 = subset(generalDataPoolOrg_S05C05, REF != "unclassified")
          generalDataPoolOrg_S05C05$REF <- factor(generalDataPoolOrg_S05C05$REF)
          
          generalDataPoolOrg_S05C03 = subset(generalDataPoolOrg_S05C03, REF != "unclassified")
          generalDataPoolOrg_S05C03$REF <- factor(generalDataPoolOrg_S05C03$REF)
          
          generalDataPoolOrg_S03C07 = subset(generalDataPoolOrg_S03C07, REF != "unclassified")
          generalDataPoolOrg_S03C07$REF <- factor(generalDataPoolOrg_S03C07$REF)
          
          generalDataPoolOrg_S03C05 = subset(generalDataPoolOrg_S03C05, REF != "unclassified")
          generalDataPoolOrg_S03C05$REF <- factor(generalDataPoolOrg_S03C05$REF)
          
          generalDataPoolOrg_S01C09 = subset(generalDataPoolOrg_S01C09, REF != "unclassified")
          generalDataPoolOrg_S01C09$REF <- factor(generalDataPoolOrg_S01C09$REF)
          
          recordCount_shape = nrow(generalDataPoolOrg_S01C09)
          
          generalDataPool = rbind(setNames(generalDataPool_scale[,1:28],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S01C09[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )), 
                                  setNames(generalDataPoolOrg_S03C05[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S03C07[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S05C03[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S05C05[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S05C07[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S07C03[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )),
                                  setNames(generalDataPoolOrg_S09C01[,3:30],c(objInfoNames[-length(objInfoNames)],"REF","use" )))
          
          char_columns <- which(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class) == "character")
          generalDataPool[char_columns] <- lapply(generalDataPool[char_columns], function(x) as.numeric(as.character(x)))
          unique(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class))
          
          if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
            generalDataPool=classificationProblem(generalDataPool)
          }
          ########################################  Scaling  ########################################
          
          normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
          normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):ncol(generalDataPool)]
          rm(generalDataPool)
          
          normalizedFeat = cbind(normalizedFeat[1:recordCount_shape,],
                                 normalizedFeat[(recordCount_shape+1):(2*recordCount_shape),],
                                 normalizedFeat[((2*recordCount_shape)+1):(3*recordCount_shape),],
                                 normalizedFeat[((3*recordCount_shape)+1):(4*recordCount_shape),],
                                 normalizedFeat[((4*recordCount_shape)+1):(5*recordCount_shape),], 
                                 normalizedFeat[((5*recordCount_shape)+1):(6*recordCount_shape),],
                                 normalizedFeat[((6*recordCount_shape)+1):(7*recordCount_shape),],
                                 normalizedFeat[((7*recordCount_shape)+1):(8*recordCount_shape),],
                                 normalizedFeat[((8*recordCount_shape)+1):(9*recordCount_shape),])

          #normalization of  data ("range" scales the data to the interval [0, 1]; c("center", "scale") centers and scales the input data)
          preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
          
          # **************************************** data for map visualization ****************************************
          normalized_data = predict(preProc, setNames(generalDataPool_scale[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
          # ************************************************************************************************************
        }
      }
      
      ###############################################  Map Prediction  #################################################
      
      setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
      
      cat("\n","CPU cores: ",num_cores,sep="")
      
      ############################################################
      ############################################################
      ##                      apply SVM                         ##
      ############################################################
      ############################################################
      start.time <- Sys.time()
      
      tunedSVM <- readRDS("20240924SVM_cologne_multiclass_shape_ALTSLv1_48sampleSizePor_20Unl_140seed.rds")
      
      predLabels_data_modell_apply = predict(tunedSVM, normalized_data_modell_apply)
      
      cat("Execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h\n")      
      
      setwd(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/",city))
      outputfile = paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[1],"Size_",b,"Unl_samples.csv")
      write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
      
      
      cat("performance results: acquired\n\n\n")
      
    }
  }
}