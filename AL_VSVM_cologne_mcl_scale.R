# ##LOAD LIBRARIES:
# install.packages("caret")
# install.packages("kernlab")
# install.packages("sampling")
# install.packages("foreign")
# install.packages("e1071")

library(caret)
library(kernlab)
library(sampling)
library(foreign)
library(e1071)


##################################################################################
##################################################################################

## adjustments only need to be made here ##

## define for which sample size the model should be applied (define j)

# value   sample size per class
#   1   =  500
#   2   =  350
#   3   =  200 
#   4   =  125
#   5   =  80
#   6   =  50
#   7   =  20
#   8   =  10
#   9   =  3


#j = 3 ## insert value here
b=30 ## insert value here for the unlabeled sample size per class 

## define path into folder "apply_model" manually  (e.g. D:/Projects/VSVM/apply_model)

apply_model_path = '/home/rsrg9/Documents/tunc_oz/apply_model' ##insert path here


## execute the script

## in the folder "results" a csv table will be created containing the lables for each pixel.
## it can be joined easily in ArcGIS to a Feature class through connecting the first column 
## of the table to the Object ID of the features.

## hope it works, have fun ;)

##################################################################################
##################################################################################

######## FUNCTIONS ###############################################################

####grid search (coarse, narrow) SVM parameter tuning ##########################


svmFit = function(x, y, indexTrain){{ #x = training descriptors, y = class labels
  
  #expand coarse grid
  coarseGrid = expand.grid(sigma = 2^seq(-5,3,by=2), C = 2^seq(-4,12,by=2))
  
  #set seed
  set.seed(13)
  
  #coarse grid search
  svmFitCoarse = train(x,    
                       y, 
                       method = "svmRadial",
                       metric = "Kappa",
                       maximize = TRUE,
                       tuneGrid = coarseGrid,
                       trControl = trainControl ( method = "cv",
                                                  verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]]),
                       scaled = FALSE)
  
  #get best coarse grid sigma,C pair
  sigmaCoarse = svmFitCoarse$finalModel@kernelf@kpar$sigma
  cCoarse = svmFitCoarse$finalModel@param$C
  
  #define narrow grid boarders (aX: upper, bX: lower)
  #sigma:
  aS = log2(sigmaCoarse) - 2
  bS = log2(sigmaCoarse) + 2
  #C:
  aC = log2(cCoarse) - 2
  bC = log2(cCoarse) + 2
  
  #expand narrow grid
  narrowGrid = expand.grid(sigma = 2^seq(aS,bS,by=0.5), C = 2^seq(aC,bC,by=0.5))
  
  #set seed
  set.seed(31)
  
  #narrow grid search
  svmFitNarrow = train(x, 
                       y, 
                       method = "svmRadial",
                       metric = "Kappa",
                       maximize = TRUE,
                       tuneGrid = narrowGrid,
                       trControl = trainControl ( method = "cv",
                                                  verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]]),
                       scaled = FALSE)
}
  return(svmFitNarrow)  
}

#### calculate euclidian distance of 2 points in feature space ########################################

euc_dis = function(a, b){
  temp = 0
  for(ii in seq(along = c(1:length(a)))){
    temp = temp +((a[[ii]]-b[[ii]])^2)
  }
  return(sqrt(temp))
}


#### get original SV + VSV; return VSV that move in "close" distance to origianl SV ###################
# with org=SV, VSV1=VSV, a = Factor of mean distance to calculate threshold

rem_extrem = function(org, VSV1, a){      
  

  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()
  
  numClass = nlevels(org$REF)
  SVClass = list()
  
  # split SV according to its classes
  for(f in seq(along = c(1:numClass))){
    SVClass[[f]]=org[which(org$REF==levels(org$"REF")[[f]]),]
  }
  
  
  # save label of sample and the distance between SV and VSV in "distance" for each pair of SV and VSV
  for(l in seq(along = c(1:nrow(org)))){
    distance[l,1] =as.character( org[l,ncol(org)])
    distance[l,2] = euc_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)])
  }
  distance$X1 = factor(distance$X1)
  
  
  # save label of sample and the distance between SV and VSV in distance for each pair of SV and VSV
#  for(l in seq(along = c(1:nrow(org)))){
#    min = 1000
#    distance[l,1] =as.character( org[l,ncol(org)])
#    switch(org[l,ncol(org)],
#           levels(org$"REF")[[1]] = {
#             
#             distance[l,2] = euc_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)])
#           },
#          levels(org$"REF")[[2]] = {
#             for (k in seq(along = c(1:nrow(SVClass2)))){
#               min = min(min,euc_dis(SVClass2[k,-ncol(org)],VSV1[l,-ncol(VSV1)]))
#             }
#             distance[l,2] = min
#           },
#           levels(org$"REF")[[3]] = {
#             for (k in seq(along = c(1:nrow(SVClass3)))){
#               min = min(min,euc_dis(SVClass3[k,-ncol(org)],VSV1[l,-ncol(VSV1)]))
#             }
#             distance[l,2] = min
#           },
#           levels(org$"REF")[[4]] = {
#             for (k in seq(along = c(1:nrow(SVClass4)))){
#               min = min(min,euc_dis(SVClass4[k,-ncol(org)],VSV1[l,-ncol(VSV1)]))
#             }
#             distance[l,2] = min
#           },
#           levels(org$"REF")[[5]] = {
#             for (k in seq(along = c(1:nrow(SVClass5)))){
#               min = min(min,euc_dis(SVClass5[k,-ncol(org)],VSV1[l,-ncol(VSV1)]))
#             }
#             distance[l,2] = min
#           },
#           levels(org$"REF")[[6]] = {
#             for (k in seq(along = c(1:nrow(SVClass6)))){
 #              min = min(min,euc_dis(SVClass6[k,-ncol(org)],VSV1[l,-ncol(VSV1)]))
#             }
#             distance[l,2] = min
#           }
#    )
#    }
  
    boundClass = list()
    
  
    # calculate the distance for each SV in ClassX to all the SV in  classX, 
    # get the mean of the distances and multiply it with a to get the final threshold
    for(f in seq(along = c(1:length(SVClass)))){
      distanceSVC1 = c()
      if(nrow(SVClass[[f]])>0){
        for(n in seq(along = 1:(nrow(SVClass[[f]])-1))){
          for(nn in seq(along = c(n:(nrow(SVClass[[f]])-1)))){
            distanceSVC1[length(distanceSVC1)+1] = euc_dis(SVClass[[f]][n,-ncol(SVClass[[f]])], SVClass[[f]][(n+nn),-ncol(SVClass[[f]])])
        }
      }
        disClass1mean = mean(distanceSVC1)
        boundClass[[f]] = disClass1mean*a
      }
    }
    

  distance$X1 = factor(distance$X1)
  
  ############# continue  #################
  
  ###complete with switch!!!
  
  
  # Iterate over the distance vector and substitude in VSV1 the samples which overstep the threshold
  
  ### vorkommen von negativen distancen pr?fen und eventuell mit be?tragsfunktionen transformieren! 
  
  for(k in seq(along = c(1:nrow(org)))){
    
    if(as.integer(distance[k,1]) == 1){
      if(!is.na(boundClass[1])){
        if(distance[k,2] != 0 && distance[k,2] > (boundClass[[1]])){
          VSV1[k,]=NA
        }
      }
    }else{
      if(as.integer(distance[k,1]) == 2){
        if(!is.na(boundClass[[2]])){
          if(distance[k,2] != 0 && distance[k,2] > (boundClass[[2]])){
            VSV1[k,]=NA
          }
        }
      }else{
        if(as.integer(distance[k,1]) == 3){
          if(!is.na(boundClass[[3]])){
            if(distance[k,2] != 0 && distance[k,2] > (boundClass[[3]])){
              VSV1[k,]=NA
            }
          }
        }else{
          if(as.integer(distance[k,1]) == 4){
            if(!is.na(boundClass[[4]])){
              if(distance[k,2] != 0 && distance[k,2] > (boundClass[[4]])){
                VSV1[k,]=NA
              }
            }
          }else{
            if(as.integer(distance[k,1]) == 5){
              if(!is.na(boundClass[[5]])){
                if(distance[k,2] != 0 && distance[k,2] > (boundClass[[5]])){
                  VSV1[k,]=NA
                }
              }
            }else{
              if(as.integer(distance[k,1]) == 6){
                if(!is.na(boundClass[[6]])){
                  if(distance[k,2] != 0 && distance[k,2] > (boundClass[[6]])){
                    VSV1[k,]=NA
                  }
                }
              }else{
                if(is.na(distance[k,1])){
                    VSV1[k,]=NA
            }
          }
        }
      }
        }
      }
    }
  }
      
    
    
  return(VSV1)
}




#### kernal distance #####################################################

kern_dis = function(a, b, kernelfunc){
  a = unlist(a)
  b = unlist(b)
  dk =sqrt( kernelfunc(a,a)+kernelfunc(b,b)-2*kernelfunc(a,b))
  return(dk)
}




##########################

rem_extrem_kerneldist = function(org, VSV1, a, kernelfunc){      
  
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()
  
  # save label of sample and the distance between SV and VSV in distance for each pair of SV and VSV
  for(l in seq(along = c(1:nrow(org)))){
    distance[l,1] =as.character( org[l,ncol(org)])
    distance[l,2] = kern_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)],kernelfunc)
  }
  
  # split SV according to its classes
  SVClass1=org[which(org$REF==levels(org$"REF")[[1]]),]
  SVClass2=org[which(org$REF==levels(org$"REF")[[2]]),]
  
  # calculate the distance for each SV in Class1 to all the SV in  class1, 
  # get the mean of the distances and multiply it with a to get the final threshold
  if(nrow(SVClass1)>0){
    for(n in seq(along = 1:(nrow(SVClass1)-1))){
      for(nn in seq(along = c(n:(nrow(SVClass1)-1))))
        distanceSVC1[length(distanceSVC1)+1] = kern_dis(SVClass1[n,-ncol(SVClass1)], SVClass1[(n+nn),-ncol(SVClass1)],kernelfunc)
    }
    
    disClass1mean = mean(distanceSVC1)
    boundClass1 = disClass1mean*a
  }
  
  # calculate the distance for each SV in Class2 to all the SV in  class2, 
  # get the mean of the distances and multiply it with a to get the final threshold
  if(nrow(SVClass2)>0){
    for(n in seq(along = 1:(nrow(SVClass2)-1))){
      for(nn in seq(along = c(n:(nrow(SVClass2)-1))))
        distanceSVC2[length(distanceSVC2)+1] = kern_dis(SVClass2[n,-ncol(SVClass2)], SVClass2[(n+nn),-ncol(SVClass2)],kernelfunc)
    }
    disClass2median = mean(distanceSVC2)
    boundClass2 = disClass2median*a
  }  
  
  distance$X1 = factor(distance$X1)
  
  # Iterate over the distance vector and substitude in VSV1 the samples which overstep the threshold
  for(k in seq(along = c(1:nrow(org)))){
    if(is.na(distance[k,2])){
      VSV1[k,]=NA
    }
    else{
      if(!is.na(boundClass1)){
        if(distance[k,1] == levels(distance[[1]])[1]){
          if(distance[k,2] > (boundClass1)){
            VSV1[k,]=NA
          }
        }
      }else{
        if(!is.na(boundClass2)) {
          if(distance[k,1]== levels(distance[[1]])[2]){
            if(distance[k,2] > (boundClass2)){
              VSV1[k,]=NA
            }
          }
        }
      }
    }
  }
  return(VSV1)
}

#### evaluate distance to margin of modefied VSV in SVM model #########################################

## Sum(a_i * y_i * K(x_i, n)) + b
## Sum(coef_i * K(x_i, n)) - b
## for multiclass problems maybe use predict and $probabilities or with type = desicion
pred_one = function(modelfin, dataPoint, binaryClassProblem ){
  
  smallestDistance = 1000
  dataPointLabel = dataPoint[length(dataPoint)]
  for(l in seq(along = binaryClassProblem)){
    if(as.integer(dataPointLabel) %in% binaryClassProblem[[l]]){
      
      pred = sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
        modelfin@kernelf(xmatrix(modelfin)[[l]][j,],dataPoint[1:(length(dataPoint)-1)])*modelfin@coef[[l]][j]))-modelfin@b[l]
      
      if(abs(pred) < abs(smallestDistance))
        smallestDistance = pred
    }
  }
  return(smallestDistance)   
}


####calculation of mean and sd and export of .csv ###################################

ExCsvMSD = function (datadase, filename = NA){{
  
  datadase = as.matrix(datadase)
  n = ncol(datadase)
  MSDdata = matrix(data = NA, nrow = 3, ncol = n)
  
  rownames(MSDdata) = c("M","SD","MD")
  
  m = 1:n
  for (l in seq(along=m)){
    
    MSDdata[1,l] = mean(datadase[,l])
    MSDdata[2,l] = sd(datadase[,l])
    MSDdata[3,l] = median(datadase[,l])
  }
  
  
  MSDdata_final = rbind(datadase, MSDdata) 
  
  #export final kappa value table to .csv-file
  if(!missing(filename)){
    write.csv(MSDdata_final, filename)
  }
}
  return(MSDdata)
}

######## END FUNCTIONS ###############################################################

#input parameter:
## sample size to apply model
i=1
#position of sampleSizePor

inputPath ="cologne_res_100_L2-L13.csv"                               #file with input data
nR = 10                                               #definition of number of realizations
sMax = 1500                                             #definition of max. sample size
bound = c(0.3,0.6,0.9)                                  #threshold for radius around SV
boundMargin = c(1.5,1.0,0.5)                                    #threshold for distance of hyperplane
plotmatrix = c(3,3)
#sampleSizesPor = c(100,70,40,25,16,10,6,3,1)                         #vector with % of max
sampleSizesPor = c(60,50,40,25,16,10,6,3)
#colheader =c("100","70","40","25","16","10","6","3","1")       #corresponding column names
colheader = c("60","50","40","25","16","10","6","3")
sindexSVMDATA = 37                                                    #start of base data
numFeat = 18                                                          #number of features per level(dimensionality)
eindexSVMDATA = sindexSVMDATA + numFeat -1
objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                  "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                  "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
                  "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                  "label")

columnClass = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
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
                "factor","integer")

#####################################################################################
#####################################################################################


##INPUT DATA FROM dbf: 
#with training and testdata in one file
setwd(apply_model_path)
#working driectory
setwd("csv_data_r_import/cologne/scale")


generalDataPoolOrg = read.csv2(inputPath,header = T, sep =";",colClasses =columnClass)

### data set to apply modell and export results for visualisation in e.g. ArcGIS
data_modell_apply = generalDataPoolOrg[,sindexSVMDATA:eindexSVMDATA]

data_modell_apply_MS = generalDataPoolOrg
data_modell_apply_MS$REF <- factor(data_modell_apply_MS$REF)

#exclude unclassified and delete level of factor
generalDataPoolOrg = subset(generalDataPoolOrg, REF != "unclassified")
generalDataPoolOrg$REF <- factor(generalDataPoolOrg$REF)

#transform to 2-Class-Case "Bushes and trees VS rest
generalDataPoolOrg$REF = as.factor(generalDataPoolOrg$REF)



##### PRE-PROCESSING #####

##pre-processing of all data
normalizedFeat = generalDataPoolOrg[,1:(ncol(generalDataPoolOrg)-2)]
normalizedLabelUSE = generalDataPoolOrg[,(ncol(generalDataPoolOrg)-1):(ncol(generalDataPoolOrg))]

#normalization of  data ("range" scales the data to the interval [0, 1]; c("center", "scale") centers and scales the input data)
preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))


#apply range of basemodell to all level
##preprocess data for visualisation
normalized_data_modell_apply = predict(preProc, setNames(data_modell_apply,objInfoNames[-length(objInfoNames)]))


normalizedFeat2 = predict(preProc, setNames(normalizedFeat[,1:numFeat],objInfoNames[-length(objInfoNames)]))
normalizedFeat3 = predict(preProc, setNames(normalizedFeat[,(numFeat+1):(2*numFeat)],objInfoNames[-length(objInfoNames)]))

normalizedFeat5 = predict(preProc, setNames(normalizedFeat[,(3*numFeat+1):(4*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat6 = predict(preProc, setNames(normalizedFeat[,(4*numFeat+1):(5*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat7 = predict(preProc, setNames(normalizedFeat[,(5*numFeat+1):(6*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat8 = predict(preProc, setNames(normalizedFeat[,(6*numFeat+1):(7*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat9 = predict(preProc, setNames(normalizedFeat[,(7*numFeat+1):(8*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat10 = predict(preProc, setNames(normalizedFeat[,(8*numFeat+1):(9*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat11 = predict(preProc, setNames(normalizedFeat[,(9*numFeat+1):(10*numFeat)],objInfoNames[-length(objInfoNames)]))
#normalizedFeat12 = predict(preProc, setNames( normalizedFeat[,(10*numFeat+1):(11*numFeat)],objInfoNames[-length(objInfoNames)]))
#normalizedFeat13 = predict(preProc, setNames( normalizedFeat[,(11*numFeat+1):(12*numFeat)],objInfoNames[-length(objInfoNames)]))




normalizedFeat = cbind(normalizedFeat2,
                       normalizedFeat3,
                       normalizedFeatBase,
                       normalizedFeat5,
                       normalizedFeat6,
                       normalizedFeat7,
                       normalizedFeat8,
                       normalizedFeat9,
                       normalizedFeat10,
                       normalizedFeat11
                       #normalizedFeat12,
                       #normalizedFeat13
)

normalizedDataPoolAllLev = cbind( normalizedFeat, normalizedLabelUSE)

#normalize feature for multiscale:
nomalizedFeatMultiScale = generalDataPoolOrg[,1:(ncol(generalDataPoolOrg)-2)]
preProc = preProcess(nomalizedFeatMultiScale, method = "range")
nomalizedFeatMultiScale = predict(preProc, nomalizedFeatMultiScale)
normalizedDataPoolAllLevMultiScale = cbind( nomalizedFeatMultiScale[1:((sindexSVMDATA + 8*numFeat)-1)], normalizedLabelUSE)

normalized_data_modell_apply_MS = data_modell_apply_MS[,1:(ncol(data_modell_apply_MS)-2)]
normalized_data_modell_apply_MS = predict(preProc, normalized_data_modell_apply_MS)

normalized_data_modell_apply_MS = cbind( normalized_data_modell_apply_MS[1:((sindexSVMDATA + 8*numFeat)-1)])

#Split data in test and train data
splitdf <- split(normalizedDataPoolAllLev, normalizedDataPoolAllLev$USE)
trainDataPoolAllLev = as.data.frame(splitdf[[1]])
testDataAllLev = as.data.frame(splitdf[[2]])
validateDataAllLev = as.data.frame(splitdf[[3]])
rm(splitdf)


#Split data in test and train data Multiscale
splitdf <- split(normalizedDataPoolAllLevMultiScale, normalizedDataPoolAllLevMultiScale$USE)
trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
testDataAllLevMS = as.data.frame(splitdf[[2]])
validateDataAllLevMS = as.data.frame(splitdf[[3]])
rm(splitdf)

#reove use indicator in last column
trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]

#reove use indicator in last column MS
trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]

#split Validate Dateset in features and labels ans subset on basislevel of first SVM
validateFeatAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-2)]
validateLabels = validateDataAllLev[,(ncol(validateDataAllLev)-1)]

#split Validate Dateset in features and labels for MS
validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-2)]
validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS)-1)]

#label_test = testDataAllLev[,(ncol(testDataAllLev))]
#label_train = trainDataPoolAllLev[,(ncol(trainDataPoolAllLev))]

validateFeatsub = validateFeatAllLev[sindexSVMDATA:eindexSVMDATA]



#order train datapool by class label in alphabetical order:
trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]


##### create matrix for accuracy assessment and parameter export ##### 

#length and columnnames are specified in parameter setting (top)
KappasSVM_M = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasSVM_M) = colheader    

accuSVM_M = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuSVM_M) = colheader 

#create Kappa value matrix for unmodified SVM
KappasSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasSVM) = colheader     

accuSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuSVM) = colheader   

#create Kappa value matrix for invariant VSVM all Level
KappasVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasVSVM) = colheader 

accuVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuVSVM) = colheader
#create Kappa value matrix for invariant VSVM with self learning all Level
KappasVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasVSVM_SL) = colheader 

accuVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuVSVM_SL) = colheader 

#create Kappa value matrix for invariant VSVM with self learning + Unlabeled samples balance class all Level
KappasVSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasVSVM_SL_Un_b) = colheader 

accuVSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuVSVM_SL_Un_b) = colheader 


#create Kappa value matrix for invariant VSVM with self learning + unlabeled samples random selection all Level
KappasVSVM_SL_Un_r = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasVSVM_SL_Un_r) = colheader

accuVSVM_SL_Un_r = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuVSVM_SL_Un_r) = colheader


#create Kappa value matrix for invariant VSVM with self learning + unlabeled samples+ Virtual Unlabeled samples random selection all Level
KappasVSVM_SL_Vun_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasVSVM_SL_Vun_b) = colheader

accuVSVM_SL_Vun_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuVSVM_SL_Vun_b) = colheader


#create Kappa value matrix for invariant VSVM with self learning + Unlabeled samples balance class all Level
KappasSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappasSVM_SL_Un) = colheader 

accuSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(accuSVM_SL_Un) = colheader 

#set randomized seed for the random sampling procedure
seed = 5
nR = 2 
##loop over nR realizations
ra = 1:nR

for (i in seq(along=ra)){ 
  
  #current training data-set, uptdated after each iteration
  trainDataCur = trainDataPoolAllLev
  trainDataCurMS = trainDataPoolAllLevMS
  testDataCur = testDataAllLev
  testDataCurMS = testDataAllLevMS
  #initial seed value for randomized sampling
  seed = seed+sample(1:100, 1)
  
  counttrainDataPoolAllLev = ncol(trainDataPoolAllLev)
  
  ################################################################################
  #INPUT VARIABLES:
  
  #to get the probabilities right
  ###definition of apriori-probabilities (in the order in which the strata are given in the input data set)
  ##apriori-probabilities of class labels in alphabetical order
  
  
  #note: calculation on unique values ar rather representive??
  #pA = sum(trainDataCur$label == "bushes_trees")
  #pB = sum(trainDataCur$label == "facade")
  #pC = sum(trainDataCur$label == "meadow")
  #pD = sum(trainDataCur$label == "other_impervious_surface")
  #pE = sum(trainDataCur$label == "roofs")
  #pF = sum(trainDataCur$label == "shadow")
  
  pA = 1/6
  pB = 1/6
  pC = 1/6
  pD = 1/6
  pE = 1/6
  pF = 1/6
  #pG
  #PH
  #pI
  
  ###definition of training sample set sizes S [% of max. sample size](max. to min.)
  sampSizes = sampleSizesPor
  
  ##loop over training sample set sizes
  #definition of index vector
  n = 1:length(sampSizes)
  
  #start loop
  
  for (u in seq(along=n)){
    #current training data-set, uptdated after each iteration
    trainDataCur = trainDataPoolAllLev
    trainDataCurMS = trainDataPoolAllLevMS
    testDataCur = testDataAllLev
    testDataCurMS = testDataAllLevMS
    
    
    sCur = sMax*(sampSizes[u]/100)
    #sCur = sampSizes[j]/100
    ##definition of sample shares
    nA = round(sCur*pA)
    nB = round(sCur*pB)
    nC = round(sCur*pC)
    nD = round(sCur*pD)
    nE = round(sCur*pE)
    nF = round(sCur*pF)
    #nG = round(sCur*pG)
    #nH = round(sCur*pH)
    #nI = round(sCur*pI)
    shares = c(nA,nB,nC,nD,nE,nF#,nG,nH,nI
    )
    
    #set randomized seed for the random sampling procedure
    set.seed(seed)
    
    ###definition of sampling configuration (strata:random sampling without replacement)
    stratSamp = strata(trainDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
    
    ##get samples of trainDataCur and set trainDataCur new
    samples = getdata(trainDataCur, stratSamp)
    
    ####################################################### new implementation####################################
    ##############################################################################################################
    samplesID = samples[,182]
    trainDataCurRemaining <- trainDataCur[-c(samplesID), ]
    
    ###definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
    
    ##get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
    
    trainDataCurRemaining_b = samplesRemaining_b[,1:counttrainDataPoolAllLev]
    trainFeatRemaining_b = trainDataCurRemaining_b[,1:(counttrainDataPoolAllLev-1)]
    trainLabelsRemaining_b = trainDataCurRemaining_b[,counttrainDataPoolAllLev]
    trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]
    
    ##############################################################################################################
    ##############################################################################################################
    
    trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
    trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
    trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]
    
    stratSamp = strata(trainDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
    samples = getdata(trainDataCurMS, stratSamp)
    trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
    trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
    trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]
    
    ##subset for each outer iteration test data to speed up computing
    testDataCur = testDataCur[order(testDataCur[,ncol(testDataCur)]),]
    testDataCurMS = testDataCurMS[order(testDataCurMS[,ncol(testDataCurMS)]),]
    
    stratSamp = strata(testDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
    samples = getdata(testDataCur, stratSamp)
    testDataCur = samples[,1:ncol(testDataCur)]
    
    stratSamp = strata(testDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
    samples = getdata(testDataCurMS, stratSamp)
    testDataCurMS = samples[,1:ncol(testDataCurMS)]
    
    #split test feat from test label for later join with trainData
    testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
    testLabels = testDataCur[,ncol(testDataCur)]
    
    testFeatMS = testDataCurMS[,1:(ncol(testDataCurMS)-1)]
    testLabelsMS = testDataCurMS[,ncol(testDataCurMS)]
    
    #Do subset on base level
    testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]
    
    
    #get list with index of trainData to split between train and test in svmFit
    countTrainData = nrow(trainFeat)
    indexTrainData = list(c(1:countTrainData))
    
    countTrainDataMS = nrow(trainFeatMS)
    indexTrainDataMS = list(c(1:countTrainDataMS))
    
    #### multi scale SVM ##############################################################################
    
    
    #join of train and test test data (through indesTrainData in svmFit seperable)
    tuneFeatmultiScale = rbind(trainFeatMS, testFeatMS)
    tuneLabelmultiScale = unlist(list(trainLabelsMS, testLabelsMS))
    
    
    #SVM parameter tuning
    tunedSVMmultiScale = svmFit(tuneFeatmultiScale, tuneLabelmultiScale, indexTrainDataMS)
    
    ##run classification and accuracy assesment for unmodifierd SV
    #predict labels of test data
    predLabelsSVMmultiScale = predict(tunedSVMmultiScale, validateFeatAllLevMS)
    
    ##accuracy assessment
    accSVM_M = confusionMatrix(predLabelsSVMmultiScale, validateLabelsMS)
    
    #get current kappa value
    KSVM_M = accSVM_M$overall["Kappa"]
    o_accSVM_M= accSVM_M$overall["Accuracy"]
    
    #write current kappa value and accuracyin Kappas matrix
    KappasSVM_M[i,u] = as.numeric(KSVM_M)
    accuSVM_M[i,u] = as.numeric(o_accSVM_M)
    
    #### SVM base for invariants ######################################################################
    
    ##subset on L_4
    eindexSVMDATA = sindexSVMDATA + numFeat -1
    trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA]
    
    #join of train and test test data (through indesTrainData in svmFit seperable)
    tuneFeat = rbind(trainFeat, testFeatsub)
    tuneLabel = unlist(list(trainLabels, testLabels))
    
    #SVM parameter tuning
    tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
    
    ##run classification and accuracy assesment for unmodifierd SV
    #predict labels of test data
    predLabelsSVM = predict(tunedSVM, validateFeatsub)
    
    ##accuracy assessment
    accSVM = confusionMatrix(predLabelsSVM, validateLabels)

    #get current kappa value
    KSVM = accSVM$overall["Kappa"]
    o_accSVM= accSVM$overall["Accuracy"]
    
    #write current kappa and accuracy value in Kappas matrix
    KappasSVM[i,u] = as.numeric(KSVM)
    accuSVM[i,u] = as.numeric(o_accSVM)
    ############# VSVM on all Level ############################################################################################
    
    #get SV of tunedSVM
    SVindex = tunedSVM$finalModel@SVindex
    SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
    #get VSV, means rows of SV but with subset on diferent level
    SVL2 = trainDataCur[SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
    SVL3 = trainDataCur[SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]
    
    SVL5 = trainDataCur[SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
    SVL6 = trainDataCur[SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
    SVL7 = trainDataCur[SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
    SVL8 = trainDataCur[SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
    SVL9 = trainDataCur[SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
    SVL10 = trainDataCur[SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
    SVL11 = trainDataCur[SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]
    #SVL12 = trainDataCur[SVindex,c((sindexSVMDATA + 8*numFeat):((sindexSVMDATA + 9*numFeat)-1),ncol(trainDataCur))]
    #SVL13 = trainDataCur[SVindex,c((sindexSVMDATA + 9*numFeat):((sindexSVMDATA + 10*numFeat)-1),ncol(trainDataCur))]
    
    #bind original SV with modified to new train data set
    SVinvar = rbind(setNames(SVtotal,objInfoNames),
                    setNames(SVL2,objInfoNames),
                    setNames(SVL3,objInfoNames),
                    setNames(SVL5,objInfoNames), 
                    setNames(SVL6,objInfoNames),
                    setNames(SVL7,objInfoNames),
                    setNames(SVL8,objInfoNames),
                    setNames(SVL9,objInfoNames),
                    setNames(SVL10,objInfoNames),
                    setNames(SVL11,objInfoNames)
                   # setNames(SVL12,objInfoNames),
                   # setNames(SVL13,objInfoNames)
                   )
    
    
    
    #split for training to feature and label
    trainFeatVSVM = SVinvar[,1:(ncol(SVinvar)-1)]
    trainLabelsVSVM = SVinvar[,ncol(SVinvar)]
    
    #get list with index of trainData to split between train and test in svmFit
    countTrainData = nrow(SVinvar)
    indexTrainData = list(c(1:countTrainData))
    
    #join of train and test test data (through indesTrainData in svmFit seperable)
    names = objInfoNames[1:length(objInfoNames)-1]
    tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
    tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))
    
    #VSVM parameter tuning
    tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
    tunedVSVM_apply = tunedVSVM
    
    ##run classification and accuracy assesment for modifierd SV
    ##predict labels of test data
    predLabelsVSVM = predict(tunedVSVM, validateFeatsub)
    
    ##accuracy assessment
    accVSVM = confusionMatrix(predLabelsVSVM, validateLabels)

    #get current kappa value
    KVSVM = accVSVM$overall["Kappa"]
    o_accVSVM= accVSVM$overall["Accuracy"]
    
    # write current kappa and accuracy value in Kappas matrix
    KappasVSVM[i,u] = as.numeric(KVSVM)
    accuVSVM[i,u] = as.numeric(o_accVSVM)
    
    ############################################################## new implementation
    #############################################################
    ###########################################################
    REF_b = predict(tunedVSVM, trainDataCurRemainingsub_b)
    
    #get SV of unlabelled samples 
    SVindexUn_b = 1:nrow(trainDataCurRemainingsub_b)
    SVtotalUn_b = trainDataCurRemaining_b[SVindexUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
    SVtotalUn_b = cbind(SVtotalUn_b, REF_b)
    
    #get VSs, means rows of SV but with subset on diferent level
    SVL2Un_b = cbind(trainDataCurRemaining[SVindexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)
    SVL3Un_b = cbind(trainDataCurRemaining[SVindexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)
    
    SVL5Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)
    SVL6Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)
    SVL7Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)
    SVL8Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)
    SVL9Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)
    SVL10Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)
    SVL11Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b)
    #SVL12 = cbind(trainDataCur[SVindex,c((sindexSVMDATA + 8*numFeat):((sindexSVMDATA + 9*numFeat)-1))], REF)
    #SVL13 = cbind(trainDataCur[SVindex,c((sindexSVMDATA + 9*numFeat):((sindexSVMDATA + 10*numFeat)-1))], REF)
    
    #bind original SV with modified to new train data set
    SVinvarUn_b = rbind(setNames(SVtotalUn_b,objInfoNames),
                        setNames(SVL2Un_b,objInfoNames),
                        setNames(SVL3Un_b,objInfoNames),
                        setNames(SVL5Un_b,objInfoNames), 
                        setNames(SVL6Un_b,objInfoNames),
                        setNames(SVL7Un_b,objInfoNames),
                        setNames(SVL8Un_b,objInfoNames),
                        setNames(SVL9Un_b,objInfoNames),
                        setNames(SVL10Un_b,objInfoNames),
                        setNames(SVL11Un_b,objInfoNames)
                        #setNames(SVL12,objInfoNames)
                        #setNames(SVL13,objInfoNames)
    )
    
    SVinvarUn_b = rbind(setNames(SVinvar,objInfoNames),
                        setNames(SVinvarUn_b,objInfoNames)
    )
    
    
    ##### VSVM with evaluation of VSV with all Level AND  BALANCED UNLABELLED SAMPLES #####
    
    ## iteration over bound to test different bound thresholds determining the radius of acseption
    actKappa = 0
    
    
    ## records which 2 classes are involved in 2 class problems
    binaryClassProblem = list()
    
    for(jj in seq(along = c(1:length(tunedVSVM$finalModel@xmatrix)))){
      binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]] ,ncol(trainDataCur)]))
    }
    
    
    ## iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along = c(1:length(bound)))){
      
      
      ## remove VSV which are not located within certain distance to org.SV; 
      # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
      # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
      SVinvarRadiUn_b = rbind(setNames(rem_extrem(SVtotal, SVL2, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL3, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL5, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL6, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL7, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL8, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL9, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL10, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL11, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL2Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL3Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL5Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL6Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL7Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL8Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL9Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL10Un_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalUn_b, SVL11Un_b, bound[jj]),objInfoNames)
      )  # remove NAs 
      
      # remove NAs 
      SVinvarRadiUn_b = na.omit(SVinvarRadiUn_b)
      
      ## iterating over boundMargin to test different threshold on margin distance
      for (kk in seq(along = c(1:length(boundMargin)))){
        
        # remove VSV which are not located in certain distance to desicion function
        # data.frame to store elected VSV within the margin
        SVinvarUn_b=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
        
        #iterate over SVinvarRadi and evaluate distance to hyperplane
        #implementation checks class membership for case that each class should be evaluate on different bound
        for(m in seq(along = c(1:nrow(SVinvarRadiUn_b)))){
          
          signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadiUn_b[m,]),binaryClassProblem))
          
          if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
            SVinvarUn_b = rbind(SVinvarUn_b, SVinvarRadiUn_b[m,])
          }
        }
        
        #merge elected VSV with original SV
        SVinvar_orgUn_b = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarUn_b,objInfoNames))
        
        SVinvar_orgUn_b=na.omit(SVinvar_orgUn_b)
        
        #split for training to feature and label
        trainFeatVSVMUn_b = SVinvar_orgUn_b[,1:(ncol(SVinvar_orgUn_b)-1)]
        trainLabelsVSVMUn_b = SVinvar_orgUn_b[,ncol(SVinvar_orgUn_b)]
        
        #get list with index of trainData to split between train and test in svmFit
        countTrainDataUn_b = nrow(SVinvar_orgUn_b)
        indexTrainDataUn_b = list(c(1:countTrainDataUn_b))
        
        #join of train and test data (through indesTrainData in svmFit seperable)
        names = objInfoNames[1:length(objInfoNames)-1]
        tuneFeatVSVMUn_b = rbind(trainFeatVSVMUn_b, setNames(testFeatsub, names))
        tuneLabelsVSVMUn_b = unlist(list(trainLabelsVSVMUn_b, testLabels))
        
        #VSVMcontroll parameter tuning
        tunedVSVMUn_b = svmFit(tuneFeatVSVMUn_b, tuneLabelsVSVMUn_b, indexTrainDataUn_b)
        
        #of all Different bound dettings get the one with best Kappa ans save its model
        if(actKappa < tunedVSVMUn_b$resample$Kappa){
          bestFittingModelUn_b = tunedVSVMUn_b
          actKappa = tunedVSVMUn_b$resample$Kappa
        }
      }
    }
    
    
    ##run classification and accuracy assesment for the best bound setting
    ##predict labels of test data
    predLabelsVSVMsumUn_b = predict(bestFittingModelUn_b, validateFeatsub)
    
    ##accuracy assessment
    accVSVM_SL_Un_b = confusionMatrix(predLabelsVSVMsumUn_b, validateLabels)
    
    #get current kappa value
    KVSVM_SL_Un_b = accVSVM_SL_Un_b$overall["Kappa"]
    o_accVSVM_SL_Un_b= accVSVM_SL_Un_b$overall["Accuracy"]
    
    # write current kappa and accuracy value in Kappas matrix
    KappasVSVM_SL_Un_b[i,u] = as.numeric(KVSVM_SL_Un_b)
    accuVSVM_SL_Un_b[i,u] = as.numeric(o_accVSVM_SL_Un_b)
    
    ##### VSVM with evaluation of VSV with all Level######################################
    ## Approch like:  Lu et al.(2016): A Novel Synergetic Classification Approach for  Hyperspectral and Panchromatic Images Based on Self-Learning
    
    ## iteration over bound to test different bound thresholds determining the radius of acseption
    actKappa = 0
    
    ## records which 2 classes are involved in 2 class problems
     binaryClassProblem = list()
    
    for(jj in seq(along = c(1:length(tunedVSVM$finalModel@xmatrix)))){
      binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]] ,ncol(trainDataCur)]))
    }
   
    
    
    ## iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along = c(1:length(bound)))){
      
      
      ## remove VSV which are not located within certain distance to org.SV 
      SVinvarRadi = rbind(setNames(rem_extrem(SVtotal, SVL2, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL3, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL5, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL6, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL7, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL8, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL9, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL10, bound[jj]),objInfoNames),
                          setNames(rem_extrem(SVtotal, SVL11, bound[jj]),objInfoNames)
                          #setNames(rem_extrem(SVtotal, SVL12, bound[jj]),objInfoNames),
                          #setNames(rem_extrem(SVtotal, SVL13, bound[jj]),objInfoNames)
                          )  
      # remove NAs 
      SVinvarRadi = na.omit(SVinvarRadi)
      
      ## iterating over boundMargin to test different threshold on margin distance
      for (kk in seq(along = boundMargin)){
        
        # remove VSV which are not located in certain distance to desicion function
        SVinvar=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
        
        for(m in seq(along = c(1:nrow(SVinvarRadi)))){
          signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadi[m,]),binaryClassProblem))
       
            if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
              SVinvar = rbind(SVinvar, SVinvarRadi[m,])
            }
          }
        
        SVinvar_org = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvar,objInfoNames))
        
        SVinvar_org=na.omit(SVinvar_org)
        
        #split for training to feature and label
        trainFeatVSVM = SVinvar_org[,1:(ncol(SVinvar_org)-1)]
        trainLabelsVSVM = SVinvar_org[,ncol(SVinvar_org)]
        
        #get list with index of trainData to split between train and test in svmFit
        countTrainData = nrow(SVinvar_org)
        indexTrainData = list(c(1:countTrainData))
        
        #join of train and test test data (through indesTrainData in svmFit seperable)
        names = objInfoNames[1:length(objInfoNames)-1]
        tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
        tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))
        
        #VSVM parameter tuning
        tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
        
        #of all Different bound dettings get the one with best Kappa ans save its model
        if(actKappa < tunedVSVM$resample$Kappa){
          bestFittingModel = tunedVSVM
          actKappa = tunedVSVM$resample$Kappa
        }
      }
    }
     ##run classification and accuracy assesment for the best bound setting
     ##predict labels of test data
     predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
     
     predLabels_data_modell_apply = predict(bestFittingModel, normalized_data_modell_apply)
    
     ##accuracy assessment
     accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
     
     #get current kappa value
     KVSVM_SL = accVSVM_SL$overall["Kappa"]
     o_accVSVM_SL= accVSVM_SL$overall["Accuracy"]
     
     # write current kappa and accuracy value in Kappas matrix
     KappasVSVM_SL[i,u] = as.numeric(KVSVM_SL)
     accuVSVM_SL[i,u] = as.numeric(o_accVSVM_SL)
     
     
     ######################################################################################################################
     ######################################################################################################################
     ##### VSVM sL with VIRTUAL UNLABELED SAMPLES, evaluation of VSV with all Level AND UNBALLANCED (Balanced) UNLABELLED SAMPLES  #####
     
     REF_v = predict(tunedVSVMUn_b, trainDataCurRemainingsub_b)
     
     #get SV of unlabelled samples 
     SVindexvUn_b = 1:nrow(trainDataCurRemainingsub_b)
     SVtotalvUn_b = trainDataCurRemaining_b[SVindexvUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
     SVtotalvUn_b = cbind(SVtotalUn_b, REF_v)
     
     #extracting previously assigned reference coloumn 
     SVtotalvUn_bFeat = SVtotalvUn_b[,1:(ncol(SVtotalvUn_b)-2)]
     REF_v = SVtotalvUn_b[,(ncol(SVtotalvUn_b))]
     SVtotalvUn_b = cbind(SVtotalvUn_bFeat,REF_v)
     
     
     #get VSs, means rows of SV but with subset on diferent level
     SVL2vUn_b = cbind(trainDataCurRemaining[SVindexvUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)
     SVL3vUn_b = cbind(trainDataCurRemaining[SVindexvUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)
     
     SVL5vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)
     SVL6vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)
     SVL7vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)
     SVL8vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)
     SVL9vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)
     SVL10vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)
     SVL11vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v)
     #SVL12 = cbind(trainDataCur[SVindex,c((sindexSVMDATA + 8*numFeat):((sindexSVMDATA + 9*numFeat)-1))], REF)
     #SVL13 = cbind(trainDataCur[SVindex,c((sindexSVMDATA + 9*numFeat):((sindexSVMDATA + 10*numFeat)-1))], REF)
     
     
     #bind original SV with modified to new train data set
     SVinvarvUn_b = rbind(setNames(SVtotalvUn_b,objInfoNames),
                          setNames(SVL2vUn_b,objInfoNames),
                          setNames(SVL3vUn_b,objInfoNames),
                          setNames(SVL5vUn_b,objInfoNames), 
                          setNames(SVL6vUn_b,objInfoNames),
                          setNames(SVL7vUn_b,objInfoNames),
                          setNames(SVL8vUn_b,objInfoNames),
                          setNames(SVL9vUn_b,objInfoNames),
                          setNames(SVL10vUn_b,objInfoNames),
                          setNames(SVL11vUn_b,objInfoNames)
                          #setNames(SVL12,objInfoNames)
                          #setNames(SVL13,objInfoNames)
     )
     
     SVinvarvUn_b = rbind(setNames(SVinvar,objInfoNames),
                          setNames(SVinvarvUn_b,objInfoNames)
     )
     
     ##### VSVM with evaluation of VSV with all Level 
     ## Approch like:  Lu et al.(2016): A Novel Synergetic Classification Approach for  Hyperspectral and Panchromatic Images Based on Self-Learning
     
     ## iteration over bound to test different bound thresholds determining the radius of acseption
     actKappa = 0
     
     ## records which 2 classes are involved in 2 class problems
     binaryClassProblem = list()
     
     for(jj in seq(along = c(1:length(tunedVSVM$finalModel@xmatrix)))){
       binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]] ,ncol(trainDataCur)]))
     }
     
     
     ## iteration over bound to test different bound thresholds determining the radius of acception
     for(jj in seq(along = c(1:length(bound)))){
       
       
       ## remove VSV which are not located within certain distance to org.SV; 
       # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
       # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
       SVinvarRadivUn = rbind(setNames(rem_extrem(SVtotal, SVL2, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL3, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL5, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL6, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL7, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL8, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL9, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL10, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotal, SVL11, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL2vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL3vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL5vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL6vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL7vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL8vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL9vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL10vUn_b, bound[jj]),objInfoNames),
                              setNames(rem_extrem(SVtotalvUn_b, SVL11vUn_b, bound[jj]),objInfoNames)
                              #setNames(rem_extrem(SVtotal, SVL12, bound[jj]),objInfoNames)
                              #setNames(rem_extrem(SVtotal, SVL13, bound[jj]),objInfoNames)
       )  # remove NAs 
       
       # remove NAs 
       SVinvarRadivUn = na.omit(SVinvarRadivUn)
       
       ## iterating over boundMargin to test different threshold on margin distance
       for (kk in seq(along = c(1:length(boundMargin)))){
         
         # remove VSV which are not located in certain distance to desicion function
         # data.frame to store elected VSV within the margin
         SVinvarvUn_b=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
         
         #iterate over SVinvarRadi and evaluate distance to hyperplane
         #implementation checks class membership for case that each class should be evaluate on different bound
         for(m in seq(along = c(1:nrow(SVinvarRadivUn)))){
           
           signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadivUn[m,]),binaryClassProblem))
           
           if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
             SVinvarvUn_b = rbind(SVinvarvUn_b, SVinvarRadivUn[m,])
           }
         }
         
         #merge elected VSV with original SV
         SVinvar_orgvUn_b = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarvUn_b,objInfoNames))
         
         SVinvar_orgvUn_b=na.omit(SVinvar_orgvUn_b)
         
         #split for training to feature and label
         trainFeatVSVMvUn_b = SVinvar_orgvUn_b[,1:(ncol(SVinvar_orgvUn_b)-1)]
         trainLabelsVSVMvUn_b = SVinvar_orgvUn_b[,ncol(SVinvar_orgvUn_b)]
         
         #get list with index of trainData to split between train and test in svmFit
         countTrainDatavUn_b = nrow(SVinvar_orgvUn_b)
         indexTrainDatavUn_b = list(c(1:countTrainData))
         
         #join of train and test data (through indesTrainData in svmFit seperable)
         names = objInfoNames[1:length(objInfoNames)-1]
         tuneFeatVSVMvUn_b = rbind(trainFeatVSVMvUn_b, setNames(testFeatsub, names))
         tuneLabelsVSVMvUn_b = unlist(list(trainLabelsVSVMvUn_b, testLabels))
         
         #VSVMcontroll parameter tuning
         tunedVSVMvUn_b = svmFit(tuneFeatVSVMvUn_b, tuneLabelsVSVMvUn_b, indexTrainDatavUn_b)
         
         #of all Different bound dettings get the one with best Kappa ans save its model
         if(actKappa < tunedVSVMvUn_b$resample$Kappa){
           bestFittingModelvUn_b = tunedVSVMvUn_b
           actKappa = tunedVSVMvUn_b$resample$Kappa
         }
       }
     }
     
     ##run classification and accuracy assesment for the best bound setting
     ##predict labels of test data
     predLabelsVSVMvUn_bsum = predict(bestFittingModelvUn_b, validateFeatsub)
     
     ##accuracy assessment
     accVSVM_SL_vUn_b = confusionMatrix(predLabelsVSVMvUn_bsum, validateLabels)
     
     #get current kappa value
     KVSVM_SL_vUn_b = accVSVM_SL_vUn_b$overall["Kappa"]
     o_accVSVM_SL_vUn_b= accVSVM_SL_vUn_b$overall["Accuracy"]
     
     # write current kappa and accuracy value in Kappas matrix
     KappasVSVM_SL_Vun_b[i,u] = as.numeric(KVSVM_SL_vUn_b)
     accuVSVM_SL_Vun_b[i,u] = as.numeric(o_accVSVM_SL_vUn_b)
     
     ######################################################################################################################
     ######################################################################################################################
     
     ##### SVM with evaluation of SL with Level 4 AND  BALANCED UNLABELLED SAMPLE (SVM-SL +Unlabeled Samples Approach) #####
     
     #get SV of tunedSVM
     SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
     SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
     
     
     ## iteration over bound to test different bound thresholds determining the radius of acseption
     actKappa = 0
     
     ## records which 2 classes are involved in 2 class problems
     binaryClassProblem = list()
     
     for(jj in seq(along = c(1:length(tunedVSVM$finalModel@xmatrix)))){
       binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]] ,ncol(trainDataCur)]))
     }
     
     ## iteration over bound to test different bound thresholds determining the radius of acception
     for(jj in seq(along = c(1:length(bound)))){
       
       
       ## remove VSV which are not located within certain distance to org.SV; 
       # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
       # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
       
       SVinvarRadiUn = rbind(setNames(rem_extrem(SVtotalUn_b, SVL2Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL3Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL5Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL6Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL7Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL8Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL9Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL10Un_b, bound[jj]),objInfoNames),
                             setNames(rem_extrem(SVtotalUn_b, SVL11Un_b, bound[jj]),objInfoNames)
       )  # remove NAs 
       
       # remove NAs 
       SVinvarRadiUn = na.omit(SVinvarRadiUn)
       
       ## iterating over boundMargin to test different threshold on margin distance
       for (kk in seq(along = c(1:length(boundMargin)))){
         
         # remove VSV which are not located in certain distance to desicion function
         # data.frame to store elected VSV within the margin
         SVinvarUn=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
         
         #iterate over SVinvarRadi and evaluate distance to hyperplane
         #implementation checks class membership for case that each class should be evaluate on different bound
         for(m in seq(along = c(1:nrow(SVinvarRadiUn)))){
           signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadiUn[m,]),binaryClassProblem))
           
           if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
             SVinvarUn = rbind(SVinvarUn, SVinvarRadiUn[m,])
           }
         }
           
         
         #merge elected VSV with original SV
         SVinvar_orgUn = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarUn,objInfoNames))
         
         SVinvar_orgUn=na.omit(SVinvar_orgUn)
         
         #split for training to feature and label
         trainFeatSVMUn = SVinvar_orgUn[,1:(ncol(SVinvar_orgUn)-1)]
         trainLabelsSVMUn = SVinvar_orgUn[,ncol(SVinvar_orgUn)]
         
         #get list with index of trainData to split between train and test in svmFit
         countTrainDataUn = nrow(SVinvar_orgUn)
         indexTrainDataUn = list(c(1:countTrainDataUn))
         
         #join of train and test data (through indesTrainData in svmFit seperable)
         names = objInfoNames[1:length(objInfoNames)-1]
         tuneFeatSVMUn = rbind(trainFeatSVMUn, setNames(testFeatsub, names))
         tuneLabelsSVMUn = unlist(list(trainLabelsSVMUn, testLabels))
         
         #VSVMcontroll parameter tuning
         tunedSVMUn = svmFit(tuneFeatSVMUn, tuneLabelsSVMUn, indexTrainDataUn)
         
         #of all Different bound settings get the one with best Kappa ans save its model
         if(actKappa < tunedSVMUn$resample$Kappa){
           bestFittingModelUn = tunedSVMUn
           actKappa = tunedSVMUn$resample$Kappa
         }
       }
     }
     
     ##run classification and accuracy assesment for the best bound setting
     ##predict labels of test data
     predLabelsSVMsumUn = predict(bestFittingModelUn, validateFeatsub)
     
     ##accuracy assessment
     accSVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)
     
     #get current kappa value
     KVSVM_SL_Un = accSVM_SL_Un$overall["Kappa"]
     o_accSVM_SL_Un= accSVM_SL_Un$overall["Accuracy"]
     
     # write current kappa and accuracy value in Kappas matrix
     KappasSVM_SL_Un[i,u] = as.numeric(KVSVM_SL_Un)
     accuSVM_SL_Un[i,u] = as.numeric(o_accSVM_SL_Un)
     
     
     
     #setwd("../../../")
     #setwd("C:/Users/tunc_oz/Desktop/apply_model/results/Cologne/Multi_Scale_60")
     
     
     
     ########################################################################################################################
     ########################################################################################################################
     save(accSVM,accSVM_M,accVSVM,accVSVM_SL,accSVM_SL_Un,accVSVM_SL_vUn_b,accVSVM_SL_Un_b, file =paste("ColScaleMulti_accuracyMatrix_image_",shares[1],"real",i,"samples.RData",sep=""))
     #save(tunedSVM,tunedSVM_M,tunedVSVM,tunedVSVM_SL,tunedVSVMvUn_b,tunedVSVMUn_b, tunedSVM_SL_Un,file =paste("ColScaleMulti_modelObjects_image_",shares[nA],"real",i,"samples.RData",sep=""))
     
     
  }
  
}
     
    ############################################################
    #get mean and SD of each kappas table and optional print .csv files with Kappas, SV and parameter
    #change variable names of the S3VM methods according to unlabeled samples for ex b=10
    

    setwd("C:/Users/tunc_oz/Desktop/apply_model/results/hyperparameter")
    #accuVSVM_SL_Vun_b = accuVSVM_SL_Vun_b[1:4,]


    MSDSVM= ExCsvMSD(KappasSVM)
    MSDSVM_M = ExCsvMSD(KappasSVM_M)
    MSDSVM_SL_Un_60 = ExCsvMSD(KappasSVM_SL_Un)
    MSDVSVM = ExCsvMSD(KappasVSVM)
    MSDVSVM_SL = ExCsvMSD(KappasVSVM_SL)
    MSDVSVM_SL_Un_b_60 = ExCsvMSD(KappasVSVM_SL_Un_b)
    MSDVSVM_SL_Vun_b_60 = ExCsvMSD(KappasVSVM_SL_Vun_b)
    
    
    
    MDACSVM = ExCsvMSD(accuSVM)
    MDACSVM_M= ExCsvMSD(accuSVM_M)
    MDACSVM_SL_Un_60= ExCsvMSD(accuSVM_SL_Un)
    MDACVSVM= ExCsvMSD(accuVSVM)
    MDACVSVM_SL= ExCsvMSD(accuVSVM_SL)
    MDACVSVM_SL_Un_b_60= ExCsvMSD(accuVSVM_SL_Un_b)
    MDACVSVM_SL_Vun_b_60 = ExCsvMSD(accuVSVM_SL_Vun_b)
    
    
    #save mean and sd of each kappa matrices for all variations ##
    save(MSDSVM,MSDSVM_M,MSDSVM_SL_Un_60,MSDVSVM,MSDVSVM_SL,MSDVSVM_SL_Un_b_60,MSDVSVM_SL_Vun_b_60,file =paste("ColScaleMulti_KappaSDMatrix_",(2*b),"unlabledsamples.RData",sep=""))
    save(MDACSVM,MDACSVM_M,MDACSVM_SL_Un_60,MDACVSVM,MDACVSVM_SL,MDACVSVM_SL_Un_b_60,MDACVSVM_SL_Vun_b_60,file =paste("ColScaleMulti_AccuracyMatrix_",(2*b),"unlabledsamples.RData",sep=""))
    save(accuSVM,accuSVM_M,accuSVM_SL_Un, accuVSVM, accuVSVM_SL,accuVSVM_SL_Un_b,accuVSVM_SL_Vun_b, file = paste("ColScaleMultiAccuracyMax", nR, "realizations",(2*b),"unlabeledsamp.RData", sep =""))

    ############################################################
    ############################################################
    ##                      apply SVM-SL-Un                   ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(bestFittingModelUn, normalized_data_modell_apply)
    
    tunedSVM_SL_Un = bestFittingModelUn
    
    outputfile = paste("ColScaleMulti_SVM_SL_Un",b,"unl",shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    ############################################################
    ############################################################
    ##                      apply VSVM-SL-Un-b                ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(bestFittingModelUn_b, normalized_data_modell_apply)
    
    tunedVSVM_SL_Unb = bestFittingModelUn_b
    
    outputfile = paste("ColScaleMulti_VSVM_SL_Unb",b,"unl",shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    
    ############################################################
    ############################################################
    ##                      apply VSVM-SL-Un-Vun              ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(bestFittingModelvUn_b, normalized_data_modell_apply)
    
    tunedVSVM_SL_vUn = bestFittingModelvUn_b
    
    outputfile = paste("ColScaleMulti_VSVM_SL_VUn",b,"unl",shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    
    
    ############################################################
    ############################################################
    ##                      apply VSVM-SL                     ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(bestFittingModel, normalized_data_modell_apply)
    
    tunedVSVM_SL = bestFittingModel
    
    outputfile = paste("ColScaleMulti_VSVM_SL_",shares[1],shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    ##accuracy assessment
    accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
    
    
    ############################################################
    ############################################################
    ##                      apply SVM                         ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(tunedSVM, normalized_data_modell_apply)
    
    outputfile = paste("ColScaleMulti_SVM_",shares[1],shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    ############################################################
    ############################################################
    ##                      apply SVM-M                      ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(tunedSVMmultiScale, normalized_data_modell_apply_MS)
    
    tunedSVM_M = tunedSVMmultiScale
    
    
    outputfile = paste("ColScaleMulti_SVM_M_",shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
    ############################################################
    ############################################################
    ##                      apply VSVM                        ##
    ############################################################
    ############################################################
    
    predLabels_data_modell_apply = predict(tunedVSVM_apply, normalized_data_modell_apply)
    
    tunedVSVM = tunedVSVM_apply
    
    outputfile = paste("ColScaleMulti_VSVM_",shares[nA],"real",i,"samples.csv",sep="")
    write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
    
