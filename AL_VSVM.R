library(caret)
library(kernlab)
library(sampling)
library(foreign)
library(e1071)

# Define the class sample size 
sample_size = 2

# Define the size of unlabeled samples in each class
balanced_unlabeled_samples = 30
random_unlabeled_samples = 60

path = "D:/tunc_oz/apply_model"

########################################  Utils  ########################################

# Coarse and Narrow grid search for SVM parameters tuning
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

# Euclidean Distance between two points lying in the input space
euc_dis = function(a, b){
  temp = 0
  for(ii in seq(along = c(1:length(a)))){
    temp = temp +((a[[ii]]-b[[ii]])^2)
  }
  return(sqrt(temp))
}

# Kernel distance between two point lying in the hyperspace
kern_dis = function(a, b, kernelfunc){
  a = unlist(a)
  b = unlist(b)
  dk =sqrt( kernelfunc(a,a)+kernelfunc(b,b)-2*kernelfunc(a,b))
  return(dk)
}

# Evaluate the distance between Virtual Support Vectors and Support Vectors lying in the input space
rem_extrem = function(org, VSV1, a){      
  
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()
  
  # save label of sample and the distance between SV and VSV for each pair of SV and VSV
  for(l in seq(along = c(1:nrow(org)))){
    distance[l,1] =as.character( org[l,ncol(org)])
    distance[l,2] = euc_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)])
  }
  
  # split SV according to its classes
  SVClass1=org[which(org$REF==levels(org$"REF")[[1]]),]
  SVClass2=org[which(org$REF==levels(org$"REF")[[2]]),]
  
  # compute the distance for each SV in Class1 to all the SV in class1, 
  # get the mean of the distances and multiply it with a to get the final threshold
  if(nrow(SVClass1)>0){
    for(n in seq(along = 1:(nrow(SVClass1)-1))){
      for(nn in seq(along = c(n:(nrow(SVClass1)-1))))
        distanceSVC1[length(distanceSVC1)+1] = euc_dis(SVClass1[n,-ncol(SVClass1)], SVClass1[(n+nn),-ncol(SVClass1)])
    }
    
    disClass1median = mean(distanceSVC1)
    boundClass1 = disClass1median*a
  }
  
  # calculate the distance for each SV in Class2 to all the SV in  class2, 
  # get the mean of the distances and multiply it with a to get the final threshold
  if(nrow(SVClass2)>0){
    for(n in seq(along = 1:(nrow(SVClass2)-1))){
      for(nn in seq(along = c(n:(nrow(SVClass2)-1))))
        distanceSVC2[length(distanceSVC2)+1] = euc_dis(SVClass2[n,-ncol(SVClass2)], SVClass2[(n+nn),-ncol(SVClass2)])
    }
    disClass2median = mean(distanceSVC2)
    boundClass2 = disClass2median*a
  }  
  
  distance$X1 = factor(distance$X1)
  
  # Iterate over the distance vector and substitute in VSV1 the samples which overstep the threshold
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

# Evaluate the distance between Virtual Support Vectors and Support Vectors lying in the hyperspace
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
    
    disClass1median = mean(distanceSVC1)
    boundClass1 = disClass1median*a
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
  
  # Iterate over the distance vector and substitute in VSV1 the samples which overstep the threshold
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

pred_one = function(modelfin, dataPoint ){
  pred = sum(sapply(1:modelfin@nSV, function(j) 
  modelfin@kernelf(xmatrix(modelfin)[[1]][j,],dataPoint)*modelfin@coef[[1]][j]))-modelfin@b
  
  return(pred)   
}

pred_one = function(model, data_point) {
  # Extract necessary components from the SVM model
  support_vectors <- model@nSV
  kernel_function <- model@kernelf
  coefficients <- model@coef[[1]]
  intercept <- model@b
  
  # Initialize prediction variable
  prediction <- 0
  
  # Iterate over each support vector
  for (j in 1:support_vectors) {
    # Compute kernel function value between the j-th support vector and the data point
    kernel_value <- kernel_function(xmatrix(model)[[1]][j,], data_point)
    
    # Multiply kernel value by the corresponding coefficient and add to prediction
    weighted_value <- kernel_value * coefficients[j]
    prediction <- prediction + weighted_value
  }
  
  # Subtract intercept to get the final prediction
  final_prediction <- prediction - intercept
  
  return(final_prediction)
}

# Evaluate the distance between samples and Support Vectors lying in the hyperspace
uncertainty_dist_v2 = function(org, samp){
  #add two parameters org = tunedXXX and samp = data set to add distance column 
  
  #create dataframe to store label and distance
  distance = data.frame(matrix(nrow=nrow(samp),ncol=2))
  colnames(distance) = c("control_label", "distance")
  
  print("Progress:")
  increment_size <- floor(nrow(samp) / 100)
  for(k in seq(along = c(1:nrow(samp)))){
    
    distance[k,1] =as.character(samp[k,ncol(samp)])
    
    signa = as.numeric(pred_one(org$finalModel, unlist(samp[k,-ncol(samp)])))
    
    if (signa > 0){
      distance[k,2] = signa
    }else{
      distance[k,2] = signa*(-1)
    }
    
    progress <- (k / nrow(samp)) * 100
    if (k %% increment_size == 0) {
      print(paste(round(progress, 1), "%"))
    }
  } 
  
  #normalize distances
  preProc = preProcess(distance, method = "range")
  normdistance = predict(preProc, distance)
  
  
  #join distance column to the original samples
  samp = cbind(samp,normdistance)
  
  return(samp)
  
}

alter_labels = function(distance_data, ref){
  
  #merge features and original labels
  ref_added = cbind(distance_data,ref)
  #order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  #re-label most uncertain n number of samples
  ref_added_or[1:250,]$label = ref_added_or[1:250,]$ref
  ref_added_or[1:250,]$distance = 1.0000000000
  #re-order data set by its index
  ref_added_or$index = as.numeric(row.names(ref_added_or))
  ref_added_reor = ref_added_or [order(ref_added_or$index),]
  
  #extract labels for prediction
  labels= ref_added_reor[,(ncol(ref_added_reor)-4)]
  uncertainty= ref_added_reor[,(ncol(ref_added_reor)-2)]
  
  return(labels)
}

# Export mean and standard deviation
ExCsvMSD = function (datadase, filename = NA){{
  
  datadase = as.matrix(datadase)
  n = ncol(datadase)
  MSDdata = matrix(data = NA, nrow = 2, ncol = n)
  
  rownames(MSDdata) = c("M","SD")
  
  m = 1:n
  for (l in seq(along=m)){
    
    MSDdata[1,l] = mean(datadase[,l])
    MSDdata[2,l] = sd(datadase[,l])
  }
  
  
  MSDdata_final = rbind(datadase, MSDdata) 
  
  #export final kappa value table to .csv-file
  if(!missing(filename)){
    write.csv(MSDdata_final, filename)
  }
}
  return(MSDdata)
}


########################################  Input  ########################################

inputPath ="cologne_res_100_L2-L13.csv"                             
sMax = 1000                                               # maximum sample size
bound = c(0.3,0.6,0.9)                                    # radius around SV threshold
boundMargin  = c(1.5,1.0,0.5)                             # distance on positive side of hyperplane threshold 
  

sampleSizesPor = c(40,25,16,12,10,8,6,4,3,2,1)                    # vector with % of max
colheader = c("40","25","16","12","10","8","6","4","3","2","1")   # corresponding column names
sindexSVMDATA = 37                                                # start of base data; i.e., baseline model with one segmentation scale
numFeat = 18                                                      # number of features per level (dimensionality)
eindexSVMDATA = sindexSVMDATA + numFeat -1                        # end of base data

#names to use in rbind() of VSV                                   # 18 features names + 19.label
objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                  "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                  "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
                  "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                  "label")

#import format; "NULL" for subset of data on only some level (speed up import)
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

setwd(path)

setwd("csv_data_r_import/cologne/scale")

# import data
generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)

data = generalDataPool[,sindexSVMDATA:eindexSVMDATA]

data_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]

REF = generalDataPool[,ncol(generalDataPool)-1]

# exclude unclassified and delete level of factor
generalDataPool = subset(generalDataPool, REF != "unclassified")
generalDataPool$REF <- factor(generalDataPool$REF)

# transform to 2-Class-Case "Bushes Trees" VS rest
print(levels(generalDataPool$REF)[1]) # note that the first record is of class "bushes trees"
f=levels(generalDataPool$REF)[1]
generalDataPool$REF = as.character(generalDataPool$REF)
generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
generalDataPool$REF = as.factor(generalDataPool$REF)

data_label = data[,ncol(data)]

########################################  Preprocessing  ########################################

# Scaling data
normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]

preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))

# apply range of basemodel to all level
normalizedFeat2 = predict(preProc, setNames(normalizedFeat[,1:numFeat],objInfoNames[-length(objInfoNames)]))
normalizedFeat3 = predict(preProc, setNames(normalizedFeat[,(numFeat+1):(2*numFeat)],objInfoNames[-length(objInfoNames)]))

normalizedFeat5 = predict(preProc, setNames(normalizedFeat[,(3*numFeat+1):(4*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat6 = predict(preProc, setNames(normalizedFeat[,(4*numFeat+1):(5*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat7 = predict(preProc, setNames(normalizedFeat[,(5*numFeat+1):(6*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat8 = predict(preProc, setNames(normalizedFeat[,(6*numFeat+1):(7*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat9 = predict(preProc, setNames(normalizedFeat[,(7*numFeat+1):(8*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat10 = predict(preProc, setNames(normalizedFeat[,(8*numFeat+1):(9*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat11 = predict(preProc, setNames(normalizedFeat[,(9*numFeat+1):(10*numFeat)],objInfoNames[-length(objInfoNames)]))



# recombine normalized sets to one data frame
normalizedDataPoolAllLev = cbind(normalizedFeat2,
                                 normalizedFeat3,
                                 normalizedFeatBase,
                                 normalizedFeat5,
                                 normalizedFeat6,
                                 normalizedFeat7,
                                 normalizedFeat8,
                                 normalizedFeat9,
                                 normalizedFeat10,
                                 normalizedFeat11,
                                 
                                 
                                 normalizedLabelUSE
)

# remove used temporary variables
rm(normalizedFeat,
   normalizedFeat2,
   normalizedFeat3,
   normalizedFeatBase,
   normalizedFeat5,
   normalizedFeat6,
   normalizedFeat7,
   normalizedFeat8,
   normalizedFeat9,
   normalizedFeat10,
   normalizedFeat11
   
   
)


normalized_data = predict(preProc, setNames(data,objInfoNames[-length(objInfoNames)]))
rm(data)

# WHERE DO WE USE THESE DATA?

# WHAT IS THE DIFFERENCE WITH THE OTHER generalDataPool?
# normalize feature for multiscale
nomalizedFeatMultiScale = generalDataPool[,1:(ncol(generalDataPool)-2)]
preProc = preProcess(nomalizedFeatMultiScale, method = "range")
nomalizedFeatMultiScale = predict(preProc, nomalizedFeatMultiScale)
normalizedDataPoolAllLevMultiScale = cbind( nomalizedFeatMultiScale[1:((sindexSVMDATA + 8*numFeat)-1)], normalizedLabelUSE)

rm(nomalizedFeatMultiScale)

#normalize feature for multiscale apply:
normalized_data_MS = predict(preProc, data_MS)
normalized_data_MS = cbind( normalized_data_MS[1:((sindexSVMDATA + 8*numFeat)-1)])

rm(data_MS)

# Split data in test, train and validate data
splitdf <- split(normalizedDataPoolAllLev, normalizedDataPoolAllLev$USE)
trainDataPoolAllLev = as.data.frame(splitdf[[1]])
testDataAllLev = as.data.frame(splitdf[[2]])
validateDataAllLev = as.data.frame(splitdf[[3]])

rm(splitdf, normalizedDataPoolAllLev)

#Split data in test, train and validate data Multiscale
splitdf <- split(normalizedDataPoolAllLevMultiScale, normalizedDataPoolAllLevMultiScale$USE)
trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
testDataAllLevMS = as.data.frame(splitdf[[2]])
validateDataAllLevMS = as.data.frame(splitdf[[3]])

# remove used temporary variables
rm(splitdf, normalizedDataPoolAllLevMultiScale)

# remove use indicator in last column
trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
validateDataAllLev = validateDataAllLev[,1:ncol(validateDataAllLev)-1]

# remove use indicator in last column MS
trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
validateDataAllLevMS = validateDataAllLevMS[,1:ncol(validateDataAllLevMS)-1]

# split Validate data in features and labels and subset on basislevel of first SVM
validateFeatAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-1)]
validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
validateFeatsub = validateFeatAllLev[sindexSVMDATA:eindexSVMDATA]

# remove used temporary variables
rm(validateDataAllLev)

# split Validate data in features and labels for MS
validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-1)]
validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS))]

# remove used temporary variables
rm(validateDataAllLevMS)

# order train datapool by class label in alphabetical order:
trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]

# CHECK NAME data_modell_apply -> DONE


########################################  Sampling  ########################################

# set randomized seed for the random sampling procedure
seed = 5

# current training data-set, updated (refreshed) after each iteration 
# WHAT DOES IT MEAN?

trainDataCur = trainDataPoolAllLev
trainDataCurMS = trainDataPoolAllLevMS
testDataCur = testDataAllLev
testDataCurMS = testDataAllLevMS

# initial seed value for randomized sampling
seed = seed + sample(1:100, 1)

# to get the probabilities right
# definition of apriori-probabilities 
# (in the order in which the strata are given in the input data set)
# apriori-probabilities of class labels in alphabetical order
# pA = sum(trainDataCur$label == "bushes_trees")
# pB = sum(trainDataCur$label == "facade")
# pC = sum(trainDataCur$label == "meadow")
# pD = sum(trainDataCur$label == "other_impervious_surface")
# pE = sum(trainDataCur$label == "roofs")
# pF = sum(trainDataCur$label == "shadow")
# samplesize of classes; equal class sizes (balanced); 
# in 2Class Setting only first two records are used
pA = 1/6
pB = 1/6
pC = 1/6
pD = 1/6
pE = 1/6
pF = 1/6

# definition of training sample set sizes S [% of max. sample size]
sCur = sMax*(sampleSizesPor[2]/100)
##definition of sample shares
nA = round(sCur*pA)
nB = round(sCur*pB)
nC = round(sCur*pC)
nD = round(sCur*pD)
nE = round(sCur*pE)
nF = round(sCur*pF)
shares = c(nA,nB,nC,nD,nE,nF)

# set randomized seed for the random sampling procedure
set.seed(seed)

# definition of sampling configuration (strata:random sampling without replacement)
stratSamp = strata(trainDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

# get samples of trainDataCur and set trainDataCur new
samples = getdata(trainDataCur, stratSamp)

samplesID = samples[,182]
trainDataCurRemaining <- trainDataCur[-c(samplesID), ]

# definition of sampling configuration (strata:random sampling without replacement)
stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
stratSampRemaining_r = strata(trainDataCurRemaining, size = r, method = "srswor")
#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

# get samples of trainDataCurRemaining and set trainDataCurRemaining new
samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
samplesRemaining_r = getdata(trainDataCurRemaining, stratSampRemaining_r)

trainDataCurRemaining_b = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
trainFeatRemaining_b = trainDataCurRemaining_b[,1:(ncol(trainDataPoolAllLev)-1)]
trainLabelsRemaining_b = trainDataCurRemaining_b[,ncol(trainDataPoolAllLev)]
trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]

trainDataCurRemaining_r = samplesRemaining_r[,1:ncol(trainDataPoolAllLev)]
trainFeatRemaining_r = trainDataCurRemaining_r[,1:(ncol(trainDataPoolAllLev)-1)]
trainLabelsRemaining_r = trainDataCurRemaining_r[,ncol(trainDataPoolAllLev)]
trainDataCurRemainingsub_r = trainDataCurRemaining_r[sindexSVMDATA:eindexSVMDATA]

trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]

stratSamp = strata(trainDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
samples = getdata(trainDataCurMS, stratSamp)
trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]

# subset for each outer iteration test data to speed up computing
testDataCur = testDataCur[order(testDataCur[,ncol(testDataCur)]),]
testDataCurMS = testDataCurMS[order(testDataCurMS[,ncol(testDataCurMS)]),]

stratSamp = strata(testDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

samples = getdata(testDataCur, stratSamp)
testDataCur = samples[,1:ncol(testDataCur)]

stratSamp = strata(testDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
samples = getdata(testDataCurMS, stratSamp)
testDataCurMS = samples[,1:ncol(testDataCurMS)]

# split test feat from test label for later join with trainData
testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
testLabels = testDataCur[,ncol(testDataCur)]

testFeatMS = testDataCurMS[,1:(ncol(testDataCurMS)-1)]
testLabelsMS = testDataCurMS[,ncol(testDataCurMS)]

# subset on base level
testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]














