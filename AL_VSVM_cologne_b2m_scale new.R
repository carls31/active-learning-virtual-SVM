library(caret)
library(kernlab)
library(sampling)
library(progress)   # progress bar visualization
library(foreach)    # parallel processing
library(doParallel) # multiple CPU core
library(stats)      # k-means clustering

num_cores <- parallel::detectCores() # Numbers of cores deployed for multicore

nR = 3 # Number of Realizations

# sampleSizePor = c(10,20,40,60,90,120,160,200)    # vector with % of max  # c(40,25,16,12,10,8,6,4,3,2,1) 
sampleSizePor = c(10,20,40,60,90,120,160)     # Class sample size: round(250/6) label per class i.e. 42

bound = c(0.3, 0.6, 0.9)          # radius around SV threshold                           # c(0.3,0.45,0.6,0.75,0.9)
boundMargin = c(1.5, 0.75)        # distance on positive side of hyperplane threshold    # c(0.5,0.75,1,1.25,1.5)
newSizes = c(20, 25)              # number of samples picked in each Active Learning iteration # 3, 4, 5, 10
clusterSizes = c(90, 120)         # number of clusters used to picked samples from different group regions # 60, 80, 90, 100, 300
resampledSize = c(150, 200)       # sampleSize*2.5 # or just 100, 200

b = 20 # Size of balanced_unlabeled_samples in each class

train  = TRUE      # Decide if train the models or, if present, load them from dir 
binary = TRUE      # Choose between Binary classification or Multiclass 
save_models = FALSE # if TRUE, after training the models it saves them
invariance = "scale"
if(binary){model_class="binary"}else{model_class="multiclass"}

path = '/home/rsrg9/Documents/tunc_oz/apply_model/'
model_path = "/home/rsrg9/Documents/GitHub/active-learning-virtual-SVM/"

########################################  Utils  ########################################

# Coarse and Narrow grid search for SVM parameters tuning
svmFit = function(x, y, indexTrain, classProb = FALSE, showPrg = TRUE){ #x = training descriptors, y = class labels
  
  #expand coarse grid
  coarseGrid = expand.grid(sigma = 2^seq(-5,3,by=2), C = 2^seq(-4,12,by=2))
  
  #set seed
  set.seed(13)
  if(showPrg){print("Running coarse grid search...")}
  
  svmFitCoarse = train(x, y, 
                       method = "svmRadial",
                       metric = "Kappa",
                       maximize = TRUE,
                       tuneGrid = coarseGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]], # classProbs =  classProb
                                                  ),
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
  
  set.seed(31)
  
  if(showPrg){print("Running narrow grid search...")}
  svmFitNarrow = train(x, y, 
                       method = "svmRadial",
                       metric = "Kappa", # "ROC"
                       maximize = TRUE,
                       tuneGrid = narrowGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]],
                                                  classProbs =  classProb
                                                  ),
                       scaled = FALSE)
  
  return(svmFitNarrow)  
}
# ***********************************

# Evaluate the distance between Virtual Support Vectors and Support Vectors lying in the input space
rem_extrem = function(org, VSV1, a){
  # Euclidean Distance between two points lying in the input space
  euc_dis = function(a, b){
    temp = 0
    for(ii in seq(along = c(1:length(a)))){
      temp = temp +((a[[ii]]-b[[ii]])^2)
    }
    return(sqrt(temp))
  }
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()
  # save label of sample and the distance between SV and VSV for each pair of SV and VSV
  for(l in seq(along = c(1:nrow(org)))){
    distance[l,1] = as.character( org[l,ncol(org)])
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
# rem_extrem <- function(org, VSV1, a) {
#   # Euclidean Distance between two points lying in the input space
#   euc_dis <- function(a, b) sqrt(sum((a - b) ^ 2))
# 
#   # Initialize distance dataframe
#   distance <- data.frame(label = org[[ncol(org)]], dist = numeric(nrow(org)))
# 
#   # Calculate distances between SV and VSV
#   for (l in seq_len(nrow(org))) {
#     distance$dist[l] <- euc_dis(org[l, -ncol(org)], VSV1[l, -ncol(VSV1)])
#   }
# 
#   # Split SV according to its classes
#   SVClass1 <- subset(org, REF == levels(org$REF)[1])
#   SVClass2 <- subset(org, REF == levels(org$REF)[2])
# 
#   # Calculate threshold for Class1
#   boundClass1 <- if (nrow(SVClass1) > 1) {
#     disClass1 <- dist(as.matrix(SVClass1[, -ncol(SVClass1)]))
#     mean(disClass1) * a
#   } else NA
# 
#   # Calculate threshold for Class2
#   boundClass2 <- if (nrow(SVClass2) > 1) {
#     disClass2 <- dist(as.matrix(SVClass2[, -ncol(SVClass2)]))
#     mean(disClass2) * a
#   } else NA
# 
#   # Iterate over the distance vector and substitute in VSV1 the samples which overstep the threshold
#   for (k in seq_len(nrow(org))) {
#     if (is.na(distance$dist[k]) ||
#         (!is.na(boundClass1) && distance$label[k] == levels(distance$label)[1] && distance$dist[k] > boundClass1) ||
#         (!is.na(boundClass2) && distance$label[k] == levels(distance$label)[2] && distance$dist[k] > boundClass2)) {
#       VSV1[k, ] <- NA
#     }
#   }
#   return(VSV1)
# }

# rem_extrem_kerneldist(SVtotal, SVL7, bound[jj])
rem_extrem_kerneldist = function(org, VSV1, a, kernel_func){
  
  # Kernel distance between two point lying in the hyperspace
  kern_dis = function(a, b, kernel_func){
    a  <- unlist(a)
    b  <- unlist(b)
    dk <- sqrt( kernel_func(a,a)+kernel_func(b,b)-2*kernel_func(a,b))
    return(dk)
  }
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()

  numClass = nlevels(org$REF)
  SVClass = list()

  # split SV according to its classes
  for(f in seq(along = c(1:numClass))){
    SVClass[[f]]=org[which(org$REF==levels(org$"REF")[[f]]),]
  }
  # save label of sample and the distance between SV and VSV in distance for each pair of SV and VSV
  for(l in seq(along = c(1:nrow(org)))){
    distance[l,1] = as.character( org[l,ncol(org)])
    distance[l,2] = kern_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)],kernel_func)
  }
  boundClass = list()

  # Compute the distance threshold boundClass for each class
  for(f in seq(along = c(1:length(SVClass)))){
    distanceSVC1 = c()
    if(nrow(SVClass[[f]])>0){
      for(n in seq(along = 1:(nrow(SVClass[[f]])-1))){
        for(nn in seq(along = c(n:(nrow(SVClass[[f]])-1)))){
          distanceSVC1[length(distanceSVC1)+1] = kern_dis(SVClass[[f]][n,-ncol(SVClass[[f]])], SVClass[[f]][(n+nn),-ncol(SVClass[[f]])],kernel_func)
        }
      }
      disClass1mean = mean(distanceSVC1)
      boundClass[[f]] = disClass1mean*a
    }
  }
  distance$X1 = factor(distance$X1)

  for(k in seq(along = c(1:nrow(org)))){
    tmp_cond <- FALSE
    for(class in seq(along = c(1:length(SVClass)))){
      if(as.integer(distance[k,1]) == class){
        if(!is.null(boundClass[[class]]) && !is.na(boundClass[[class]])){
          if(distance[k,2] != 0 && distance[k,2] > (boundClass[[class]])){
            VSV1[k,]=NA
            tmp_cond <- TRUE
          }
        }
      }
    }#if(!tmp_cond){VSV1[k,]=NA}
  }
  return(VSV1)
}

pred_one = function(modelfin, dataPoint, dataPointLabels){
  smallestDistance = 9999
  
  for(ll in seq(along = dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along = binaryClassProblem)){ #print(binaryClassProblem[[l]])
      
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProblem[[l]])){ #print(paste("vero", pred))
        pred = sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
            modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[1:length(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
        
        if(abs(pred) < abs(smallestDistance))
          smallestDistance = abs(pred)
      }
    }
  }
  return(smallestDistance)   
}

# Evaluate the distance between samples and Support Vectors lying in the hyperspace
uncertainty_dist_v2_2 = function(org, samp) {
  
  distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  registerDoParallel(num_cores)
  distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), samp[k, ncol(samp)])
  }
  registerDoSEQ()
  
  scaled_distances <- apply(distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  distance$distance <- scaled_distances
  samp <- cbind(samp, distance)
  
  return(samp)
}

# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING new_tunedVSVM, predLabelsVSVM_unc
margin_sampling_multicore <- function(org, samp) {
  
  # Initialize data frame to store margin distance for each sample
  margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_margin_distance <- function(k) {
    probabilities <- predict(org, newdata = samp[k, -ncol(samp)], type = "prob")
    max_confidence_class <- which.max(probabilities)
    
    distances <- rep(0, length(probabilities))
    for (i in 1:length(probabilities)) {
      distances[factor(names(probabilities))[i]] <- pred_one(org$finalModel, unlist(samp[k,-ncol(samp)]),factor(names(probabilities))[i])
    }
    return(distances[max_confidence_class] - max(distances[-which.max(probabilities)]))
  }
  # Set up parallel backend
  registerDoParallel(num_cores)
  
  # Use foreach for parallel processing
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_margin_distance(k)
  }
  registerDoSEQ()
  
  # Apply "range" normalization to mclp_distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Assign scaled distances to probability dataframe
  margin_distance$distance <- scaled_distances
  merged_data <- cbind(samp, margin_distance)
  
  return(merged_data)
}

# Evaluate Multiclass Level Uncertainty (MCLU)
mclu_sampling <- function(org, samp) {
  
  # Initialize data frame to store uncertainty for each sample
  uncertainty <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_mclu_distance <- function(k) {
    probabilities <- predict(org, newdata = samp[k, -ncol(samp)], type = "prob")
    
    # Get the two most probable classes
    top_classes <- as.factor(names(sort(unlist(probabilities), decreasing = TRUE)))[1:2]
    
    # Calculate the difference between the distances to the margin for the two most probable classes
    distance_top1 <- pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), top_classes[1])
    distance_top2 <- pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), top_classes[2])
    
    return(abs(distance_top1 - distance_top2))
  }
  registerDoParallel(num_cores)
  # Use foreach for parallel processing
  mclu_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_mclu_distance(k)
  }
  registerDoSEQ()
  
  mclu_distances <- apply(mclu_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  uncertainty$distance <- mclu_distances
  merged_data <- cbind(samp, uncertainty)
  
  return(merged_data)
}

mclp_sampling <- function(org, samp) {
  
  # Initialize data frame to store PROBABILITY for each sample
  probability <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_mclp_distance <- function(k) {
    probabilities <- predict(org, newdata = samp[k, -ncol(samp)], type = "prob")
    
    # Get the two most probable classes
    top_classes <- (sort(unlist(probabilities), decreasing = TRUE))[1:2]
    
    # Calculate the difference between the probabilities for the two most probable classes
    return(abs(top_classes[[1]] - top_classes[[2]]))
  }
  registerDoParallel(num_cores)
  
  # Use foreach for parallel processing with " %dopar% "
  mclp_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_mclp_distance(k)
  }
  registerDoSEQ()
  # Apply "range" normalization to mclp_distances
  scaled_distances <- apply(mclp_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Assign scaled distances to probability dataframe
  probability$distance <- scaled_distances
  merged_data <- cbind(samp, probability)
  
  return(merged_data)
}

alter_labels = function(distance_data, ref, newSize=100){
  
  #merge features and original labels
  ref_added = cbind(distance_data,ref)
  #order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  #re-label most uncertain n number of samples
  ref_added_or[1:newSize,]$label = ref_added_or[1:newSize,]$ref
  # ref_added_or[1:newSize,]$distance = 1.0000000000
  #re-order data set by its index
  ref_added_or$index = as.numeric(row.names(ref_added_or))
  ref_added_reor = ref_added_or [order(ref_added_or$index),]
  
  # extract labels for prediction
  return(ref_added_reor[,(ncol(ref_added_reor)-4)])
}
add_new_samples = function(distance_data,
                           ref, features=NA,
                           new_trainFeatVSVM=NA, new_trainLabelsVSVM=NA,
                           newSize=4, cluster=120){
  # merge features and original labels
  ref_added = cbind(distance_data, ref)
  
  # order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  
  # Perform k-means clustering
  km_data <- kmeans(ref_added_or[, 1:18], centers = cluster, iter.max = 20, nstart = 200)
  
  # Add cluster information to the data
  ref_added_or$cluster <- km_data$cluster
  
  # Initialize a vector to store selected sample indices
  selected_indices <- c()
  cluster_samples <- c()
  tmpSize = 0
  # Iterate over clusters and select one sample from each cluster
  for (sample in seq_len(nrow(ref_added_or))) {
    if (!( ref_added_or[sample,]$cluster  %in% cluster_samples) && tmpSize < newSize){
      cluster_samples <- c(cluster_samples, ref_added_or[sample,]$cluster)
      tmpSize = tmpSize+1
      
      ref_added_or[sample,]$label <- ref_added_or[sample,]$ref
      
      selected_indices <- c(selected_indices, as.numeric(rownames(ref_added_or[sample,])))
    }
  }
  ref_added_reor = ref_added_or[order(as.numeric(rownames(ref_added_or))),]
  
  # Add relabeled samples to new_trainFeatVSVM and new_trainLabelsVSVM
  if(length(features)>1){
    # Remove relabeled samples from validateLabels
    features <- features[!(rownames(features) %in% selected_indices), ]
    reor_idx <- which(rownames(ref_added_reor) %in% selected_indices)
    ref <- ref[-reor_idx]
    new_trainFeatVSVM <- rbind(new_trainFeatVSVM, ref_added_reor[reor_idx, 1:(ncol(ref_added_reor)-5)])
    new_trainLabelsVSVM <- c(new_trainLabelsVSVM, ref_added_reor[reor_idx, (ncol(ref_added_reor)-4)])
    return(list(features = features, labels = ref, 
                new_trainFeatVSVM = new_trainFeatVSVM, 
                new_trainLabelsVSVM = new_trainLabelsVSVM))
  } else{
    return(ref_added_reor[, (ncol(ref_added_reor)-4)])
    } 
}   

Self_Learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVMfinModel, SVL_variables, train=TRUE, classProb = FALSE)
  {
  if (file.exists(model_name) && !train) {
    bestFittingModel <- readRDS(model_name)
    actKappa = bestFittingModel$resample$Kappa
    print("Luckily, model already exists!")
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa))
  } else {
    actKappa = 0
    # iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along = c(1:length(bound)))){
      print("Removing VSVs out of boundaries...")
      
      registerDoParallel(num_cores)
      
      # Apply foreach loop to process each SVL variable and bind the results
      if(binary){ # print("step 1")
        SVinvarRadi <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
        setNames(rem_extrem(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
        }
      }else{ # print("step 1.5")
        SVinvarRadi <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
        setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj], SVMfinModel@kernelf), objInfoNames)
        }
      } # print("step 2")
      registerDoSEQ() # print("step 3")
    
      # remove NAs 
      SVinvarRadi = na.omit(SVinvarRadi)
      
      # iterating over boundMargin to test different threshold on margin distance
      for (kk in seq(along = c(1:length(boundMargin)))){
        print(paste0("Testing bound margin: ",kk,"/",length(boundMargin)," and radius threshold: ",jj,"/",length(bound)))
        
        # remove VSV which are not located in certain distance to decision function
        # data.frame to store elected VSV within the margin
        SVinvar=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
        
        pb <- progress_bar$new(
          format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
          total = nrow(SVinvarRadi),
          clear = FALSE
        )
        # iterate over SVinvarRadi and evaluate distance to hyperplane
        # implementation checks class membership for case that each class should be evaluate on different bound
        for(m in seq(along = c(1:nrow(SVinvarRadi)))){
          signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadi[m,-ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
          
          if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
            SVinvar = rbind(SVinvar, SVinvarRadi[m,])
          }
          pb$tick()
        }
        
        # merge elected VSV with original SV
        SVinvar_org = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvar,objInfoNames))
        
        SVinvar_org=na.omit(SVinvar_org)
        
        # split for training to feature and label
        trainFeatVSVM = SVinvar_org[,1:(ncol(SVinvar_org)-1)]
        trainLabelsVSVM = SVinvar_org[,ncol(SVinvar_org)]
        
        # get list with index of trainData to split between train and test in svmFit
        countTrainData = nrow(SVinvar_org)
        indexTrainData = list(c(1:countTrainData))
        
        # join of train and test data (through indesTrainData in svmFit seperable)
        names = objInfoNames[1:length(objInfoNames)-1]
        tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
        tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))
        
        ######################################## VSVM control parameter tuning ########################################
        tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData, classProb)
        
        # of all Different bound settings get the one with best Kappa ans save its model
        if(actKappa < tunedVSVM$resample$Kappa){
          bestFittingModel = tunedVSVM
          actKappa = tunedVSVM$resample$Kappa
          best_trainFeatVSVM = trainFeatVSVM
          best_trainLabelsVSVM = trainLabelsVSVM
          best_bound= bound[jj]
          best_boundMargin = boundMargin[kk]
        }
      }
    } 
    if(save_models){saveRDS(bestFittingModel, model_name)}
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa, 
                best_trainFeatVSVM = best_trainFeatVSVM, 
                best_trainLabelsVSVM = best_trainLabelsVSVM, 
                best_bound = best_bound, 
                best_boundMargin = best_boundMargin))
  }
}
########################################  Input  ########################################
inputPath ="cologne_res_100_L2-L13.csv"

colheader = as.character(sampleSizePor)                 # corresponding column names    
sindexSVMDATA = 37                                      # start of baseline model with one segmentation scale data
numFeat = 18                                            # number of features per level (dimensionality)
eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data

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

setwd(paste0(path, "csv_data_r_import/cologne/scale"))

# import data
generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)

# exclude unclassified and delete level of factor
generalDataPool = subset(generalDataPool, REF != "unclassified")
generalDataPool$REF <- factor(generalDataPool$REF)
generalDataPool <- na.omit(generalDataPool) # *********************************************************************************************************************************************************** ISSUE

if(binary){
  # transform to 2-Class-Case "Bushes Trees" VS rest
  print(levels(generalDataPool$REF)[1]) # note that the first record is of class "bushes trees"
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
}

data = generalDataPool[,sindexSVMDATA:eindexSVMDATA]

REF = generalDataPool[,ncol(generalDataPool)-1]

data = cbind(data, REF)
data_label = data[,ncol(data)]

########################################  Scaling  ########################################

normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]

preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
# ********************************************************************************************
normalized_data = predict(preProc, setNames(data,objInfoNames[-length(objInfoNames)]))
rm(data) # WHY DO WE USE THIS DATA AT THE END? IS IT CORRECT TO USE THEM INSTEAD OF THE TRAIN TEST VALID SETS?
# ********************************************************************************************
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
normalizedDataPoolAllLev = cbind(normalizedFeat2, normalizedFeat3, normalizedFeatBase,
                                 normalizedFeat5, normalizedFeat6, normalizedFeat7,
                                 normalizedFeat8, normalizedFeat9, normalizedFeat10,
                                 normalizedFeat11, normalizedLabelUSE
)
# remove used temporary variables
rm(normalizedFeat, normalizedFeat2, normalizedFeat3, normalizedFeatBase,
   normalizedFeat5,  normalizedFeat6, normalizedFeat7, normalizedFeat8, normalizedFeat9, normalizedFeat10, normalizedFeat11
)
########################################  Splitting & Sampling  ########################################

# Split data in test, train and validate data
splitdf <- split(normalizedDataPoolAllLev, normalizedDataPoolAllLev$USE)
trainDataPoolAllLev = as.data.frame(splitdf[[1]])
testDataAllLev = as.data.frame(splitdf[[2]])
validateDataAllLev = as.data.frame(splitdf[[3]])
rm(splitdf, normalizedDataPoolAllLev)

# remove use indicator in last column
trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
validateDataAllLev = validateDataAllLev[,1:ncol(validateDataAllLev)-1]

# split Validate data in features and labels and subset on basislevel of first SVM
validateFeatAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-1)]
validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
validateFeatsub = validateFeatAllLev[sindexSVMDATA:eindexSVMDATA]

# remove used temporary variables
rm(validateDataAllLev)

# order train datapool by class label in alphabetical order:
trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]

###########################################  MultiScale ###################################################

data_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]

# normalize feature for MultiScale
nomalizedFeat_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]
# WHAT IS THE DIFFERENCE WITH THE OTHER generalDataPool, i.e. data_MS?
preProc = preProcess(nomalizedFeat_MS, method = "range")
nomalizedFeat_MS = predict(preProc, nomalizedFeat_MS)
normalizedDataPoolAllLev_MS = cbind( nomalizedFeat_MS[1:((sindexSVMDATA + 8*numFeat)-1)], normalizedLabelUSE)
rm(nomalizedFeat_MS)

# normalize feature for multiscale apply:
normalized_data_MS = predict(preProc, data_MS)
normalized_data_MS = cbind( normalized_data_MS[1:((sindexSVMDATA + 8*numFeat)-1)])
rm(data_MS)

#Split data in test, train and validate data Multiscale
splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$USE)
trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
testDataAllLevMS = as.data.frame(splitdf[[2]])
validateDataAllLevMS = as.data.frame(splitdf[[3]])

# remove used temporary variables
rm(splitdf, normalizedDataPoolAllLev_MS)

# remove use indicator in last column MS
trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
validateDataAllLevMS = validateDataAllLevMS[,1:ncol(validateDataAllLevMS)-1]

# split Validate data in features and labels for MS
validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-1)]
validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS))]

# remove used temporary variables
rm(validateDataAllLevMS)

# order train datapool by class label in alphabetical order:
trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]
##################################################################################################

# KappasSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(KappasSVM) = colheader     
AccuracySVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM) = colheader
AccuracySVM_M = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM_M) = colheader

AccuracyVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL) = colheader
AccuracyVSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b) = colheader

AccuracyVSVM_SL_Un_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_ud) = colheader
AccuracyVSVM_SL_Un_b_ms = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_ms) = colheader
AccuracyVSVM_SL_Un_b_mclu = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_mclu) = colheader
AccuracyVSVM_SL_Un_b_mclp = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_mclp) = colheader
AccuracyVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_it) = colheader

AccuracyVSVM_SL_vUn_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_b) = colheader
AccuracyVSVM_SL_vUn_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_b_ud) = colheader
AccuracyVSVM_SL_vUn_mclp = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_mclp) = colheader
AccuracyVSVM_SL_vUn_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_it) = colheader

# set randomized seed for the random sampling procedure
# seed = 72 # is an unlucky choice, it works well only with 41 sample per class in the train set 
seed = 20 # 5, 73, 20 # ************************************************************************************************************************************************************************** ISSUE

best_bound_oa_SL = c()
best_boundMargine_oa_SL = c()
best_bound_oa_SL_Un = c()
best_boundMargine_oa_SL_Un = c()
best_newSize_oa=c()
best_cluster_oa=c()

for(realization in seq(along = c(1:nR))){#}
  print(paste0("Realization: ",realization,"/",nR))  
  # initial seed value for randomized sampling
  if(train){seed = seed + sample(100, 1)}
  
  trainDataCurBeg = trainDataPoolAllLev
  testDataCurBeg = testDataAllLev
  # subset for each outer iteration test data to speed up computing
  testDataCurBeg = testDataCurBeg[order(testDataCurBeg[,ncol(testDataCurBeg)]),]
  
  ######  MultiScale
  trainDataCurBegMS = trainDataPoolAllLevMS
  testDataCurBegMS = testDataAllLevMS
  # subset for each outer iteration test data to speed up computing
  testDataCurBegMS = testDataCurBegMS[order(testDataCurBegMS[,ncol(testDataCurBegMS)]),]
  
for(sample_size in seq(along = c(1:length(sampleSizePor)))){#}

print(paste0("Sample size: ",sampleSizePor[sample_size]," -> ",sample_size,"/",length(sampleSizePor)))

# if(length(sampleSizePor)>1){}else{}
# definition of sample shares
# if(sample_size>1){sampleSize = sampleSizePor[sample_size] - sampleSizePor[sample_size-1]
# }else{          sampleSize = sampleSizePor[sample_size] }
sampleSize = sampleSizePor[sample_size]
shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)

# set randomized seed for the random sampling procedure
set.seed(seed)

# definition of sampling configuration (strata:random sampling without replacement)
stratSamp = strata(trainDataCurBeg, c("REF"), size = shares, method = "srswor")
#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

# get samples of trainDataCur and set trainDataCur new
samples = getdata(trainDataCurBeg, stratSamp)

samplesID = samples$ID_unit

# if(length(sampleSizePor)>1){}else{}
# if(sample_size>1){trainDataCur = rbind(trainDataCur, samples[,1:ncol(trainDataPoolAllLev)])
#                   trainDataCurRemaining <- trainDataCurRemaining[-c(samplesID), ]
#   }else{          trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
#                   trainDataCurRemaining <- trainDataCurBeg[-c(samplesID), ]}
trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
trainDataCurRemaining <- trainDataCurBeg[-c(samplesID), ]

trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]

stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")

samples = getdata(testDataCurBeg, stratSamp)

# if(length(sampleSizePor)>1){}else{}
# if(sample_size>1){testDataCur = rbind(testDataCur,samples[,1:ncol(testDataAllLev)])
# }else{            testDataCur = samples[,1:ncol(testDataAllLev)]}
testDataCur = samples[,1:ncol(testDataAllLev)]

# split test feat from test label for later join with trainData
testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
testLabels = testDataCur[,ncol(testDataCur)]

# subset on base level
testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]

# trainData index to split between train and test in svmFit
countTrainData = nrow(trainFeat)
indexTrainData = list(c(1:countTrainData))

# subset on L_4 ***************************** SVM base for invariants ************************************
trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and valid set
# ************************************************ *******************************************************
# join of train and test test data (separable through indexTrainData in svmFit)
tuneFeat = rbind(trainFeat, testFeatsub)
tuneLabel = unlist(list(trainLabels, testLabels))
########################################  SVM parameter tuning  #########################################
setwd(paste0(model_path, "saved_models/"))
model_name = paste0("tunedSVM_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
if (file.exists(model_name) && !train) {
  tunedSVM <- readRDS(model_name)
  print("Model already exists!")
} else {
  tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
  if(save_models){saveRDS(tunedSVM, model_name)}
}
# run classification and accuracy assessment for unmodified SV and predict labels of test data
predLabelsSVM = predict(tunedSVM, validateFeatsub)
print("SVM Accuracy assessment...")
accSVM = confusionMatrix(predLabelsSVM, validateLabels)
print(accSVM$overall["Accuracy"])

best_acc <- accSVM$overall["Accuracy"]
new_bestTunedVSVM <- tunedSVM
new_bestTrainFeatVSVM <- trainFeat 
new_bestTrainLabelsVSVM <- trainLabels 

######################################### VSVM on all Level SV #########################################

# get SV of tunedSVM
SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]

# get VSs, means rows of SV but with subset on different level
SVL2 = trainDataCur[SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
SVL3 = trainDataCur[SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]

SVL5 = trainDataCur[SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
SVL6 = trainDataCur[SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
SVL7 = trainDataCur[SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
SVL8 = trainDataCur[SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
SVL9 = trainDataCur[SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
SVL10 = trainDataCur[SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
SVL11 = trainDataCur[SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]

# bind original SV with modified to new train data set
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
) # THE NEW TRAIN DATA SET IS MADE BY SVs AND VSVs ONLY

# split for training to feature and label
trainFeatVSVM = SVinvar[,1:(ncol(SVinvar)-1)]
trainLabelsVSVM = SVinvar[,ncol(SVinvar)]

# get list with index of train data to split between train and test in svmFit
countTrainData = nrow(SVinvar)
indexTrainData = list(c(1:countTrainData))

# join of train and test test data (through indexTrainData in svmFit seperable)
names = objInfoNames[1:length(objInfoNames)-1]
tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))

########################################  VSVM parameter tuning  ########################################
model_name = paste0("tunedVSVM_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
if (file.exists(model_name) && !train) {
  tunedVSVM <- readRDS(model_name)
  print("Model already exists!")
} else {
  tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
  if(save_models){saveRDS(tunedVSVM, model_name)}
}

# predict labels of test data i.e. run classification and accuracy assessment for modified SV
predLabelsVSVM = predict(tunedVSVM, validateFeatsub)

print("VSVM Accuracy assessment...")
accVSVM = confusionMatrix(predLabelsVSVM, validateLabels)
print(accVSVM$overall["Accuracy"])

if(accVSVM$overall["Accuracy"]>best_acc){
  best_acc <- accVSVM$overall["Accuracy"]
  new_bestTunedVSVM <- tunedVSVM
  new_bestTrainFeatVSVM <- trainFeatVSVM
  new_bestTrainLabelsVSVM <- trainLabelsVSVM} 

##################################### VSVM - EVALUATION (SL) of all Level ################################
# **********************
# records which 2 classes are involved in 2 class problems
binaryClassProblem = list()
for(jj in seq(along = c(1:length(tunedSVM$finalModel@xmatrix)))){ # CHECK WHY DOES IT HAS TO BE BUILD IN THIS WAY
  binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
}
# **********************
print("Evaluation of VSVM Self Learning...")
model_name = paste0("bestFittingModel_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
SLresult <- Self_Learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, #classProb=TRUE,
                       SVL_variables = list(
                          list(SVtotal, SVL2),
                          list(SVtotal, SVL3),
                          list(SVtotal, SVL5),
                          list(SVtotal, SVL6),
                          list(SVtotal, SVL7),
                          list(SVtotal, SVL8),
                          list(SVtotal, SVL9),
                          list(SVtotal, SVL10),
                          list(SVtotal, SVL11)
                          )
)
bestFittingModel <- SLresult$bestFittingModel
best_trainFeatVSVM <- SLresult$best_trainFeatVSVM
best_trainLabelsVSVM <- SLresult$best_trainLabelsVSVM
best_bound_SL = SLresult$best_bound
best_boundMargin_SL = SLresult$best_boundMargin
# predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
print("VSVM_SL Accuracy assessment...")
accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
print(accVSVM_SL$overall["Accuracy"])

if(accVSVM_SL$overall["Accuracy"]>best_acc){
  best_acc <- accVSVM_SL$overall["Accuracy"]
  new_bestTunedVSVM <- bestFittingModel
  new_bestTrainFeatVSVM <- best_trainFeatVSVM
  new_bestTrainLabelsVSVM <- best_trainLabelsVSVM} 

########################################### unlabeled samples ############################################
# Definition of sampling configuration (strata:random sampling without replacement)
stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
#stratSampRemaining = strata(trainDataCurRemaining, size = 6*b, method = "srswor") # if trainDataCur is balanced apriori

# get samples of trainDataCurRemaining and set trainDataCurRemaining new
samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining)
trainDataCurRemaining_it <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]

trainDataCurRemaining_b = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]

REF_b = predict(tunedVSVM, trainDataCurRemainingsub_b)
# get SV of unlabeled samples
SVindexUn_b = 1:nrow(trainDataCurRemainingsub_b) # ******************************************************************************************************************************************** ISSUE
SVtotalUn_b = trainDataCurRemaining_b[SVindexUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
SVtotalUn_b = cbind(SVtotalUn_b, REF_b)
# get VSs, means rows of SV but with subset on different level
SVL2Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b) # ****************************************************************** ISSUE
SVL3Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b) # *******************************************************************************

SVL5Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b) # ****************************************************************** ISSUE
SVL6Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)
SVL7Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)
SVL8Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)
SVL9Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)
SVL10Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)
SVL11Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b)

print("Evaluation of VSVM Self Learning with Unlabeled Samples...")
model_name = paste0("bestFittingModelUn_b_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
SLresult <- Self_Learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, #classProb=TRUE, # ********************************************************************** ISSUE
                       SVL_variables = list(
                           list(SVtotal, SVL2),
                           list(SVtotal, SVL3),
                           list(SVtotal, SVL5),
                           list(SVtotal, SVL6),
                           list(SVtotal, SVL7),
                           list(SVtotal, SVL8),
                           list(SVtotal, SVL9),
                           list(SVtotal, SVL10),
                           list(SVtotal, SVL11),
                           list(SVtotalUn_b, SVL2Un_b),
                           list(SVtotalUn_b, SVL3Un_b),
                           list(SVtotalUn_b, SVL5Un_b),
                           list(SVtotalUn_b, SVL6Un_b),
                           list(SVtotalUn_b, SVL7Un_b),
                           list(SVtotalUn_b, SVL8Un_b),
                           list(SVtotalUn_b, SVL9Un_b),
                           list(SVtotalUn_b, SVL10Un_b),
                           list(SVtotalUn_b, SVL11Un_b)
                        )
)
bestFittingModelUn_b <- SLresult$bestFittingModel
best_trainFeatVSVMUn_b <- SLresult$best_trainFeatVSVM
best_trainLabelsVSVMUn_b <- SLresult$best_trainLabelsVSVM
best_bound_SL_Un = SLresult$best_bound
best_boundMargin_SL_Un = SLresult$best_boundMargin
# predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
predLabelsVSVMsumUn_b = predict(bestFittingModelUn_b, validateFeatsub)
print("VSVM_SL_Un Accuracy assessment...")
accVSVM_SL_Un_b = confusionMatrix(predLabelsVSVMsumUn_b, validateLabels)
print(accVSVM_SL_Un_b$overall["Accuracy"])

if(accVSVM_SL_Un_b$overall["Accuracy"]>best_acc){
  best_acc <- accVSVM_SL_Un_b$overall["Accuracy"]
  new_bestTunedVSVM <- bestFittingModelUn_b
  new_bestTrainFeatVSVM <- best_trainFeatVSVMUn_b
  new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn_b} 

######################################## UNCERTAINTY function on VSVM-SL  #########################################

for(rS in 1:length(resampledSize)){
# ****** # 
print("Computing margin distance using Iterative Active Learning Procedure...")
classSize = 3000 # number of samples for each class # 250, 500, 750, 1000, 1500, 3000, 5803
stratSampSize = c(classSize,classSize,classSize,classSize,classSize,classSize)
# Definition of sampling configuration (strata:random sampling without replacement)
stratSampRemaining = strata(trainDataCurRemaining_it, c("REF"), size = stratSampSize, method = "srswor")
# Get new samples from trainDataCurRemaining_it
samplesRemaining = getdata(trainDataCurRemaining_it, stratSampRemaining)
# trainDataCurRemaining_iter <- trainDataCurRemaining_it[-c(samplesRemaining$ID_unit), ]
for(nS4iter in 1:length(newSizes)){
  for(cS in 1:length(clusterSizes)){
    print(paste0("Samples for iteration: ",newSizes[nS4iter]," - ",nS4iter,"/",length(newSizes)," and number of clusters: ",cluster=clusterSizes[cS], " - ",cS,"/",length(clusterSizes)))
    actKappa = 0
    
    new_tunedVSVM <- new_bestTunedVSVM
    new_trainFeatVSVM <- setNames(new_bestTrainFeatVSVM, names)
    new_trainLabelsVSVM <- new_bestTrainLabelsVSVM
    
    upd_trainDataCurFeatsub = samplesRemaining[sindexSVMDATA:eindexSVMDATA]
    upd_trainDataCurLabels = samplesRemaining$REF
    
    newSize_for_iter = newSizes[nS4iter] #sampleSize/10 # or just 4
    num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
    
    pb <- progress_bar$new(
      format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
      total = num_iters,
      clear = FALSE
    )
    for (iter in 1:num_iters){
      # print(paste0("Iteration ",iter,"/",num_iters,"..."))
      predLabelsVSVM = predict(new_tunedVSVM, upd_trainDataCurFeatsub)
      # Add predicted labels to the features data set
      predLabelsVSVM_unc = cbind(upd_trainDataCurFeatsub, predLabelsVSVM)
      predLabelsVSVM_unc = setNames(predLabelsVSVM_unc, objInfoNames)
      # print(paste0("Computing distances..."))
      sampled_data <- uncertainty_dist_v2_2(new_tunedVSVM, predLabelsVSVM_unc)
      # print(paste0("Relabeling samples..."))
      # Get new labels and updated datasets
      result <- add_new_samples(sampled_data,
                                upd_trainDataCurLabels, upd_trainDataCurFeatsub,
                                new_trainFeatVSVM, new_trainLabelsVSVM,
                                newSize=newSize_for_iter,
                                cluster=clusterSizes[cS] ) # always greater than newSize_for_iter, # 60, 80, 100, 120
      # Extract new datasets
      upd_trainDataCurFeatsub <- result$features
      upd_trainDataCurLabels <- result$labels
      new_trainFeatVSVM <- result$new_trainFeatVSVM
      new_trainLabelsVSVM <- result$new_trainLabelsVSVM
      # Get list with index of trainData to split between train and test in svmFit
      countTrainDataUn = nrow(new_trainFeatVSVM)
      indexTrainDataUn_it = list(c(1:countTrainDataUn))
      # Join of train and test data (seperable in svmFit through indexes)
      tuneFeatVSVMUn_it = rbind(new_trainFeatVSVM, setNames(testFeatsub, names))
      tuneLabelsVSVMUn_it = unlist(list(new_trainLabelsVSVM, testLabels))
      tmp_new_tunedVSVM = svmFit(tuneFeatVSVMUn_it, tuneLabelsVSVMUn_it, indexTrainDataUn_it, showPrg = FALSE)
      pb$tick()
    }
    if(actKappa < tmp_new_tunedVSVM$resample$Kappa){
      new_tunedVSVM = tmp_new_tunedVSVM
      actKappa = tmp_new_tunedVSVM$resample$Kappa
      best_newSize4iter= newSizes[nS4iter]
      best_cluster = clusterSizes[cS]
    }
  }
  }
fin_predLabelsVSVM = predict(new_tunedVSVM, validateFeatsub)
accVSVM_SL_Un_it  = confusionMatrix(fin_predLabelsVSVM, validateLabels)
print(accVSVM_SL_Un_it$overall["Accuracy"])
model_name = paste0("bestFittingModel_Un_mclp_it_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
if(save_models){saveRDS(new_tunedVSVM, model_name)}
# ************************************************************************************************

# predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
# Add predicted labels to the features data set
predLabelsVSVM_Un_unc = cbind(validateFeatsub, fin_predLabelsVSVM)
predLabelsVSVM_Un_unc = setNames(predLabelsVSVM_Un_unc, objInfoNames)
# predict(bestFittingModelUn_b, predLabelsVSVM_Un_unc[1,1:ncol(predLabelsVSVM_Un_unc) - 1])

# ******
print("Computing samples margin distance using Uncertainty distance...")
#calculate uncertainty of the samples by selecting SV's and data set
normdistvsvm_sl_un = uncertainty_dist_v2_2(new_tunedVSVM, predLabelsVSVM_Un_unc)
# predlabels_vsvm_Slu = alter_labels(normdistvsvm_sl_un, validateLabels, resampledSize)
predlabels_vsvm_Slu = add_new_samples(normdistvsvm_sl_un, validateLabels, newSize=resampledSize, cluster=round(resampledSize*1.2))
accVSVM_SL_Un_b_ud = confusionMatrix(predlabels_vsvm_Slu, validateLabels)
print(accVSVM_SL_Un_b_ud$overall["Accuracy"])

# # ****** #
# print("Computing samples margin distance using Marging Samples MS...")
# # margin_sampled_data <- margin_sampling(bestFittingModelUn_b, predLabelsVSVM_Un_unc)
# ms_data_multicore <- margin_sampling_multicore(bestFittingModel, predLabelsVSVM_Un_unc)
# predlabels_vsvm_ms = alter_labels(ms_data_multicore, validateLabels, resampledSize)
# accVSVM_SL_Un_b_ms = confusionMatrix(predlabels_vsvm_ms, validateLabels)
# print(accVSVM_SL_Un_b_ms$overall["Accuracy"])
# 
# # ****** #
# print("Computing samples margin distance using Multiclass Level Uncertainty...")
# mclu_sampled_data <- mclu_sampling(bestFittingModel, predLabelsVSVM_Un_unc)
# predlabels_vsvm_mclu = alter_labels(mclu_sampled_data, validateLabels, resampledSize)
# accVSVM_SL_Un_b_mclu = confusionMatrix(predlabels_vsvm_mclu, validateLabels)
# print(accVSVM_SL_Un_b_mclu$overall["Accuracy"])

# # ****** #
# print("Computing samples margin distance using Multiclass Level Probability...")
# mclp_sampled_data <- mclp_sampling(bestFittingModel, predLabelsVSVM_Un_unc)
# predlabels_vsvm_mclp <- alter_labels(mclp_sampled_data, validateLabels, resampledSize)
# accVSVM_SL_Un_b_mclp = confusionMatrix(predlabels_vsvm_mclp, validateLabels)
# print(accVSVM_SL_Un_b_mclp$overall["Accuracy"])
}
# ################################## VSVM-SL + VIRTUAL (Balanced) Unlabeled Samples ####################################
# REF_v = predict(bestFittingModelUn_b, trainDataCurRemainingsub_b) # ******************************************************************************************************************************* ISSUE
# 
# # get SV of unlabeled samples
# SVindexvUn_b = 1:nrow(trainDataCurRemainingsub_b)
# SVtotalvUn_b = trainDataCurRemaining_b[SVindexvUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
# SVtotalvUn_b = cbind(SVtotalUn_b, REF_v)
# 
# # extracting previously assigned reference column
# SVtotalvUn_bFeat = SVtotalvUn_b[,1:(ncol(SVtotalvUn_b)-2)]
# REF_v = SVtotalvUn_b[,(ncol(SVtotalvUn_b))]
# SVtotalvUn_b = cbind(SVtotalvUn_bFeat,REF_v)
# 
# # get VSs, means rows of SV but with subset on different level
# SVL2vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)
# SVL3vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)
# 
# SVL5vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)
# SVL6vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)
# SVL7vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)
# SVL8vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)
# SVL9vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)
# SVL10vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)
# SVL11vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v)
# 
# # bind original SV with modified to new train data set
# SVinvarvUn_b = rbind(setNames(SVtotalvUn_b,objInfoNames),
#                      setNames(SVL2vUn_b,objInfoNames),
#                      setNames(SVL3vUn_b,objInfoNames),
#                      setNames(SVL5vUn_b,objInfoNames),
#                      setNames(SVL6vUn_b,objInfoNames),
#                      setNames(SVL7vUn_b,objInfoNames),
#                      setNames(SVL8vUn_b,objInfoNames),
#                      setNames(SVL9vUn_b,objInfoNames),
#                      setNames(SVL10vUn_b,objInfoNames),
#                      setNames(SVL11vUn_b,objInfoNames)
# )
# SVinvarvUn_b = rbind(setNames(SVinvar,objInfoNames),
#                      setNames(SVinvarvUn_b,objInfoNames)
# )
# print("Evaluation of VSVM SL with Virtual Unlabeled Samples...")
# model_name = paste0("bestFittingModelvUn_b_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
# SLresult <- Self_Learn(testFeatsub, testLabels, bound, boundMargin, model_name, classProb=TRUE,
#                        SVL_variables = list(
#                          list(SVtotal, SVL2),
#                          list(SVtotal, SVL3),
#                          list(SVtotal, SVL5),
#                          list(SVtotal, SVL6),
#                          list(SVtotal, SVL7),
#                          list(SVtotal, SVL8),
#                          list(SVtotal, SVL9),
#                          list(SVtotal, SVL10),
#                          list(SVtotal, SVL11),
#                          list(SVtotalvUn_b, SVL2Un_b),
#                          list(SVtotalvUn_b, SVL3Un_b),
#                          list(SVtotalvUn_b, SVL5Un_b),
#                          list(SVtotalvUn_b, SVL6Un_b),
#                          list(SVtotalvUn_b, SVL7Un_b),
#                          list(SVtotalvUn_b, SVL8Un_b),
#                          list(SVtotalvUn_b, SVL9Un_b),
#                          list(SVtotalvUn_b, SVL10Un_b),
#                          list(SVtotalvUn_b, SVL11Un_b)
#                         )
# )
# bestFittingModelvUn_b <- SLresult$bestFittingModel
# 
# # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
# predLabelsVSVMvUn_bsum = predict(bestFittingModelvUn_b, validateFeatsub)
# print("VSVM_SL_vUn Accuracy assessment...")
# accVSVM_SL_vUn_b = confusionMatrix(predLabelsVSVMvUn_bsum, validateLabels)
# print(accVSVM_SL_vUn_b$overall["Accuracy"])
# ######################################## UNCERTAINTY function on VSVM-SL-VUNL ########################################
# 
# #add predicted labels to the features data set
# predLabelsVSVMsumVUn_unc= cbind(validateFeatsub, predLabelsVSVMvUn_bsum)
# predLabelsVSVMsumVUn_unc = setNames(predLabelsVSVMsumVUn_unc, objInfoNames)
# 
# # # # ****** # 
# # # print("Computing samples margin distance from Virtual Unlabeled using Marging Sampling MS...")
# # # ms_data_vsvm_Slvu <- margin_sampling_multicore(bestFittingModelvUn_b, predLabelsVSVMsumVUn_unc)
# # # predlabels_vsvm_ms_Slvu = alter_labels(ms_data_vsvm_Slvu, validateLabels, resampledSize)
# # # accVSVM_SL_vUn_b_ms = confusionMatrix(predlabels_vsvm_ms_Slvu, validateLabels)
# # # print(accVSVM_SL_vUn_b_ms$overall["Accuracy"])
# # 
# # ****** #
# print("Computing samples margin distance from Virtual Unlabeled using Multiclass Level Probability...")
# mclp_data_vsvm_Slvu <- mclp_sampling(bestFittingModelvUn_b, predLabelsVSVMsumVUn_unc)
# predlabels_vsvmSlvu_mclp <- alter_labels(mclp_data_vsvm_Slvu, validateLabels, resampledSize)
# accVSVM_SL_vUn_b_mclp = confusionMatrix(predlabels_vsvmSlvu_mclp, validateLabels)
# print(accVSVM_SL_vUn_b_mclp$overall["Accuracy"])

#############################################  MultiScale  ##############################################
print("Computing Multiscale SVM...")
model_name = paste0("tunedSVM_MS_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
if (file.exists(model_name) && !train) {
  tunedSVM_MS <- readRDS(model_name)
  print("Model already exists!")
} else {
  stratSamp = strata(trainDataCurBegMS, c("REF"), size = shares, method = "srswor")
  samples = getdata(trainDataCurBegMS, stratSamp)
  
  # if(length(sampleSizePor)>1){}else{}
  # if(sample_size>1){trainDataCurMS = rbind(trainDataCurMS,samples[,1:ncol(trainDataPoolAllLevMS)])
  # }else{            trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]}
  trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
  trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
  trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]
  
  stratSamp = strata(testDataCurBegMS, c("REF"), size = shares, method = "srswor")
  samples = getdata(testDataCurBegMS, stratSamp)
  
  # if(length(sampleSizePor)>1){}else{}
  # if(sample_size>1){testDataCurMS = rbind(testDataCurMS,samples[,1:ncol(trainDataPoolAllLevMS)])
  # }else{            testDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]}
  testDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
  # split test feat from test label for later join with trainData MS
  testFeatMS = testDataCurMS[,1:(ncol(testDataCurMS)-1)]
  testLabelsMS = testDataCurMS[,ncol(testDataCurMS)]
  
  countTrainDataMS = nrow(trainFeatMS)
  indexTrainDataMS = list(c(1:countTrainDataMS))
  
  #join train and test test data (through indexTrainData in svmFit separable)
  tuneFeat_MS = rbind(trainFeatMS, testFeatMS)
  tuneLabel_MS = unlist(list(trainLabelsMS, testLabelsMS))
  
  #######################################  SVM parameter tuning MS  ########################################
  tunedSVM_MS = svmFit(tuneFeat_MS, tuneLabel_MS, indexTrainDataMS)
  if(save_models){saveRDS(tunedSVM_MS, model_name)}
}
# run classification and accuracy assessment for unmodified SV and predict labels of test data
predLabelsSVMmultiScale = predict(tunedSVM_MS, validateFeatAllLevMS)
print("SVM_M Accuracy assessment...")
accSVM_M = confusionMatrix(predLabelsSVMmultiScale, validateLabelsMS)
print(accSVM_M$overall["Accuracy"])

############################################## Save Accuracies #############################################

# write current kappa and accuracy value in Kappas matrix
# KappaSVM[i,u] = as.numeric(accSVM$overall["Kappa"])
AccuracySVM[realization,sample_size] = as.numeric(accSVM$overall["Accuracy"])
AccuracySVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Accuracy"])

AccuracyVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Accuracy"])
AccuracyVSVM_SL_Un_b[realization,sample_size] = as.numeric(accVSVM_SL_Un_b$overall["Accuracy"])

AccuracyVSVM_SL_Un_b_ud[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ud$overall["Accuracy"])
# AccuracyVSVM_SL_Un_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ms$overall["Accuracy"])
# AccuracyVSVM_SL_Un_b_mclu[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclu$overall["Accuracy"])
# AccuracyVSVM_SL_Un_b_mclp[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclp$overall["Accuracy"])
AccuracyVSVM_SL_Un_it[realization,sample_size] = as.numeric(accVSVM_SL_Un_it$overall["Accuracy"])

# AccuracyVSVM_SL_vUn_b[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b$overall["Accuracy"])
# AccuracyVSVM_SL_vUn_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b_ms$overall["Accuracy"])
# AccuracyVSVM_SL_vUn_it[realization,sample_size] = as.numeric(accVSVM_SL_vUn_it$overall["Accuracy"])
# AccuracyVSVM_SL_vUn_mclp[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b_mclp$overall["Accuracy"])

}
  # Find the best hyperparameters overall
  best_bound_oa_SL = c(best_bound_oa_SL, best_bound_SL)
  best_boundMargine_oa_SL = c(best_boundMargine_oa_SL, best_boundMargin_SL)
  best_bound_oa_SL_Un = c(best_bound_oa_SL_Un, best_bound_SL_Un)
  best_boundMargine_oa_SL_Un = c(best_boundMargine_oa_SL_Un, best_boundMargin_SL_Un)
  best_newSize_oa=c(best_newSize_oa, best_newSize4iter)
  best_cluster_oa=c(best_cluster_oa, best_cluster_oa)
}
setwd(paste0(model_path,"results"))
save(AccuracySVM,AccuracySVM_M,AccuracyVSVM_SL,AccuracyVSVM_SL_Un_b,AccuracyVSVM_SL_Un_b_ud,AccuracyVSVM_SL_Un_it,
     file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_Col_",invariance,"_",model_class,"_accuracy_",b,"Unl_",nR,"nR.RData"))
print("Accuracy results: acquired.")