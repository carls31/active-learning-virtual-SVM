library(caret)
library(kernlab)
library(sampling)
library(progress)   # progress bar visualization
library(stats)      # k-means clustering
library(foreach)    # parallel processing
library(doParallel) # multiple CPU core
num_cores <- parallel::detectCores() # Numbers of cores deployed for multicore
city = "Hag"
invariance = "scale"
binary = TRUE   # Choose between Binary or Multiclass classification

nR = 2  # Number of Realizations # 10

bound = c(0.3, 0.6, 0.9)            # radius around SV - threshold            # c(0.3,0.45,0.6,0.75,0.9)
boundMargin = c(1.5, 1, 0.5)        # distance from hyperplane - threshold    # c(0.5,0.75,1,1.25,1.5)
b = 20                              # Size of balanced_unlabeled_samples in each class

newSizes = c(2)              # number of samples picked in each Active Learning iteration # 3, 4, 5, 10,20,25
clusterSizes = c(40,120)         # number of clusters used to pick samples from different groups # 60, 80, 90, 100, 300
resampledSize = c(b)         # total number of relabeld samples # 100, 150, 200, 250

train  = TRUE         # if TRUE, train the models otherwise load them from dir 
save_models = TRUE    # if TRUE, save the models into dir after training
if(binary){
  model_class="binary"
  sampleSizePor = c(2,5,10,20) # vector with % of max  # c(2,5,10,20,35,53,75,100) c(5,10,20)
}else{
  model_class="multiclass"
  sampleSizePor = c(5,10,20,32,46,62,80,100) # Class sample size: round(250/6) label per class i.e. 42 # c(5,10,20,32,46,62,80,100)
} 
path = '/home/rsrg9/Documents/tunc_oz/apply_model/'
model_path = "/home/rsrg9/Documents/GitHub/active-learning-virtual-SVM/"
if(!dir.exists(path)){path = "D:/tunc_oz/apply_model/"
  model_path = "D:/GitHub/active-learning-virtual-SVM/"
  nR = 1
  num_cores = 3
  # sampleSizePor = c(40)
}
########################################  Utils  ########################################
svmFit = function(x, y, indexTrain, classProb = FALSE, showPrg = TRUE, metric = "Kappa"){ #x = training descriptors, y = class labels
  
  coarseGrid = expand.grid(sigma = 2^seq(-5,3,by=2), C = 2^seq(-4,12,by=2))
  
  set.seed(13)
  if(showPrg){print("running coarse grid search and narrow grid search...")}
  svmFitCoarse = train(x, y, 
                       method = "svmRadial",
                       metric = metric, 
                       maximize = TRUE,
                       tuneGrid = coarseGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]]#, classProbs =  classProb
                       ),
                       scaled = FALSE
  )
  
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
  
  narrowGrid = expand.grid(sigma = 2^seq(aS,bS,by=0.5), C = 2^seq(aC,bC,by=0.5))
  
  set.seed(31)
  svmFitNarrow = train(x, y, 
                       method = "svmRadial",
                       metric = metric, 
                       maximize = TRUE,
                       tuneGrid = narrowGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]], classProbs =  classProb
                       ),
                       scaled = FALSE
  )
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
  
  boundClass1 = NA
  boundClass2 = NA
  
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

# rem_extrem_kerneldist(SVtotal, SVL7, bound[jj])
rem_extrem_kerneldist = function(org, VSV1, a, kernel_func){
  
  # Kernel distance between two point lying in the hyperspace
  kern_dis = function(a, b, kernel_func){
    a  <- unlist(a)
    b  <- unlist(b)
    dk <- sqrt( kernel_func(a,a) + kernel_func(b,b) -2*kernel_func(a,b) )
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
  
  for(k in c(1:nrow(org))){
    tmp_cond <- FALSE
    for(class in c(1:length(SVClass))){
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
  
  for(k in c(1:nrow(org))){
    print("step1")
    tmp_cond <- FALSE
    for(class in c(1:length(SVClass))){
      print("step2")
      if(as.integer(distance[k,1]) == class){
        print("step3")
        if(!is.null(boundClass[[class]]) && !is.na(boundClass[[class]])){
          print("step4")
          if(distance[k,2] != 0 && distance[k,2] > (boundClass[[class]])){
            print("step5")
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
# pred_all(org$finalModel, unlist(samp[k, -ncol(samp)]), classes)
pred_all = function(modelfin, dataPoint, dataPointLabels){
  smallestDistance = 9999
  distance = c()
  for(ll in seq(along = dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along = binaryClassProblem)){ #print(binaryClassProblem[[l]])
      
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProblem[[l]])){ #print(paste("vero", pred))
        pred = sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
          modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[1:length(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
        
        if(abs(pred) < abs(smallestDistance)){
          smallestDistance = abs(pred)}
      }
    }
    distance = c(distance, smallestDistance)
  }
  return(distance)   
}

# Evaluate the distance between samples and Support Vectors lying in the hyperspace
uncertainty_dist_v2_2 = function(org, samp) {
  
  distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  registerDoParallel(num_cores)
  distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    # calculate_margin_distance(k)
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), samp[k, ncol(samp)])
  }
  registerDoSEQ()
  
  scaled_distances <- apply(distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  distance$distance <- scaled_distances
  samp <- cbind(samp, distance)
  
  return(samp)
}

# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING new_tunedVSVM, predLabelsVSVM_unc
margin_sampling <- function(org, samp, classes=NA) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  
  # Initialize data frame to store margin distance for each sample
  margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Set up parallel backend
  registerDoParallel(num_cores)
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    # calculate_margin_distance(k)
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), classes)
  }
  registerDoSEQ()
  
  # Apply "range" normalization to mclp_distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # Assign scaled distances to probability dataframe
  margin_distance$distance <- scaled_distances
  merged_data <- cbind(samp, margin_distance)
  
  return(merged_data)
}

# mclu_sampling(new_tunedVSVM, predLabelsVSVM_unc)
# Evaluate Multiclass Level Uncertainty (MCLU)
mclu_sampling <- function(org, samp, classes=NA) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  # Initialize data frame to store uncertainty for each sample
  uncertainty <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_mclu_distance <- function(k) {
    
    distances <- pred_all(org$finalModel, unlist(samp[k, -ncol(samp)]), classes)
    
    distance_top <- sort(unlist(distances), decreasing = TRUE)[1:2]
    
    return(abs(distance_top[1] - distance_top[2]))
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

add_new_samples = function(distance_data,
                           ref, features=NA,
                           new_trainFeatVSVM=NA, new_trainLabelsVSVM=NA,
                           newSize=4, cluster=5){
  if(cluster<newSize){cluster=newSize+1}
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

self_learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVMfinModel, SVtotal, SVL_variables,objInfoNames,rem_extrem,rem_extrem_kerneldist, train=TRUE, classProb = FALSE)
{
  if (file.exists(model_name) && !train) {
    bestFittingModel <- readRDS(model_name)
    actKappa = bestFittingModel$resample$Kappa
    print("Luckily, model already exists!")
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa))
  } else {
    actKappa = -1e-6
    print("applying constraints to VSVs candidates...")
    # iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along = c(1:length(bound)))){
      
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
        print(paste0("testing similarity threshold: ",bound[jj]," [",jj,"/",length(bound),"] | bound margin: ",boundMargin[kk]," [",kk,"/",length(boundMargin),"]"))
        
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
          
          if(signa < boundMargin[kk]){
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
        if(actKappa < tunedVSVM$resample$Kappa){ print(paste("found new best kappa:",round(tunedVSVM$resample$Kappa,4)))
          bestFittingModel = tunedVSVM
          actKappa = tunedVSVM$resample$Kappa
          best_trainFeatVSVM = trainFeatVSVM
          best_trainLabelsVSVM = trainLabelsVSVM
          best_bound= bound[jj]
          best_boundMargin = boundMargin[kk]
        }
      }
    } 
    if(save_models && sample_size==5){saveRDS(bestFittingModel, model_name)}
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa, 
                best_trainFeatVSVM = best_trainFeatVSVM, 
                best_trainLabelsVSVM = best_trainLabelsVSVM, 
                best_bound = best_bound, 
                best_boundMargin = best_boundMargin))
  }
}
########################################  Input  ########################################
inputPath ="hagadera_all_level_scale_specgeomtex.csv"  

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

columnClass = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                "factor","integer")

setwd(paste0(path, "csv_data_r_import/hagadera/scale"))

# import data
generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)
colnames(generalDataPool)[209] = "REF"

# exclude unclassified and delete level of factor
generalDataPool = subset(generalDataPool, REF != "unclassified")
generalDataPool$REF <- factor(generalDataPool$REF)
generalDataPool <- na.omit(generalDataPool)

char_columns <- which(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class) == "character")
generalDataPool[char_columns] <- lapply(generalDataPool[char_columns], function(x) as.numeric(as.character(x)))
unique(sapply(generalDataPool[,1:(ncol(generalDataPool)-2)], class))

if(binary){
  # transform to 2-Class-Case "Bushes Trees" VS rest
  print(levels(generalDataPool$REF)[1]) # note that the first record is of class "bushes trees"
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
}

REF = generalDataPool[,ncol(generalDataPool)-1]

###################################################  Scaling  ################################################

normalizedFeat = generalDataPool[,1:(ncol(generalDataPool)-2)]
normalizedLabelUSE = generalDataPool[,(ncol(generalDataPool)-1):(ncol(generalDataPool))]

preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")

# # *************************************** data for map visualization *****************************************
# normalized_data = predict(preProc, setNames(cbind(generalDataPool[,sindexSVMDATA:eindexSVMDATA], REF),objInfoNames[-length(objInfoNames)]))
# # ************************************************************************************************************

normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))

normalizedFeat2 = predict(preProc, setNames(normalizedFeat[,1:numFeat],objInfoNames[-length(objInfoNames)]))
normalizedFeat3 = predict(preProc, setNames(normalizedFeat[,(numFeat+1):(2*numFeat)],objInfoNames[-length(objInfoNames)]))

normalizedFeat5 = predict(preProc, setNames(normalizedFeat[,(3*numFeat+1):(4*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat6 = predict(preProc, setNames(normalizedFeat[,(4*numFeat+1):(5*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat7 = predict(preProc, setNames(normalizedFeat[,(5*numFeat+1):(6*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat8 = predict(preProc, setNames(normalizedFeat[,(6*numFeat+1):(7*numFeat)],objInfoNames[-length(objInfoNames)]))
normalizedFeat9 = predict(preProc, setNames(normalizedFeat[,(7*numFeat+1):(8*numFeat)],objInfoNames[-length(objInfoNames)]))

normalizedDataPoolAllLev = cbind(normalizedFeat2, normalizedFeat3, normalizedFeatBase,
                                 normalizedFeat5,  normalizedFeat6, normalizedFeat7,  
                                 normalizedFeat8, normalizedFeat9, normalizedLabelUSE
)
rm(normalizedFeatBase, normalizedFeat2, normalizedFeat3, normalizedFeat5, 
   normalizedFeat6, normalizedFeat7, normalizedFeat8, normalizedFeat9
   )
############################################  Splitting & Sampling  ###########################################

#Split data in test and train data
splitdf <- split(normalizedDataPoolAllLev, normalizedDataPoolAllLev$USE)
trainDataPoolAllLev = as.data.frame(splitdf[[1]])
testDataAllLev = as.data.frame(splitdf[[2]])
validateDataAllLev = as.data.frame(splitdf[[3]])
rm(splitdf)

# remove use indicator in last column
trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
validateDataAllLev = validateDataAllLev[,1:ncol(validateDataAllLev)-1]

# split Validate data in features and labels and subset on basislevel of first SVM
validateFeatAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-1)]
validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
validateFeatsub = validateFeatAllLev[sindexSVMDATA:eindexSVMDATA]

# order train datapool by class label in alphabetical order:
trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]

# remove used temporary variables
rm(validateDataAllLev, validateFeatAllLev
   )
################################################ MultiScale ###################################################

# normalize feature for MultiScale
nomalizedFeat_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]

preProc = preProcess(nomalizedFeat_MS, method = "range")
nomalizedFeat_MS = predict(preProc, nomalizedFeat_MS)
normalizedDataPoolAllLev_MS = cbind( nomalizedFeat_MS[1:((sindexSVMDATA + 8*numFeat)-1)], normalizedLabelUSE)

# # *************************************** data for map visualization *****************************************
# normalized_data_MS = predict(preProc, generalDataPool[,1:(ncol(generalDataPool)-2)])
# normalized_data_MS = cbind( normalized_data_MS[1:((sindexSVMDATA + 8*numFeat)-1)])
# # ************************************************************************************************************

#Split data in test, train and validate data Multiscale
splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$USE)
trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
testDataAllLevMS = as.data.frame(splitdf[[2]])
validateDataAllLevMS = as.data.frame(splitdf[[3]])

# remove use indicator in last column MS
trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
validateDataAllLevMS = validateDataAllLevMS[,1:ncol(validateDataAllLevMS)-1]

# split Validate data in features and labels for MS
validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-1)]
validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS))]

# order train datapool by class label in alphabetical order:
trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]

# remove used temporary variables
rm(nomalizedFeat_MS, validateDataAllLevMS, splitdf, normalizedDataPoolAllLev_MS
   )
###############################################################################################################

AccuracySVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM) = colheader
AccuracySVM_M = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM_M) = colheader
AccuracySVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM_SL_Un_b) = colheader

AccuracyVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM) = colheader
AccuracyVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL) = colheader
AccuracyVSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b) = colheader

AccuracyVSVM_SL_vUn_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_b) = colheader
# AccuracyVSVM_SL_vUn_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(AccuracyVSVM_SL_vUn_b_ud) = colheader
# AccuracyVSVM_SL_vUn_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(AccuracyVSVM_SL_vUn_it) = colheader

AccuracyVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_it) = colheader
AccuracyVSVM_SL_Un_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_ud) = colheader
AccuracyVSVM_SL_Un_b_ms = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_ms) = colheader
AccuracyVSVM_SL_Un_b_mclu = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_mclu) = colheader
AccuracyVSVM_SL_Un_b_mclp = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_mclp) = colheader

# ******** KAPPA SCORE
KappaSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM) = colheader
KappaSVM_M = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM_M) = colheader
KappaSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM_SL_Un_b) = colheader

KappaVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM) = colheader
KappaVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL) = colheader
KappaVSVM_SL_Un_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_b) = colheader

KappaVSVM_SL_vUn_b = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_vUn_b) = colheader
# KappaVSVM_SL_vUn_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(KappaVSVM_SL_vUn_b_ud) = colheader
# KappaVSVM_SL_vUn_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(KappaVSVM_SL_vUn_it) = colheader

KappaVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_it) = colheader
KappaVSVM_SL_Un_b_ud = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_b_ud) = colheader
KappaVSVM_SL_Un_b_ms = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_b_ms) = colheader
KappaVSVM_SL_Un_b_mclu = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_b_mclu) = colheader
KappaVSVM_SL_Un_b_mclp = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un_b_mclp) = colheader

best_bound_oa_SL = c()
best_boundMargine_oa_SL = c()
best_bound_oa_SL_Un = c()
best_boundMargine_oa_SL_Un = c()
best_bound_oa_SL_vUn = c()
best_boundMargine_oa_SL_vUn = c()
best_newSize_oa=c()
best_cluster_oa=c()
best_resample_oa=c()
best_model_oa=c()

# set randomized seed for the random sampling procedure
seed = 20 # 5, 73, 20 

for(realization in seq(along = c(1:nR))){#}
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
    print(paste0(model_class ," - ",invariance," invariance | realization [",realization,"/",nR,"] | labeled sample: ",sampleSizePor[sample_size]*2," [",sample_size,"/",length(sampleSizePor),"]"))
    
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
    trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and validate set
    # ************************************************ *******************************************************
    # join of train and test test data (separable through indexTrainData in svmFit)
    tuneFeat = rbind(trainFeat, testFeatsub)
    tuneLabel = unlist(list(trainLabels, testLabels))

    setwd(paste0(model_path, "saved_models/hagadera"))
    
    print("computing SVM...")
    model_name = paste0(format(Sys.time(),"%Y%m%d"),"tunedSVM_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl",".rds")
    if (file.exists(model_name) && !train) {
      tunedSVM <- readRDS(model_name)
      print("Luckily, model already exists!")
    } else {
      tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      if(save_models && sample_size==5){saveRDS(tunedSVM, model_name)}
    }
    # run classification and accuracy assessment for unmodified SV and predict labels of test data
    predLabelsSVM = predict(tunedSVM, validateFeatsub)
    accSVM = confusionMatrix(predLabelsSVM, validateLabels)
    print(paste0("SVM accuracy assessment result: ",round(accSVM$overall["Accuracy"],4)))
    
    best_acc <- accSVM$overall["Accuracy"]
    new_bestTunedVSVM <- tunedSVM
    new_bestTrainFeatVSVM <- trainFeat 
    new_bestTrainLabelsVSVM <- trainLabels 
    best_model <- model_name
    AccuracySVM[realization,sample_size] = as.numeric(accSVM$overall["Accuracy"])
    KappaSVM[realization,sample_size] = as.numeric(accSVM$overall["Kappa"])
    
    # ********************** 
    # get original SVs of base SVM
    SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
    SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
    # **********************
    binaryClassProblem = list()
    for(jj in seq(along = c(1:length(tunedSVM$finalModel@xmatrix)))){ # COMPARE EVERY COUPLE COMBINATION OF CLASSES
      binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
    }
    # **********************
    
    ################################################ SVM MS  #############################################
    model_name = paste0(format(Sys.time(),"%Y%m%d"),"tunedSVM_MS_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl",".rds")
    if (file.exists(model_name) && !train) {
      tunedSVM_MS <- readRDS(model_name)
      print("Luckily, model already exists!")
    } else {
      stratSamp = strata(trainDataCurBegMS, c("REF"), size = shares, method = "srswor")
      samples = getdata(trainDataCurBegMS, stratSamp)

      if(length(sampleSizePor)>1 && sample_size>1){
            trainDataCurMS = rbind(trainDataCurMS,samples[,1:ncol(trainDataPoolAllLevMS)])
      }else{trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]}
      trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
      trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]

      stratSamp = strata(testDataCurBegMS, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBegMS, stratSamp)

      if(length(sampleSizePor)>1 && sample_size>1){
            testDataCurMS = rbind(testDataCurMS,samples[,1:ncol(trainDataPoolAllLevMS)])
      }else{testDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]}
      # split test feat from test label for later join with trainData MS
      testFeatMS = testDataCurMS[,1:(ncol(testDataCurMS)-1)]
      testLabelsMS = testDataCurMS[,ncol(testDataCurMS)]

      countTrainDataMS = nrow(trainFeatMS)
      indexTrainDataMS = list(c(1:countTrainDataMS))

      #join train and test test data (through indexTrainData in svmFit separable)
      tuneFeat_MS = rbind(trainFeatMS, testFeatMS)
      tuneLabel_MS = unlist(list(trainLabelsMS, testLabelsMS))

      print("computing multilevel SVM...")
      tunedSVM_MS = svmFit(tuneFeat_MS, tuneLabel_MS, indexTrainDataMS)
      if(save_models && sample_size==5){saveRDS(tunedSVM_MS, model_name)}
    }
    # run classification and accuracy assessment for unmodified SV and predict labels of test data
    predLabelsSVMmultiScale = predict(tunedSVM_MS, validateFeatAllLevMS)
    accSVM_M = confusionMatrix(predLabelsSVMmultiScale, validateLabelsMS)
    print(paste0("SVM_M accuracy assessment result: ",round(accSVM_M$overall["Accuracy"],4)))
    AccuracySVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Accuracy"])
    KappaSVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Kappa"])
    ####################################### SVM-SL + semi-labeled samples #####################################
    
    # Definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
    #stratSampRemaining = strata(trainDataCurRemaining, size = 6*b, method = "srswor") # if trainDataCur is balanced apriori

    # get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemainingSVM_b = getdata(trainDataCurRemaining, stratSampRemaining)
    trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemainingSVM_b$ID_unit), ]

    trainDataCurRemainingSVM_b = samplesRemainingSVM_b[,1:ncol(trainDataPoolAllLev)]
    trainDataCurRemainingSVMsub_b = trainDataCurRemainingSVM_b[sindexSVMDATA:eindexSVMDATA]

    REFSVM_b = predict(tunedSVM, trainDataCurRemainingSVMsub_b)
    # get SV of unlabeled samples
    SVindexSVMUn_b = 1:nrow(trainDataCurRemainingSVMsub_b)
    SVtotalSVMUn_b = trainDataCurRemainingSVM_b[SVindexSVMUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
    SVtotalSVMUn_b = cbind(SVtotalSVMUn_b, REFSVM_b)
    # get VSs, means rows of SV but with subset on different level
    SVL2SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM_b)
    SVL3SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM_b)

    SVL5SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM_b)
    SVL6SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM_b)
    SVL7SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM_b)
    SVL8SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM_b)
    SVL9SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM_b)

    print("evaluation of SVM self learning with semi-labeled samples...")
    model_name = paste0("bestFittingModelSVMUn_b_",city,"_",model_class,"_",invariance,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                           SVL_variables = list(
                             list(SVtotalSVMUn_b, SVL2SVMUn_b),
                             list(SVtotalSVMUn_b, SVL3SVMUn_b),
                             list(SVtotalSVMUn_b, SVL5SVMUn_b),
                             list(SVtotalSVMUn_b, SVL6SVMUn_b),
                             list(SVtotalSVMUn_b, SVL7SVMUn_b),
                             list(SVtotalSVMUn_b, SVL8SVMUn_b),
                             list(SVtotalSVMUn_b, SVL9SVMUn_b)
                           )
    )
    bestFittingModelSVMUn_b <- SLresult$bestFittingModel
    best_trainFeatSVMUn_b <- SLresult$best_trainFeatVSVM
    best_trainLabelsSVMUn_b <- SLresult$best_trainLabelsVSVM
    best_boundSVM_SL_Un = SLresult$best_bound
    best_boundMarginSVM_SL_Un = SLresult$best_boundMargin
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsSVMsumUn_b = predict(bestFittingModelSVMUn_b, validateFeatsub)
    accSVM_SL_Un_b = confusionMatrix(predLabelsSVMsumUn_b, validateLabels)
    print(paste0("SVM_SL_Un accuracy assessment result: ",round(accSVM_SL_Un_b$overall["Accuracy"],4)))
    
    if(accSVM_SL_Un_b$overall["Accuracy"]>best_acc){
      best_acc <- accSVM_SL_Un_b$overall["Accuracy"]
      new_bestTunedSVM <- bestFittingModelSVMUn_b
      new_bestTrainFeatSVM <- best_trainFeatSVMUn_b
      new_bestTrainLabelsSVM <- best_trainLabelsSVMUn_b
      best_model <- model_name
    }
    AccuracySVM_SL_Un_b[realization,sample_size] = as.numeric(accSVM_SL_Un_b$overall["Accuracy"])
    KappaSVM_SL_Un_b[realization,sample_size] = as.numeric(accSVM_SL_Un_b$overall["Kappa"])
    ################################################# VSVM ################################################# 
    
    # get VSs, means rows of SV but with subset on different level
    SVL2 = trainDataCur[SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
    SVL3 = trainDataCur[SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]
    
    SVL5 = trainDataCur[SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
    SVL6 = trainDataCur[SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
    SVL7 = trainDataCur[SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
    SVL8 = trainDataCur[SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
    SVL9 = trainDataCur[SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]

    # bind original SV with modified to new train data set
    SVinvar = rbind(setNames(SVtotal,objInfoNames),
                    setNames(SVL2,objInfoNames),
                    setNames(SVL3,objInfoNames),
                    setNames(SVL5,objInfoNames), 
                    setNames(SVL6,objInfoNames),
                    setNames(SVL7,objInfoNames),
                    setNames(SVL8,objInfoNames),
                    setNames(SVL9,objInfoNames)
    ) 
    
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
    
    print("computing VSVM...")
    model_name = paste0(format(Sys.time(),"%Y%m%d"),"tunedVSVM_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl",".rds")
    if (file.exists(model_name) && !train) {
      tunedVSVM <- readRDS(model_name)
      print("Luckily, model already exists!")
    } else {
      tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
      if(save_models && sample_size==5){saveRDS(tunedVSVM, model_name)}
    }
    
    # predict labels of test data i.e. run classification and accuracy assessment for modified SV
    predLabelsVSVM = predict(tunedVSVM, validateFeatsub)
    accVSVM = confusionMatrix(predLabelsVSVM, validateLabels)
    print(paste0("VSVM accuracy assessment result: ",round(accVSVM$overall["Accuracy"],4)))
    
    if(accVSVM$overall["Accuracy"]>best_acc){
      best_acc <- accVSVM$overall["Accuracy"]
      new_bestTunedVSVM <- tunedVSVM
      new_bestTrainFeatVSVM <- trainFeatVSVM
      new_bestTrainLabelsVSVM <- trainLabelsVSVM
      best_model <- model_name
    } 
    AccuracyVSVM[realization,sample_size] = as.numeric(accVSVM$overall["Accuracy"])
    KappaVSVM[realization,sample_size] = as.numeric(accVSVM$overall["Kappa"])
    
    print("evaluation of VSVM SL...")
    model_name = paste0(format(Sys.time(),"%Y%m%d"),"bestFittingModel_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl",".rds")
    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, SVtotal,objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                           SVL_variables = list(
                              list(SVtotal, SVL2),
                              list(SVtotal, SVL3),
                              list(SVtotal, SVL5),
                              list(SVtotal, SVL6),
                              list(SVtotal, SVL7),
                              list(SVtotal, SVL8),
                              list(SVtotal, SVL9)
                              )
    )
    bestFittingModel <- SLresult$bestFittingModel
    best_trainFeatVSVM <- SLresult$best_trainFeatVSVM
    best_trainLabelsVSVM <- SLresult$best_trainLabelsVSVM
    best_bound_SL = SLresult$best_bound
    best_boundMargin_SL = SLresult$best_boundMargin
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
    accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
    print(paste0("VSVM_SL accuracy assessment result: ",round(accVSVM_SL$overall["Accuracy"],4)))

    if(accVSVM_SL$overall["Accuracy"]>best_acc){
      best_acc <- accVSVM_SL$overall["Accuracy"]
      new_bestTunedVSVM <- bestFittingModel
      new_bestTrainFeatVSVM <- best_trainFeatVSVM
      new_bestTrainLabelsVSVM <- best_trainLabelsVSVM
      best_model <- model_name
    }
    AccuracyVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Accuracy"])
    KappaVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Kappa"])
    ################################### VSVM-SL + semi-labeled samples #####################################
    # Definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
    # Get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
    trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]
    
    trainDataCurRemaining_b = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
    trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]
    
    REF_b = predict(tunedVSVM, trainDataCurRemainingsub_b)
    # get SV of unlabeled samples
    indexUn_b = 1:nrow(trainDataCurRemainingsub_b)
    totalUn_b = trainDataCurRemaining_b[indexUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
    totalUn_b = cbind(totalUn_b, REF_b)
    # get VSs, means rows of SV but with subset on different level
    L2Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)
    L3Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)

    L5Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)
    L6Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)
    L7Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)
    L8Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)
    L9Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)

    print("Evaluation of VSVM Self Learning with semi-labeled samples...")
    model_name = paste0("bestFittingModelUn_b_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size] ,"_unl",b,".rds")
    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, SVtotal,objInfoNames,rem_extrem,rem_extrem_kerneldist, # classProb=TRUE,
                           SVL_variables = list(
                               list(SVtotal, SVL2),
                               list(SVtotal, SVL3),
                               list(SVtotal, SVL5),
                               list(SVtotal, SVL6),
                               list(SVtotal, SVL7),
                               list(SVtotal, SVL8),
                               list(SVtotal, SVL9),
                               list(totalUn_b, L2Un_b),
                               list(totalUn_b, L3Un_b),
                               list(totalUn_b, L5Un_b),
                               list(totalUn_b, L6Un_b),
                               list(totalUn_b, L7Un_b),
                               list(totalUn_b, L8Un_b),
                               list(totalUn_b, L9Un_b)
                            )
    )
    bestFittingModelUn_b <- SLresult$bestFittingModel
    best_trainFeatVSVMUn_b <- SLresult$best_trainFeatVSVM
    best_trainLabelsVSVMUn_b <- SLresult$best_trainLabelsVSVM
    best_bound_SL_Un = SLresult$best_bound
    best_boundMargin_SL_Un = SLresult$best_boundMargin
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsVSVMsumUn_b = predict(bestFittingModelUn_b, validateFeatsub)
    accVSVM_SL_Un_b = confusionMatrix(predLabelsVSVMsumUn_b, validateLabels)
    print(paste0("VSVM_SL_Un accuracy assessment result: ",round(accVSVM_SL_Un_b$overall["Accuracy"],4)))

    if(accVSVM_SL_Un_b$overall["Accuracy"]>best_acc){
      best_acc <- accVSVM_SL_Un_b$overall["Accuracy"]
      new_bestTunedVSVM <- bestFittingModelUn_b
      new_bestTrainFeatVSVM <- best_trainFeatVSVMUn_b
      new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn_b
      best_model <- model_name
    }
    AccuracyVSVM_SL_Un_b[realization,sample_size] = as.numeric(accVSVM_SL_Un_b$overall["Accuracy"])
    KappaVSVM_SL_Un_b[realization,sample_size] = as.numeric(accVSVM_SL_Un_b$overall["Kappa"])
    ################################ VSVM-SL + Virtual semi-labeled Samples ##################################
    # Definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining_v = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
    # Get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemaining_v = getdata(trainDataCurRemaining, stratSampRemaining_v)
    trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_v$ID_unit), ]
    
    trainDataCurRemaining_v = samplesRemaining_v[,1:ncol(trainDataPoolAllLev)]
    # trainDataCurRemainingsub_v = trainDataCurRemaining_v[sindexSVMDATA:eindexSVMDATA]
    # 
    # REF_v = predict(bestFittingModelUn_b, trainDataCurRemainingsub_v) 
    # 
    # get SV of unlabeled samples
    SVindexvUn_v = bestFittingModelUn_b$finalModel@SVindex # 1:nrow(trainDataCurRemainingsub_v) 
    SVtotalvUn_v = na.omit(trainDataCurRemaining_v[SVindexvUn_v ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_v))])
    # SVtotalvUn_v = cbind(SVtotalvUn_v, REF_v)
    # 
    # # extracting previously assigned reference column
    # SVtotalvUn_vFeat = SVtotalvUn_v[,1:(ncol(SVtotalvUn_v)-1)]
    # REF_v = SVtotalvUn_v[,(ncol(SVtotalvUn_v))]
    # SVtotalvUn_v = cbind(SVtotalvUn_vFeat,REF_v)

    # get VSs, means rows of SV but with subset on different level
    SVL2vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1),ncol(trainDataCurRemaining))]) #)], REF_v)
    SVL3vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1),ncol(trainDataCurRemaining))]) #)], REF_v)
    
    SVL5vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCurRemaining))]) #)], REF_v)
    SVL6vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCurRemaining))]) #)], REF_v)
    SVL7vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCurRemaining))]) #)], REF_v)
    SVL8vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCurRemaining))]) #)], REF_v)
    SVL9vUn_b = na.omit(trainDataCurRemaining_v[SVindexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCurRemaining))]) #)], REF_v)

    print("evaluation of VSVM self learning with virtual semi-labeled samples...")
    model_name = paste0(format(Sys.time(),"%Y%m%d"),"bestFittingModelvUn_b_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl",".rds")
    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name, tunedSVM$finalModel, SVtotal,objInfoNames,rem_extrem,rem_extrem_kerneldist, # classProb=TRUE,
                           SVL_variables = list(
                             list(SVtotal, SVL2),
                             list(SVtotal, SVL3),
                             list(SVtotal, SVL5),
                             list(SVtotal, SVL6),
                             list(SVtotal, SVL7),
                             list(SVtotal, SVL8),
                             list(SVtotal, SVL9),
                             list(SVtotalvUn_v, SVL2vUn_b),
                             list(SVtotalvUn_v, SVL3vUn_b),
                             list(SVtotalvUn_v, SVL5vUn_b),
                             list(SVtotalvUn_v, SVL6vUn_b),
                             list(SVtotalvUn_v, SVL7vUn_b),
                             list(SVtotalvUn_v, SVL8vUn_b),
                             list(SVtotalvUn_v, SVL9vUn_b)
                           )
    )
    bestFittingModelvUn_b <- SLresult$bestFittingModel
    best_trainFeatVSVMvUn_b <- SLresult$best_trainFeatVSVM
    best_trainLabelsVSVMvUn_b <- SLresult$best_trainLabelsVSVM
    best_bound_SLvUn_b = SLresult$best_bound
    best_boundMargin_SLvUn_b = SLresult$best_boundMargin
    
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsVSVMvUn_bsum = predict(bestFittingModelvUn_b, validateFeatsub)
    accVSVM_SL_vUn_b = confusionMatrix(predLabelsVSVMvUn_bsum, validateLabels)
    print(paste0("VSVM_SL_vUn accuracy assessment result: ",round(accVSVM_SL_vUn_b$overall["Accuracy"],4)))

    if(accVSVM_SL_vUn_b$overall["Accuracy"]>best_acc){
      best_acc <- accVSVM_SL_vUn_b$overall["Accuracy"]
      new_bestTunedVSVM <- bestFittingModelvUn_b
      new_bestTrainFeatVSVM <- best_trainFeatVSVMvUn_b
      new_bestTrainLabelsVSVM <- best_trainLabelsVSVMvUn_b
      best_model <- model_name
    }
    AccuracyVSVM_SL_vUn_b[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b$overall["Accuracy"])
    KappaVSVM_SL_vUn_b[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b$overall["Kappa"])
    ###################################### UNCERTAINTY DISTANCE FUNCTIONS  #######################################
    if(num_cores>4){
      print(paste0("computing uncertainty distance for iterative active learning procedure... [",realization,"/",nR,"] | ",sampleSizePor[sample_size]*2," [",sample_size,"/",length(sampleSizePor),"]"))
      classSize = round(min(table(trainDataCurRemaining$REF))/10 )# number of samples for each class # 250, 500, 750, 1000, 1500, 3000, 5803 for multiclass # min(table(trainDataCurRemaining_it$REF))
      stratSampSize = c(classSize,classSize,classSize,classSize,classSize,classSize)
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
      # Get new samples from trainDataCurRemaining_it
      samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
      # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining$ID_unit), ]
      actKappa = -1e-6
      for(rS in 1:length(resampledSize)){
        for(nS4it in 1:length(newSizes)){
          for(cS in 1:length(clusterSizes)){
            print(paste0("total resampled size: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | ","samples for iteration: ",newSizes[nS4it]," [",nS4it,"/",length(newSizes),"] | ","number of clusters: ",cluster=clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]"))
            
            # new_tunedVSVM <- new_bestTunedVSVM
            # new_trainFeatVSVM <- setNames(new_bestTrainFeatVSVM, names)
            # new_trainLabelsVSVM <- new_bestTrainLabelsVSVM
            
            new_tunedVSVM <- bestFittingModel
            new_trainFeatVSVM <- setNames(best_trainFeatVSVM, names)
            new_trainLabelsVSVM <- best_trainLabelsVSVM
            
            # upd_trainDataCurFeatsub = samplesRemaining[sindexSVMDATA:eindexSVMDATA]
            # upd_trainDataCurLabels = samplesRemaining$REF
            
            # get VSs, means rows of SV but with subset on different level
            SVtotal = samplesRemaining[c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
            
            SVL2 = samplesRemaining[c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(samplesRemaining))]
            SVL3 = samplesRemaining[c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(samplesRemaining))]
            
            SVL5 = samplesRemaining[c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(samplesRemaining))]
            SVL6 = samplesRemaining[c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(samplesRemaining))]
            SVL7 = samplesRemaining[c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(samplesRemaining))]
            SVL8 = samplesRemaining[c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(samplesRemaining))]
            SVL9 = samplesRemaining[c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(samplesRemaining))]
            
            # bind original SV with modified to new train data set
            upd_trainDataCur = rbind(setNames(SVtotal,objInfoNames),
                                     setNames(SVL2,objInfoNames),
                                     setNames(SVL3,objInfoNames),
                                     setNames(SVL5,objInfoNames), 
                                     setNames(SVL6,objInfoNames),
                                     setNames(SVL7,objInfoNames),
                                     setNames(SVL8,objInfoNames),
                                     setNames(SVL9,objInfoNames)
            )
            
            # split for training to feature and label
            upd_trainDataCurFeatsub = upd_trainDataCur[,1:(ncol(upd_trainDataCur)-1)]
            upd_trainDataCurLabels = upd_trainDataCur[,ncol(upd_trainDataCur)]
            
            newSize_for_iter = newSizes[nS4it] #sampleSize/10 # or just 4
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
              if(binary){sampled_data <- margin_sampling(new_tunedVSVM, predLabelsVSVM_unc)
              }else{sampled_data <- mclu_sampling(new_tunedVSVM, predLabelsVSVM_unc)}
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
            if(actKappa < tmp_new_tunedVSVM$resample$Kappa){ print(paste("found new best kappa:",round(tmp_new_tunedVSVM$resample$Kappa,4)))
              new_tunedVSVM = tmp_new_tunedVSVM
              actKappa = tmp_new_tunedVSVM$resample$Kappa
              best_newSize4iter= newSizes[nS4it]
              best_cluster = clusterSizes[cS]
              best_resample = resampledSize[rS]
              
              # *******************************
              tmp_pred = predict(new_tunedVSVM, validateFeatsub)
              tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
              print(paste0("new best accuracy: ",round(tmp_acc$overall["Accuracy"],4)))
              
            }
          }
        }
      }
      fin_predLabelsVSVM = predict(new_tunedVSVM, validateFeatsub)
      accVSVM_SL_Un_it  = confusionMatrix(fin_predLabelsVSVM, validateLabels)
      print(paste0("VSVM_SL - AL accuracy assessment result: ",round(accVSVM_SL_Un_it$overall["Accuracy"],4)))
      model_name = paste0(format(Sys.time(),"%Y%m%d"),"VSVM_SL_ITAL_",city,"_",invariance,"_",model_class,"_",sampleSizePor[sample_size],"_",b,"Unl_",seed,".rds")
      if(save_models && sample_size==5){saveRDS(new_tunedVSVM, model_name)}
      # ************************************************************************************************
      # 
      # # Add predicted labels to the features data set
      # predLabelsVSVM_Un_unc = cbind(validateFeatsub, fin_predLabelsVSVM)
      # predLabelsVSVM_Un_unc = setNames(predLabelsVSVM_Un_unc, objInfoNames)
      # 
      # print("Computing uncertainty samples distance...")
      # #calculate uncertainty of the samples by selecting SV's and data set
      # uncertain_sampled_data = uncertainty_dist_v2_2(new_tunedVSVM, predLabelsVSVM_Un_unc)
      # # predlabels_vsvm_Slu = alter_labels(normdistvsvm_sl_un, validateLabels, resampledSize)
      # predlabels_vsvm_Slu = add_new_samples(uncertain_sampled_data, validateLabels, newSize=resampledSize[1], cluster=round(resampledSize[1]*1.2))
      # accVSVM_SL_Un_b_ud = confusionMatrix(predlabels_vsvm_Slu, validateLabels)
      # print(accVSVM_SL_Un_b_ud$overall["Accuracy"])
      # 
      # ****** #
      # predLabelsVSVM_Un_unc = cbind(validateFeatsub, predLabelsVSVMsumUn_b)
      # predLabelsVSVM_Un_unc = setNames(predLabelsVSVM_Un_unc, objInfoNames)
      # 
      # print("Computing margin samples distance...")
      # # margin_sampled_data <- margin_sampling(bestFittingModelUn_b, predLabelsVSVM_Un_unc)
      # ms_sampled_data <- margin_sampling(bestFittingModel, predLabelsVSVM_Un_unc)
      # predlabels_vsvm_ms = add_new_samples(ms_sampled_data, validateLabels, newSize=resampledSize[1], cluster=round(resampledSize[1]*1.2))
      # accVSVM_SL_Un_b_ms = confusionMatrix(predlabels_vsvm_ms, validateLabels)
      # print(accVSVM_SL_Un_b_ms$overall["Accuracy"])
      # 
      # # ****** #
      # print("Computing Multiclass Level Uncertainty samples distance...")
      # mclu_sampled_data <- mclu_sampling(bestFittingModelUn_b, predLabelsVSVM_Un_unc)
      # predlabels_vsvm_mclu = add_new_samples(mclu_sampled_data, validateLabels, newSize=resampledSize[1], cluster=round(resampledSize[1]*1.2))
      # accVSVM_SL_Un_b_mclu = confusionMatrix(predlabels_vsvm_mclu, validateLabels)
      # print(accVSVM_SL_Un_b_mclu$overall["Accuracy"])
      # 
      # # ****** #
      # print("Computing Multiclass Level Probability samples distance...")
      # mclp_sampled_data <- mclp_sampling(bestFittingModelUn_b, predLabelsVSVM_Un_unc)
      # predlabels_vsvm_mclp <- add_new_samples(mclp_sampled_data, validateLabels, newSize=resampledSize[1], cluster=round(resampledSize[1]*1.2))
      # accVSVM_SL_Un_b_mclp = confusionMatrix(predlabels_vsvm_mclp, validateLabels)
      # print(accVSVM_SL_Un_b_mclp$overall["Accuracy"])
      # 
      AccuracyVSVM_SL_Un_it[realization,sample_size] = as.numeric(accVSVM_SL_Un_it$overall["Accuracy"])
      KappaVSVM_SL_Un_it[realization,sample_size] = as.numeric(accVSVM_SL_Un_it$overall["Kappa"])
    }
    ############################################ Save Accuracies ###########################################

    # AccuracyVSVM_SL_vUn_b_ud[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b_ud$overall["Accuracy"])
    # AccuracyVSVM_SL_vUn_it[realization,sample_size] = as.numeric(accVSVM_SL_vUn_it$overall["Accuracy"])
    
    # AccuracyVSVM_SL_Un_b_ud[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ud$overall["Accuracy"])
    # AccuracyVSVM_SL_Un_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ms$overall["Accuracy"])
    # AccuracyVSVM_SL_Un_b_mclu[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclu$overall["Accuracy"])
    # AccuracyVSVM_SL_Un_b_mclp[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclp$overall["Accuracy"])
    
    # ******** KAPPA SCORE

    # KappaVSVM_SL_vUn_b_ud[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b_ud$overall["Kappa"])
    # KappaVSVM_SL_vUn_it[realization,sample_size] = as.numeric(accVSVM_SL_vUn_it$overall["Kappa"])

    # KappaVSVM_SL_Un_b_ud[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ud$overall["Kappa"])
    # KappaVSVM_SL_Un_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ms$overall["Kappa"])
    # KappaVSVM_SL_Un_b_mclu[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclu$overall["Kappa"])
    # KappaVSVM_SL_Un_b_mclp[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_mclp$overall["Kappa"])
  }
  # Find the best hyperparameters overall
  best_bound_oa_SL = c(best_bound_oa_SL, best_bound_SL)
  best_boundMargine_oa_SL = c(best_boundMargine_oa_SL, best_boundMargin_SL)
  best_bound_oa_SL_Un = c(best_bound_oa_SL_Un, best_bound_SL_Un)
  best_boundMargine_oa_SL_Un = c(best_boundMargine_oa_SL_Un, best_boundMargin_SL_Un)
  best_bound_oa_SL_vUn = c(best_bound_oa_SL_vUn, best_bound_SLvUn_b)
  best_boundMargine_oa_SL_vUn = c(best_boundMargine_oa_SL_vUn, best_boundMargin_SLvUn_b)
  # best_newSize_oa=c(best_newSize_oa, best_newSize4iter)
  # best_cluster_oa=c(best_cluster_oa, best_cluster)
  # best_resample_oa=c(best_resample_oa, best_resample)
  best_model_oa=c(best_model_oa, best_model)
}
if(length(sampleSizePor)>=8){
  setwd(paste0(model_path,"results/hagadera"))
  if(num_cores>4){
    save(AccuracySVM,AccuracySVM_M,AccuracySVM_SL_Un_b,AccuracyVSVM,AccuracyVSVM_SL,AccuracyVSVM_SL_Un_b,AccuracyVSVM_SL_vUn_b,AccuracyVSVM_SL_Un_it,
         file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",invariance,"_",model_class,"_acc_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
    save(KappaSVM,KappaSVM_M,KappaSVM_SL_Un_b,KappaVSVM,KappaVSVM_SL,KappaVSVM_SL_Un_b,KappaVSVM_SL_vUn_b,KappaVSVM_SL_Un_it,
         file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",invariance,"_",model_class,"_Kappa_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
  }else{
    save(AccuracySVM,AccuracySVM_M,AccuracySVM_SL_Un_b,AccuracyVSVM,AccuracyVSVM_SL,AccuracyVSVM_SL_Un_b,AccuracyVSVM_SL_vUn_b,
         file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",invariance,"_",model_class,"_acc_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
    save(KappaSVM,KappaSVM_M,KappaSVM_SL_Un_b,KappaVSVM,KappaVSVM_SL,KappaVSVM_SL_Un_b,KappaVSVM_SL_vUn_b,
         file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",invariance,"_",model_class,"_Kappa_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
  }
print("accuracy results: acquired.")
}
print(best_bound_oa_SL)
print(best_boundMargine_oa_SL)
print(best_bound_oa_SL_Un)
print(best_boundMargine_oa_SL_Un)
print(best_bound_oa_SL_vUn)
print(best_boundMargine_oa_SL_vUn)
# print(best_newSize_oa)
# print(best_cluster_oa)
# print(best_resample_oa)