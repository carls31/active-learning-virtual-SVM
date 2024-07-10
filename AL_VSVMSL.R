library(caret)
library(kernlab)
library(sampling)
library(progress)   # progress bar visualization
library(stats)      # k-means clustering
library(foreach)    # parallel processing
library(doParallel) # multiple CPU cores

nR = 1                   # realizations
cities = c("cologne")    # cologne or hagadera
invariances = c("shape","scale")   # scale or shape invariance
model_probs = c("multiclass")  # multiclass or binary problem

b = c(20)           # Size of balanced_unlabeled_samples per class
bound = c(0.3, 0.6, 0.9)           # radius around SV - threshold    # c(0.3, 0.6, 0.9) # c(0.5, 0.8)        
boundMargin = c(1.5, 1, 0.5)       # distance from hyperplane - threshold   # c(1.5, 1, 0.5) # c(1.5, 1)
sampleSizePor = c(5,10,20,32,46,62,80,100) # Class sample size: round(250/6) label per class i.e. 42 # c(100,80,62,46,32,20,10,5)

resampledSize = c(3*b)    # total number of relabeled samples # b, 2*b, 3*b, 6*b
newSizes = c(0.6*b) # = resampledSize[rS]       # number of samples picked per iteration # 4, 5, 10, 20, resampledSize
# classSize = c(100*b) #1200 # number of samples per class # 25, 50, 75, 100, 150, 300, 580 for multiclass #  min(100*b,as.numeric(min(table(trainDataCurRemaining$REF)))/3)
clusterSizes = c(6*b,48*b) #60*b # number of clusters used to pick samples from different groups # 40, 60, 80, 100, 120, 300

train  = TRUE              # if TRUE, train the models otherwise load them from dir 
num_cores <- parallel::detectCores()-6 # Numbers of CPU cores for parallel processing  
path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}
########################################  Utils  ########################################

svmFit = function(x, y, indexTrain, classProb = FALSE, showPrg = TRUE, metric = "Kappa"){ #x = training descriptors, y = class labels
  
  coarseGrid = expand.grid(sigma = 2^seq(-5,3,by=2), C = 2^seq(-4,12,by=2))
  
  set.seed(13)
  if(showPrg){cat("running coarse grid search and narrow grid search...\n")}
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
# rem_extrem(variable[[1]], variable[[2]], bound[jj])
# rem_extrem(org= SVtotal_ud, VSV1=S01C09, a=0.7)
# Evaluate the distance between Virtual Support Vectors and Support Vectors lying in the input space
rem_extrem = function(org, VSV1, a=0.7){
  # Euclidean Distance between two points lying in the input space
  euc_dis = function(a, b){
    temp = 0
    for(ii in 1:length(a)){
      temp = temp +((a[[ii]]-b[[ii]])^2)
    }
    return(sqrt(temp))
  }
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  distanceSVC2 = c()
  # save label of sample and the distance between SV and VSV for each pair of SV and VSV
  for(l in seq(nrow(org))){
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
  distance$X1 = factor(distance$X1,levels=levels(org$"REF"))
  # Iterate over the distance vector and substitute in VSV1 the samples which overstep the threshold
  for(k in seq(nrow(org))){
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
# rem_extrem_kerneldist(org=SVtotal, VSV1=SVL2, a= 0.5)
# rem_extrem_kerneldist(org=SVtotalSVMUn_b, VSV1=SVL2SVMUn_b, a=0.7,kernel_func=tunedSVM$finalModel@kernelf)
# rem_extrem_kerneldist(org= SVtotal_ud, VSV1=S01C09, a=0.7, kernel_func=SVMfinModel@kernelf)
rem_extrem_kerneldist = function(org, VSV1, a=0.7, kernel_func=tunedSVM$finalModel@kernelf){
  # print("rem_extrem_kerneldist called")
  # print(paste("org rows:", nrow(org), "org cols:", ncol(org)))
  # print(paste("VSV1 rows:", nrow(VSV1), "VSV1 cols:", ncol(VSV1)))
  # print(paste("a:", a))
  # Kernel distance between two point lying in the hyperspace
  kern_dis = function(a, b, kernel_func){
    a  <- unlist(a)
    b  <- unlist(b)
    dk <- sqrt( kernel_func(a,a) + kernel_func(b,b) -2*kernel_func(a,b) )
    return(dk)
  }
  
  # Convert all columns except the last one (reference) to numeric
  org[, -ncol(org)] <- lapply(org[, -ncol(org)], as.numeric)
  VSV1[, -ncol(VSV1)] <- lapply(VSV1[, -ncol(VSV1)], as.numeric)
  
  distance = data.frame(matrix(nrow=nrow(org),ncol=2))
  distanceSVC1 = c()
  
  numClass = nlevels(org$REF)
  SVClass = list()
  
  # split SV according to its classes
  for(f in seq(numClass)){
    SVClass[[f]]=org[which(org$REF==levels(org$"REF")[[f]]),]
  }
  # save label of sample and the distance between SV and VSV in distance for each pair of SV and VSV
  for(l in seq(nrow(org))){
    distance[l,1] = as.character( org[l,ncol(org)])
    distance[l,2] = kern_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)],kernel_func)
  }
  boundClass = list()
  
  # Compute the distance threshold boundClass for each class
  for(f in seq(SVClass)){
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
  distance$X1 = factor(distance$X1,levels=levels(org$"REF"))
  
  for(k in seq(nrow(org))){
    # tmp_cond <- FALSE
    for(class in seq(SVClass)){
      if(as.integer(distance[k,1]) == class){
        if(!is.null(boundClass[[class]]) && !is.na(boundClass[[class]])){
          if(distance[k,2] != 0 && distance[k,2] > (boundClass[[class]])){
            VSV1[k,]=NA
            # tmp_cond <- TRUE
          }
        }
      }
    }#if(!tmp_cond){VSV1[k,]=NA}
  }
  
  for(k in seq(nrow(org))){ #print("step1")
    
    tmp_cond <- FALSE
    for(class in seq(SVClass)){ # print("step2")
      
      if(as.integer(distance[k,1]) == class){ # print("step3")
        
        if(!is.null(boundClass[[class]]) && !is.na(boundClass[[class]])){ # print("step4")
          
          if(distance[k,2] != 0 && distance[k,2] > (boundClass[[class]])){ # print("step5")
            
            VSV1[k,]=NA
            tmp_cond <- TRUE
          }
        }
      }
    }#if(!tmp_cond){VSV1[k,]=NA}
  }
  return(VSV1)
}
# pred_one(modelfin=org$finalModel, dataPoint=unlist(samp[1, -ncol(samp)]), dataPointLabels=classes)
pred_one = function(modelfin, dataPoint, dataPointLabels, binaryClassProb=binaryClassProblem){
  smallestDistance = 9999
  
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
        pred = sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
          # modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[1:length(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
          modelfin@kernelf(modelfin@xmatrix[[l]][j,], dataPoint[1:length(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
        
        if(abs(pred) < abs(smallestDistance))
          smallestDistance = abs(pred)
      }
    }
  }
  return(smallestDistance)   
}
# pred_all(modelfin=org$finalModel, dataPoint=unlist(samp[k, -ncol(samp)]), dataPointLabels=classes)
pred_all = function(modelfin, dataPoint, dataPointLabels, binaryClassProb=binaryClassProblem){
  smallestDistance = 9999
  distance = c()
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
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
# margin_sampling(org=tmp_new_tunedSVM, samp=predLabelsVSVM_unc,pred_one,binaryClassProblem, classes=NA)
# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING new_tunedSVM, predLabelsVSVM_unc
margin_sampling <- function(org, samp, pred_one,binaryClassProblem, classes=NA) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  
  # Initialize data frame to store margin distance for each sample
  margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Set up parallel backend
  registerDoParallel(num_cores)
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    # calculate_margin_distance(k)
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
  }
  registerDoSEQ()
  
  # Apply "range" normalization to mclp_distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # Assign scaled distances to probability dataframe
  margin_distance$distance <- scaled_distances
  merged_data <- cbind(samp, margin_distance)
  
  return(merged_data)
}

# mclu_sampling(new_tunedSVM, predLabelsVSVM_unc)
# Evaluate Multiclass Level Uncertainty (MCLU)
mclu_sampling <- function(org, samp, pred_all,binaryClassProblem, classes=NA) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  # Initialize data frame to store uncertainty for each sample
  uncertainty <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_mclu_distance <- function(k) {
    
    distances <- pred_all(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
    
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
  km_data <- kmeans(ref_added_or[, 1:18], centers = cluster, iter.max = 25, nstart = 200)
  
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
# add_new_samples_AL(distance_data=sampled_data,
#                    ref=upd_dataCurLabels, features=upd_dataCurFeatsub, ID_unit=upd_dataCur$ID_unit,
#                    new_trainFeatVSVM, new_trainLabelsVSVM,
#                    newSize=newSize_for_iter,
#                    cluster=clusterSizes[cS], nFeat=numFeat )
add_new_samples_AL = function(distance_data,
                              ref, features=NA,ID_unit,
                              new_trainFeatVSVM=NA, new_trainLabelsVSVM=NA,
                              newSize=4, cluster=5, nFeat=numFeat){
  if(cluster<newSize){cluster=round(newSize*1.01)}
  # merge features and original labels
  ref_added = cbind(distance_data, ref)
  
  # order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  
  # Perform PCA using prcomp from stats library
  pca_result <- prcomp(ref_added_or[, 1:nFeat], center = TRUE, scale. = TRUE)
  
  # Extract the first two principal components
  pca_data <- data.frame(pca_result$x[, 1:9])
  colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")
  
  # ref_data_with_distance <- cbind(ref_added_or[, 1:nFeat], ref_added_or[, nFeat+3])
  ref_data_with_distance <- cbind(pca_data[, 1:9], ref_added_or[, nFeat+3])
  
  # wss <- (nrow(ref_data_with_distance) - 1) * sum(apply(ref_data_with_distance, 2, var))
  # # wss <- sum(kmeans(ref_data_with_distance, centers = 10)$tot.withinss)
  # for (i in 1:30) wss[i+1] <- sum(kmeans(ref_data_with_distance, centers = i+1, iter.max = 15, nstart = 50)$tot.withinss)
  # plot(1:31, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
  
  # Apply k-means clustering 
  km_result <- kmeans(ref_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
  
  # Add the cluster assignments to the pca_data dataframe
  pca_data$Cluster <- as.factor(km_result$cluster)
  
  # # Plot the first two principal components with k-means clusters
  # cluster_colors <- rainbow(length(unique(pca_data$Cluster)))
  # # Set up plotting area with more space on the right for the legend
  # par(mar = c(5, 4, 4, 8), xpd = TRUE)
  # # Plotting PC1 vs PC2 with different colors for each cluster
  # plot( pca_data$PC1,ref_added_or[, 21], col = cluster_colors[pca_data$Cluster],
  #      pch = 20, cex = 2, main = "K-means Clustering on PCA",
  #      xlab = "Principal Component 1", ylab = "Distance")
  # # legend("right", legend = levels(pca_data$Cluster), col = cluster_colors, pch = 20,
  # #        title = "Cluster",xpd = TRUE, bty = "n")
  # # Adding the legend outside the plot
  # legend("topright", inset = c(-0.2, 0), legend = levels(pca_data$Cluster), col = cluster_colors, pch = 20,
  #        title = "Cluster", bty = "n")

  # # Perform k-means clustering
  # km_result <- kmeans(ref_added_or[, 1:nFeat], centers = cluster, iter.max = 25, nstart = 200)
  
  # Add cluster information to the data
  ref_added_or$cluster <- km_result$cluster
  
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
    new_trainFeatVSVM <- ref_added_reor[reor_idx, 1:(ncol(ref_added_reor)-5)]
    new_trainLabelsVSVM <- ref_added_reor[reor_idx, (ncol(ref_added_reor)-4)]
    ID_unit <- ID_unit[reor_idx]
    return(list(features = features, labels = ref, IDunit=ID_unit,
                new_trainFeatVSVM = new_trainFeatVSVM, 
                new_trainLabelsVSVM = new_trainLabelsVSVM))
  } else{
    return(ref_added_reor[, (ncol(ref_added_reor)-4)])
  } 
}   

# upd_SLresult <- self_learn(testFeatsub, testLabels, bound = c(0.01, 0.1), boundMargin = c(1.5, 0.5), model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables, tmp_new_tunedSVM$finalModel)
# upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables=list(list(SVtotal_ud, S01C09=(cbind(upd_dataCur[upd_SVindex_ud,c((numFeat+1):(2*numFeat))],REF_ud)))))
self_learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVtotal, objInfoNames, rem_extrem, rem_extrem_kerneldist, SVL_variables, SVMfinModel=tunedSVM$finalModel, train=TRUE, classProb = FALSE) {
  if (file.exists(model_name) && !train) {
    bestFittingModel <- readRDS(model_name)
    actKappa = bestFittingModel$resample$Kappa
    cat("Luckily, model already exists!")
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa))
  } else {
    actKappa = -1e-6
    cat("applying constraints to VSVs candidates\n")
    # iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along=bound)){
      
      registerDoParallel(num_cores)
      # Apply foreach loop to process each SVL variable and bind the results
      if(model_prob=="binary"){ # print("step 1")
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
      for(kk in seq(along=boundMargin)){
        cat("tuning similarity threshold: ",bound[jj]," [",jj,"/",length(bound),"] | bound margin: ",boundMargin[kk]," [",kk,"/",length(boundMargin),"]","\n",sep="")
        
        if (nrow(SVinvarRadi) > 0) { # Check if SVinvarRadi has rows to process
          # Remove VSV which are not located within a certain distance to the decision function
          # data.frame to store elected VSV within the margin
          SVinvar = setNames(data.frame(matrix(ncol = numFeat + 1, nrow = 0)), objInfoNames)
          
          # Progress bar initialization
          pb <- progress_bar$new(
            format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
            total = nrow(SVinvarRadi),
            clear = FALSE
          )
          # Iterate over SVinvarRadi and evaluate distance to hyperplane
          for (m in 1:nrow(SVinvarRadi)) {
            signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
            
            if (signa < boundMargin[kk]) {
              SVinvar = rbind(SVinvar, SVinvarRadi[m, ])
            }
            pb$tick()
          }
        } else { warning("SVinvarRadi has no rows to process") }
        
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
        
        # *********************** VSVM control parameter tuning ***********************
        tStart.time <- Sys.time()
        tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData, classProb)
        t.time <- round(as.numeric((Sys.time() - tStart.time), units = "secs"), 3)
        # of all Different bound settings get the one with best Kappa ans save its model
        if (actKappa < tunedVSVM$resample$Kappa) {cat("current best kappa: ",round(tunedVSVM$resample$Kappa,4)," | training time: ",t.time,"sec\n",sep="")
          bestFittingModel = tunedVSVM
          actKappa = tunedVSVM$resample$Kappa
          best_trainFeatVSVM = trainFeatVSVM
          best_trainLabelsVSVM = trainLabelsVSVM
          best_bound= bound[jj]
          best_boundMargin = boundMargin[kk]
          # Check if actKappa is 1 and break if true
          if (actKappa == 1) {
            return(list(bestFittingModel = bestFittingModel, 
                        actKappa = actKappa, 
                        best_trainFeatVSVM = best_trainFeatVSVM, 
                        best_trainLabelsVSVM = best_trainLabelsVSVM, 
                        best_bound = best_bound, 
                        best_boundMargin = best_boundMargin))
          }
        }
      }
    } 
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa, 
                best_trainFeatVSVM = best_trainFeatVSVM, 
                best_trainLabelsVSVM = best_trainLabelsVSVM, 
                best_bound = best_bound, 
                best_boundMargin = best_boundMargin))
  }
}

classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}
########################################  Preprocessing  ########################################
for (model_prob in model_probs) {
  if (model_prob=="binary") { sampleSizePor = c(2,5,10,20,35,53,75,100) # c(100,75,53,35,20,10,5,2)
  # bound = c(0.3, 0.9)          
  # boundMargin = c(1.5, 1, 0.5)
  resampledSize = c(2*b)
  newSizes = c(0.4*b)
  # classSize = c(30*b)
  clusterSizes = c(0.8*b,2*b)
  }
  if (num_cores<5) { nR=1
  sampleSizePor = c(20)  
  resampledSize = c(0.5*b)
  newSizes = c(0.5*b)
  clusterSizes = c(0.55*b) 
  }
  colheader = as.character(sampleSizePor) # corresponding column names
  
  for (invariance in invariances) {
    for (city in cities) {
      
      start.time_oa <- Sys.time()
      cat("preprocessing",city,model_prob,invariance,"\n")
      
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
                        "factor","factor")
        
        # import data
        setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/scale"))
        generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)
        
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
          # normalized_data = predict(preProc, setNames(generalDataPool[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
          # ************************************************************************************************************
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
          rm(normalizedFeat)
          
          # recombine normalized sets to one data frame
          normalizedDataPoolAllLev = cbind(normalizedFeat2, normalizedFeat3, normalizedFeatBase,
                                           normalizedFeat5, normalizedFeat6, normalizedFeat7,
                                           normalizedFeat8, normalizedFeat9, normalizedFeat10,
                                           normalizedFeat11, normalizedLabelUSE
          )
          # remove used temporary variables
          rm(normalizedFeat2, normalizedFeat3, normalizedFeatBase,normalizedFeat5,  normalizedFeat6, normalizedFeat7, 
             normalizedFeat8, normalizedFeat9, normalizedFeat10, normalizedFeat11
          )
          #############################################  Splitting & Sampling  ############################################
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
          validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
          validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
          rm(validateDataAllLev)
          
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
          ##################################################  MultiLevel ###################################################
          # normalize feature for MultiScale
          nomalizedFeat_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]
          # WHAT IS THE DIFFERENCE WITH THE OTHER generalDataPool, i.e. data_MS?
          preProc = preProcess(nomalizedFeat_MS, method = "range")
          nomalizedFeat_MS = predict(preProc, nomalizedFeat_MS)
          normalizedDataPoolAllLev_MS = cbind( nomalizedFeat_MS[1:((sindexSVMDATA + 8*numFeat)-1)], normalizedLabelUSE)
          rm(nomalizedFeat_MS, normalizedLabelUSE)
          # **************************************** data for map visualization ****************************************
          # normalized_data_MS = predict(preProc, generalDataPool[,1:(ncol(generalDataPool)-2)])
          # normalized_data_MS = cbind( normalized_data_MS[1:((sindexSVMDATA + 8*numFeat)-1)])
          # ************************************************************************************************************
          rm(generalDataPool)
          
          #Split data in test, train and validate data Multiscale
          splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$USE)
          trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
          testDataAllLevMS = as.data.frame(splitdf[[2]])
          validateDataAllLevMS = as.data.frame(splitdf[[3]])
          rm(splitdf, normalizedDataPoolAllLev_MS)
          
          # remove use indicator in last column MS
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
          testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
          validateDataAllLevMS = validateDataAllLevMS[,1:ncol(validateDataAllLevMS)-1]
          
          # split Validate data in features and labels for MS
          validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-1)]
          validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS))]
          rm(validateDataAllLevMS)
           
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]
          #########################################################################################
        } else {
          ########################################  Input  ########################################
          sindexSVMDATA = 1   # start of baseline model with one segmentation scale data
          eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
          
          generalDataPool = cbind(generalDataPool[,37:54], generalDataPool[,(ncol(generalDataPool)-1):ncol(generalDataPool)])
          
          setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/",invariance))
          
          # import data
          generalDataPoolOrg_S09C01 = read.csv2("cologne_res_100cm_S09C01_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S07C03 = read.csv2("cologne_res_100cm_S07C03_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S05C07 = read.csv2("cologne_res_100cm_S05C07_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S05C05 = read.csv2("cologne_res_100cm_S05C05_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S05C03 = read.csv2("cologne_res_100cm_S05C03_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S03C07 = read.csv2("cologne_res_100cm_S03C07_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S03C05 = read.csv2("cologne_res_100cm_S03C05_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          generalDataPoolOrg_S01C09 = read.csv2("cologne_res_100cm_S01C09_allclass_CSV.csv" ,header = T, sep =";",colClasses=tail(columnClass,22))
          
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
          # normalized_data = predict(preProc, setNames(generalDataPool[1:18],objInfoNames[-length(objInfoNames)]))
          # ************************************************************************************************************
          # apply range of basemodell to all level
          normalizedFeatS09C01 = predict(preProc, setNames(normalizedFeat[,(numFeat+1):(2*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS07C03 = predict(preProc, setNames(normalizedFeat[,(2*numFeat+1):(3*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C07 = predict(preProc, setNames(normalizedFeat[,(3*numFeat+1):(4*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C05 = predict(preProc, setNames(normalizedFeat[,(4*numFeat+1):(5*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C03 = predict(preProc, setNames(normalizedFeat[,(5*numFeat+1):(6*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS03C07 = predict(preProc, setNames(normalizedFeat[,(6*numFeat+1):(7*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS03C05 = predict(preProc, setNames(normalizedFeat[,(7*numFeat+1):(8*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS01C09 = predict(preProc, setNames(normalizedFeat[,(8*numFeat+1):(9*numFeat)],objInfoNames[-length(objInfoNames)]))
           
          normalizedFeat = cbind(normalizedFeatBase, 
                                 normalizedFeatS09C01, normalizedFeatS07C03, normalizedFeatS05C07, normalizedFeatS05C05, 
                                 normalizedFeatS05C03, normalizedFeatS03C07, normalizedFeatS03C05, normalizedFeatS01C09
          )
          generalDataPoolfinal_shape = cbind(normalizedFeat, normalizedLabelUSE)
          
          rm(normalizedFeatBase, normalizedFeatS09C01, normalizedFeatS07C03, normalizedFeatS05C07, normalizedFeatS05C05, 
             normalizedFeatS05C03, normalizedFeatS03C07, normalizedFeatS03C05, normalizedFeatS01C09
          )
          #############################################  Splitting & Sampling  ############################################
          # Split data in train, test and validate data
          splitdf <- split(generalDataPoolfinal_shape, generalDataPoolfinal_shape$use)
          trainDataPoolAllLev = as.data.frame(splitdf[[1]])
          testDataAllLev = as.data.frame(splitdf[[2]])
          validateDataAllLev = as.data.frame(splitdf[[3]])
          rm(splitdf, generalDataPoolfinal_shape)
          
          # remove use indicator in last column
          trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
          testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
          validateDataAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-1)]
          
          # split Validate Dateset in features and labels ans subset on basislevel of first SVM
          validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
          validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
          rm(validateDataAllLev)
          
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
           
          ##################################################  MultiLevel ###################################################
          tempnames = 1: ncol(normalizedFeat)
          tempnames = as.character(tempnames)
          nomalizedFeatMS = setNames(normalizedFeat,tempnames)
          preProc = preProcess(nomalizedFeatMS, method = "range")
          nomalizedFeatMS= predict(preProc, nomalizedFeatMS)
          normalizedDataPoolAllLev_MS = cbind( nomalizedFeatMS, normalizedLabelUSE)
          rm(nomalizedFeatMS,normalizedLabelUSE)
          # **************************************** data for map visualization ****************************************
          # normalized_data_MS = cbind(generalDataPool[,1:18],
          #                 generalDataPoolOrg_S01C09[,3:20], 
          #                 generalDataPoolOrg_S03C05[,3:20],
          #                 generalDataPoolOrg_S03C07[,3:20],
          #                 generalDataPoolOrg_S05C03[,3:20],
          #                 generalDataPoolOrg_S05C05[,3:20],
          #                 generalDataPoolOrg_S05C07[,3:20],
          #                 generalDataPoolOrg_S07C03[,3:20],
          #                 generalDataPoolOrg_S09C01[,3:20])
          # normalized_data_MS = setNames(normalized_data_MS,tempnames)
          # normalized_data_MS = predict(preProc, normalized_data_MS[,1:ncol(data_MS)])
          # ************************************************************************************************************
          rm(generalDataPool, generalDataPoolOrg_S09C01, generalDataPoolOrg_S07C03,  
             generalDataPoolOrg_S05C07, generalDataPoolOrg_S05C05, generalDataPoolOrg_S05C03, generalDataPoolOrg_S03C07, 
             generalDataPoolOrg_S03C05, generalDataPoolOrg_S01C09 ) 
          
          #Split data in test and train data Multilevel
          splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$use)
          trainDataPoolAllLevMS = (as.data.frame(splitdf[[1]]))
          testDataAllLevMS = (as.data.frame(splitdf[[2]]))
          validateDataAllLevMS = (as.data.frame(splitdf[[3]]))
          rm(splitdf,normalizedDataPoolAllLev_MS)
          
          #reove use indicator in last column MS
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
          testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
          trainDataPoolAllLevMS = na.omit(trainDataPoolAllLevMS)
          testDataAllLevMS = na.omit(testDataAllLevMS)
          
          #split Validate Dateset in features and labels for MS
          validateDataAllLevMS = na.omit(validateDataAllLevMS)
          validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-2)]
          validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS)-1)]
          rm(validateDataAllLevMS)
          
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]
           
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
        lightC = 2 # lighter validate dataset for running faster prediction 
        if (invariance=="scale") {
          ########################################  Input  ########################################
          inputPath ="hagadera_all_level_scale_specgeomtex.csv"  
          
          sindexSVMDATA = 53                                      # start of baseline model with one segmentation scale data
          eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
          
          columnClass = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
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
          generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)
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
          # normalized_data = predict(preProc, setNames(generalDataPool[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
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
             normalizedFeat6, normalizedFeat7, normalizedFeat8, normalizedFeat9)
          
          #############################################  Splitting & Sampling  ############################################
          # Split data in train, test and validate data
          splitdf <- split(normalizedDataPoolAllLev, normalizedDataPoolAllLev$USE)
          trainDataPoolAllLev = as.data.frame(splitdf[[1]])
          testDataAllLev = as.data.frame(splitdf[[2]])
          validateDataAllLev = as.data.frame(splitdf[[3]])
          rm(splitdf, normalizedDataPoolAllLev)
          
          # Remove "USE" indicator in last column
          trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
          testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
          validateDataAllLev = validateDataAllLev[,1:ncol(validateDataAllLev)-1]
          
          # split Validate data in features and labels and subset on basislevel of first SVM
          validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
          validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
          rm(validateDataAllLev)
          
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
           
          ################################################## MultiScale ####################################################
          # normalize feature for MultiScale
          nomalizedFeat_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]
          
          preProc1 = preProcess(nomalizedFeat_MS[,1:50], method = "range")
          preProc2 = preProcess(nomalizedFeat_MS[,51:100], method = "range")
          preProc3 = preProcess(nomalizedFeat_MS[,101:150], method = "range")
          preProc4 = preProcess(nomalizedFeat_MS[,151:200], method = "range")
          preProc5 = preProcess(nomalizedFeat_MS[,201:208], method = "range")
           
          nomalizedFeat_MS[,1:50]= predict(preProc1, normalizedFeat[,1:50])
          nomalizedFeat_MS[,51:100]= predict(preProc2, normalizedFeat[,51:100])
          nomalizedFeat_MS[,101:150]= predict(preProc3, normalizedFeat[,101:150])
          nomalizedFeat_MS[,151:200]= predict(preProc4, normalizedFeat[,151:200])
          nomalizedFeat_MS[,201:208]= predict(preProc5, normalizedFeat[,201:208])
           
          normalizedDataPoolAllLev_MS = cbind( nomalizedFeat_MS, normalizedLabelUSE)
          rm(nomalizedFeat_MS, normalizedLabelUSE)
           
          # **************************************** data for map visualization ****************************************
          # normalized_data_MS = generalDataPool[,1:(ncol(generalDataPool)-2)]
          # normalized_data_MS[,1:50] = predict(preProc1, generalDataPool[,1:50])
          # normalized_data_MS[,51:100]= predict(preProc2, generalDataPool[,51:100])
          # normalized_data_MS[,101:150]= predict(preProc3, generalDataPool[,101:150])
          # normalized_data_MS[,151:200]= predict(preProc4, generalDataPool[,151:200])
          # normalized_data_MS[,201:208]= predict(preProc5, generalDataPool[,201:208])
          # ************************************************************************************************************
          rm(generalDataPool)
          
          #Split data in test, train and validate data Multiscale
          splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$USE)
          trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
          testDataAllLevMS = as.data.frame(splitdf[[2]])
          validateDataAllLevMS = as.data.frame(splitdf[[3]])
          rm(splitdf,normalizedDataPoolAllLev_MS)
          
          # remove use indicator in last column MS
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
          testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
          validateDataAllLevMS = validateDataAllLevMS[,1:ncol(validateDataAllLevMS)-1]
          
          # split Validate data in features and labels for MS
          validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-1)]
          validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS))]
          rm(validateDataAllLevMS)
          
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]
           
          #############################################################################################################
        } else {
          ##################################################  Input  ##################################################
          
          sindexSVMDATA = 1                                       # start of baseline model with one segmentation scale data
          eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
          
          #import format; "NULL" for subset of data on only some level (speed up import)
          columnClass = c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"factor","integer")
          columnClass2 = c(NA,NA,"factor",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"factor")
          
          setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/shape1"))
          inputPath ="base_level_complete.csv"   
          generalDataPool_scale = read.csv2(inputPath,header = T, sep =";",colClasses =columnClass)
          
          setwd(paste0(path, "tunc_oz/apply_model/", "csv_data_r_import/",city,"/",invariance))
          
          # import data
          generalDataPoolOrg_S09C01 = read.csv2("hagadera_s25_S09C01_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S07C03 = read.csv2("hagadera_s25_S07C03_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C07 = read.csv2("hagadera_s25_S05C07_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C05 = read.csv2("hagadera_s25_S05C05_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S05C03 = read.csv2("hagadera_s25_S05C03_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S03C07 = read.csv2("hagadera_s25_S03C07_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S03C05 = read.csv2("hagadera_s25_S03C05_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          generalDataPoolOrg_S01C09 = read.csv2("hagadera_s25_S01C09_allclass_use.csv" ,header = T, sep =";",colClasses =columnClass2)#, nrows = 2000000)
          
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
          nomalizedFeatMS = normalizedFeat
          
          #normalization of  data ("range" scales the data to the interval [0, 1]; c("center", "scale") centers and scales the input data)
          preProc = preProcess(setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]), method = "range")
           
          # **************************************** data for map visualization ****************************************
          # normalized_data = predict(preProc, setNames(generalDataPool_scale[,sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
          # ************************************************************************************************************
          
          normalizedFeatBase = predict(preProc, setNames(normalizedFeat[sindexSVMDATA:eindexSVMDATA],objInfoNames[-length(objInfoNames)]))
          
          normalizedFeatS09C01 = predict(preProc, setNames(normalizedFeat[,(numFeat+1):(2*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS07C03 = predict(preProc, setNames(normalizedFeat[,(2*numFeat+1):(3*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C07 = predict(preProc, setNames(normalizedFeat[,(3*numFeat+1):(4*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C05 = predict(preProc, setNames(normalizedFeat[,(4*numFeat+1):(5*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS05C03 = predict(preProc, setNames(normalizedFeat[,(5*numFeat+1):(6*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS03C07 = predict(preProc, setNames(normalizedFeat[,(6*numFeat+1):(7*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS03C05 = predict(preProc, setNames(normalizedFeat[,(7*numFeat+1):(8*numFeat)],objInfoNames[-length(objInfoNames)]))
          normalizedFeatS01C09 = predict(preProc, setNames(normalizedFeat[,(8*numFeat+1):(9*numFeat)],objInfoNames[-length(objInfoNames)]))
          
          normalizedFeat = cbind(normalizedFeatBase, 
                                 normalizedFeatS09C01, normalizedFeatS07C03, normalizedFeatS05C07, normalizedFeatS05C05, 
                                 normalizedFeatS05C03, normalizedFeatS03C07, normalizedFeatS03C05, normalizedFeatS01C09
          )
          rm(normalizedFeatBase, normalizedFeatS09C01, normalizedFeatS07C03, normalizedFeatS05C07, normalizedFeatS05C05, 
             normalizedFeatS05C03, normalizedFeatS03C07, normalizedFeatS03C05, normalizedFeatS01C09)
           
          generalDataPoolfinal_shape = cbind(normalizedFeat, normalizedLabelUSE)
          
          ############################################# Splitting & Sampling #############################################
          # Split data in test, train and validate data
          splitdf <- split(generalDataPoolfinal_shape, generalDataPoolfinal_shape$use)
          trainDataPoolAllLev = as.data.frame(splitdf[[1]])
          testDataAllLev = as.data.frame(splitdf[[2]])
          validateDataAllLev = as.data.frame(splitdf[[3]])
          rm(splitdf, generalDataPoolfinal_shape)
           
          # remove use indicator in last column
          trainDataPoolAllLev = trainDataPoolAllLev[,1:ncol(trainDataPoolAllLev)-1]
          testDataAllLev = testDataAllLev[,1:ncol(testDataAllLev)-1]
          validateDataAllLev = validateDataAllLev[,1:(ncol(validateDataAllLev)-1)]
          
          #split Validate Dataset in features and labels ans subset on basislevel of first SVM
          validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
          validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
          rm(validateDataAllLev)
           
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
          ################################################## MultiScale ####################################################
          
          preProc1 = preProcess(nomalizedFeatMS[,1:50], method = "range")
          preProc2 = preProcess(nomalizedFeatMS[,51:100], method = "range")
          preProc3 = preProcess(nomalizedFeatMS[,101:150], method = "range")
          preProc4 = preProcess(nomalizedFeatMS[,151:200], method = "range")
          preProc5 = preProcess(nomalizedFeatMS[,201:234], method = "range")
           
          nomalizedFeatMS[,1:50]= predict(preProc1, normalizedFeat[,1:50])
          nomalizedFeatMS[,51:100]= predict(preProc2, normalizedFeat[,51:100])
          nomalizedFeatMS[,101:150]= predict(preProc3, normalizedFeat[,101:150])
          nomalizedFeatMS[,151:200]= predict(preProc4, normalizedFeat[,151:200])
          nomalizedFeatMS[,201:234]= predict(preProc5, normalizedFeat[,201:234])
          normalizedDataPoolAllLev_MS = cbind(nomalizedFeatMS, normalizedLabelUSE)
          rm(nomalizedFeatMS, normalizedLabelUSE
          )
           
          # **************************************** data for map visualization ****************************************
          # data_MS = cbind(generalDataPool_scale[,1:26],
          #                 generalDataPoolOrg_S01C09[,3:28], 
          #                 generalDataPoolOrg_S03C05[,3:28],
          #                 generalDataPoolOrg_S03C07[,3:28],
          #                 generalDataPoolOrg_S05C03[,3:28],
          #                 generalDataPoolOrg_S05C05[,3:28],
          #                 generalDataPoolOrg_S05C07[,3:28],
          #                 generalDataPoolOrg_S07C03[,3:28],
          #                 generalDataPoolOrg_S09C01[,3:28])
          # data_MS = setNames(data_MS,colnames(normalizedFeat))
          # normalized_data_MS = data_MS
          # normalized_data_MS[,1:50]= predict(preProc1, data_MS[,1:50])
          # normalized_data_MS[,51:100]= predict(preProc2, data_MS[,51:100])
          # normalized_data_MS[,101:150]= predict(preProc3, data_MS[,101:150])
          # normalized_data_MS[,151:200]= predict(preProc4, data_MS[,151:200])
          # normalized_data_MS[,201:234]= predict(preProc5, data_MS[,201:234])
          # rm(data_MS)
          # ************************************************************************************************************
          rm(generalDataPool_scale, generalDataPoolOrg_S09C01, generalDataPoolOrg_S07C03,
             generalDataPoolOrg_S05C07, generalDataPoolOrg_S05C05, generalDataPoolOrg_S05C03,
             generalDataPoolOrg_S03C07, generalDataPoolOrg_S03C05, generalDataPoolOrg_S01C09
          )
           
          # Split data in test and train data Multilevel
          splitdf <- split(normalizedDataPoolAllLev_MS, normalizedDataPoolAllLev_MS$use)
          trainDataPoolAllLevMS = as.data.frame(splitdf[[1]])
          testDataAllLevMS = as.data.frame(splitdf[[2]])
          validateDataAllLevMS = as.data.frame(splitdf[[3]])
          rm(splitdf, normalizedDataPoolAllLev_MS)
           
          # remove "USE" indicator in last column MS
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[,1:ncol(trainDataPoolAllLevMS)-1]
          testDataAllLevMS = testDataAllLevMS[,1:ncol(testDataAllLevMS)-1]
          trainDataPoolAllLevMS = na.omit(trainDataPoolAllLevMS)
          testDataAllLevMS = na.omit(testDataAllLevMS)
          
          # split Validate Dateset in features and labels for MS
          validateDataAllLevMS = na.omit(validateDataAllLevMS)
          validateFeatAllLevMS = validateDataAllLevMS[,1:(ncol(validateDataAllLevMS)-2)]
          validateLabelsMS = validateDataAllLevMS[,(ncol(validateDataAllLevMS)-1)]
          rm(validateDataAllLevMS)
          
          trainDataPoolAllLevMS = trainDataPoolAllLevMS[order(trainDataPoolAllLevMS[,ncol(trainDataPoolAllLevMS)]),]
           
          ##################################################################################################################
          lightC = 4 # lighter validate dataset for running faster prediction
        }
        if (model_prob=="multiclass") { 
          lightS=round(as.numeric(c(table(validateLabels)[1],table(validateLabels)[2],table(validateLabels)[5],table(validateLabels)[4],table(validateLabels)[3]))/lightC)
          validateData = cbind(validateFeatsub,validateLabels)
          val_stratSamp = strata(validateData, c("validateLabels"), size = lightS, method = "srswor")
          validateData = getdata(validateData, val_stratSamp)
          validateFeatsub = validateData[,1:ncol(validateFeatsub)]
          validateLabels = validateData[,ncol(validateFeatsub)+1]
          rm(validateData, val_stratSamp)
          validateData_MS = cbind(validateFeatAllLevMS,validateLabelsMS)
          val_stratSamp_MS = strata(validateData_MS, c("validateLabelsMS"), size = lightS, method = "srswor")
          validateData_MS = getdata(validateData_MS, val_stratSamp_MS)
          validateFeatAllLevMS = validateData_MS[,1:ncol(validateFeatAllLevMS)]
          validateLabelsMS = validateData_MS[,ncol(validateFeatAllLevMS)+1]
          rm(validateData_MS,val_stratSamp_MS) }
      }

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
      
      AccuracyVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_it) = colheader
      
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
      
      KappaVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_it) = colheader
      
      # ********
      best_bound_oa_SL = c()
      best_boundMargin_oa_SL = c()
      best_bound_oa_SL_Un = c()
      best_boundMargin_oa_SL_Un = c()
      best_bound_oa_SL_vUn = c()
      best_boundMargin_oa_SL_vUn = c()
      best_resample_oa=c()
      best_newSize_oa=c()
      best_classSize_oa=c()
      best_cluster_oa=c()
      best_model_oa=c()
      time.taken_iter = c()
      best_resample=NA
      best_newSize4iter = NA
      best_classSize=NA
      best_cluster=NA
      ########################################  Training  ########################################
      
      # set randomized seed for the random sampling procedure
      seed = 20 # 5, 73, 20 
      
      for (realization in seq(nR)) {#}
        start.time <- Sys.time()
        # initial seed value for randomized sampling
        if (train) {seed = seed + sample(100, 1)}
        cat("CPU cores: ",num_cores," | seed: ",seed,"\n",sep="")
        
        trainDataCurBeg = trainDataPoolAllLev
        testDataCurBeg = testDataAllLev
        # subset for each outer iteration test data to speed up computing
        testDataCurBeg = testDataCurBeg[order(testDataCurBeg[,ncol(testDataCurBeg)]),]
        
        ######  MultiScale
        trainDataCurBegMS = trainDataPoolAllLevMS
        testDataCurBegMS = testDataAllLevMS
        # subset for each outer iteration test data to speed up computing
        testDataCurBegMS = testDataCurBegMS[order(testDataCurBegMS[,ncol(testDataCurBegMS)]),]
        
        for (sample_size in seq(along=sampleSizePor)) {#}
          
          cat(city," ",model_prob ," ",invariance," | realization [",realization,"/",nR,"] | labeled samples: ",sampleSizePor[sample_size]*2," [",sample_size,"/",length(sampleSizePor),"]","\n",sep="")
          
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
          
          trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
          trainDataCurRemaining <- trainDataCurBeg[-c(samplesID), ]
          
          trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
          trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]
          
          stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
          
          samples = getdata(testDataCurBeg, stratSamp)
          
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
          
          setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
          
          cat("training SVM\n")
          model_name_tunedSVM = paste0(format(Sys.time(),"%Y%m%d"),"SVM_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl",".rds")
          if (file.exists(model_name_tunedSVM) && !train) {
            tunedSVM <- readRDS(model_name_tunedSVM)
            cat("Luckily, model already exists!")
          } else { trainStart.time <- Sys.time()
          tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
          train.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 3)
          }
          # run classification and accuracy assessment for unmodified SV and predict labels of test data
          predLabelsSVM = predict(tunedSVM, validateFeatsub)
          accSVM = confusionMatrix(predLabelsSVM, validateLabels)
          cat("SVM accuracy: ",round(accSVM$overall["Accuracy"],5)," | training time: ",train.time,"sec","\n",sep="")
      
          AccuracySVM[realization,sample_size] = as.numeric(accSVM$overall["Accuracy"])
          KappaSVM[realization,sample_size] = as.numeric(accSVM$overall["Kappa"])
          best_acc <- accSVM$overall["Accuracy"]
          new_bestTunedVSVM <- tunedSVM
          new_bestTrainFeatVSVM <- trainFeat 
          new_bestTrainLabelsVSVM <- trainLabels 
          best_model <- model_name_tunedSVM
          
          # get original SVs of base SVM *************************
          SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
          SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
          # ******************************************************
          binaryClassProblem = list()
          for (jj in 1:length(tunedSVM$finalModel@xmatrix)) { # COMPARE EVERY COUPLE COMBINATION OF CLASSES
            binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
          }
          # ******************************************************
          ################################################ SVM MS  #############################################
          model_name_tunedSVM_MS = paste0(format(Sys.time(),"%Y%m%d"),"SVM_MS_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl",".rds")
          if (file.exists(model_name_tunedSVM_MS) && !train) {
            tunedSVM_MS <- readRDS(model_name_tunedSVM_MS)
            cat("Luckily, model already exists!")
          } else {
            stratSamp = strata(trainDataCurBegMS, c("REF"), size = shares, method = "srswor")
            samples = getdata(trainDataCurBegMS, stratSamp)
            
            trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
            trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
            trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]
            
            stratSamp = strata(testDataCurBegMS, c("REF"), size = shares, method = "srswor")
            samples = getdata(testDataCurBegMS, stratSamp)
            
            testDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
            # split test feat from test label for later join with trainData MS
            testFeatMS = testDataCurMS[,1:(ncol(testDataCurMS)-1)]
            testLabelsMS = testDataCurMS[,ncol(testDataCurMS)]
            
            countTrainDataMS = nrow(trainFeatMS)
            indexTrainDataMS = list(c(1:countTrainDataMS))
            
            #join train and test test data (through indexTrainData in svmFit separable)
            tuneFeat_MS = rbind(trainFeatMS, testFeatMS)
            tuneLabel_MS = unlist(list(trainLabelsMS, testLabelsMS))
            
            cat("training SVM multilevel\n")
            trainStart.time <- Sys.time()
            tunedSVM_MS = svmFit(tuneFeat_MS, tuneLabel_MS, indexTrainDataMS)
            train.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 3)
          }
          # run classification and accuracy assessment for unmodified SV and predict labels of test data
          predLabelsSVMmultiScale = predict(tunedSVM_MS, validateFeatAllLevMS)
          accSVM_M = confusionMatrix(predLabelsSVMmultiScale, validateLabelsMS)
          cat("SVM_M accuracy: ",round(accSVM_M$overall["Accuracy"],5)," | training time: ",train.time,"sec","\n",sep="")
          
          AccuracySVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Accuracy"])
          KappaSVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Kappa"])
          ####################################### SVM-SL + semi-labeled samples #####################################
          
          # Definition of sampling configuration (strata:random sampling without replacement)
          stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
          
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
          
          cat("evaluation of SVM with self learning and semi-labeled samples\n")
          model_name_SVMUn_b = paste0(format(Sys.time(),"%Y%m%d"),"SVM_SLUn_b_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl",".rds")
          if (invariance=="scale") {
            if (city=="cologne") {# get VSs, means rows of SV but with subset on different level
              SVL_variables = list( 
                list(SVtotalSVMUn_b, SVL2SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL3SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL5SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL6SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL7SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL8SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL9SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL10SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL11SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFSVM_b))
              )
            } else {
              SVL_variables = list( 
                list(SVtotalSVMUn_b, SVL2SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL3SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL5SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL6SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL7SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL8SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM_b)),
                list(SVtotalSVMUn_b, SVL9SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM_b))
              )
            }
          } else {
            SVL_variables = list(
              list(SVtotalSVMUn_b, S01C09SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c((numFeat+1):(2*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S03C05SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((2*numFeat)+1):(3*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S03C07SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((3*numFeat)+1):(4*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S05C03SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((4*numFeat)+1):(5*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S05C05SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((5*numFeat)+1):(6*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S05C07SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((6*numFeat)+1):(7*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S07C03SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((7*numFeat)+1):(8*numFeat))], REFSVM_b)),
              list(SVtotalSVMUn_b, S09C01SVMUn_b = cbind(trainDataCurRemainingSVM_b[SVindexSVMUn_b,c(((8*numFeat)+1):(9*numFeat))], REFSVM_b))
            )
          }
          SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_SVMUn_b, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                 SVL_variables)
          bestFittingModelSVMUn_b <- SLresult$bestFittingModel
          best_trainFeatSVMUn_b <- SLresult$best_trainFeatVSVM
          best_trainLabelsSVMUn_b <- SLresult$best_trainLabelsVSVM
          best_boundSVM_SL_Un = SLresult$best_bound
          best_boundMarginSVM_SL_Un = SLresult$best_boundMargin
          # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
          predLabelsSVMsumUn_b = predict(bestFittingModelSVMUn_b, validateFeatsub)
          accSVM_SL_Un_b = confusionMatrix(predLabelsSVMsumUn_b, validateLabels)
          cat("SVM_SL_Un accuracy: ",round(accSVM_SL_Un_b$overall["Accuracy"],5),"\n",sep="")
          
          AccuracySVM_SL_Un_b[realization,sample_size] = as.numeric(accSVM_SL_Un_b$overall["Accuracy"])
          KappaSVM_SL_Un_b[realization,sample_size] = as.numeric(accSVM_SL_Un_b$overall["Kappa"])
          if (accSVM_SL_Un_b$overall["Accuracy"]>best_acc) {
            best_acc <- accSVM_SL_Un_b$overall["Accuracy"]
            new_bestTunedSVM <- bestFittingModelSVMUn_b
            new_bestTrainFeatSVM <- best_trainFeatSVMUn_b
            new_bestTrainLabelsSVM <- best_trainLabelsSVMUn_b
            best_model <- model_name_SVMUn_b
          }
          ################################################# VSVM ################################################# 
          cat("training VSVM\n")
          if (invariance=="scale") {
            if (city=="cologne") { # get VSs, means rows of SV but with subset on different level
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
              ) # The new Train Data Set is made by SVs and VSVs only
            } else { 
              SVL2 = trainDataCur[SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
              SVL3 = trainDataCur[SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]
              
              SVL5 = trainDataCur[SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
              SVL6 = trainDataCur[SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
              SVL7 = trainDataCur[SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
              SVL8 = trainDataCur[SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
              SVL9 = trainDataCur[SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
              
              SVinvar = rbind(setNames(SVtotal,objInfoNames),
                              setNames(SVL2,objInfoNames),
                              setNames(SVL3,objInfoNames),
                              setNames(SVL5,objInfoNames), 
                              setNames(SVL6,objInfoNames),
                              setNames(SVL7,objInfoNames),
                              setNames(SVL8,objInfoNames),
                              setNames(SVL9,objInfoNames)
              ) 
            }  
          } else {
            S01C09 = trainDataCur[SVindex,c((numFeat+1):(2*numFeat),ncol(trainDataCur))]
            S03C05 = trainDataCur[SVindex,c(((2*numFeat)+1):(3*numFeat),ncol(trainDataCur))]
            S03C07 = trainDataCur[SVindex,c(((3*numFeat)+1):(4*numFeat),ncol(trainDataCur))]
            S05C03 = trainDataCur[SVindex,c(((4*numFeat)+1):(5*numFeat),ncol(trainDataCur))]
            S05C05 = trainDataCur[SVindex,c(((5*numFeat)+1):(6*numFeat),ncol(trainDataCur))]
            S05C07 = trainDataCur[SVindex,c(((6*numFeat)+1):(7*numFeat),ncol(trainDataCur))]
            S07C03 = trainDataCur[SVindex,c(((7*numFeat)+1):(8*numFeat),ncol(trainDataCur))]
            S09C01 = trainDataCur[SVindex,c(((8*numFeat)+1):(9*numFeat),ncol(trainDataCur))]
            
            SVinvar = rbind(setNames(SVtotal,objInfoNames),
                            setNames(S01C09,objInfoNames),
                            setNames(S03C05,objInfoNames),
                            setNames(S03C07,objInfoNames),
                            setNames(S05C03,objInfoNames),
                            setNames(S05C05,objInfoNames),
                            setNames(S05C07,objInfoNames),
                            setNames(S07C03,objInfoNames),
                            setNames(S09C01,objInfoNames)
            )
          }
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
          
          model_name_tunedVSVM = paste0(format(Sys.time(),"%Y%m%d"),"VSVM_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl",".rds")
          if (file.exists(model_name_tunedVSVM) && !train) {
            tunedVSVM <- readRDS(model_name_tunedVSVM)
            cat("Luckily, model already exists!")
          } else {trainStart.time <- Sys.time()
          tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
          train.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 3)
          }
          # predict labels of test data i.e. run classification and accuracy assessment for modified SV
          predLabelsVSVM = predict(tunedVSVM, validateFeatsub)
          accVSVM = confusionMatrix(predLabelsVSVM, validateLabels)
          cat("VSVM accuracy: ",round(accVSVM$overall["Accuracy"],5)," | training time: ",train.time,"sec","\n",sep="")
          
          AccuracyVSVM[realization,sample_size] = as.numeric(accVSVM$overall["Accuracy"])
          KappaVSVM[realization,sample_size] = as.numeric(accVSVM$overall["Kappa"])
          if (accVSVM$overall["Accuracy"]>best_acc) {
            best_acc <- accVSVM$overall["Accuracy"]
            new_bestTunedVSVM <- tunedVSVM
            new_bestTrainFeatVSVM <- trainFeatVSVM
            new_bestTrainLabelsVSVM <- trainLabelsVSVM
            best_model <- model_name_tunedVSVM
          } 
          ################################################ VSVM-SL ################################################
          cat("evaluation of VSVM with self learning\n")
          model_name_VSVM_SL = paste0(format(Sys.time(),"%Y%m%d"),"VSVM_SL_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl",".rds")
          if (invariance=="scale") {
            if (city=="cologne") { 
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
            } else {
              SVL_variables = list(
                list(SVtotal, SVL2),
                list(SVtotal, SVL3),
                list(SVtotal, SVL5),
                list(SVtotal, SVL6),
                list(SVtotal, SVL7),
                list(SVtotal, SVL8),
                list(SVtotal, SVL9)
              )
            }
          } else {
            SVL_variables = list(
              list(SVtotal, S01C09),
              list(SVtotal, S03C05),
              list(SVtotal, S03C07),
              list(SVtotal, S05C03),
              list(SVtotal, S05C05),
              list(SVtotal, S05C07),
              list(SVtotal, S07C03),
              list(SVtotal, S09C01)
            )
          }
          SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_VSVM_SL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                 SVL_variables)
          bestFittingModel <- SLresult$bestFittingModel
          best_trainFeatVSVM <- SLresult$best_trainFeatVSVM
          best_trainLabelsVSVM <- SLresult$best_trainLabelsVSVM
          best_bound_SL = SLresult$best_bound
          best_boundMargin_SL = SLresult$best_boundMargin
          # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
          predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
          accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
          cat("VSVM_SL accuracy: ",round(accVSVM_SL$overall["Accuracy"],5),"\n",sep="")
          
          KappaVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Kappa"])
          AccuracyVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Accuracy"])
          if (accVSVM_SL$overall["Accuracy"]>best_acc) {
            best_acc <- accVSVM_SL$overall["Accuracy"]
            new_bestTunedVSVM <- bestFittingModel
            new_bestTrainFeatVSVM <- best_trainFeatVSVM
            new_bestTrainLabelsVSVM <- best_trainLabelsVSVM
            best_model <- model_name_tunedVSVM
          }
          ################################### VSVM-SL + semi-labeled samples #####################################
          actAcc_vUn = -1e-6 # actAcc_Un = = -1e-6
          for (bb in seq(along=b)) {
            # Definition of sampling configuration (strata:random sampling without replacement)
            stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
            
            # get samples of trainDataCurRemaining and set trainDataCurRemaining new
            samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
            
            trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]
            
            trainDataCurRemaining_b = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
            trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]
            
            REF_b = predict(tunedVSVM, trainDataCurRemainingsub_b)
            
            # get SV of unlabeled samples
            indexUn_b = 1:nrow(trainDataCurRemainingsub_b) 
            totalUn_b = trainDataCurRemaining_b[indexUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
            totalUn_b = cbind(totalUn_b, REF_b)
            
            cat("evaluation of VSVM SL with ",b[bb]," semi-labeled samples [",bb,"/",length(b),"]","\n",sep="")
            model_name_Un_b = paste0(format(Sys.time(),"%Y%m%d"),"VSVM_SLUn_b_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b[bb],"Unl",".rds")
            if (invariance=="scale") {
              if (city=="cologne") { # get VSs, means rows of SV but with subset on different level
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
                  list(totalUn_b, L2Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)),
                  list(totalUn_b, L3Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)),
                  list(totalUn_b, L5Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)),
                  list(totalUn_b, L6Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)),
                  list(totalUn_b, L7Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)),
                  list(totalUn_b, L8Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)),
                  list(totalUn_b, L9Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)),
                  list(totalUn_b, L10Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)),
                  list(totalUn_b, L11Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b))
                )
              } else {
                SVL_variables = list(
                  list(SVtotal, SVL2),
                  list(SVtotal, SVL3),
                  list(SVtotal, SVL5),
                  list(SVtotal, SVL6),
                  list(SVtotal, SVL7),
                  list(SVtotal, SVL8),
                  list(SVtotal, SVL9),
                  list(totalUn_b, L2Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)),
                  list(totalUn_b, L3Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)),
                  list(totalUn_b, L5Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)),
                  list(totalUn_b, L6Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)),
                  list(totalUn_b, L7Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)),
                  list(totalUn_b, L8Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)),
                  list(totalUn_b, L9Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b))
                )
              }
            } else {
              SVL_variables=list(
                list(SVtotal, S09C01),
                list(SVtotal, S07C03),
                list(SVtotal, S05C07),
                list(SVtotal, S05C05),
                list(SVtotal, S05C03),
                list(SVtotal, S03C07),
                list(SVtotal, S03C05),
                list(SVtotal, S01C09),
                list(totalUn_b, S01C09Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c((numFeat+1):(2*numFeat))], REF_b)),
                list(totalUn_b, S03C05Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((2*numFeat)+1):(3*numFeat))], REF_b)),
                list(totalUn_b, S03C07Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((3*numFeat)+1):(4*numFeat))], REF_b)),
                list(totalUn_b, S05C03Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((4*numFeat)+1):(5*numFeat))], REF_b)),
                list(totalUn_b, S05C05Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((5*numFeat)+1):(6*numFeat))], REF_b)),
                list(totalUn_b, S05C07Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((6*numFeat)+1):(7*numFeat))], REF_b)),
                list(totalUn_b, S07C03Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((7*numFeat)+1):(8*numFeat))], REF_b)),
                list(totalUn_b, S09C01Un_b = cbind(trainDataCurRemaining_b[indexUn_b,c(((8*numFeat)+1):(9*numFeat))], REF_b))
              )
            }
            SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_Un_b, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                   SVL_variables)
            bestFittingModelUn_b <- SLresult$bestFittingModel
            best_trainFeatVSVMUn_b <- SLresult$best_trainFeatVSVM
            best_trainLabelsVSVMUn_b <- SLresult$best_trainLabelsVSVM
            best_bound_SL_Un = SLresult$best_bound
            best_boundMargin_SL_Un = SLresult$best_boundMargin
            # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
            predLabelsVSVMsumUn_b = predict(bestFittingModelUn_b, validateFeatsub)
            accVSVM_SL_Un_b = confusionMatrix(predLabelsVSVMsumUn_b, validateLabels)
            cat("VSVM_SL_Un accuracy: ",round(accVSVM_SL_Un_b$overall["Accuracy"],5),"\n",sep="")
            
            AccuracyVSVM_SL_Un_b[realization,sample_size] = as.numeric(accVSVM_SL_Un_b$overall["Accuracy"])
            KappaVSVM_SL_Un_b[realization,sample_size] = as.numeric(accVSVM_SL_Un_b$overall["Kappa"])
            if (accVSVM_SL_Un_b$overall["Accuracy"] > best_acc) {
              best_acc <- accVSVM_SL_Un_b$overall["Accuracy"]
              new_bestTunedVSVM <- bestFittingModelUn_b
              new_bestTrainFeatVSVM <- best_trainFeatVSVMUn_b
              new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn_b
              best_model <- model_name_Un_b
            }
            ################################ VSVM-SL + Virtual semi-labeled Samples ##################################
            
            REF_v = predict(bestFittingModelUn_b, trainDataCurRemainingsub_b)
            
            # get SV of unlabeled samples
            indexvUn_v = 1:nrow(trainDataCurRemainingsub_b) #  bestFittingModelUn_b$finalModel@SVindex 
            SVtotalvUn_v = trainDataCurRemaining_b[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA)] #na.omit(trainDataCurRemaining_b[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_b))])
            SVtotalvUn_v = cbind(SVtotalvUn_v, REF_v)
            
            # extracting previously assigned reference column
            SVtotalvUn_vFeat = SVtotalvUn_v[,1:(ncol(SVtotalvUn_v)-1)] # -1)]
            REF_v = SVtotalvUn_v[,(ncol(SVtotalvUn_v))]
            SVtotalvUn_v = cbind(SVtotalvUn_vFeat, REF_v)
            
            cat("evaluation of VSVM SL with ",b[bb]," virtual semi-labeled samples [",bb,"/",length(b),"]","\n",sep="")
            model_name_vUn_b = paste0(format(Sys.time(),"%Y%m%d"),"VSVM_SLvUn_b_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b[bb],"Unl",".rds")
            if (invariance=="scale") {
              if (city=="cologne") { # get VSs, means rows of SV but with subset on different level
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
                  list(SVtotalvUn_v, SVL2vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)),
                  list(SVtotalvUn_v, SVL3vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)),
                  list(SVtotalvUn_v, SVL5vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL6vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL7vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL8vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL9vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL10vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL11vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v))
                )
              } else { 
                SVL_variables = list(
                  list(SVtotal, SVL2),
                  list(SVtotal, SVL3),
                  list(SVtotal, SVL5),
                  list(SVtotal, SVL6),
                  list(SVtotal, SVL7),
                  list(SVtotal, SVL8),
                  list(SVtotal, SVL9),
                  list(SVtotalvUn_v, SVL2vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)),
                  list(SVtotalvUn_v, SVL3vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)),
                  list(SVtotalvUn_v, SVL5vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL6vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL7vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL8vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL9vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v))
                )
              }
            } else {
              SVL_variables=list(
                list(SVtotal, S09C01),
                list(SVtotal, S07C03),
                list(SVtotal, S05C07),
                list(SVtotal, S05C05),
                list(SVtotal, S05C03),
                list(SVtotal, S03C07),
                list(SVtotal, S03C05),
                list(SVtotal, S01C09),
                list(SVtotalvUn_v, S01C09vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c((numFeat+1):(2*numFeat))], REF_v)),
                list(SVtotalvUn_v, S03C05vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((2*numFeat)+1):(3*numFeat))], REF_v)),
                list(SVtotalvUn_v, S03C07vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((3*numFeat)+1):(4*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C03vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((4*numFeat)+1):(5*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C05vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((5*numFeat)+1):(6*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C07vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((6*numFeat)+1):(7*numFeat))], REF_v)),
                list(SVtotalvUn_v, S07C03vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((7*numFeat)+1):(8*numFeat))], REF_v)),
                list(SVtotalvUn_v, S09C01vUn_b = cbind(trainDataCurRemaining_b[indexvUn_v,c(((8*numFeat)+1):(9*numFeat))], REF_v))
              )
            }
            SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_vUn_b, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                   SVL_variables)
            new_bestFittingModelvUn_b <- SLresult$bestFittingModel
            new_best_trainFeatVSVMvUn_b <- SLresult$best_trainFeatVSVM
            new_best_trainLabelsVSVMvUn_b <- SLresult$best_trainLabelsVSVM
            new_best_bound_SLvUn_b = SLresult$best_bound
            new_best_boundMargin_SLvUn_b = SLresult$best_boundMargin
            # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
            new_predLabelsVSVMvUn_bsum = predict(new_bestFittingModelvUn_b, validateFeatsub)
            new_accVSVM_SL_vUn_b = confusionMatrix(new_predLabelsVSVMvUn_bsum, validateLabels)
            
            if (new_accVSVM_SL_vUn_b$overall["Accuracy"] > actAcc_vUn) {print
              actAcc_vUn <- new_accVSVM_SL_vUn_b$overall["Accuracy"]
              bestFittingModelvUn_b <- new_bestFittingModelvUn_b
              accVSVM_SL_vUn_b <- new_accVSVM_SL_vUn_b
              best_trainFeatVSVMvUn_b <- new_best_trainFeatVSVMvUn_b
              best_trainLabelsVSVMvUn_b <- new_best_trainLabelsVSVMvUn_b
              best_bound_SLvUn_b = new_best_bound_SLvUn_b
              best_boundMargin_SLvUn_b = new_best_boundMargin_SLvUn_b
            }
          }
          cat("VSVM_SL_vUn accuracy: ",round(accVSVM_SL_vUn_b$overall["Accuracy"],5),"\n",sep="")
          
          AccuracyVSVM_SL_vUn_b[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b$overall["Accuracy"])
          KappaVSVM_SL_vUn_b[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b$overall["Kappa"])
          if (accVSVM_SL_vUn_b$overall["Accuracy"] > best_acc){
            best_acc <- accVSVM_SL_vUn_b$overall["Accuracy"]
            new_bestTunedVSVM <- bestFittingModelvUn_b
            new_bestTrainFeatVSVM <- best_trainFeatVSVMvUn_b
            new_bestTrainLabelsVSVM <- best_trainLabelsVSVMvUn_b
            best_model <- model_name_vUn_b
          }
          ###################################### AL_VSVM+SL #######################################
          
          model_name_AL_VSVMSL = paste0(format(Sys.time(),"%Y%m%d"),"AL_VSVM+SL_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl_",seed,"seed.rds")
          if (num_cores>=4) {
            cat("computing uncertainty distance for active learning procedure [",realization,"/",nR,"] | ",sampleSizePor[sample_size]*2," [",sample_size,"/",length(sampleSizePor),"]","\n",sep="")
            actAcc = -1e-6
            classSize=c(min(150*b,round(as.numeric(min(table(trainDataCurRemaining$REF)))/1)))
            if (model_prob=="multiclass") {classSize=round(classSize/3)}
            for (clS in 1:length(classSize)) {
              stratSampSize = c(classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS])
              # Definition of sampling configuration (strata:random sampling without replacement)
              stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
              # Get new samples from trainDataCurRemaining
              samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
              # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining$ID_unit), ]
              for (nS4it in 1:length(newSizes)) {
                for (cS in 1:length(clusterSizes)) {
                  for (rS in 1:length(resampledSize)) {
                    cat("tot samples: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | per iter: ",newSizes[nS4it]," [",nS4it,"/",length(newSizes),"] | pool size: ",classSize[clS]," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]","\n",sep="")
                    
                    upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
                    upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
                    upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
                    
                    # new_trainFeatVSVM <- setNames(trainFeat, names)
                    # new_trainLabelsVSVM <- trainLabels
                    # tmp_new_tunedSVM <- tunedSVM
                    new_trainFeatVSVM <- setNames(best_trainFeatVSVM, names)
                    new_trainLabelsVSVM <- best_trainLabelsVSVM
                    tmp_new_tunedSVM <- bestFittingModel
                    
                    newSize_for_iter = newSizes[nS4it] #sampleSize/10 # or just 4
                    num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
                    
                    trainStart.time <- Sys.time()
                    for (iter in 1:num_iters) {
                      cat("iteration [",iter,"/",num_iters,"]","\n",sep="")
                      predLabelsVSVM = predict(tmp_new_tunedSVM, upd_dataCurFeatsub)
                      # Add predicted labels to the features data set
                      predLabelsVSVM_unc = cbind(upd_dataCurFeatsub, predLabelsVSVM)
                      predLabelsVSVM_unc = setNames(predLabelsVSVM_unc, objInfoNames)
                      # print(paste0("computing distances"))
                      if (model_prob=="binary") { sampled_data <- margin_sampling(tmp_new_tunedSVM, predLabelsVSVM_unc, pred_one, binaryClassProblem)
                      } else {                    sampled_data <- mclu_sampling(tmp_new_tunedSVM, predLabelsVSVM_unc, pred_all, binaryClassProblem) }
                      # print(paste0("labeling new samples"))
                      # Get new labels and updated datasets
                      result <- add_new_samples_AL(sampled_data,
                                                   upd_dataCurLabels, upd_dataCurFeatsub, upd_dataCur$ID_unit,
                                                   new_trainFeatVSVM, new_trainLabelsVSVM,
                                                   newSize_for_iter,
                                                   clusterSizes[cS] ) # always greater than newSize_for_iter, # 60, 80, 100, 120
                      # Extract new datasets
                      upd_dataCurFeatsub <- result$features
                      upd_dataCurLabels <- result$labels
                      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit
                      
                      new_trainFeat <- result$new_trainFeatVSVM
                      new_trainLabels <- result$new_trainLabelsVSVM

                      # **********************
                      # get original SVs of base SVM
                      SVindex_ud = tmp_new_tunedSVM$finalModel@SVindex
                      new_trainFeatVSVM = new_trainFeatVSVM[SVindex_ud,]
                      new_trainLabelsVSVM = new_trainLabelsVSVM[SVindex_ud]
                      
                      new_trainFeatVSVM <- rbind(new_trainFeatVSVM, setNames(new_trainFeat, names))
                      new_trainLabelsVSVM <- unlist(list(new_trainLabelsVSVM, new_trainLabels))
                      
                      SVtotal = setNames(cbind(new_trainFeatVSVM, new_trainLabelsVSVM),c(objInfoNames[-length(objInfoNames)],"REF"))
                      # **********************
                      
                      # REF_ud = predict(tmp_new_tunedSVM, new_trainFeat)
                      REF_ud = new_trainLabels
                      SVtotal_ud = cbind(new_trainFeat, REF_ud)
                      
                      if (invariance=="scale") {
                        if (city=="cologne") {
                          SVL_variables = list(
                            list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
                            list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
                            list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL10=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL11=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))],REF_ud))
                          )
                        } else {
                          SVL_variables = list(
                            list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
                            list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
                            list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
                            list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud))
                          )
                        }
                      } else {
                        SVL_variables = list(
                          list(SVtotal_ud, S01C09=cbind(upd_dataCur[upd_SVindex_ud,c((numFeat+1):(2*numFeat))],REF_ud)),
                          list(SVtotal_ud, S03C05=cbind(upd_dataCur[upd_SVindex_ud,c(((2*numFeat)+1):(3*numFeat))],REF_ud)),
                          list(SVtotal_ud, S03C07=cbind(upd_dataCur[upd_SVindex_ud,c(((3*numFeat)+1):(4*numFeat))],REF_ud)),
                          list(SVtotal_ud, S05C03=cbind(upd_dataCur[upd_SVindex_ud,c(((4*numFeat)+1):(5*numFeat))],REF_ud)),
                          list(SVtotal_ud, S05C05=cbind(upd_dataCur[upd_SVindex_ud,c(((5*numFeat)+1):(6*numFeat))],REF_ud)),
                          list(SVtotal_ud, S05C07=cbind(upd_dataCur[upd_SVindex_ud,c(((6*numFeat)+1):(7*numFeat))],REF_ud)),
                          list(SVtotal_ud, S07C03=cbind(upd_dataCur[upd_SVindex_ud,c(((7*numFeat)+1):(8*numFeat))],REF_ud)),
                          list(SVtotal_ud, S09C01=cbind(upd_dataCur[upd_SVindex_ud,c(((8*numFeat)+1):(9*numFeat))],REF_ud))
                        )
                      } #    = c(0.01, 0.3, 0.9)      = c(1.5, 1, 0.5)
                      upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                                 SVL_variables, tmp_new_tunedSVM$finalModel)
                      tmp_new_tunedSVM2 <- upd_SLresult$bestFittingModel
                      new_trainFeatVSVM <- upd_SLresult$best_trainFeatVSVM
                      new_trainLabelsVSVM <- upd_SLresult$best_trainLabelsVSVM
                      upd_dataCur <- upd_dataCur[!upd_SVindex_ud, ]
                      
                      t.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 3)
                      tmp_pred = predict(tmp_new_tunedSVM2, validateFeatsub)
                      tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
                      # if(actAcc < tmp_new_tunedSVM$resample$Kappa){ print(paste0("current best kappa: ",round(tmp_new_tunedSVM$resample$Kappa,4)))
                      if (actAcc < tmp_acc$overall["Accuracy"]) { cat("current best accuracy: ",round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",round(tmp_new_tunedSVM2$resample$Kappa,4),"\n",sep="")
                        tmp_new_tunedSVM = tmp_new_tunedSVM2
                        actAcc = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
                        accVSVM_SL_itAL = tmp_acc
                        best_resample = resampledSize[rS]
                        best_newSize4iter = newSizes[nS4it]
                        best_classSize = classSize[clS]
                        best_cluster = clusterSizes[cS]
                        train.time = t.time
                      } else { cat("discarded accuracy: ",round(tmp_acc$overall["Accuracy"],5),"\n",sep="") }
                    }
                  }
                }
              }
            }
            cat("VSVM_SL - AL accuracy: ",round(accVSVM_SL_itAL$overall["Accuracy"],5)," | training time: ",train.time,"sec","\n",sep="")
            
            AccuracyVSVM_SL_Un_it[realization,sample_size] = as.numeric(accVSVM_SL_itAL$overall["Accuracy"])
            KappaVSVM_SL_Un_it[realization,sample_size] = as.numeric(accVSVM_SL_itAL$overall["Kappa"])
            if (actAcc>best_acc) { 
              best_acc <- actAcc 
              best_model <- model_name_AL_VSVMSL
            }
          }
          cat("\n") ############################ End Sample Portion ######################################
          
          if (realization==1 && sample_size==4) {
            saveRDS(tunedSVM, model_name_tunedSVM)
            saveRDS(tunedSVM_MS, model_name_tunedSVM_MS)
            saveRDS(bestFittingModelSVMUn_b, model_name_SVMUn_b)
            saveRDS(tunedVSVM, model_name_tunedVSVM)
            saveRDS(bestFittingModel, model_name_VSVM_SL)
            saveRDS(bestFittingModelUn_b, model_name_Un_b)
            saveRDS(bestFittingModelvUn_b, model_name_vUn_b)
            saveRDS(tmp_new_tunedSVM, model_name_AL_VSVMSL)
          }
        }
        # Store the overall best hyperparameters 
        best_bound_oa_SL = c(best_bound_oa_SL," ", best_bound_SL)
        best_boundMargin_oa_SL = c(best_boundMargin_oa_SL," ", best_boundMargin_SL)
        best_bound_oa_SL_Un = c(best_bound_oa_SL_Un," ", best_bound_SL_Un)
        best_boundMargin_oa_SL_Un = c(best_boundMargin_oa_SL_Un," ", best_boundMargin_SL_Un)
        best_bound_oa_SL_vUn = c(best_bound_oa_SL_vUn," ", best_bound_SLvUn_b)
        best_boundMargin_oa_SL_vUn = c(best_boundMargin_oa_SL_vUn," ", best_boundMargin_SLvUn_b)
        best_resample_oa=c(best_resample_oa," ", best_resample)
        best_newSize_oa=c(best_newSize_oa," ", best_newSize4iter)
        best_classSize_oa=c(best_classSize_oa," ", best_classSize)
        best_cluster_oa=c(best_cluster_oa," ", best_cluster)
        best_model_oa=c(best_model_oa,best_model,": ",as.numeric(best_acc),"\n")
        time.taken_iter = c(time.taken_iter, c("Realization ",realization," execution time: ",round(as.numeric(round(Sys.time() - start.time,2), units = "hours"), 3),"h"),"\n")
        cat("\n") ############################## End Realization #########################################
      }
      time.taken_oa <- round(Sys.time() - start.time_oa,2)
      if (length(sampleSizePor)>=8) {
        setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
        save(AccuracySVM,AccuracySVM_M,AccuracySVM_SL_Un_b,AccuracyVSVM,AccuracyVSVM_SL,AccuracyVSVM_SL_Un_b,AccuracyVSVM_SL_vUn_b,AccuracyVSVM_SL_Un_it,
             file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_acc_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
        save(KappaSVM,KappaSVM_M,KappaSVM_SL_Un_b,KappaVSVM,KappaVSVM_SL,KappaVSVM_SL_Un_b,KappaVSVM_SL_vUn_b,KappaVSVM_SL_Un_it,
             file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_Kappa_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.RData"))
        cat("OA Execution time: ", time.taken_oa, "h\n", time.taken_iter,
            "\nbest_bound_oa_SL: ", best_bound_oa_SL,        "\nbest_boundMargin_oa_SL: ", best_boundMargin_oa_SL,
            "\nbest_bound_oa_SL_Un: ", best_bound_oa_SL_Un,  "\nbest_boundMargin_oa_SL_Un: ",best_boundMargin_oa_SL_Un,
            "\nbest_bound_oa_SL_vUn: ", best_bound_oa_SL_vUn,"\nbest_boundMargin_oa_SL_vUn: ",best_boundMargin_oa_SL_vUn,
            "\nbest_resample_oa: ", best_resample_oa,        "\nbest_newSize_oa: ", best_newSize_oa,
            "\nbest_classSize_oa: ", best_classSize_oa,  "\nbest_cluster_oa: ",best_cluster_oa,"\n",best_model_oa, sep = "",
            file = paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_metadata_",city,"_",model_prob,"_",invariance,"_",b,"Unl_",nR,"nR_",length(sampleSizePor),"SizePor.txt"))
        cat("accuracy results: acquired\n")
      }
      print(confusionMatrix(new_trainLabels,predict(bestFittingModel, new_trainFeat)))
      cat("length best_trainLabelsVSVM: ",length(best_trainLabelsVSVM),"\nlength bestFittingModel$finalModel@SVindex: ", length(bestFittingModel$finalModel@SVindex),"\nlength new_trainLabels: ",length(new_trainLabels),"\nlength new_trainLabelsVSVM: ",length(new_trainLabelsVSVM),"\n\n\n",sep="")
    }
  }
}