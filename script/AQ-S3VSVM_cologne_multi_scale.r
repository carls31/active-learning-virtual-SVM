script = "ALTSLv3"  # -> New train samples and Active labeled samples are distinct data
###############################################  Libraries  ################################################
library(caret)
library(kernlab)
library(sampling)
library(progress)   # progress bar visualization
library(stats)      # k-means clustering
library(foreach)    # parallel processing
library(doParallel) # multiple CPU cores
library(Rtsne)      # t-distributed stochastic neighbour embedding
############################################################################################################

nR = 1                   # number of realizations
city = "cologne"    # cologne or hagadera location
invariance = "scale"   # scale or shape invariance
model_prob = "multiclass" # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
bound = c(0.3, 0.6)           # radius around SV - threshold       
boundMargin = c(1.5, 0.5)     # distance from hyperplane - threshold   
# sampleSizePor = c(25, 30, 40, 60, 100, 180, 340, 500, 1000) 
sampleSizePor = c(78, 120) # 96,  only one sample size for plotting the thematic map for two setup

path = "D:/"
###############################################  Utils  ####################################################
# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach out for any question                                  #
# ************************************************************************************************************** #

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

# rem_extrem(variable[[1]], variable[[2]], bound[jj])
# rem_extrem(org= SVtotal_ud, VSV1=S01C09, a=0.7)
# Evaluate the distance between Virtual Support Vectors and Support Vectors lying in the input space
rem_extrem = function(org, VSV1, a=0.7){
  # Euclidean Distance between two points lying in the input space
  euc_dis = function(a, b){
    temp = 0
    for(ii in seq_along(a)){
      temp = temp +((1e-16+a[[ii]]-b[[ii]])^2)
    }
    if (is.nan(sqrt(pmax(0,temp)))) {
      warning("NaN produced in euc_dis")
    }
    return(sqrt(temp))
  }
  org=setNames(org,c(objInfoNames[1:(length(objInfoNames)-1)],"REF"))
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
# rem_extrem_kerneldist(org=SVtotalSVMUn, VSV1=S05C07SVMUn, a=0.3,kernel_func=tunedSVM$finalModel@kernelf)
# rem_extrem_kerneldist(org= SVtotal_ud, VSV1=S01C09, a=0.7, kernel_func=SVMfinModel@kernelf)
# rem_extrem_kerneldist(org= semiAL_tot, VSV1=sAL2, a=0.7, kernel_func=SVMfinModel@kernelf)

rem_extrem_kerneldist = function(org, VSV1, a=0.7, kernel_func=tunedSVM$finalModel@kernelf){
  # print("rem_extrem_kerneldist called")
  # print(paste("org rows:", nrow(org), "org cols:", ncol(org)))
  # print(paste("VSV1 rows:", nrow(VSV1), "VSV1 cols:", ncol(VSV1)))
  # print(paste("a:", a))
  # Kernel distance between two point lying in the hyperspace
  kern_dis = function(a, b, kernel_func){
    a  <- unlist(a)
    b  <- unlist(b)
    
    k_aa <- kernel_func(a, a)
    k_bb <- kernel_func(b, b)
    k_ab <- kernel_func(a, b)
    
    dk <- sqrt( pmax((1e-16 + k_aa + k_bb - 2 * k_ab) , 0) )
    
    if (is.nan(dk)) {
      warning(paste("NaN produced in kern_dis |","k_aa:", k_aa, "k_bb:", k_bb, "k_ab:", k_ab))
      
    }
    return(dk)
  }
  org=setNames(org,c(objInfoNames[1:(length(objInfoNames)-1)],"REF"))
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
          # modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[seq_along(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
          modelfin@kernelf(modelfin@xmatrix[[l]][j,], dataPoint[seq_along(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
        
        if(abs(pred) < abs(smallestDistance))
          smallestDistance = abs(pred)
      }
    }
  }
  return(smallestDistance)   
}
# pred_all(modelfin=org$finalModel, dataPoint=unlist(samp[k, -ncol(samp)]), dataPointLabels=classes)
pred_all = function(modelfin, dataPoint, dataPointLabels, binaryClassProb=binaryClassProblem){
  smallestDistance <- 9999
  distance <- c()
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
        pred <- sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
          # modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[seq_along(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
          modelfin@kernelf(modelfin@xmatrix[[l]][j,], dataPoint[seq_along(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]

        pred=abs(pred)+1e-9
        if(pred < smallestDistance){
          smallestDistance = pred}
      }
    }
    distance = c(distance, smallestDistance)
  }
  return(distance)   
}

# # Evaluate the distance between samples and Support Vectors lying in the hyperspace
# uncertainty_dist_v2_2 = function(org, samp) {
#   
#   distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
#   
#   registerDoParallel(cl)
#   distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
#     # calculate_margin_distance(k)
#     pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), samp[k, ncol(samp)])
#   }
#   registerDoSEQ()
#   
#   scaled_distances <- apply(distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
#   distance$distance <- scaled_distances
#   samp <- cbind(samp, distance)
#   
#   return(samp)
# }
# margin_sampling(org=tmp_new_tunedSVM, samp=predLabelsVSVM_unc,pred_one,binaryClassProblem, classes=NA)
# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING new_tunedSVM, predLabelsVSVM_unc
margin_sampling <- function(org, samp, pred_one,binaryClassProblem, classes=NA,
                            realiz=realization, s_size=sample_size, plot_flag=FALSE ) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  
  # Initialize data frame to store margin distance for each sample
  margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  library(parallel)
  library(doParallel)
  library(foreach)
  
  print(paste("Starting cluster with", num_cores, "cores"))
  showConnections(all = TRUE)
  
  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    # calculate_margin_distance(k)
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
  }
  
  # Clean up
  stopCluster(cl)
  registerDoSEQ()
  
  # Debug - after cleanup
  print("Cluster stopped.")
  showConnections(all = TRUE)
  
  # scale distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # Assign scaled distances to probability dataframe
  # margin_distance$distance <- scaled_distances
  margin_distance$distance <- log1p(scaled_distances * (exp(1) - 1))
  merged_data <- cbind(samp, margin_distance)
  
  # ***********************************************************************************
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) {
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MSUD_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=24,
        res=96)
    
    
    # ***********************************************************************************
    
    # Flatten the matrices to vectors for plotting
    # distances_power <- scaled_distances^0.87
    distances_log <- log1p((scaled_distances) * (exp(1) - 1))
    # distances_exp <- 1 - exp(-as.vector(scaled_distances))
    
    # Plotting the histograms
    par(mfrow=c(1, 3))  # Set up the plotting area to have 4 rows and 1 column
    
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(margin_distances), main="Histogram of Original Margin Sampling Distances",
         xlab="Original Margin Sampling Distance", col="black", breaks=500 )
    
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(scaled_distances), main="Histogram of Scaled MS Distances",
         xlab="Scaled MS Distance", col="purple", breaks=500)
    
    # # Histogram for Power transformation
    # hist(as.vector(distances_power), main="Histogram of Power Transformed Distances",
    #      xlab="Power Transformed Distance", col="blue", breaks=500, xlim=c(0, 1))
    
    # Histogram for Logarithmic transformation
    hist(as.vector(distances_log), main="Histogram of Logarithmic Transformed MS Distances",
         xlab="Logarithmic Transformed MS Distance", col="green", breaks=500)
    
    # # Histogram for Exponential transformation
    # hist(distances_exp, main="Histogram of Exponential Transformed Distances",
    #      xlab="Exponential Transformed Distance", col="red", breaks=500, xlim=c(0, 1))
    
    # ****************************************************************************************
    
    
    # # Histogram for Standard Scaling of mclu_distances
    # hist(as.vector(margin_distance$distance), main="Histogram of Self-Learning Distances",
    #      xlab="Margin Distance", col="black", breaks=500, xlim=c(0, 1))
    dev.off()
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
  }
  
  # ****************************************************************************************
  
  return(merged_data)
}

# mclu_sampling(new_tunedSVM, predLabelsVSVM_unc)
# mclu_sampling(org=new_tunedVSVM_v1, samp=predLabelsVSVM_unc, pred_all, binaryClassProblem, classes=NA) 
# Evaluate Multiclass Level Uncertainty (MCLU) with multi-cores CPU processing
mclu_sampling <- function(org, samp, pred_all,binaryClassProblem, classes=NA,
                          realiz=realization, s_size=sample_size, plot_flag=FALSE ) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  # Initialize data frame to store uncertainty for each sample
  uncertainty <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  # Define the function to calculate margin distance for a single sample
  calculate_mclu_distance <- function(k) {
    distances <- pred_all(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
    distance_top <- sort(unlist(distances), decreasing = TRUE)[1:2]
    # return(abs(distance_top[1] - distance_top[2]))
    return(abs(distance_top[1] - distance_top[2])+1e-1*distance_top[1])
    # return(cbind(abs(distance_top[1] - distance_top[2]),abs(distance_top[1] - distance_top[2])+1e-6*distance_top[1],distance_top[1],distance_top[2]))
  }
  
  library(parallel)
  library(doParallel)
  library(foreach)
  
  print(paste("Starting cluster with", num_cores, "cores"))
  showConnections(all = TRUE)
  
  # Use foreach for parallel processing
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  mclu_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_mclu_distance(k)
  }
  
  # Clean up
  stopCluster(cl)
  registerDoSEQ()
  
  # Debug - after cleanup
  print("Cluster stopped.")
  showConnections(all = TRUE)
  
  mclu_scaled_distances <- apply(mclu_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # uncertainty$distance <- mclu_distances
  uncertainty$distance <- log1p(mclu_scaled_distances * (exp(1) - 1))
  merged_data <- cbind(samp, uncertainty)
  
  # ***********************************************************************************
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) { 
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MS_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=24,
        res=96)
    
    # Plotting the histograms
    par(mfrow=c(1, 3))  # Set up the plotting area to have 4 rows and 1 column
    
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(mclu_distances), main="Histogram of Original MCLU Distances",
         xlab="Original MCLU Distance", col="black", breaks=500 )
    
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(mclu_scaled_distances), main="Histogram of Scaled MCLU Distances",
         xlab="Scaled MCLU Distance", col="purple", breaks=500)
    
    # Histogram for Standard Scaling of mclu_distances
    hist(as.vector(uncertainty$distance), main="Histogram of Logarithmic Transformed Distances",
         xlab="Logarithmic Transformed Distance", col="green", breaks=500)
    dev.off()
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
  }
  
  # ****************************************************************************************
  
  return(merged_data)
}


add_AL_samples = function(distance_data,
                          features=NULL, ref, 
                          new_trainFeat_AL=NULL, new_trainLabels_AL=NULL,
                          newSize=10, cluster=100, ID_unit=NULL, nFeat=numFeat, PCA_flag=FALSE, tSNE_flag=FALSE, 
                          realiz=realization, s_size=sample_size, newSize2=20*nclass, plot_flag=FALSE, flag_cluster=FALSE, flag_class=FALSE){
  if(cluster<newSize){cluster=round(max(newSize*1.01,nrow(distance_data)/10))}
  # if(cluster>nrow(distance_data)){cluster=round(nrow(distance_data)/10)}
  
  # merge features and original labels
  # distance_data$label <- factor(distance_data$label, levels = levels(ref)) # solved in self_learn_AL
  ref_added = cbind(distance_data, ref)
  
  # order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  
  if(tSNE_flag) { flag_cluster=TRUE
  # ********************** duplicates check
  duplicate_rows <- duplicated(ref_added_or[, 1:nFeat])
  num_duplicates <- sum(duplicate_rows)
  if (num_duplicates > 0) {
    cat("Number of duplicate rows:", num_duplicates, "\n")
    # Find indices of duplicates
    duplicate_indices <- which(duplicate_rows)

    # # Display the first pairs of duplicate pixels
    # for (i in seq_len(min(1, num_duplicates))) {
    #   original_index <- duplicate_indices[i]
    #   duplicate_index <- which(apply(ref_added_or[, 1:nFeat], 1, function(row) all(row == ref_added_or[original_index, 1:nFeat])))[1]
    #   cat("Duplicate pair", i, ":\n")
    #   print(ref_added_or[c(duplicate_index, original_index), ])
    # }
    
    # Remove duplicates
    ref_added_or <- ref_added_or[!duplicate_rows, ]
    cat("Duplicates removed. Number of rows after removing duplicates:", nrow(ref_added_or), "\n")
  }
  # Perform t-SNE
  tsne_result <- Rtsne(ref_added_or[, 1:nFeat], dims = 2, perplexity = 30, verbose = FALSE, max_iter = 500)
  tsne_data <- data.frame(tsne_result$Y)
  colnames(tsne_data) <- c("tSNE1", "tSNE2")
  
  tsne_data_with_distance <- cbind(tsne_data, distance = ref_added_or$distance)
  
  km_tsne <- kmeans(tsne_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
  
  # Add cluster information to the data
  ref_added_or$cluster <- km_tsne$cluster
  
  } else if(PCA_flag){ flag_cluster=TRUE
    # Perform PCA
    pca_result <- prcomp(ref_added_or[, 1:nFeat], center = TRUE, scale. = TRUE)
    pca_data <- data.frame(pca_result$x[, 1:2])
    colnames(pca_data) <- c("PC1", "PC2")
    
    # Combine all data with the distance column
    pca_data_with_distance <- cbind(pca_data, distance = ref_added_or$distance)
    
    # Apply k-means clustering on each
    km_pca <- kmeans(pca_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
    
    ref_added_or$cluster <- km_pca$cluster
  } else {
    ref_data_with_distance <- cbind(ref_added_or[, 1:nFeat], setNames(ref_added_or$'distance', 'distance'))
    km_result <- kmeans(ref_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
    ref_added_or$cluster <- km_result$cluster
  }
  # **********************
  
  # ***********************************************************************************
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==1) {
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
    
    # Define colors for clusters
    cluster_colors <- rainbow(cluster)
    
    if(PCA_flag){
      pca_data_with_distance$Cluster <- as.factor(km_pca$cluster)
      
      png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"), plot_flag, "_cluster_PCA_distance_", script, "_", city, "_", model_prob, "_", invariance, ".png"),
          units="in", 
          width=16, 
          height=20, 
          pointsize=24,
          res=96)
      par(mfrow = c(2, 1), mar = c(5, 4, 4, 8), xpd = TRUE)
      
      # Plot PCA
      plot(pca_data$PC1, pca_data$PC2, col = cluster_colors[pca_data_with_distance$Cluster],
           pch = 20, cex = 0.5, main = "PCA with K-means Clustering",
           xlab = "Principal Component 1", ylab = "Principal Component 2")
      # legend("topright", inset = c(-0.2, 0), legend = levels(pca_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      
      # Plot PCA with Distance
      plot(pca_data_with_distance$PC1, ref_added_or$distance, col = cluster_colors[pca_data_with_distance$Cluster],
           pch = 20, cex = 0.5, main = "K-means Clustering on PCA + Distance",
           xlab = "Principal Component 1", ylab = "Distance")
      # legend("topright", inset = c(-0.2, 0), legend = levels(pca_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      dev.off()
    }
    
    if(tSNE_flag){
      tsne_data_with_distance$Cluster <- as.factor(km_tsne$cluster)
      
      png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"), plot_flag, "_cluster_tSNE_distance_", script, "_", city, "_", model_prob, "_", invariance, ".png"),
          units="in", 
          width=16, 
          height=20, 
          pointsize=24,
          res=96)
      par(mfrow = c(2, 1), mar = c(5, 4, 4, 8), xpd = TRUE)
      
      # Plot t-SNE
      plot(tsne_data$tSNE1, tsne_data$tSNE2, col = cluster_colors[tsne_data_with_distance$Cluster],
           pch = 20, cex = 0.5, main = c("t-SNE with K-means Clustering ", cluster),
           xlab = "t-SNE 1", ylab = "t-SNE 2")
      # legend("topright", inset = c(-0.2, 0), legend = levels(tsne_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      
      # Plot t-SNE with Distance
      plot(tsne_data_with_distance$tSNE1, ref_added_or$distance, col = cluster_colors[tsne_data_with_distance$Cluster],
           pch = 20, cex = 0.5, main = "K-means Clustering on t-SNE + Distance",
           xlab = "t-SNE 1", ylab = "Distance")
      # legend("topright", inset = c(-0.2, 0), legend = levels(tsne_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      dev.off()
    }
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
  }
  # ***********************************************************************************

  
  # Initialize vectors to store selected sample indices for two selections
  selected_indices <- c()
  selected_indices2 <- c()
  # Calculate the number of samples to select per class
  samples_per_class <- ceiling(newSize / nclass)
  samples_per_class2 <- ceiling(newSize2 / nclass)
  
  # Initialize a list to keep track of the clusters selected per class
  selected_clusters <- list()
  
  # Initialize a named vector to store how many samples have been selected per class
  class_labels <- unique(as.character(ref_added_or$label))
  class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
  
  # Initialize the list with empty vectors for each class label
  for (label in class_labels) { selected_clusters[[label]] <- c() }
  
  # Initialize a flag to indicate if the first set of selected_indices is completed
  first_selection_done <- FALSE
  
  # Iterate over the samples and select based on classes
  for (sample in seq_len(nrow(ref_added_or))) {
    # Convert class_label to character to avoid issues with factors
    class_label <- as.character(ref_added_or[sample,]$label)
    cluster_id <- ref_added_or[sample,]$cluster
    # Debugging: Print class_label to ensure it's valid
    # print(paste("Processing sample:", sample, "with class label:", class_label))
    
    # Check if we have already completed the first selection
    if (!first_selection_done) {
      # Check if the current class has not exceeded the allowed number of samples
      #if (class_sample_count[class_label] < samples_per_class || !flag_class) {
        # Check if the cluster has not been selected for this class
        if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || length(unique(ref_added_or[ref_added_or$label==class_label,"cluster"]))<samples_per_class) {
          
          # Add the sample's cluster to the selected clusters for this class
          if(flag_cluster){ selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id) }
          
          # Assign the label and add the index
          ref_added_or[sample,]$label <- ref_added_or[sample,]$ref
          selected_indices <- c(selected_indices, as.numeric(rownames(ref_added_or[sample,])))
          
          # Increment the count of selected samples for this class
          class_sample_count[class_label] <- class_sample_count[class_label] + 1
        }
      #}
      
      # Check if the first selection is done
      if (sum(class_sample_count) >= newSize) {
        first_selection_done <- TRUE
        
        # Reset class_sample_count 
        class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
        selected_clusters <- list()
        for (label in class_labels) { selected_clusters[[label]] <- c()  }
        ref_added <- ref_added[order(-ref_added$distance), ]
      }
    } else {  # Start selecting for the second set of selected_indices2

      # Check if the current class has not exceeded the allowed number of samples
      #if (class_sample_count[class_label] < samples_per_class2 || !flag_class ) {
        # Check if the cluster has not been selected for this class
        if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || length(unique(ref_added_or[ref_added_or$label==class_label,"cluster"]))<samples_per_class2) {
          # Add the sample's cluster to the selected clusters for this class
          selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id)
          # Add the index of the selected sample to the second selection
          selected_indices2 <- c(selected_indices2, as.numeric(rownames(ref_added_or[sample,])))
          # Increment the count of selected samples for this class
          class_sample_count[class_label] <- class_sample_count[class_label] + 1
        }
      #}
      # Stop if we have reached the desired total number of samples for the second selection
      if (sum(class_sample_count) >= newSize2) break
    }
  }
  ref_added_reor = ref_added_or[order(as.numeric(rownames(ref_added_or))),]
  
  # Add relabeled samples to new_trainFeat_AL and new_trainLabels_AL
  if(length(features)>1){
    # Remove relabeled samples from validateLabels
    features <- features[!(rownames(features) %in% selected_indices), ]
    reor_idx <- which(rownames(ref_added_reor) %in% selected_indices)
    semi_idx <- which(rownames(ref_added_reor) %in% selected_indices2)
    
    if(length(ID_unit)>100){
      new_trainFeat_AL <- ref_added_reor[reor_idx, 1:nFeat]
      new_trainLabels_AL <- ref_added_reor[reor_idx, nFeat+1]
      return(list(IDunit=ID_unit[reor_idx], semiIDunit=ID_unit[semi_idx], 
                  semi_samples = ref_added_reor[semi_idx, 1:(nFeat+1)],
                  # features = features, labels = ref[-reor_idx], 
                  new_trainFeat_AL = new_trainFeat_AL, 
                  new_trainLabels_AL = new_trainLabels_AL))
    } 
    cat("Not returning ID_unit\n")
    new_trainFeat_AL <- rbind(new_trainFeat_AL, ref_added_reor[reor_idx, 1:nFeat])
    new_trainLabels_AL <- c(new_trainLabels_AL, ref_added_reor[reor_idx, nFeat+1])
    return(list(features = features, labels = ref[-reor_idx],
                new_trainFeat_AL = new_trainFeat_AL, 
                new_trainLabels_AL = new_trainLabels_AL))
  } 
  return(ref_added_reor[, nFeat+1])
}

# upd_SLresult <- self_learn(testFeatsub, testLabels, bound = c(0.01, 0.1), boundMargin = c(1.5, 0.5), model_name_AL_MCLU, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables, tmp_new_tunedSVM$finalModel)
# upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_AL_MCLU, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables=list(list(SVtotal_ud, S01C09=(cbind(upd_dataCur[upd_SVindex_ud,c((numFeat+1):(2*numFeat))],REF_ud)))))
self_learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVtotal, objInfoNames, rem_extrem, rem_extrem_kerneldist, SVL_variables, SVMfinModel=tunedSVM$finalModel, train=TRUE, classProb = FALSE) {

  actKappa = -1e-6
  cat("applying constraints to VSVs candidates\n")
  
  library(parallel)
  library(doParallel)
  library(foreach)
  
  print(paste("Starting cluster with", num_cores, "cores"))
  showConnections(all = TRUE)
  
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # iteration over bound to test different bound thresholds determining the radius of acception
  for(jj in seq(along=bound)){

    # Apply foreach loop to process each SVL variable and bind the results
    if(model_prob=="binary"){ # print("binary")
      SVinvarRadi <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
        setNames(rem_extrem(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
      }
    }else{  # print("multiclass")
      SVinvarRadi <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
        
        tryCatch({
          setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj], SVMfinModel@kernelf), objInfoNames)
        }, error = function(e) {
          message(paste("Error in task", variable, ":", e$message))
          NULL  # Return NULL in case of error
        })
        # assign("last.warning", NULL, envir = baseenv())
      }
    } # print("step 2")

    # remove NAs 
    SVinvarRadi = na.omit(SVinvarRadi)
    
    # iterating over boundMargin to test different threshold on margin distance
    for(kk in seq(along=boundMargin)){
      cat("tuning similarity threshold: ",bound[jj]," [",jj,"/",length(bound),"] | bound margin: ",boundMargin[kk]," [",kk,"/",length(boundMargin),"]\n",sep="")
      
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
      names = objInfoNames[1:(length(objInfoNames)-1)]
      tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
      tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))
      
      # *********************** VSVM control parameter tuning ***********************
      tStart.time <- Sys.time()
      tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData, classProb)
      t.time <- round(as.numeric((Sys.time() - tStart.time), units = "secs"), 1)
      # of all Different bound settings get the one with best Kappa ans save its model
      if (actKappa < tunedVSVM$resample$Kappa) {cat("current best kappa: ",round(tunedVSVM$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="")
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
  
  # Clean up
  stopCluster(cl)
  registerDoSEQ()
  
  # Debug - after cleanup
  print("Cluster stopped.")
  showConnections(all = TRUE)
  
  return(list(bestFittingModel = bestFittingModel, 
              actKappa = actKappa, 
              best_trainFeatVSVM = best_trainFeatVSVM, 
              best_trainLabelsVSVM = best_trainLabelsVSVM, 
              best_bound = best_bound, 
              best_boundMargin = best_boundMargin))

}

# self_learn_AL(testFeatsub, testLabels, SVtotal, objInfoNames,
#                                SVinvarRadi=SVL_variables, plot_flag = model_name_AL_MS_semiAL)
self_learn_AL = function(
    # testFeatsub, testLabels,
  SVtotal, objInfoNames, SVinvarRadi, 
  # validateFeatsub, validateLabels, 
  upd_dataCurFeatsub, upd_dataCurLabels, 
  realiz=realization, s_size=sample_size, plot_flag=TRUE) {
  # actKappa = -1e-6
  cat("computing margin distance\n",sep="")
  if (nrow(SVinvarRadi) > 0) { # Check if SVinvarRadi has rows to process
    # Remove VSV which are not located within a certain distance to the decision function
    # data.frame to store elected VSV within the margin
    SVinvar = setNames(data.frame(matrix(ncol = numFeat + 2, nrow = 0)), c(objInfoNames, "distance"))
    upd_Labels <- factor(character(0), levels = levels(upd_dataCurLabels))
    # Progress bar initialization
    pb <- progress_bar$new(
      format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
      total = nrow(SVinvarRadi),
      clear = FALSE
    )
    # Iterate over SVinvarRadi and evaluate distance to hyperplane
    for (m in 1:nrow(SVinvarRadi)) {
      signa = as.numeric(pred_one(new_bestModel$finalModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
      # if (signa < boundMargin[kk]) {
      row_with_distance = c(SVinvarRadi[m, ], distance = signa)
      SVinvar = rbind(SVinvar, row_with_distance)
      upd_Labels <- c(upd_Labels, upd_dataCurLabels[m])
      # }
      pb$tick()
    }
  } else { 
    warning("SVinvarRadi has no rows to process") 
  }
  
  # merge elected VSV with original SV
  SVinvar_org = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvar[, -ncol(SVinvar)],objInfoNames))
  
  SVinvar_org=na.omit(SVinvar_org)
  
  SVinvar=setNames(SVinvar,c(objInfoNames,"distance"))
  SVinvar$label <- factor(SVinvar$label, levels = levels(upd_Labels))
  
  # ***********************************************************************************
  
  # Flatten the matrices to vectors for plotting
  scaled_distances <- (SVinvar$distance - min(SVinvar$distance)) / (max(SVinvar$distance) - min(SVinvar$distance))
  # distances_power <- scaled_distances^0.87
  distances_log <- log1p((scaled_distances) * (exp(1) - 1))
  # distances_exp <- 1 - exp(-as.vector(scaled_distances))
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==1) { 
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MSSL_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=24,
        res=96)
    
    
    # ***********************************************************************************
    
    # Plotting the histograms
    par(mfrow=c(1, 3)) 
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(SVinvar$distance), main="Histogram of Original SL Margin Distances",
         xlab="Original SL Margin Distance", col="black", breaks=500 )
    
    # Histogram for Standard Scaling of margin_distances
    hist(as.vector(scaled_distances), main="Histogram of Scaled SL Margin Distances",
         xlab="Scaled SL Margin Distance", col="purple", breaks=500)
    
    # # Histogram for Power transformation
    # hist(as.vector(distances_power), main="Histogram of Power Transformed Distances",
    #      xlab="Power Transformed Distance", col="blue", breaks=500, xlim=c(0, 1))
    
    # Histogram for Logarithmic transformation
    hist(as.vector(distances_log), main="Histogram of Logarithmic Transformed Distances",
         xlab="Logarithmic Transformed Distance", col="green", breaks=500)
    
    # ****************************************************************************************
    
    
    # # Histogram for Standard Scaling of mclu_distances
    # hist(as.vector(SVinvar$distance), main="Histogram of Self-Learning Distances",
    #      xlab="Margin Distance", col="black", breaks=500, xlim=c(0, 1))
    dev.off()
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
  }
  
  # ****************************************************************************************
  SVinvar$distance <- distances_log
  # if(length(boundMargin)==1){
  return(list(sampled_data = SVinvar, 
              best_updCur_Labels = upd_Labels))
  # }
  #   # split for training to feature and label
  #   trainFeatVSVM = SVinvar_org[,1:(ncol(SVinvar_org)-1)]
  #   trainLabelsVSVM = SVinvar_org[,ncol(SVinvar_org)]
  #   
  #   # get list with index of trainData to split between train and test in svmFit
  #   countTrainData = nrow(SVinvar_org)
  #   indexTrainData = list(c(1:countTrainData))
  #   
  #   # join of train and test data (through indesTrainData in svmFit seperable)
  #   names = objInfoNames[1:(length(objInfoNames)-1)]
  #   tuneFeatVSVM = rbind(trainFeatVSVM, setNames(testFeatsub, names))
  #   tuneLabelsVSVM = unlist(list(trainLabelsVSVM, testLabels))
  #   
  #   # *********************** VSVM control parameter tuning ***********************
  #   tStart.time <- Sys.time()
  #   tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
  #   t.time <- round(as.numeric((Sys.time() - tStart.time), units = "secs"), 1)
  #   
  #   tmp_pred = predict(tunedVSVM, validateFeatsub)
  #   tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
  #   if (actKappa < tmp_new_tunedSVM$resample$Kappa) { cat("current best kappa: ",sep="")
  #     bestFittingModel = tunedVSVM
  #     actKappa = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
  #     best_trainAS = SVinvar
  #     best_upd_Labels = upd_Labels
  #     # best_resample = resampledSize[rS]
  #     best_boundMargin = boundMargin[kk]
  #     if (actKappa == 1) {
  #       cat(round(tunedVSVM$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="")
  #       return(list(sampled_data = best_trainAS, 
  #                   best_updCur_Labels = best_upd_Labels,
  #                   best_boundMargin = best_boundMargin))
  #     }
  #   } else { cat("discarded kappa: ",sep="")} 
  #   cat(round(tunedVSVM$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="") #round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",
  # # } 
  # return(list(sampled_data = best_trainAS, 
  #             best_updCur_Labels = best_upd_Labels,
  #             best_boundMargin = best_boundMargin))
}


classificationProblem = function(generalDataPool){
  cat("note that the first record is of class: ",levels(generalDataPool$REF)[1],"\n",sep="")
  f=levels(generalDataPool$REF)[1]
  generalDataPool$REF = as.character(generalDataPool$REF)
  generalDataPool$REF[generalDataPool$REF != as.character(f)] = "other"
  generalDataPool$REF = as.factor(generalDataPool$REF)
  return(generalDataPool)
}
############################################################################################################



###############################################  Preprocessing  ###############################################

# sampleSizePor = c(5,10,20,32,46,62,80,100) # Class sample size: round(250/6) label per class i.e. 42 # c(100,80,62,46,32,20,10,5)
lgtS=TRUE
train  = TRUE              # if TRUE, train the models otherwise load them from dir 
num_cores <- parallel::detectCores()/2 # Numbers of CPU cores for parallel processing

# if(!dir.exists(path)){path = '/home/data1/Lorenzo/'}

numFeat = 18                                            # number of features per level (dimensionality)

colheader = as.character(sampleSizePor)                 # corresponding column names

#names to use in rbind() of VSV                         # 18 features names + 19.label
objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
                  "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
                  "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
                  "Lx_t_diss","Lx_t_hom","Lx_t_mean",
                  "label")


sindexSVMDATA = 37        # start of baseline model with one segmentation scale data
eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data

  
# # To load them back:
setwd(paste0(path, "tunc_oz/apply_model/", "rds_data_r_import/",city,"/",invariance))
if (file.exists(paste0(city,"_",model_prob,"_",invariance,"_trainDataPoolAllLev.rds"))) {
  
  cat("loading",city,model_prob,invariance,"dataset\n")
  trainDataPoolAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_trainDataPoolAllLev.rds"))
  testDataAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_testDataAllLev.rds"))
  validateDataAllLev <- readRDS(paste0(city,"_",model_prob,"_",invariance,"_validateDataAllLev.rds"))
  validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
  validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
  
} else {
  ###############################################  Input  #################################################  
  cat("preprocessing",city,model_prob,invariance,"\n")
  
  inputPath ="cologne_res_100_L2-L13.csv" 
  
  # numFeat = 18                                            # number of features per level (dimensionality)
  # 
  # #names to use in rbind() of VSV                         # 18 features names + 19.label
  # objInfoNames =  c("Lx_g_comp","Lx_g_elfi","Lx_g_refi","Lx_g_roun","Lx_g_shin",
  #                   "Lx_m_bl","Lx_m_gr","Lx_m_ndvi","Lx_m_nir","Lx_m_re",
  #                   "Lx_sd_bl","Lx_sd_gr","Lx_sd_ndvi","Lx_sd_nir","Lx_sd_re",
  #                   "Lx_t_diss","Lx_t_hom","Lx_t_mean",
  #                   "label")
  
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
  

  # sindexSVMDATA = 37        # start of baseline model with one segmentation scale data
  # eindexSVMDATA = sindexSVMDATA + numFeat -1              # end of base data
  # 
  # exclude unclassified and delete level of factor
  generalDataPool = subset(generalDataPool, REF != "unclassified")
  generalDataPool$REF <- factor(generalDataPool$REF)
  # generalDataPool <- na.omit(generalDataPool) 
  
  if (model_prob=="binary") { #transform to 2-Class-Case "bushes trees" [cologne] or "bare soil" [hagadera] VS rest 
    generalDataPool=classificationProblem(generalDataPool)
  }
  ###############################################  Scaling  ################################################
  
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
  ###############################################  Splitting & Sampling  ############################################
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
  
  
  # order train datapool by class label in alphabetical order:
  trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
  
  #########################################################################################
  

  setwd(paste0(path, "tunc_oz/apply_model/", "rds_data_r_import/",city,"/",invariance))
  saveRDS(trainDataPoolAllLev, paste0(city,"_",model_prob,"_",invariance,"_trainDataPoolAllLev.rds"))
  saveRDS(testDataAllLev, paste0(city,"_",model_prob,"_",invariance,"_testDataAllLev.rds"))
  saveRDS(validateDataAllLev, paste0(city,"_",model_prob,"_",invariance,"_validateDataAllLev.rds"))
  cat("preprocessed data: stored ")
}
rm(validateDataAllLev)

AccuracySVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM) = colheader
AccuracySVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracySVM_SL_Un) = colheader

# AccuracyVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(AccuracyVSVM) = colheader
AccuracyVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL) = colheader
AccuracyVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un) = colheader
AccuracyVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn) = colheader

AccuracyAL_MCLU = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MCLU) = colheader
AccuracyAL_MS_tSNE = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MS_tSNE) = colheader
AccuracyAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MS) = colheader
AccuracyAL_MS_semiAL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MS_semiAL) = colheader

AccuracyALSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALSVM) = colheader
AccuracyALSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALSVM_SL_Un) = colheader
AccuracyALVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL) = colheader
AccuracyALVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL_Un) = colheader
AccuracyALVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL_vUn) = colheader


# ******** KAPPA SCORE
KappaSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM) = colheader
KappaSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM_SL_Un) = colheader

# KappaVSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
# colnames(KappaVSVM) = colheader
KappaVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL) = colheader
KappaVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un) = colheader
KappaVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_vUn) = colheader

KappaAL_MCLU = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MCLU) = colheader
KappaAL_MS_tSNE = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS_tSNE) = colheader
KappaAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS) = colheader
KappaAL_MS_semiAL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS_semiAL) = colheader


KappaALSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALSVM) = colheader
KappaALSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALSVM_SL_Un) = colheader
KappaALVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL) = colheader
KappaALVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL_Un) = colheader
KappaALVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL_vUn) = colheader



# ************************************************    
SVsSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsSVM) = colheader
SVsSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsSVM_SL_Un) = colheader

SVsVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsVSVM_SL) = colheader
SVsVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsVSVM_SL_Un) = colheader
SVsVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsVSVM_SL_vUn) = colheader

SVsAL_MCLU = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MCLU) = colheader
SVsAL_MS_tSNE = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MS_tSNE) = colheader
SVsAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MS) = colheader
SVsAL_MS_semiAL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MS_semiAL) = colheader

SVsALSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALSVM) = colheader
SVsALSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALSVM_SL_Un) = colheader
SVsALVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL) = colheader
SVsALVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_Un) = colheader
SVsALVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_vUn) = colheader
# ************************************************ 

# ************************************************    
tr.timeSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeSVM) = colheader
tr.timeSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeSVM_SL_Un) = colheader

tr.timeVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL) = colheader
tr.timeVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un) = colheader
tr.timeVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_vUn) = colheader

tr.timeVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un_it) = colheader
tr.timeVSVM_SL_Un_itSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un_itSL) = colheader
tr.timeVSVM_SL_Un_itTSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un_itTSL) = colheader
tr.timeVSVM_SL_Un_itSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un_itSL2) = colheader
tr.timeVSVM_SL_Un_itTSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeVSVM_SL_Un_itTSL2) = colheader

tr.timeALSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeALSVM) = colheader
tr.timeALSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeALSVM_SL_Un) = colheader

tr.timeALVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeALVSVM_SL) = colheader
tr.timeALVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeALVSVM_SL_Un) = colheader
tr.timeALVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(tr.timeALVSVM_SL_vUn) = colheader
# ************************************************ 

# ********
# best_bound_oa_SL       = c()
# best_boundMargin_oa_SL = c()
# best_resample_oa       = c()
# best_newSize_oa        = c()
# best_classSize_oa      = c()
# best_cluster_oa        = c()
best_model_oa          = c()
time.taken_iter        = c()
# table_trainLabels = NULL
best_boundMargin  = 1
# best_resample     = NULL
# best_newSize4iter = NULL
# best_classSize    = NULL
# best_cluster      = NULL
best_boundMargin = 1
trainSVMUn.time_oa  = 0
trainSL.time_oa  = 0
trainUn.time_oa  = 0
trainvUn.time_oa = 0
train.timeALv1_VSVMSL_oa      = 0
train.timeALv1_tSNE_VSVMSL_oa = 0
train.timeALv2_tSNE_VSVMSL_oa = 0
train.timeALv2_SEMI_VSVMSL_oa = 0
nclass=6
if(model_prob=="binary"){ nclass=2  }

# set randomized seed for the random sampling procedure
seed = 5 # 5, 73, 20, 98, 133

###############################################  Training  #################################################

start.time_oa <- Sys.time()

for (realization in seq(1,nR)) {
  
  cat("\n","CPU cores: ",num_cores,sep="")
  start.time <- Sys.time()
  
  
  trainDataCurBeg = trainDataPoolAllLev
  testDataCurBeg = testDataAllLev
  # subset for each outer iteration test data to speed up computing
  testDataCurBeg = testDataCurBeg[order(testDataCurBeg[,ncol(testDataCurBeg)]),]
  
  
  for (sample_size in seq(1, length(sampleSizePor))) { # , by=2
    cat("\n") #################################  Sampling train and test data ##############################
    
    # initial seed value for randomized sampling
    if (train) {seed = seed + sample(100, 1)}
    cat(city," ",model_prob ," ",invariance," | realization [",realization,"/",nR,"] | labeled samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round(length(sampleSizePor)),"] | seed: ",seed,"\n",sep="")
    
    # set randomized seed for the random sampling procedure
    set.seed(seed)
    

    if(sample_size==1){
      
      
      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)
      
      
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
      # *********************************************************************
      
      # subset on L_4 ***************************** SVM base for invariants ************************************
      trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and validate set
      # ************************************************ *******************************************************
      
      
      
      stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBeg, stratSamp)
      testDataCur = samples[,1:ncol(testDataAllLev)]
      
      # split test feat from test label for later join with trainData
      testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
      testLabels = testDataCur[,ncol(testDataCur)]
      
      # subset on base level
      testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]
    } else {
      # sampleSize = round((sampleSizePor[sample_size+1]-sampleSizePor[sample_size])/nclass)
      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)
      
      
      # definition of sampling configuration (strata:random sampling without replacement)
      stratSamp = strata(trainDataCurRemaining, c("REF"), size = shares, method = "srswor")
      #size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
      
      # get samples of trainDataCur and set trainDataCur new
      samples = getdata(trainDataCurRemaining, stratSamp)
      samplesID = samples$ID_unit
      
      trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemaining <- trainDataCurRemaining[-c(samplesID), ]
      
      
      trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
      trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]
      # *********************************************************************
      
      # subset on L_4 ***************************** SVM base for invariants ************************************
      trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and validate set
      # ************************************************ *******************************************************
      # newtrainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
      # newtrainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]
      # # *********************************************************************
      # 
      # # subset on L_4 ***************************** SVM base for invariants ************************************
      # newtrainFeat = newtrainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and validate set
      # # ************************************************ *******************************************************
      # 
      # # **********************
      # # get next train set portion
      # # trainFeat <- rbind(trainFeat[,], setNames(newtrainFeat, names))
      # trainFeat <- rbind(setNames(trainFeat[,], names), setNames(newtrainFeat, names))
      # trainLabels <- unlist(list(trainLabels[], newtrainLabels))
      
    }
    
    
    # trainData index to split between train and test in svmFit
    countTrainData = nrow(trainFeat)
    indexTrainData = list(c(1:countTrainData))
      
    # join of train and test test data (separable through indexTrainData in svmFit)
    tuneFeat = rbind(setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
    tuneLabel = unlist(list(trainLabels, testLabels))
    
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
    
    cat("\n") #################################  SVM #####################################
    model_name_tunedSVM = "SVM"
    
    # SVM = function(){
    #   return()
    # }
    
    cat("training SVM\n")
    trainStart.time <- Sys.time()
    tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
    trainSVM.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
    
    # run classification and accuracy assessment for unmodified SV and predict labels of test data
    predLabelsSVM = predict(tunedSVM, validateFeatsub)
    cm_SVM = confusionMatrix(predLabelsSVM, validateLabels)
    cat("SVM accuracy: ",round(cm_SVM$overall["Accuracy"],5)," | execution time: ",trainSVM.time,"sec\n",sep="")
    
    AccuracySVM[realization,sample_size] = as.numeric(cm_SVM$overall["Accuracy"])
    KappaSVM[realization,sample_size] = as.numeric(cm_SVM$overall["Kappa"])
    SVsSVM[realization,sample_size] = as.numeric(length(tunedSVM$finalModel@SVindex))
    tr.timeSVM[realization,sample_size] = trainSVM.time
    
    if(sample_size==1 || cm_SVM$overall["Accuracy"]>best_acc){
      best_acc <- cm_SVM$overall["Accuracy"]
      best_model_name <- model_name_tunedSVM
      new_bestModel <- tunedSVM
      new_bestTrainFeatVSVM <- trainFeat 
      new_bestTrainLabelsVSVM <- trainLabels
      best_train.time <- trainSVM.time
    }
    # get original SVs of base SVM ************************* prevent removing an entire class label
    SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
    SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
    freq_table <- table(SVtotal$REF)
    zero_label_classes <- names(freq_table[freq_table == 0])
    if (length(zero_label_classes) > 0) {
      cat("\nFound an empty class label")
      for (class in zero_label_classes) {
        # Find the indices of rows in train DataCur with the zero label class
        class_indices <- which(trainLabels == class)
        # Extract the corresponding rows from trainFeat and trainLabels
        class_feat <- trainFeat[class_indices, , drop = FALSE]
        class_labels <- trainLabels[class_indices, drop = FALSE]
        # Create a data frame with the same structure as SVtotal
        class_data <- cbind(class_feat, class_labels)
        # Append the class_data to SVtotal
        SVtotal <- rbind(setNames(SVtotal,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")), setNames(class_data,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")))
        SVindex <- c(SVindex, class_indices)
      }
    }
    # ******************************************************
    binaryClassProblem = list()
    for (jj in seq_along(tunedSVM$finalModel@xmatrix)) { # COMPARE EVERY COUPLE COMBINATION OF CLASSES
      binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
    }
    # ******************************************************
    names = objInfoNames[1:(length(objInfoNames)-1)]

    cat("\n") #################################  SVM-SL + semi-labeled samples #####################################
    model_name_SVMUn = "SVM_SLUn"

    trainStart.time <- Sys.time()
    # Definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")

    # get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemainingSVM = getdata(trainDataCurRemaining, stratSampRemaining)
    trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemainingSVM$ID_unit), ]

    trainDataCurRemainingSVM_Un = samplesRemainingSVM[,1:ncol(trainDataPoolAllLev)]
    trainDataCurRemainingSVM_Unsub_b = trainDataCurRemainingSVM_Un[sindexSVMDATA:eindexSVMDATA]

    REFSVM = predict(tunedSVM, trainDataCurRemainingSVM_Unsub_b)

    # get SV of unlabeled samples
    SVindexSVMUn = 1:nrow(trainDataCurRemainingSVM_Unsub_b)
    SVtotalSVMUn = trainDataCurRemainingSVM_Un[SVindexSVMUn ,c(sindexSVMDATA:eindexSVMDATA)]
    SVtotalSVMUn = cbind(SVtotalSVMUn, REFSVM)

    cat("evaluation of SVM with self learning and semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")

      SVL_variables = list(
        list(SVtotalSVMUn, SVL2SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM)),
        list(SVtotalSVMUn, SVL3SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM)),
        list(SVtotalSVMUn, SVL5SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL6SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL7SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL8SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL9SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL10SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL11SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFSVM))
      )

    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_SVMUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                           SVL_variables)
    bestFittingModelSVMUn <- SLresult$bestFittingModel
    best_trainFeatSVMUn <- SLresult$best_trainFeatVSVM
    best_trainLabelsSVMUn <- SLresult$best_trainLabelsVSVM
    best_boundMarginSVMUn <- SLresult$best_boundMargin
    # best_boundSVM_SL_Un = SLresult$best_bound
    # best_boundMarginSVM_SL_Un = SLresult$best_boundMargin
    t.timeSVMUn <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsSVMsumUn = predict(bestFittingModelSVMUn, validateFeatsub)
    cm_SVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)
    cat("SVM_SL_Un accuracy: ",round(cm_SVM_SL_Un$overall["Accuracy"],5)," | execution time: ",t.timeSVMUn,"sec\n",sep="")

    AccuracySVM_SL_Un[realization,sample_size] = as.numeric(cm_SVM_SL_Un$overall["Accuracy"])
    KappaSVM_SL_Un[realization,sample_size] = as.numeric(cm_SVM_SL_Un$overall["Kappa"])
    SVsSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelSVMUn$finalModel@SVindex))
    if (cm_SVM_SL_Un$overall["Accuracy"]>best_acc) {
      best_acc <- cm_SVM_SL_Un$overall["Accuracy"]
      new_bestModel <- bestFittingModelSVMUn

      # best_boundMargin <- best_boundMarginSVMUn
      best_model_name <- model_name_SVMUn
      best_train.time <- t.timeSVMUn
    }

    cat("\n") #################################  VSVM-SL ################################################
    model_name_VSVM_SL = "VSVM_SL"

    trainStart.time <- Sys.time()

      SVL2 = trainDataCur[SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
      SVL3 = trainDataCur[SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]

      SVL5 = trainDataCur[SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
      SVL6 = trainDataCur[SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
      SVL7 = trainDataCur[SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
      SVL8 = trainDataCur[SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
      SVL9 = trainDataCur[SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
      SVL10 = trainDataCur[SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
      SVL11 = trainDataCur[SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]

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
      )
    
    cat("evaluation of VSVM with self learning | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
    
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

    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_VSVM_SL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                           SVL_variables)
    bestFittingModel <- SLresult$bestFittingModel
    best_trainFeatVSVM <- SLresult$best_trainFeatVSVM
    best_trainLabelsVSVM <- SLresult$best_trainLabelsVSVM
    best_bound_SL = SLresult$best_bound
    best_boundMargin_SL = SLresult$best_boundMargin
    t.timeSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+trainSVM.time, 1)

    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
    cm_VSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
    cat("VSVM_SL accuracy: ",round(cm_VSVM_SL$overall["Accuracy"],5)," | execution time: ",t.timeSL,"sec\n",sep="")

    AccuracyVSVM_SL[realization,sample_size] = as.numeric(cm_VSVM_SL$overall["Accuracy"])
    KappaVSVM_SL[realization,sample_size] = as.numeric(cm_VSVM_SL$overall["Kappa"])
    SVsVSVM_SL[realization,sample_size] = as.numeric(length(bestFittingModel$finalModel@SVindex))
    if (cm_VSVM_SL$overall["Accuracy"]>best_acc) {
      best_acc <- cm_VSVM_SL$overall["Accuracy"]
      new_bestModel <- bestFittingModel
      # new_bestTrainFeatVSVM <- best_trainFeatVSVM
      # new_bestTrainLabelsVSVM <- best_trainLabelsVSVM
      # best_boundMargin <- best_boundMargin_SL
      best_model_name <- model_name_VSVM_SL
      best_train.time <- t.timeSL
    }


    cat("\n") #################################  VSVM-SL + semi-labeled samples #####################################
    model_name_Un = "VSVM_SLUn"

    trainStart.timeUn <- Sys.time()
    actAcc_vUn = -1e-6
    bb = 1 # for (bb in seq(along=b)) {

      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")

      # get samples of trainDataCurRemaining and set trainDataCurRemaining new
      samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)

      trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]

      trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]

      REF_b = predict(bestFittingModel, trainDataCurRemainingsub_SL)

      # get SV of unlabeled samples
      indexUn = 1:nrow(trainDataCurRemainingsub_SL)
      totalUn = trainDataCurRemaining_SL[indexUn ,c(sindexSVMDATA:eindexSVMDATA)]
      totalUn = cbind(totalUn, REF_b)

      cat("evaluation of VSVM SL with ",b[bb]," semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      # get VSs, means rows of SV but with subset on different level
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
          list(totalUn, L2Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)),
          list(totalUn, L3Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)),
          list(totalUn, L5Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)),
          list(totalUn, L6Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)),
          list(totalUn, L7Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)),
          list(totalUn, L8Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)),
          list(totalUn, L9Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)),
          list(totalUn, L10Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)),
          list(totalUn, L11Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b))
        )

      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_Un, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingModelUn <- SLresult$bestFittingModel
      best_trainFeatVSVMUn <- SLresult$best_trainFeatVSVM
      best_trainLabelsVSVMUn <- SLresult$best_trainLabelsVSVM
      # best_bound_SL_Un = SLresult$best_bound
      best_boundMargin_SL_Un = SLresult$best_boundMargin
      trainUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsVSVMsumUn = predict(bestFittingModelUn, validateFeatsub)
      cm_VSVM_SL_Un = confusionMatrix(predLabelsVSVMsumUn, validateLabels)
      cat("VSVM_SL_Un accuracy: ",round(cm_VSVM_SL_Un$overall["Accuracy"],5)," | execution time: ",trainUn.time,"sec","\n",sep="")

      AccuracyVSVM_SL_Un[realization,sample_size] = as.numeric(cm_VSVM_SL_Un$overall["Accuracy"])
      KappaVSVM_SL_Un[realization,sample_size] = as.numeric(cm_VSVM_SL_Un$overall["Kappa"])
      SVsVSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelUn$finalModel@SVindex))
      if (cm_VSVM_SL_Un$overall["Accuracy"] > best_acc) {
        best_acc <- cm_VSVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingModelUn
        # new_bestTrainFeatVSVM <- best_trainFeatVSVMUn
        # new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn
        # best_boundMargin <- best_boundMargin_SL_Un
        best_model_name <- model_name_Un
        best_train.time <- trainUn.time
      }


    cat("\n") #################################  VSVM-SL + virtual semi-labeled samples ##################################
      model_name_vUn = "VSVM_SLvUn"

      REF_v = predict(bestFittingModelUn, trainDataCurRemainingsub_SL)

      # get SV of unlabeled samples
      indexvUn_v = 1:nrow(trainDataCurRemainingsub_SL) #  bestFittingModelUn$finalModel@SVindex
      SVtotalvUn_v = trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA)] #na.omit(trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_SL))])
      SVtotalvUn_v = cbind(SVtotalvUn_v, REF_v)

      # extracting previously assigned reference column
      SVtotalvUn_vFeat = SVtotalvUn_v[,1:(ncol(SVtotalvUn_v)-1)] # -1)]
      REF_v = SVtotalvUn_v[,(ncol(SVtotalvUn_v))]
      SVtotalvUn_v = cbind(SVtotalvUn_vFeat, REF_v)

      cat("evaluation of VSVM SL with ",b[bb]," virtual semi-labeled samples"," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="") #  [",bb,"/",length(b),"]","

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
          list(SVtotalvUn_v, SVL2vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)),
          list(SVtotalvUn_v, SVL3vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)),
          list(SVtotalvUn_v, SVL5vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL6vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL7vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL8vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL9vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL10vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)),
          list(SVtotalvUn_v, SVL11vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v))
        )

      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_vUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingModelvUn <- SLresult$bestFittingModel
      # new_best_trainFeatVSVMvUn <- SLresult$best_trainFeatVSVM
      # new_best_trainLabelsVSVMvUn <- SLresult$best_trainLabelsVSVM
      # new_best_bound_SLvUn = SLresult$best_bound
      # new_best_boundMargin_SLvUn = SLresult$best_boundMargin
      trainvUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      new_predLabelsVSVMvUnsum = predict(bestFittingModelvUn, validateFeatsub)
      cm_VSVM_SL_vUn = confusionMatrix(new_predLabelsVSVMvUnsum, validateLabels)

    cat("VSVM_SL_vUn accuracy: ",round(cm_VSVM_SL_vUn$overall["Accuracy"],5)," | execution time: ",trainvUn.time,"sec","\n",sep="")

    AccuracyVSVM_SL_vUn[realization,sample_size] = as.numeric(cm_VSVM_SL_vUn$overall["Accuracy"])
    KappaVSVM_SL_vUn[realization,sample_size] = as.numeric(cm_VSVM_SL_vUn$overall["Kappa"])
    SVsVSVM_SL_vUn[realization,sample_size] = as.numeric(length(bestFittingModelvUn$finalModel@SVindex))
    if (cm_VSVM_SL_vUn$overall["Accuracy"] > best_acc){
      best_acc <- cm_VSVM_SL_vUn$overall["Accuracy"]
      new_bestModel <- bestFittingModelvUn
      # new_bestTrainFeatVSVM <- best_trainFeatVSVMvUn
      # new_bestTrainLabelsVSVM <- best_trainLabelsVSVMvUn
      # best_boundMargin <- best_boundMargin_SLvUn
      best_model_name <- model_name_vUn
    }
    ########################################################################################################
    
    
    
    
    if (num_cores>=3 && sample_size<length(sampleSizePor)){ 
      
      cat("\n") ###############################  Sampling unlabeled data #####################################
      
      # get the new size for the active labeling
      newSize = sampleSizePor[sample_size+1]-sampleSizePor[sample_size]
      # if(sample_size==1){ 
      #   
      #    # +1
      # } else { 
      #   newSize = sampleSizePor[sample_size+1]-sampleSizePor[sample_size-1] 
      # }
      
      sampleSize = round(sampleSizePor[sample_size+1]/nclass)
      shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)
      
      if (sample_size==1){
        trainFeat_AL = setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)])
        trainLabels_AL = trainLabels
        
        trainDataCurRemaining_AL = trainDataCurRemaining
      }

      stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBeg, stratSamp)
      testDataCur = samples[,1:ncol(testDataAllLev)]
      
      # split test feat from test label for later join with trainData
      testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
      testLabels = testDataCur[,ncol(testDataCur)]
      
      # subset on base level
      testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]
      
      
      
      
      
      
      clusterSizes = newSize+1 # c(round(max(classPor/40,newSize+1)))
      
      
      classSize=c(25000)            
      clS=1
      cat("sampling ", classSize," unlabeled data\n",sep="")
      samplingStart.time <- Sys.time()
      
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining = strata(trainDataCurRemaining_AL, size = classSize[clS], method = "srswor")
      # Get new samples from trainDataCurRemaining_AL
      samplesRemaining = getdata(trainDataCurRemaining_AL, stratSampRemaining)
      
      sampling.time = round(as.numeric((Sys.time() - samplingStart.time), units = "secs"), 1)
      # }
      
      
      # Final check for duplicates
      final_duplicate_count <- sum(duplicated(samplesRemaining[, c(sindexSVMDATA:eindexSVMDATA)]))
      cat("final unlabeled pool size: ",nrow(samplesRemaining)," | duplicates: ", final_duplicate_count," | sampling required ", sampling.time,"sec\n",sep="")
      cat("using currently best model: ",best_model_name," | accuracy: ",best_acc,"\n",sep="")
      cS=1  
    
      
      cat("\n") ###############################  AL MCLU #########################################
      model_name_AL_MCLU ="AL_MCLU+kmeans_VSVM"
      
      cat("active labeling ",model_name_AL_MCLU," | ",length(trainLabels_AL)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")
      
      upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
      upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
      upd_dataCurLabels  <- upd_dataCur[,ncol(trainDataCur)]
      
      MCLU_trainFeat_AL <- setNames(trainFeat_AL, names)
      MCLU_trainLabels_AL <- trainLabels_AL
      
      # newSize_for_iter = newSize #sampleSize/10 # or just 4
      # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
      
      trainStart.time <- Sys.time()
      
      predLabelsVSVM = predict(new_bestModel, upd_dataCurFeatsub)
      # Add predicted labels to the features data set
      predLabelsVSVM_unc = cbind(upd_dataCurFeatsub, predLabelsVSVM)
      predLabelsVSVM_unc = setNames(predLabelsVSVM_unc, objInfoNames)
      
      if (model_prob=="binary") { sampled_data <- margin_sampling(new_bestModel, predLabelsVSVM_unc, pred_one, binaryClassProblem, plot_flag = model_name_AL_MCLU)
      } else {                    sampled_data <- mclu_sampling(  new_bestModel, predLabelsVSVM_unc, pred_all, binaryClassProblem, plot_flag = model_name_AL_MCLU) }
      cat("computing distances required ", round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1),"sec\n",sep="")
      ALSamplesStart.time <- Sys.time()
      result <- add_AL_samples(sampled_data,
                               upd_dataCurFeatsub, upd_dataCurLabels,
                               MCLU_trainFeat_AL, MCLU_trainLabels_AL,
                               newSize, cluster=round(min(clusterSizes[cS],nrow(sampled_data)/20)), # always greater than newSize, # 60, 80, 100, 120
                               upd_dataCur$ID_unit, tSNE_flag = FALSE, flag_cluster = TRUE)
      cat("getting active-labeled samples and updated datasets required ", round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1),"sec\n",sep="")
      # Extract new datasets
      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL
      semiAL_tot <- result$semi_samples
      semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit
      
      # **********************
      # get original SVs of base SVM
      # SVindex_ud = tmp_new_tunedSVM$finalModel@SVindex # SVs OF THIS MODEL ARE NOT IN new_trainFeat_AL
      
      # get new al train set portion
      MCLU_trainFeat_AL <- rbind(MCLU_trainFeat_AL[,], setNames(new_trainFeat, names))
      MCLU_trainLabels_AL <- unlist(list(MCLU_trainLabels_AL[], new_trainLabels))
      
      
      # **********************
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(MCLU_trainFeat_AL)
      indexTrainData = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(MCLU_trainFeat_AL,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(MCLU_trainLabels_AL, testLabels))
      
      AL_MCLU_tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      train.timeALv1_tSNE_VSVMSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time, 1)
      
      tmp_pred = predict(AL_MCLU_tunedSVM, validateFeatsub)
      cm_AL_MCLU  = confusionMatrix(tmp_pred, validateLabels)
      
      # **********************
      # upd_dataCur <- upd_dataCur[!upd_SVindex_ud, ]
      
      # tmp_new_tunedSVM = AL_MCLU_tunedSVM
      acc_AL_MCLU = cm_AL_MCLU$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
      
      
      cat(model_name_AL_MCLU," accuracy: ",acc_AL_MCLU," | execution time: ",train.timeALv1_tSNE_VSVMSL,"sec\n",sep="")
      
      AccuracyAL_MCLU[realization,sample_size+1] = as.numeric(cm_AL_MCLU$overall["Accuracy"])
      KappaAL_MCLU[realization,sample_size+1] = as.numeric(cm_AL_MCLU$overall["Kappa"])
      SVsAL_MCLU[realization,sample_size+1] = as.numeric(length(AL_MCLU_tunedSVM$finalModel@SVindex))
      
      cat("\n") ###############################  AL MS + t-SNE&Class  ###########################################
      model_name_AL_MS_tSNE = "AL_MS+tSNE_SVM"
      
      cat("active labeling ",model_name_AL_MS_tSNE," | ",length(trainLabels_AL)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")
      
      upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
      upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
      upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
      
      new_trainFeat_AL <- setNames(trainFeat_AL, names)
      new_trainLabels_AL <- trainLabels_AL
      
      # tmp_new_tunedSVM_SL <- new_bestModel
      
      newSize_for_iter = newSize #sampleSize/10 # or just 4
      # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
      
      trainStart.time <- Sys.time()
      
      # # **********************
      # # **********************
      
      # Add predicted labels to the features data set
      SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(new_bestModel, upd_dataCurFeatsub)), objInfoNames)
      
      sampledResult <- self_learn_AL(
        # testFeatsub, testLabels, boundMargin=c(best_boundMargin),
        SVtotal, objInfoNames, SVL_variables,
        # validateFeatsub,validateLabels,
        upd_dataCurFeatsub,upd_dataCurLabels,
        # realiz=1, s_size=3, 
        plot_flag = model_name_AL_MS_tSNE
      )
      sampled_data <- sampledResult$sampled_data
      reference_label <- sampledResult$best_updCur_Labels
      
      # # **********************
      # # **********************
      
      d.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
      cat("computing distances required ", d.time,"sec\n",sep="")
      ALSamplesStart.time <- Sys.time()
      result <- add_AL_samples(sampled_data,
                               sampled_data[,1:numFeat], reference_label,
                               new_trainFeat_AL, new_trainLabels_AL,
                               newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
                               upd_dataCur$ID_unit,tSNE_flag = TRUE, flag_cluster = TRUE, plot_flag  = model_name_AL_MS_tSNE)
      ALS.time <- round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1)
      cat("getting active-labeled samples and updated datasets required ", ALS.time,"sec\n",sep="")
      # Extract new datasets
      # upd_dataCurFeatsub <- result$features
      # upd_dataCurLabels <- result$labels
      # upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL
      # semiAL_tot <- result$semi_samples
      # semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit
      
      # **********************
      # get al train set portion
      new_trainFeat_AL <- rbind(new_trainFeat_AL[,], setNames(new_trainFeat, names))
      new_trainLabels_AL <- unlist(list(new_trainLabels_AL[], new_trainLabels))
      
      # **********************
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(new_trainFeat_AL)
      indexTrainData = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(new_trainFeat_AL,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(new_trainLabels_AL, testLabels))
      
      AL_MS_tSNE_tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      
      tmp_pred = predict(AL_MS_tSNE_tunedSVM, validateFeatsub)
      cm_AL_MS_tSNE  = confusionMatrix(tmp_pred, validateLabels)
      train.timeALv2_tSNE_VSVMSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time+d.time, 1)
      
      cat(model_name_AL_MS_tSNE," accuracy: ",round(cm_AL_MS_tSNE$overall["Accuracy"],5),"\n",sep="")
      # **********************
      
      acc_AL_MS_tSNE = (cm_AL_MS_tSNE$overall["Accuracy"])
      
      # best_resample = resampledSize[rS]
      best_newSize4iter = newSize_for_iter
      best_classSize = classSize[clS]
      best_cluster = clusterSizes[cS]
      
      
      cat(model_name_AL_MS_tSNE," accuracy: ",round(cm_AL_MS_tSNE$overall["Accuracy"],5)," | execution time: ",train.timeALv2_tSNE_VSVMSL,"sec\n",sep="")#
      
      AccuracyAL_MS_tSNE[realization,sample_size+1] = as.numeric((cm_AL_MS_tSNE$overall["Accuracy"]))
      KappaAL_MS_tSNE[realization,sample_size+1] = as.numeric((cm_AL_MS_tSNE$overall["Kappa"]))
      SVsAL_MS_tSNE[realization,sample_size+1] = as.numeric(length(AL_MS_tSNE_tunedSVM$finalModel@SVindex))
      
      
      cat("\n") ###############################  AL MS + semi-AL + Train-AL #######################################
      model_name_AL_MS_semiAL = "AL_MS+kmeans+semiAL_SVM"
      model_name_AL_MS = "AL_MS+kmeans_SVM"
      
      cat("active labeling ",model_name_AL_MS," | ",length(trainLabels_AL)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")
      
      trainStart.time <- Sys.time()
      
      
      
      ALSamplesStart.time <- Sys.time()
      result <- add_AL_samples(sampled_data,
                               sampled_data[,1:numFeat], reference_label,
                               # new_trainFeat_AL, new_trainLabels_AL,
                               trainFeat_AL, trainLabels_AL,
                               newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
                               upd_dataCur$ID_unit, flag_cluster = TRUE, newSize2=b[bb]*nclass) #,
      ALS.time <- round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1)
      cat("getting active-labeled samples and updated datasets required ", ALS.time,"sec\n",sep="")
      # Extract new datasets
      # upd_dataCurFeatsub <- result$features
      # upd_dataCurLabels <- result$labels
      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL
      semiAL_tot <- result$semi_samples
      semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit
      
      
      
      
      
      
      
      
      
      
      # ## ADD ALSO THIS VERSION OF AL-S3VSVM
      # 
      # bb = 1 # for (bb in seq(along=b)) {
      # 
      # # Definition of sampling configuration (strata:random sampling without replacement)
      # stratSampRemaining_b = strata(trainDataCurRemaining_AL, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      # 
      # # get samples of trainDataCurRemaining_AL and set trainDataCurRemaining_AL new
      # samplesRemaining_b = getdata(trainDataCurRemaining_AL, stratSampRemaining_b)
      # 
      # trainDataCurRemaining_AL <- trainDataCurRemaining_AL[-c(samplesRemaining_b$ID_unit), ]
      # 
      # trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      # trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]
      # 
      # REF_b = predict(bestFittingModel, trainDataCurRemainingsub_SL)
      # 
      # # get SV of unlabeled samples
      # indexUn = 1:nrow(trainDataCurRemainingsub_SL)
      # totalUn = trainDataCurRemaining_SL[indexUn ,c(sindexSVMDATA:eindexSVMDATA)]
      # totalUn = cbind(totalUn, REF_b)
      # 
      # ##
      
      
      
      
      
      
      
      
      # **********************
      # get al train set portion
      
      trainFeat_AL <- rbind(trainFeat_AL[,], setNames(new_trainFeat, names))
      trainLabels_AL <- unlist(list(trainLabels_AL[], new_trainLabels))
      SVtotal = setNames(cbind(trainFeat_AL, trainLabels_AL),c(objInfoNames[-length(objInfoNames)],"REF"))
      
      # new_trainFeat_AL <- rbind(new_trainFeat_AL[,], setNames(new_trainFeat, names))
      # new_trainLabels_AL <- unlist(list(new_trainLabels_AL[], new_trainLabels))
      # SVtotal = setNames(cbind(new_trainFeat_AL, new_trainLabels_AL),c(objInfoNames[-length(objInfoNames)],"REF"))
      # **********************
      
      
      
      
      
      # **********************
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(trainFeat_AL)
      indexTrainData = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(trainFeat_AL,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(trainLabels_AL, testLabels))
      
      AL_MS_tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      tmp_pred = predict(AL_MS_tunedSVM, validateFeatsub)
      cm_AL_MS  = confusionMatrix(tmp_pred, validateLabels)
      cat(model_name_AL_MS," accuracy: ",round(cm_AL_MS$overall["Accuracy"],5),"\n",sep="")
      # **********************
      
      
      
      
      
      
      REF_ud = new_trainLabels
      SVtotal_ud = cbind(new_trainFeat, REF_ud)
      
      SVL_variables = list(
        # list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
        # list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
        # list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
        # list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
        # list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
        # list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
        # list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud)),
        # list(SVtotal_ud, SVL10 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], REF_ud)),
        # list(SVtotal_ud, SVL11 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], REF_ud)),
        list(semiAL_tot, sAL2=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL3=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL5=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL6=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL7=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL8=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL9=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL10 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)])),
        list(semiAL_tot, sAL11 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)]))
        
      ) #    =c(0.01, 0.3, 0.9)      =c(1.5, 1, 0.5)
      
      upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin=c(1), model_name_AL_MS_semiAL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                 SVL_variables, AL_MS_tunedSVM$finalModel)
      AL_MS_semiAL_bestFittingModel <- upd_SLresult$bestFittingModel
      tmp_pred = predict(AL_MS_semiAL_bestFittingModel, validateFeatsub)
      cm_AL_MS_semiAL  = confusionMatrix(tmp_pred, validateLabels)
      train.timeALv2_SEMI_VSVMSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time+d.time, 1)
      
      
      
      accVSVM_ALv2_TSL2 = cm_AL_MS$overall["Accuracy"]
      accVSVM_ALv2_TSL = cm_AL_MS_semiAL$overall["Accuracy"]
      # best_resample = resampledSize[rS]
      # best_classSize = classSize[clS]
      # best_cluster = clusterSizes[cS]

      
      cat(model_name_AL_MS_semiAL," accuracy: ",round(cm_AL_MS_semiAL$overall["Accuracy"],5)," | execution time: ",train.timeALv2_SEMI_VSVMSL,"sec\n",sep="")
      
      AccuracyAL_MS[realization,sample_size+1] = as.numeric((cm_AL_MS$overall["Accuracy"]))
      KappaAL_MS[realization,sample_size+1] = as.numeric((cm_AL_MS$overall["Kappa"]))
      SVsAL_MS[realization,sample_size+1] = as.numeric(length(AL_MS_tunedSVM$finalModel@SVindex))
      
      AccuracyAL_MS_semiAL[realization,sample_size+1] = as.numeric((cm_AL_MS_semiAL$overall["Accuracy"]))
      KappaAL_MS_semiAL[realization,sample_size+1] = as.numeric((cm_AL_MS_semiAL$overall["Kappa"]))
      SVsAL_MS_semiAL[realization,sample_size+1] = as.numeric(length(AL_MS_semiAL_bestFittingModel$finalModel@SVindex))
      
      # *********************************************************************
      # get original SVs of base SVM
      SVindex_ud = AL_MS_tunedSVM$finalModel@SVindex
      
      # get new al train set portion
      # trainFeat_AL <- new_trainFeat_AL # [SVindex_ud,]
      # trainLabels_AL <- new_trainLabels_AL # [SVindex_ud]
      
      # trainDataCur <- rbind(trainDataCur, upd_dataCur[upd_SVindex_ud, 1:ncol(trainDataCur)]) # IS IT REQUIRED WHEN WE ITERATE ?
      
      trainDataCurRemaining_AL <- trainDataCurRemaining_AL[!upd_SVindex_ud, ] # IS IT REQUIRED ?
      
      if(acc_AL_MCLU>best_acc){
        best_acc <- acc_AL_MCLU
        best_model_name <- model_name_AL_MCLU
        new_bestModel <- AL_MCLU_tunedSVM
        best_train.time <- train.timeALv1_tSNE_VSVMSL-best_train.time
      }
      
      if(acc_AL_MS_tSNE>best_acc){ 
        best_acc <- acc_AL_MS_tSNE
        best_model_name <- model_name_AL_MS_tSNE
        new_bestModel <- AL_MS_tSNE_tunedSVM
        best_train.time <- train.timeALv2_tSNE_VSVMSL-best_train.time
      }
      
      if(cm_AL_MS$overall["Accuracy"]>best_acc){
        best_acc <- cm_AL_MS$overall["Accuracy"]
        best_model_name <- model_name_AL_MS
        new_bestModel <- AL_MS_tunedSVM
        best_train.time <- train.timeALv2_SEMI_VSVMSL-best_train.time
      }
      
      if(cm_AL_MS_semiAL$overall["Accuracy"]>best_acc){
        best_acc <- cm_AL_MS_semiAL$overall["Accuracy"]
        best_model_name <- model_name_AL_MS_semiAL
        new_bestModel <- AL_MS_semiAL_bestFittingModel
        best_train.time <- train.timeALv2_SEMI_VSVMSL-best_train.time
      }
      # *********************************************************************
      
      
      
      
      cat("\n") ###############################  AL-SVM #####################################
      # "AL_SVM" IS EQUAL TO "AL_MS+kmeans_SVM"
      # model_name_AL_tunedSVM = "AL_SVM"
      # 
      # cat("training ",model_name_tunedSVM,"\n")
      # trainStart.time <- Sys.time()
      # tunedALSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      # trainALSVM.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
      # 
      # # run classification and accuracy assessment for unmodified SV and predict labels of test data
      # predLabelsALSVM = predict(tunedALSVM, validateFeatsub)
      # cm_AL_SVM = confusionMatrix(predLabelsALSVM, validateLabels)
      # cat(model_name_tunedALSVM," accuracy: ",round(cm_AL_SVM$overall["Accuracy"],5)," | execution time: ",trainALSVM.time,"sec\n",sep="")
      # AccuracyALSVM[realization,sample_size] = as.numeric(cm_AL_SVM$overall["Accuracy"])
      # KappaALSVM[realization,sample_size] = as.numeric(cm_AL_SVM$overall["Kappa"])
      # SVsALSVM[realization,sample_size] = as.numeric(length(tunedALSVM$finalModel@SVindex))
      # tr.timeALSVM[realization,sample_size] = trainSVM.time
      
      # get original SVs of base SVM ************************* prevent removing an entire class label
      ALSVindex = AL_MS_tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
      ALSVtotal = trainDataCurRemaining_AL[ALSVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_AL))]
      freq_table <- table(ALSVtotal$REF)
      zero_label_classes <- names(freq_table[freq_table == 0])
      if (length(zero_label_classes) > 0) {
        for (class in zero_label_classes) {
          # Find the indices of rows in train DataCur with the zero label class
          class_indices <- which(trainLabels_AL == class)
          # Extract the corresponding rows from trainFeat and trainLabels
          class_feat <- trainFeat_AL[class_indices, , drop = FALSE]
          class_labels <- trainLabels_AL[class_indices, drop = FALSE]
          # Create a data frame with the same structure as SVtotal
          class_data <- cbind(class_feat, class_labels)
          # Append the class_data to SVtotal
          ALSVtotal <- rbind(setNames(ALSVtotal,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")), setNames(class_data,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")))
          ALSVindex <- c(ALSVindex, class_indices)
        }
      }
      # ******************************************************
      binaryClassProblem = list()
      for (jj in seq_along(AL_MS_tunedSVM$finalModel@xmatrix)) { # COMPARE EVERY COUPLE COMBINATION OF CLASSES
        binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCurRemaining_AL[AL_MS_tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCurRemaining_AL)]))
      }
      # ******************************************************
      names = objInfoNames[1:(length(objInfoNames)-1)]
      
      cat("\n") ###############################  AL-SVM-SL + semi-labeled samples #####################################
      model_name_AL_SVMUn = "AL_SVM_SLUn"
      
      trainStart.time <- Sys.time()
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining = strata(trainDataCurRemaining_AL, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
      
      # get samples of trainDataCurRemaining_AL and set trainDataCurRemaining_AL new
      samplesRemainingSVM = getdata(trainDataCurRemaining_AL, stratSampRemaining)
      trainDataCurRemaining_AL <- trainDataCurRemaining_AL[-c(samplesRemainingSVM$ID_unit), ]
      
      trainDataCurRemainingSVM_Un = samplesRemainingSVM[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemainingSVM_Unsub_b = trainDataCurRemainingSVM_Un[sindexSVMDATA:eindexSVMDATA]
      
      REFALSVM = predict(AL_MS_tunedSVM, trainDataCurRemainingSVM_Unsub_b)
      
      # get SV of unlabeled samples
      SVindexALSVMUn = 1:nrow(trainDataCurRemainingSVM_Unsub_b)
      SVtotalALSVMUn = trainDataCurRemainingSVM_Un[SVindexALSVMUn ,c(sindexSVMDATA:eindexSVMDATA)]
      SVtotalALSVMUn = cbind(SVtotalALSVMUn, REFALSVM)
      
      cat("evaluation of SVM with self learning and semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      SVL_variables = list(
        list(SVtotalALSVMUn, SVL2SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL3SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM)),
        list(SVtotalALSVMUn, SVL5SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL6SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL7SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL8SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL9SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL10SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL11SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexALSVMUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFALSVM))
      )
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_SVMUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingALModelSVMUn <- SLresult$bestFittingModel
      best_trainFeatALSVMUn <- SLresult$best_trainFeatVSVM
      best_trainLabelsALSVMUn <- SLresult$best_trainLabelsVSVM
      best_boundMarginALSVMUn <- SLresult$best_boundMargin
      # best_boundSVM_SL_Un = SLresult$best_bound
      # best_boundMarginSVM_SL_Un = SLresult$best_boundMargin
      t.timeALSVMUn <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsALSVMsumUn = predict(bestFittingALModelSVMUn, validateFeatsub)
      cm_AL_SVM_SL_Un = confusionMatrix(predLabelsALSVMsumUn, validateLabels)
      cat("ALSVM_SL_Un accuracy: ",round(cm_AL_SVM_SL_Un$overall["Accuracy"],5)," | execution time: ",t.timeALSVMUn,"sec\n",sep="")
      
      AccuracyALSVM_SL_Un[realization,sample_size+1] = as.numeric(cm_AL_SVM_SL_Un$overall["Accuracy"])
      KappaALSVM_SL_Un[realization,sample_size+1] = as.numeric(cm_AL_SVM_SL_Un$overall["Kappa"])
      SVsALSVM_SL_Un[realization,sample_size+1] = as.numeric(length(bestFittingALModelSVMUn$finalModel@SVindex))
      if (cm_AL_SVM_SL_Un$overall["Accuracy"]>best_acc) {
        best_acc <- cm_AL_SVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingALModelSVMUn
        # new_bestTrainFeatSVM <- best_trainFeatSVMUn
        # new_bestTrainLabelsSVM <- best_trainLabelsSVMUn
        # best_boundMargin <- best_boundMarginSVMUn
        best_model_name <- model_name_ALSVMUn
        best_train.time <- t.timeALSVMUn
      }
      
       cat("\n") ###############################  AL-VSVM-SL ################################################
      model_name_AL_VSVM_SL = "AL_VSVM_SL"
      
      trainStart.time <- Sys.time()
      
      SVL2 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
      SVL3 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]
      
      SVL5 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
      SVL6 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
      SVL7 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
      SVL8 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
      SVL9 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
      SVL10 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
      SVL11 = trainDataCurRemaining_AL[ALSVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]
      
      SVinvar = rbind(setNames(ALSVtotal,objInfoNames),
                      setNames(SVL2,objInfoNames),
                      setNames(SVL3,objInfoNames),
                      setNames(SVL5,objInfoNames),
                      setNames(SVL6,objInfoNames),
                      setNames(SVL7,objInfoNames),
                      setNames(SVL8,objInfoNames),
                      setNames(SVL9,objInfoNames),
                      setNames(SVL10,objInfoNames),
                      setNames(SVL11,objInfoNames)
      )
      
      cat("evaluation of VSVM with self learning | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      SVL_variables = list(
        list(ALSVtotal, SVL2),
        list(ALSVtotal, SVL3),
        list(ALSVtotal, SVL5),
        list(ALSVtotal, SVL6),
        list(ALSVtotal, SVL7),
        list(ALSVtotal, SVL8),
        list(ALSVtotal, SVL9),
        list(ALSVtotal, SVL10),
        list(ALSVtotal, SVL11)
      )
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_VSVM_SL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingALModel <- SLresult$bestFittingModel
      best_trainFeatALVSVM <- SLresult$best_trainFeatVSVM
      best_trainLabelsALVSVM <- SLresult$best_trainLabelsVSVM
      best_bound_SL = SLresult$best_bound
      best_boundMargin_SL = SLresult$best_boundMargin
      t.timeALSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+trainSVM.time, 1)
      
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsALVSVMsum = predict(bestFittingALModel, validateFeatsub)
      cm_AL_VSVM_SL = confusionMatrix(predLabelsALVSVMsum, validateLabels)
      cat("AL VSVM_SL accuracy: ",round(cm_AL_VSVM_SL$overall["Accuracy"],5)," | execution time: ",t.timeALSL,"sec\n",sep="")
      
      AccuracyALVSVM_SL[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL$overall["Accuracy"])
      KappaALVSVM_SL[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL$overall["Kappa"])
      SVsALVSVM_SL[realization,sample_size+1] = as.numeric(length(bestFittingALModel$finalModel@SVindex))
      if (cm_AL_VSVM_SL$overall["Accuracy"]>best_acc) {
        best_acc <- cm_AL_VSVM_SL$overall["Accuracy"]
        new_bestModel <- bestFittingALModel
        # new_bestTrainFeatVSVM <- best_trainFeatVSVM
        # new_bestTrainLabelsVSVM <- best_trainLabelsVSVM
        # best_boundMargin <- best_boundMargin_SL
        best_model_name <- model_name_VSVM_SL
        best_train.time <- t.timeALSL
      }
      
      
      cat("\n") ###############################  AL-VSVM-SL + semi-labeled samples #####################################
      model_name_AL_Un = "ALSVM_SLUn"
      
      trainStart.timeUn <- Sys.time()
      actAcc_ALvUn = -1e-6
      bb = 1 # for (bb in seq(along=b)) {
      
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining_b = strata(trainDataCurRemaining_AL, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      
      # get samples of trainDataCurRemaining_AL and set trainDataCurRemaining_AL new
      samplesRemaining_b = getdata(trainDataCurRemaining_AL, stratSampRemaining_b)
      
      trainDataCurRemaining_AL <- trainDataCurRemaining_AL[-c(samplesRemaining_b$ID_unit), ]
      
      trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]
      
      REF_b = predict(bestFittingALModel, trainDataCurRemainingsub_SL)
      
      # get SV of unlabeled samples
      indexUn = 1:nrow(trainDataCurRemainingsub_SL)
      totalUn = trainDataCurRemaining_SL[indexUn ,c(sindexSVMDATA:eindexSVMDATA)]
      totalUn = cbind(totalUn, REF_b)
      
      cat("evaluation of VSVM SL with ",b[bb]," semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      # get VSs, means rows of SV but with subset on different level
      SVL_variables = list(
        list(ALSVtotal, SVL2),
        list(ALSVtotal, SVL3),
        list(ALSVtotal, SVL5),
        list(ALSVtotal, SVL6),
        list(ALSVtotal, SVL7),
        list(ALSVtotal, SVL8),
        list(ALSVtotal, SVL9),
        list(ALSVtotal, SVL10),
        list(ALSVtotal, SVL11),
        list(totalUn, L2Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)),
        list(totalUn, L3Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)),
        list(totalUn, L5Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)),
        list(totalUn, L6Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)),
        list(totalUn, L7Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)),
        list(totalUn, L8Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)),
        list(totalUn, L9Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)),
        list(totalUn, L10Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)),
        list(totalUn, L11Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b))
      )
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_Un, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingALModelUn <- SLresult$bestFittingModel
      best_trainFeatALVSVMUn <- SLresult$best_trainFeatVSVM
      best_trainLabelsALVSVMUn <- SLresult$best_trainLabelsVSVM
      # best_bound_SL_Un = SLresult$best_bound
      best_boundMargin_SL_Un = SLresult$best_boundMargin
      trainALUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsALVSVMsumUn = predict(bestFittingALModelUn, validateFeatsub)
      cm_AL_VSVM_SL_Un = confusionMatrix(predLabelsALVSVMsumUn, validateLabels)
      cat("AL VSVM_SL_Un accuracy: ",round(cm_AL_VSVM_SL_Un$overall["Accuracy"],5)," | execution time: ",trainALUn.time,"sec","\n",sep="")
      
      AccuracyALVSVM_SL_Un[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL_Un$overall["Accuracy"])
      KappaALVSVM_SL_Un[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL_Un$overall["Kappa"])
      SVsALVSVM_SL_Un[realization,sample_size+1] = as.numeric(length(bestFittingALModelUn$finalModel@SVindex))
      if (cm_AL_VSVM_SL_Un$overall["Accuracy"] > best_acc) {
        best_acc <- cm_AL_VSVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingALModelUn
        # new_bestTrainFeatVSVM <- best_trainFeatVSVMUn
        # new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn
        # best_boundMargin <- best_boundMargin_SL_Un
        best_model_name <- model_name_Un
        best_train.time <- trainALUn.time
      }
      
      
      cat("\n") ###############################  AL-VSVM-SL + virtual semi-labeled samples ##################################
      model_name_AL_vUn = "AL_VSVM_SLvUn"
      
      REF_v = predict(bestFittingALModelUn, trainDataCurRemainingsub_SL)
      
      # get SV of unlabeled samples
      indexvUn_v = 1:nrow(trainDataCurRemainingsub_SL) #  bestFittingModelUn$finalModel@SVindex
      SVtotalvUn_v = trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA)] #na.omit(trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_SL))])
      SVtotalvUn_v = cbind(SVtotalvUn_v, REF_v)
      
      # extracting previously assigned reference column
      SVtotalvUn_vFeat = SVtotalvUn_v[,1:(ncol(SVtotalvUn_v)-1)] # -1)]
      REF_v = SVtotalvUn_v[,(ncol(SVtotalvUn_v))]
      SVtotalvUn_v = cbind(SVtotalvUn_vFeat, REF_v)
      
      cat("evaluation of AL VSVM SL with ",b[bb]," virtual semi-labeled samples"," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="") #  [",bb,"/",length(b),"]","
      
      SVL_variables = list(
        list(ALSVtotal, SVL2),
        list(ALSVtotal, SVL3),
        list(ALSVtotal, SVL5),
        list(ALSVtotal, SVL6),
        list(ALSVtotal, SVL7),
        list(ALSVtotal, SVL8),
        list(ALSVtotal, SVL9),
        list(ALSVtotal, SVL10),
        list(ALSVtotal, SVL11),
        list(SVtotalvUn_v, SVL2vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)),
        list(SVtotalvUn_v, SVL3vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)),
        list(SVtotalvUn_v, SVL5vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL6vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL7vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL8vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL9vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL10vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)),
        list(SVtotalvUn_v, SVL11vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v))
      )
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_vUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables)
      bestFittingALModelvUn <- SLresult$bestFittingModel
      # new_best_trainFeatVSVMvUn <- SLresult$best_trainFeatVSVM
      # new_best_trainLabelsVSVMvUn <- SLresult$best_trainLabelsVSVM
      new_best_bound_SLvUn = SLresult$best_bound
      # new_best_boundMargin_SLvUn = SLresult$best_boundMargin
      trainALvUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      new_predLabelsALVSVMvUnsum = predict(bestFittingALModelvUn, validateFeatsub)
      cm_AL_VSVM_SL_vUn = confusionMatrix(new_predLabelsALVSVMvUnsum, validateLabels)
      
      cat("AL VSVM_SL_vUn accuracy: ",round(cm_AL_VSVM_SL_vUn$overall["Accuracy"],5)," | execution time: ",trainALvUn.time,"sec","\n",sep="")
      
      AccuracyALVSVM_SL_vUn[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL_vUn$overall["Accuracy"])
      KappaALVSVM_SL_vUn[realization,sample_size+1] = as.numeric(cm_AL_VSVM_SL_vUn$overall["Kappa"])
      SVsALVSVM_SL_vUn[realization,sample_size+1] = as.numeric(length(bestFittingALModelvUn$finalModel@SVindex))
      if (cm_AL_VSVM_SL_vUn$overall["Accuracy"] > best_acc){
        best_acc <- cm_AL_VSVM_SL_vUn$overall["Accuracy"]
        new_bestModel <- bestFittingALModelvUn
        # new_bestTrainFeatVSVM <- best_trainFeatVSVMvUn
        # new_bestTrainLabelsVSVM <- best_trainLabelsVSVMvUn
        # best_boundMargin <- best_boundMargin_SLvUn
        best_train.time <- trainALvUn.time
        best_model_name <- model_name_vUn
      }
      #######################################################################################################
      
      
    }
    


    cat("\n") #################################  End sample portion #########################################
    if (sample_size==length(sampleSizePor)) {
      saveRDS(tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelSVMUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelvUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      # saveRDS(  if(nR>realization){, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModelSVMUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModelUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModelvUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(AL_MCLU_tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MCLU,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(AL_MS_tSNE_tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MS_tSNE,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(AL_MS_tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MS,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(AL_MS_semiAL_bestFittingModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MS_semiAL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(cm_SVM,paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_SVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL_vUn, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(cm_AL_MCLU, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MCLU,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_MS_tSNE, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MS_tSNE,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_MS, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MS,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_MS_semiAL, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MS_semiAL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      # saveRDS(cm_AL_SVM,paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_SVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL_vUn, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_realiz",realization,"_",seed,"seed.rds"))
    }
  }
  cat("\n") ###################################  End realization ############################################
  # Store hyperparameters 
  best_model_oa=c(best_model_oa,best_model_name,": ",as.numeric(best_acc),"\n")
  trainSVMUn.time_oa = trainSVMUn.time_oa+t.timeSVMUn
  trainSL.time_oa = trainSL.time_oa+t.timeSL
  trainUn.time_oa = trainUn.time_oa+trainUn.time
  trainvUn.time_oa = trainvUn.time_oa+trainvUn.time
  train.timeALv1_VSVMSL_oa = train.timeALv1_VSVMSL_oa+train.timeALv1_tSNE_VSVMSL
  train.timeALv2_SEMI_VSVMSL_oa = train.timeALv2_SEMI_VSVMSL_oa+train.timeALv2_SEMI_VSVMSL
  train.timeALv2_tSNE_VSVMSL_oa = train.timeALv2_tSNE_VSVMSL_oa+train.timeALv2_tSNE_VSVMSL
  time.taken_iter = c(time.taken_iter, c("Realization ",realization," | seed: ",seed," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h"),"\n")
  cat("Realization ",realization," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h\n\n")
} 
if (length(sampleSizePor)>=2) {
  time.taken_oa <- round(as.numeric((Sys.time() - start.time_oa), units = "hours"), 2)
  if(nR>realization){
    AccuracySVM=AccuracySVM[1:(realization-1),]
    AccuracySVM_SL_Un=AccuracySVM_SL_Un[1:(realization-1),]
    AccuracyVSVM_SL=AccuracyVSVM_SL[1:(realization-1),]
    AccuracyVSVM_SL_Un=AccuracyVSVM_SL_Un[1:(realization-1),]
    AccuracyVSVM_SL_vUn=AccuracyVSVM_SL_vUn[1:(realization-1),]
    
    AccuracyAL_MCLU[1:(realization-1),]
    AccuracyAL_MS_tSNE=AccuracyAL_MS_tSNE[1:(realization-1),]
    AccuracyAL_MS=AccuracyAL_MS[1:(realization-1),]
    AccuracyAL_MS_semiAL=AccuracyAL_MS_semiAL[1:(realization-1),]
    
    AccuracyALSVM_SL_Un=AccuracyALSVM_SL_Un[1:(realization-1),]
    AccuracyALVSVM_SL=AccuracyALVSVM_SL[1:(realization-1),]
    AccuracyALVSVM_SL_Un=AccuracyALVSVM_SL_Un[1:(realization-1),]
    AccuracyALVSVM_SL_vUn=AccuracyALVSVM_SL_vUn[1:(realization-1),]
    
    
    KappaSVM=KappaSVM[1:(realization-1),]
    KappaSVM_SL_Un=KappaSVM_SL_Un[1:(realization-1),]
    KappaVSVM_SL=KappaVSVM_SL[1:(realization-1),]
    KappaVSVM_SL_Un=KappaVSVM_SL_Un[1:(realization-1),]
    KappaVSVM_SL_vUn=KappaVSVM_SL_vUn[1:(realization-1),]
    
    KappaAL_MCLU=KappaAL_MCLU[1:(realization-1),]
    KappaAL_MS_tSNE=KappaAL_MS_tSNE[1:(realization-1),]
    KappaAL_MS=KappaAL_MS[1:(realization-1),]
    KappaAL_MS_semiAL=KappaAL_MS_semiAL[1:(realization-1),]
    
    KappaALSVM_SL_Un=KappaALSVM_SL_Un[1:(realization-1),]
    KappaALVSVM_SL=KappaALVSVM_SL[1:(realization-1),]
    KappaALVSVM_SL_Un=KappaALVSVM_SL_Un[1:(realization-1),]
    KappaALVSVM_SL_vUn=KappaALVSVM_SL_vUn[1:(realization-1),]
    
    
    SVsSVM=SVsSVM[1:(realization-1),]
    SVsSVM_SL_Un=SVsSVM_SL_Un[1:(realization-1),]
    SVsVSVM_SL=SVsVSVM_SL[1:(realization-1),]
    SVsVSVM_SL_Un=SVsVSVM_SL_Un[1:(realization-1),]
    SVsVSVM_SL_vUn=SVsVSVM_SL_vUn[1:(realization-1),]
    
    SVsAL_MCLU=SVsAL_MCLU[1:(realization-1),]
    SVsAL_MS=SVsAL_MS[1:(realization-1),]
    SVsAL_MS_tSNE=SVsAL_MS_tSNE[1:(realization-1),]
    SVsAL_MS_semiAL=SVsAL_MS_semiAL[1:(realization-1),]
  }
  setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
  save(AccuracySVM, 
       AccuracySVM_SL_Un,
       AccuracyVSVM_SL,
       AccuracyVSVM_SL_Un,
       AccuracyVSVM_SL_vUn,
       
       AccuracyAL_MCLU,
       AccuracyAL_MS_tSNE,
       AccuracyAL_MS,
       AccuracyAL_MS_semiAL,
       
       # AccuracyALSVM, 
       AccuracyALSVM_SL_Un,
       AccuracyALVSVM_SL,
       AccuracyALVSVM_SL_Un,
       AccuracyALVSVM_SL_vUn,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_acc_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  save(KappaSVM, 
       KappaSVM_SL_Un,
       KappaVSVM_SL,
       KappaVSVM_SL_Un,
       KappaVSVM_SL_vUn,
       
       KappaAL_MCLU,
       KappaAL_MS_tSNE,
       KappaAL_MS,             
       KappaAL_MS_semiAL,
       
       # KappaALSVM, 
       KappaALSVM_SL_Un,
       KappaALVSVM_SL,
       KappaALVSVM_SL_Un,
       KappaALVSVM_SL_vUn,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_Kappa_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  save(SVsSVM, 
       SVsSVM_SL_Un,
       SVsVSVM_SL,
       SVsVSVM_SL_Un,
       SVsVSVM_SL_vUn,
       
       SVsAL_MCLU,
       SVsAL_MS_tSNE,
       SVsAL_MS,
       SVsAL_MS_semiAL,
       
       # SVsALSVM, 
       SVsALSVM_SL_Un,
       SVsALVSVM_SL,
       SVsALVSVM_SL_Un,
       SVsALVSVM_SL_vUn,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_SVs_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  # save(tr.timeSVM, tr.timeSVM_SL_Un,tr.timeVSVM_SL,tr.timeVSVM_SL_Un,tr.timeVSVM_SL_vUn,
  #      tr.timeVSVM_SL_Un_it,# tr.timeVSVM_SL_Un_itSL,# tr.timeVSVM_SL_Un_itTSL,
  #      tr.timeVSVM_SL_Un_itSL2,tr.timeVSVM_SL_Un_itTSL2,
  #      file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_trainTime_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData"))
  
  cat("OA Execution time: ", time.taken_oa, "h\n", time.taken_iter,"\n",best_model_oa,
      
      "\n",model_name_SVMUn," training time: ",trainSVMUn.time_oa/realization, "sec",
      "\n",model_name_VSVM_SL," training time: ",trainSL.time_oa/realization, "sec",
      "\n",model_name_Un," training time: ",trainUn.time_oa/realization, "sec",
      "\n",model_name_vUn," training time: ",trainvUn.time_oa/realization, "sec",
      
      "\n",model_name_AL_MCLU," training time: ",train.timeALv1_VSVMSL_oa/realization, "sec",
      "\n",model_name_AL_MS_semiAL," training time: ",train.timeALv2_SEMI_VSVMSL_oa/realization, "sec\n",
      
      # "\nbest_resample_oa: ", best_resample_oa, "\nbest_newSize_oa: ", best_newSize_oa,"\nbest_classSize_oa: ", best_classSize_oa,  "\nbest_cluster_oa: ",best_cluster_oa,
      
      "\nNumber of ", model_name_tunedSVM ," SVs: ",length(tunedSVM$finalModel@SVindex),
      "\nNumber of ", model_name_SVMUn ," SVs: ",length(bestFittingModelSVMUn$finalModel@SVindex),
      "\nNumber of ", model_name_VSVM_SL ," SVs: ",length(bestFittingModel$finalModel@SVindex),
      "\nNumber of ", model_name_Un ," SVs: ",length(bestFittingModelUn$finalModel@SVindex),
      "\nNumber of ", model_name_vUn ," SVs: ",length(bestFittingModelvUn$finalModel@SVindex),
      
      "\nNumber of ", model_name_AL_MS_tSNE ," SVs: ",length(AL_MS_tSNE_tunedSVM$finalModel@SVindex),
      "\nNumber of ", model_name_AL_MS ," SVs: ",length(AL_MS_tunedSVM$finalModel@SVindex),
      "\nNumber of ", model_name_AL_MS_semiAL ," SVs: ",length(AL_MS_semiAL_bestFittingModel$finalModel@SVindex),
      "\nNumber of ", model_name_AL_MCLU ," SVs: ", length(AL_MCLU_tunedSVM$finalModel@SVindex),
      
      "\nNumber of final train Labels AL: ",length(trainLabels_AL),
      sep = "", file = paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_metadata_",script,"_",city,"_",model_prob,"_",invariance,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.txt")
      )
  print(confusionMatrix(new_trainLabels,predict(bestFittingModel, new_trainFeat)))
  cat("Number of ", model_name_AL_MS_semiAL ," SVs: ",length(AL_MS_semiAL_bestFittingModel$finalModel@SVindex),"\nNumber of ", model_name_AL_MCLU ," SVs: ", length(AL_MCLU_tunedSVM$finalModel@SVindex),"\nNumber of final train Labels AL: ",length(trainLabels_AL),"\n\n",sep="")
  cat("performance results: acquired\n\n\n")
}

