script = "AQ-S3VSVM"  
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

nR = 20                   # number of realizations
city = "cologne"    # cologne or hagadera location
invariance = "scale"   # scale or shape invariance
model_prob = "multiclass" # multiclass or binary problem

b = c(20, 40, 60)                  # size of balanced_unlabeled_samples per class -> use only 20
bound = c(0.3, 0.6, 0.9)           # radius around SV - threshold       
boundMargin = c(1.5, 1, 0.5)     # distance from hyperplane - threshold   
sampleSizePor = c(24, 60, 96, 180, 276, 360, 480, 600) # c(24, 36, 48,  72, 120, 240, 480, 600) 
# sampleSizePor = c(120) only one sample size for plotting the thematic map for two setup

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
  # if(showPrg){cat("running coarse grid search and narrow grid search...\n")}
  svmFitCoarse = train(x, y, 
                       method = "svmRadial",
                       metric = metric, 
                       maximize = TRUE,
                       tuneGrid = coarseGrid,
                       trControl = trainControl ( 
                         method = "cv",
                         #verboseIter=T,
                         index = indexTrain,
                         indexFinal= indexTrain[[1]]
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
                       trControl = trainControl ( 
                         method = "cv",
                         #verboseIter=T,
                         index = indexTrain,
                         indexFinal= indexTrain[[1]], classProbs =  classProb
                       ),
                       scaled = FALSE
  )
  return(svmFitNarrow)  
}

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


rem_extrem_kerneldist = function(org, VSV1, a=0.7, kernel_func=tunedSVM$finalModel@kernelf){
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
          }
        }
      }
    }
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
    }
  }
  return(VSV1)
}

pred_one = function(modelfin, dataPoint, dataPointLabels, binaryClassProb=binaryClassProblem){
  smallestDistance = 9999
  
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
        pred = sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
          modelfin@kernelf(modelfin@xmatrix[[l]][j,], dataPoint[seq_along(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
        
        if(abs(pred) < abs(smallestDistance))
          smallestDistance = abs(pred)
      }
    }
  }
  return(smallestDistance)   
}

pred_all = function(modelfin, dataPoint, dataPointLabels, binaryClassProb=binaryClassProblem){
  smallestDistance <- 9999
  distance <- c()
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
        pred <- sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
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


# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING new_tunedSVM, predLabelsVSVM_unc
margin_sampling <- function(org, samp, pred_one,binaryClassProblem, classes=NA,
                            realiz=realization, s_size=sample_size, plot_flag=FALSE ) {
  if(is.na(classes)){classes=as.factor(levels(samp[, ncol(samp)]))}
  
  # Initialize data frame to store margin distance for each sample
  margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
  
  library(parallel)
  library(doParallel)
  library(foreach)
  
  # print(paste("Starting cluster with", num_cores, "cores"))
  showConnections(all = TRUE)
  
  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
  }
  
  # Clean up
  stopCluster(cl)
  registerDoSEQ()
  
  # Debug - after cleanup
  # print("Cluster stopped.")
  showConnections(all = TRUE)
  
  # scale distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # Assign scaled distances to probability dataframe
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


# add_AL_samples(distance_data=sampled_data,
#                features=sampled_data[,1:numFeat], ref=reference_label,
#                new_trainFeat_AL=trainFeat4AL_IT, new_trainLabels_AL=trainLabels4AL_IT,
#                newSize=newSize_for_iter, cluster=clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
#                ID_unit=upd_dataCur$ID_unit,tSNE_flag = TRUE, flag_cluster = TRUE, newSize2=b[bb]*nclass) #,


# add_AL_samples = function(distance_data,
#                           features=NULL, ref, 
#                           new_trainFeat_AL=NULL, new_trainLabels_AL=NULL,
#                           newSize=10, cluster=100, ID_unit=NULL, nFeat=numFeat, PCA_flag=FALSE, tSNE_flag=FALSE, 
#                           realiz=realization, s_size=sample_size, newSize2=20*nclass, plot_flag=FALSE, flag_cluster=FALSE, flag_class=FALSE){
#   if(cluster<newSize){cluster=round(max(newSize*1.01,nrow(distance_data)/10))}
# 
#   # merge features and original labels
#   ref_added = cbind(distance_data, ref)
#   
#   # order by most uncertain samples
#   ref_added_or = ref_added[order(ref_added$distance),]
#   
#   if(tSNE_flag) { flag_cluster=TRUE
#   # ********************** duplicates check
#   duplicate_rows <- duplicated(ref_added_or[, 1:nFeat])
#   num_duplicates <- sum(duplicate_rows)
#   if (num_duplicates > 0) {
#     # cat("Number of duplicate rows:", num_duplicates, "\n")
#     # Find indices of duplicates
#     duplicate_indices <- which(duplicate_rows)
# 
#     # Remove duplicates
#     ref_added_or <- ref_added_or[!duplicate_rows, ]
#     cat("Samples after removing duplicates:", nrow(ref_added_or), "\n")
#   }
#   # Perform t-SNE
#   tsne_result <- Rtsne(ref_added_or[, 1:nFeat], dims = 2, perplexity = 30, verbose = FALSE, max_iter = 500)
#   tsne_data <- data.frame(tsne_result$Y)
#   colnames(tsne_data) <- c("tSNE1", "tSNE2")
#   
#   tsne_data_with_distance <- cbind(tsne_data, distance = ref_added_or$distance)
#   
#   km_tsne <- kmeans(tsne_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
#   
#   # Add cluster information to the data
#   ref_added_or$cluster <- km_tsne$cluster
#   
#   } else {
#     ref_data_with_distance <- cbind(ref_added_or[, 1:nFeat], setNames(ref_added_or$'distance', 'distance'))
#     km_result <- kmeans(ref_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
#     ref_added_or$cluster <- km_result$cluster
#   }
#   # **********************
#   
#   # ***********************************************************************************
#   if (!(is.logical(plot_flag)) && realiz==1 && s_size==1) {
#     setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
#     
#     # Define colors for clusters
#     cluster_colors <- rainbow(cluster)
#     
#     if(tSNE_flag){
#       tsne_data_with_distance$Cluster <- as.factor(km_tsne$cluster)
#       
#       png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"), plot_flag, "_cluster_tSNE_distance_", script, "_", city, "_", model_prob, "_", invariance, ".png"),
#           units="in", 
#           width=16, 
#           height=20, 
#           pointsize=24,
#           res=96)
#       par(mfrow = c(2, 1), mar = c(5, 4, 4, 8), xpd = TRUE)
#       
#       # Plot t-SNE
#       plot(tsne_data$tSNE1, tsne_data$tSNE2, col = cluster_colors[tsne_data_with_distance$Cluster],
#            pch = 20, cex = 0.5, main = c("t-SNE with K-means Clustering ", cluster),
#            xlab = "t-SNE 1", ylab = "t-SNE 2")
# 
#       # Plot t-SNE with Distance
#       plot(tsne_data_with_distance$tSNE1, ref_added_or$distance, col = cluster_colors[tsne_data_with_distance$Cluster],
#            pch = 20, cex = 0.5, main = "K-means Clustering on t-SNE + Distance",
#            xlab = "t-SNE 1", ylab = "Distance")
#       dev.off()
#     }
#     setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
#   }
#   # ***********************************************************************************
# 
#   
#   # Initialize vectors to store selected sample indices for two selections
#   selected_indices <- c()
#   selected_indices2 <- c()
#   # Calculate the number of samples to select per class
#   samples_per_class <- ceiling(newSize / nclass)
#   samples_per_class2 <- ceiling(newSize2 / nclass)
#   
#   # Initialize a list to keep track of the clusters selected per class
#   selected_clusters <- list()
#   
#   # Initialize a named vector to store how many samples have been selected per class
#   class_labels <- unique(as.character(ref_added_or$label))
#   class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
#   
#   # Initialize the list with empty vectors for each class label
#   for (label in class_labels) { selected_clusters[[label]] <- c() }
#   
#   # Initialize a flag to indicate if the first set of selected_indices is completed
#   first_selection_done <- FALSE
#   
#   # Iterate over the samples and select based on classes
#   for (sample in seq_len(nrow(ref_added_or))) {
#     # Convert class_label to character to avoid issues with factors
#     class_label <- as.character(ref_added_or[sample,]$label)
#     cluster_id <- ref_added_or[sample,]$cluster
#     
#     # Check if we have already completed the first selection
#     if (!first_selection_done) {
#       # Check if the current class has not exceeded the allowed number of samples
#       #if (class_sample_count[class_label] < samples_per_class || !flag_class) {
#         # Check if the cluster has not been selected for this class
#         if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || length(unique(ref_added_or[ref_added_or$label==class_label,"cluster"]))<samples_per_class) {
#           
#           # Add the sample's cluster to the selected clusters for this class
#           if(flag_cluster){ selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id) }
#           
#           # Assign the label and add the index
#           ref_added_or[sample,]$label <- ref_added_or[sample,]$ref
#           selected_indices <- c(selected_indices, as.numeric(rownames(ref_added_or[sample,])))
#           
#           # Increment the count of selected samples for this class
#           class_sample_count[class_label] <- class_sample_count[class_label] + 1
#         }
#       #}
#       
#       # Check if the first selection is done
#       if (sum(class_sample_count) >= newSize) {
#         first_selection_done <- TRUE
#         
#         # Reset class_sample_count 
#         class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
#         selected_clusters <- list()
#         for (label in class_labels) { selected_clusters[[label]] <- c()  }
#         ref_added <- ref_added[order(-ref_added$distance), ]
#       }
#     # } else {  # Start selecting for the second set of selected_indices2
#     # 
#     #   # Check if the current class has not exceeded the allowed number of samples
#     #   #if (class_sample_count[class_label] < samples_per_class2 || !flag_class ) {
#     #     # Check if the cluster has not been selected for this class
#     #     if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || length(unique(ref_added_or[ref_added_or$label==class_label,"cluster"]))<samples_per_class2) {
#     #       # Add the sample's cluster to the selected clusters for this class
#     #       selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id)
#     #       # Add the index of the selected sample to the second selection
#     #       selected_indices2 <- c(selected_indices2, as.numeric(rownames(ref_added_or[sample,])))
#     #       # Increment the count of selected samples for this class
#     #       class_sample_count[class_label] <- class_sample_count[class_label] + 1
#     #     }
#     #   #}
#     #   # Stop if we have reached the desired total number of samples for the second selection
#     #   if (sum(class_sample_count) >= newSize2) break
#     }
#   }
#   ref_added_reor = ref_added_or[order(as.numeric(rownames(ref_added_or))),]
#   
#  
#   # Remove relabeled samples from validateLabels
#   features <- features[!(rownames(features) %in% selected_indices), ]
#   reor_idx <- which(rownames(ref_added_reor) %in% selected_indices)
#   semi_idx <- which(rownames(ref_added_reor) %in% selected_indices2)
#   cat("length selected_indices ",length(selected_indices))
# 
#     new_trainFeat_AL <- ref_added_reor[reor_idx, 1:nFeat]
#     new_trainLabels_AL <- ref_added_reor[reor_idx, nFeat+1]
#     return(list(IDunit=ID_unit[reor_idx], semiIDunit=ID_unit[semi_idx], 
#                 semi_samples = ref_added_reor[semi_idx, 1:(nFeat+1)],
#                 new_trainFeat_AL = new_trainFeat_AL, 
#                 new_trainLabels_AL = new_trainLabels_AL))
#     
# }

add_AL_samples <- function(distance_data,
                           features = NULL, ref, 
                           new_trainFeat_AL = NULL, new_trainLabels_AL = NULL,
                           newSize = 10, cluster = 100, ID_unit = NULL, nFeat = numFeat, 
                           PCA_flag = FALSE, tSNE_flag = FALSE, 
                           realiz = realization, s_size = sample_size, 
                           newSize2 = 20 * nclass, plot_flag = FALSE, 
                           flag_cluster = FALSE, flag_class = FALSE) {
  
  if (cluster < newSize) {
    cluster <- round(max(newSize * 1.01, nrow(distance_data) / 10))
  }
  
  # Merge features, original labels, and ID_unit
  ref_added <- cbind(distance_data, ref, ID_unit = ID_unit)
  
  # Order by most uncertain samples (distance)
  ref_added_or <- ref_added[order(ref_added$distance), ]
  
  # Duplicates check if tSNE_flag
  if (tSNE_flag) {
    flag_cluster <- TRUE
    
    duplicate_rows <- duplicated(ref_added_or[, 1:nFeat])
    if (sum(duplicate_rows) > 0) {
      ref_added_or <- ref_added_or[!duplicate_rows, ]
      cat("Samples after removing duplicates:", nrow(ref_added_or), "\n")
    }
    
    # t-SNE
    tsne_result <- Rtsne(ref_added_or[, 1:nFeat], dims = 2, perplexity = 30,
                         verbose = FALSE, max_iter = 500)
    tsne_data <- data.frame(tsne_result$Y)
    colnames(tsne_data) <- c("tSNE1", "tSNE2")
    tsne_data_with_distance <- cbind(tsne_data, distance = ref_added_or$distance)
    km_tsne <- kmeans(tsne_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
    ref_added_or$cluster <- km_tsne$cluster
  } else {
    ref_data_with_distance <- cbind(ref_added_or[, 1:nFeat], distance = ref_added_or$distance)
    km_result <- kmeans(ref_data_with_distance, centers = cluster, iter.max = 25, nstart = 50)
    ref_added_or$cluster <- km_result$cluster
  }
  
  # Initialize
  selected_IDunit <- c()
  selected_IDunit2 <- c()
  
  samples_per_class <- ceiling(newSize / nclass)
  samples_per_class2 <- ceiling(newSize2 / nclass)
  
  class_labels <- unique(as.character(ref_added_or$label))
  class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
  
  selected_clusters <- list()
  for (label in class_labels) selected_clusters[[label]] <- c()
  
  first_selection_done <- FALSE
  
  # Iterate over samples
  for (sample in seq_len(nrow(ref_added_or))) {
    class_label <- as.character(ref_added_or$label[sample])
    cluster_id <- ref_added_or$cluster[sample]
    
    if (!first_selection_done) {
      if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || 
          length(unique(ref_added_or[ref_added_or$label == class_label, "cluster"])) < samples_per_class) {
        
        if (flag_cluster) selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id)
        
        # Add original ID_unit
        selected_IDunit <- c(selected_IDunit, ref_added_or$ID_unit[sample])
        
        class_sample_count[class_label] <- class_sample_count[class_label] + 1
      }
      
      if (sum(class_sample_count) >= newSize) {
        first_selection_done <- TRUE
        class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
        selected_clusters <- list()
        for (label in class_labels) selected_clusters[[label]] <- c()
      }
    }
  }
  
  # Retrieve selected samples from original dataset
  # new_trainFeat_AL <- trainDataCurRemaining4AL_IT[selected_IDunit, 1:nFeat]
  new_trainFeat_AL <- ref_added_or[match(selected_IDunit, ref_added_or$ID_unit), 1:nFeat]
  new_trainLabels_AL <- ref_added_or$ref[match(selected_IDunit, ref_added_or$ID_unit)]
  
  return(list(
    IDunit = selected_IDunit,
    new_trainFeat_AL = new_trainFeat_AL,
    new_trainLabels_AL = new_trainLabels_AL
  ))
}


self_learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVtotal, objInfoNames, rem_extrem, rem_extrem_kerneldist, SVL_variables, SVMfinModel=tunedSVM$finalModel, train=TRUE, classProb = FALSE) {

  actKappa = -1e-6
  # cat("applying constraints to VSVs candidates\n")
  
  library(parallel)
  library(doParallel)
  library(foreach)
  
  # print(paste("Starting cluster with", num_cores, "cores"))
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
          # setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj], SVMfinModel@kernelf), objInfoNames)
          setNames(rem_extrem(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
        }, error = function(e) {
          message(paste("Error in task", variable, ":", e$message))
          NULL  
        })
        
      }
    } # print("step 2")

    # remove NAs 
    SVinvarRadi = na.omit(SVinvarRadi)
    
    # iterating over boundMargin to test different threshold on margin distance
    for(kk in seq(along=boundMargin)){
      # cat("tuning similarity threshold: ",bound[jj]," [",jj,"/",length(bound),"] | bound margin: ",boundMargin[kk]," [",kk,"/",length(boundMargin),"]\n",sep="")
      
      if (nrow(SVinvarRadi) > 0) { # Check if SVinvarRadi has rows to process
        # Remove VSV which are not located within a certain distance to the decision function
        # data.frame to store elected VSV within the margin
        SVinvar = setNames(data.frame(matrix(ncol = numFeat + 1, nrow = 0)), objInfoNames)
        
        # # Progress bar initialization
        # pb <- progress_bar$new(
        #   format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
        #   total = nrow(SVinvarRadi),
        #   clear = FALSE
        # )
        # Iterate over SVinvarRadi and evaluate distance to hyperplane
        for (m in 1:nrow(SVinvarRadi)) {
          signa = as.numeric(pred_one(SVMfinModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
          
          if (signa < boundMargin[kk]) {
            SVinvar = rbind(SVinvar, SVinvarRadi[m, ])
          }
          # pb$tick()
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
  # print("Cluster stopped.")
  showConnections(all = TRUE)
  
  return(list(bestFittingModel = bestFittingModel, 
              actKappa = actKappa, 
              best_trainFeatVSVM = best_trainFeatVSVM, 
              best_trainLabelsVSVM = best_trainLabelsVSVM, 
              best_bound = best_bound, 
              best_boundMargin = best_boundMargin))

}


self_learn_AL = function(

  SVtotal, objInfoNames, SVinvarRadi, 
  
  upd_dataCurFeatsub, upd_dataCurLabels, 
  realiz=realization, s_size=sample_size, plot_flag=FALSE) {
  
  if (nrow(SVinvarRadi) > 0) { 
    
    SVinvar = setNames(data.frame(matrix(ncol = numFeat + 2, nrow = 0)), c(objInfoNames, "distance"))
    upd_Labels <- factor(character(0), levels = levels(upd_dataCurLabels))
    
    # # Progress bar initialization
    # pb <- progress_bar$new(
    #   format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
    #   total = nrow(SVinvarRadi),
    #   clear = FALSE
    # )
   
    for (m in 1:nrow(SVinvarRadi)) {
      signa = as.numeric(pred_one(new_bestModel$finalModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
      
      row_with_distance = c(SVinvarRadi[m, ], distance = signa)
      SVinvar = rbind(SVinvar, row_with_distance)
      upd_Labels <- c(upd_Labels, upd_dataCurLabels[m])

      # pb$tick()
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
  distances_log <- log1p((scaled_distances) * (exp(1) - 1))
  
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
  
  return(list(sampled_data = SVinvar, 
              best_updCur_Labels = upd_Labels))
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

lgtS=TRUE
train  = TRUE              # if TRUE, train the models otherwise load them from dir 
num_cores <- parallel::detectCores()/2 # Numbers of CPU cores for parallel processing

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

AccuracyVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL) = colheader
AccuracyVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un) = colheader
AccuracyVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn) = colheader

AccuracyAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MS) = colheader
AccuracyAL_MS_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyAL_MS_2IT) = colheader
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

AccuracyALSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALSVM_SL_Un) = colheader
AccuracyALVSVM_SL_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL_2IT) = colheader
AccuracyALVSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL_Un_2IT) = colheader
AccuracyALVSVM_SL_vUn_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyALVSVM_SL_vUn_2IT) = colheader


# ******** KAPPA SCORE
KappaSVM = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM) = colheader
KappaSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaSVM_SL_Un) = colheader
KappaVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL) = colheader
KappaVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_Un) = colheader
KappaVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaVSVM_SL_vUn) = colheader

KappaAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS) = colheader
KappaAL_MS_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS_2IT) = colheader
KappaAL_MS_semiAL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaAL_MS_semiAL) = colheader

KappaALSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALSVM_SL_Un_2IT) = colheader
KappaALVSVM_SL_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL_2IT) = colheader
KappaALVSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL_Un_2IT) = colheader
KappaALVSVM_SL_vUn_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(KappaALVSVM_SL_vUn_2IT) = colheader


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

SVsAL_MS = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MS) = colheader
SVsAL_MS_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsAL_MS_2IT) = colheader

SVsALSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALSVM_SL_Un) = colheader
SVsALVSVM_SL = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL) = colheader
SVsALVSVM_SL_Un = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_Un) = colheader
SVsALVSVM_SL_vUn = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_vUn) = colheader

SVsALSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALSVM_SL_Un_2IT) = colheader
SVsALVSVM_SL_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_2IT) = colheader
SVsALVSVM_SL_Un_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_Un_2IT) = colheader
SVsALVSVM_SL_vUn_2IT = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(SVsALVSVM_SL_vUn_2IT) = colheader
# ************************************************ 

# ************************************************ 

# ********
best_model_oa = c()
time.taken_iter = c()
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

    
      # 1IT
      
      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2)
      
      # definition of sampling configuration (strata:random sampling without replacement)
      stratSamp = strata(trainDataCurBeg, c("REF"), size = shares, method = "srswor")
      #size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
      
      # get samples of trainDataCur and set trainDataCur new
      samples = getdata(trainDataCurBeg, stratSamp)
      samplesID = samples$ID_unit
      
      samples[1, sindexSVMDATA:eindexSVMDATA]
      trainDataCurBeg[samples$ID_unit[1], sindexSVMDATA:eindexSVMDATA]
      
      trainDataCur4AL_IT = samples[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemaining <- trainDataCurBeg[-c(samplesID), ]
      
      trainFeat = trainDataCur4AL_IT[,1:(ncol(trainDataPoolAllLev)-1)]
      trainLabels4AL_IT = trainDataCur4AL_IT[,ncol(trainDataPoolAllLev)]
      # *********************************************************************
      
      # subset on L_4 ***************************** SVM base for invariants ************************************
      trainFeat4AL_IT = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and validate set
      # ************************************************ *******************************************************
      
      trainDataCurRemaining4AL_IT = trainDataCurRemaining


      
      
      # Benchmark
      
      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2)
      
      
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
      names = objInfoNames[1:(length(objInfoNames)-1)]
      trainFeat <- rbind(setNames(trainFeat4AL_IT, names), setNames(trainFeat, names))
      trainLabels <- unlist(list(trainLabels4AL_IT, trainLabels))
      
      trainDataCur = rbind(trainDataCur4AL_IT,trainDataCur)

      
      
      
      # TEST
      
      
      # 1IT
      
      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2,sampleSize/2)
      
      stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBeg, stratSamp)
      testDataCur = samples[,1:ncol(testDataAllLev)]
      
      samplesID = samples$ID_unit
      testDataCur <- testDataCur[-c(samplesID), ]
      
      # split test feat from test label for later join with trainData
      testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
      testLabels4AL_1IT = testDataCur[,ncol(testDataCur)]
      
      # subset on base level
      testFeatsub4AL_1IT = testFeat[sindexSVMDATA:eindexSVMDATA]
      
     
       

      # 2IT

      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4)
      
      stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBeg, stratSamp)
      testDataCur = samples[,1:ncol(testDataAllLev)]
      
      # split test feat from test label for later join with trainData
      testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
      testLabels4AL_2IT = testDataCur[,ncol(testDataCur)]
      
      # subset on base level
      testFeatsub4AL_2IT = testFeat[sindexSVMDATA:eindexSVMDATA]
      
      testFeatsub4AL_2IT <- rbind(setNames(testFeatsub4AL_1IT, names), setNames(testFeatsub4AL_2IT, names))
      testLabels4AL_2IT <- unlist(list(testLabels4AL_1IT, testLabels4AL_2IT))
      
      
      
      # Benchmark

      sampleSize = round(sampleSizePor[sample_size]/nclass)
      shares = c(sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4,sampleSize/4)
      
      stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
      samples = getdata(testDataCurBeg, stratSamp)
      testDataCur = samples[,1:ncol(testDataAllLev)]
      
      # split test feat from test label for later join with trainData
      testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
      testLabels = testDataCur[,ncol(testDataCur)]
      
      # subset on base level
      testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]
      
      testFeatsub <- rbind(setNames(testFeatsub4AL_2IT, names), setNames(testFeatsub, names))
      testLabels <- unlist(list(testLabels4AL_2IT, testLabels))
      
      
      
      
      # TUNE  
      
      
      # 1IT

      # trainData index to split between train and test in svmFit
      countTrainData = nrow(trainFeat4AL_IT)
      indexTrainData4AL_1IT = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat4AL_1AL = rbind(setNames(trainFeat4AL_IT,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub4AL_1IT,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel4AL_1AL = unlist(list(trainLabels4AL_IT, testLabels4AL_1IT))
    
    
      
      
      # Benchmark
      
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(trainFeat)
      indexTrainData = list(c(1:countTrainData))
        
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(trainLabels, testLabels))
    
    
    # ==============

    
    
    
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city,"/",model_prob,"/",invariance))
    
    cat("\n") #################################  SVM #####################################
    model_name_tunedSVM = "SVM"
    
    cat("training SVM\n")
    trainStart.time <- Sys.time()
    tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
    trainSVM.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
    
    tunedSVM4AL = svmFit(tuneFeat4AL_1AL, tuneLabel4AL_1AL, indexTrainData4AL_1IT)
    
    # run classification and accuracy assessment for unmodified SV and predict labels of test data
    predLabelsSVM = predict(tunedSVM, validateFeatsub)
    cm_SVM = confusionMatrix(predLabelsSVM, validateLabels)
    cat("SVM accuracy: ",round(cm_SVM$overall["Accuracy"],5),"\n",sep="")
    
    predLabelsSVM4AL = predict(tunedSVM4AL, validateFeatsub)
    cm_SVM4AL = confusionMatrix(predLabelsSVM4AL, validateLabels)
    
    AccuracySVM[realization,sample_size] = as.numeric(cm_SVM$overall["Accuracy"])
    KappaSVM[realization,sample_size] = as.numeric(cm_SVM$overall["Kappa"])
    SVsSVM[realization,sample_size] = as.numeric(length(tunedSVM$finalModel@SVindex))

    if(sample_size==1 || cm_SVM$overall["Accuracy"]>best_acc){
      best_acc <- cm_SVM$overall["Accuracy"]
      best_model_name <- model_name_tunedSVM
      new_bestModel <- tunedSVM
    }
    # get original SVs of base SVM ************************* prevent removing an entire class label
    SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
    SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
    freq_table <- table(SVtotal$REF)
    zero_label_classes <- names(freq_table[freq_table == 0])
    if (length(zero_label_classes) > 0) {
      cat("info: prevent an entire class label to be removed | realization [",realization,"/",nR,"] | [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
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
    bb = 1
    
    
    trainStart.time <- Sys.time()
    # Definition of sampling configuration (strata:random sampling without replacement)
    stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")

    # get samples of trainDataCurRemaining and set trainDataCurRemaining new
    samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
    # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]

    trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
    trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]

    
    REFSVM = predict(tunedSVM, trainDataCurRemainingsub_SL)

    # get SV of unlabeled samples
    SVindexSVMUn = 1:nrow(trainDataCurRemainingsub_SL)
    SVtotalSVMUn = trainDataCurRemaining_SL[SVindexSVMUn ,c(sindexSVMDATA:eindexSVMDATA)]
    SVtotalSVMUn = cbind(SVtotalSVMUn, REFSVM)

    cat("evaluation of SVM with self learning and semi-labeled | realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")

      SVL_variables = list(
        list(SVtotalSVMUn, SVL2SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM)),
        list(SVtotalSVMUn, SVL3SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM)),
        list(SVtotalSVMUn, SVL5SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL6SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL7SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL8SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL9SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL10SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFSVM)),
        list(SVtotalSVMUn, SVL11SVMUn = cbind(trainDataCurRemaining_SL[SVindexSVMUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFSVM))
      )

    SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_SVMUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                           SVL_variables)
    bestFittingModelSVMUn <- SLresult$bestFittingModel
    best_trainFeatSVMUn <- SLresult$best_trainFeatVSVM
    best_trainLabelsSVMUn <- SLresult$best_trainLabelsVSVM
    best_boundMarginSVMUn <- SLresult$best_boundMargin

    t.timeSVMUn <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
    # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
    predLabelsSVMsumUn = predict(bestFittingModelSVMUn, validateFeatsub)
    cm_SVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)
    cat("SVM_SL_Un accuracy: ",round(cm_SVM_SL_Un$overall["Accuracy"],5),"\n",sep="")

    AccuracySVM_SL_Un[realization,sample_size] = as.numeric(cm_SVM_SL_Un$overall["Accuracy"])
    KappaSVM_SL_Un[realization,sample_size] = as.numeric(cm_SVM_SL_Un$overall["Kappa"])
    SVsSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelSVMUn$finalModel@SVindex))
    if (cm_SVM_SL_Un$overall["Accuracy"]>best_acc) {
      best_acc <- cm_SVM_SL_Un$overall["Accuracy"]
      new_bestModel <- bestFittingModelSVMUn
      best_model_name <- model_name_SVMUn
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
    
    cat("evaluation of VSVM with self learning | realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
    
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
    cat("VSVM_SL accuracy: ",round(cm_VSVM_SL$overall["Accuracy"],5),"\n",sep="")

    AccuracyVSVM_SL[realization,sample_size] = as.numeric(cm_VSVM_SL$overall["Accuracy"])
    KappaVSVM_SL[realization,sample_size] = as.numeric(cm_VSVM_SL$overall["Kappa"])
    SVsVSVM_SL[realization,sample_size] = as.numeric(length(bestFittingModel$finalModel@SVindex))
    if (cm_VSVM_SL$overall["Accuracy"]>best_acc) {
      best_acc <- cm_VSVM_SL$overall["Accuracy"]
      new_bestModel <- bestFittingModel
      best_model_name <- model_name_VSVM_SL
    }


    cat("\n") #################################  VSVM-SL + semi-labeled samples #####################################
    model_name_Un = "VSVM_SLUn"

    trainStart.timeUn <- Sys.time()
    actAcc_vUn = -1e-6

      # # Definition of sampling configuration (strata:random sampling without replacement)
      # stratSampRemaining_b = strata(trainDataCurRemaining, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      # 
      # # get samples of trainDataCurRemaining and set trainDataCurRemaining new
      # samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining_b)
      # 
      # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining_b$ID_unit), ]
      # 
      # trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      # trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]

      REF_b = predict(bestFittingModel, trainDataCurRemainingsub_SL)

      # get SV of unlabeled samples
      indexUn = 1:nrow(trainDataCurRemainingsub_SL)
      totalUn = trainDataCurRemaining_SL[indexUn ,c(sindexSVMDATA:eindexSVMDATA)]
      totalUn = cbind(totalUn, REF_b)

      cat("evaluation of VSVM SL with ",b[bb]," semi-labeled | realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
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
      
      
      
      
      cat("VSVM_SL_Un accuracy: ",round(cm_VSVM_SL_Un$overall["Accuracy"],5),"\n",sep="")

      AccuracyVSVM_SL_Un[realization,sample_size] = as.numeric(cm_VSVM_SL_Un$overall["Accuracy"])
      KappaVSVM_SL_Un[realization,sample_size] = as.numeric(cm_VSVM_SL_Un$overall["Kappa"])
      SVsVSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelUn$finalModel@SVindex))
      if (cm_VSVM_SL_Un$overall["Accuracy"] > best_acc) {
        best_acc <- cm_VSVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingModelUn
        best_model_name <- model_name_Un
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

      cat("evaluation of VSVM SL with ",b[bb]," virtual semi-labeled samples | realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="") 

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

    cat("VSVM_SL_vUn accuracy: ",round(cm_VSVM_SL_vUn$overall["Accuracy"],5),"\n",sep="")

    AccuracyVSVM_SL_vUn[realization,sample_size] = as.numeric(cm_VSVM_SL_vUn$overall["Accuracy"])
    KappaVSVM_SL_vUn[realization,sample_size] = as.numeric(cm_VSVM_SL_vUn$overall["Kappa"])
    SVsVSVM_SL_vUn[realization,sample_size] = as.numeric(length(bestFittingModelvUn$finalModel@SVindex))
    if (cm_VSVM_SL_vUn$overall["Accuracy"] > best_acc){
      best_acc <- cm_VSVM_SL_vUn$overall["Accuracy"]
      new_bestModel <- bestFittingModelvUn
      best_model_name <- model_name_vUn
    }
    ########################################################################################################
    
    
    
    
      
      cat("\n") ###############################  Active Query #####################################
    

      classSize=c(25000)            
      # classSize=c(50000)            
      # classSize=c(100000)            
      clS=1
      cat("sampling ", classSize," unlabeled data\n",sep="")

      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining = strata(trainDataCurRemaining4AL_IT, size = classSize[clS], method = "srswor")
      samplesRemaining = getdata(trainDataCurRemaining4AL_IT, stratSampRemaining)
      
      # head(rownames(trainDataCurRemaining4AL_IT))
      # head(rownames(samplesRemaining))
      # head(stratSampRemaining$ID_unit)
      
      # trainDataCurRemaining4AL_IT[samplesRemaining$ID_unit[1],c(sindexSVMDATA:eindexSVMDATA)]
      # samplesRemaining[1,c(sindexSVMDATA:eindexSVMDATA)]
      
    
      
      # Final check for duplicates
      final_duplicate_count <- sum(duplicated(samplesRemaining[, c(sindexSVMDATA:eindexSVMDATA)]))
      cat("final unlabeled pool size: ",nrow(samplesRemaining)," | duplicates: ", final_duplicate_count,"\n",sep="")
      # cat("using currently best model: ",best_model_name," | accuracy: ",best_acc,"\n",sep="")
      cat("using currently SVM4AL with ",length(trainLabels4AL_IT)," train samples | accuracy: ",cm_SVM4AL$overall["Accuracy"],"\n",sep="")
      
      
      cS=1  
      bb = 1
      
      upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
      upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
      upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
      
      
      trainDataCurRemaining4AL_IT[upd_dataCur$ID_unit[1],c(sindexSVMDATA:eindexSVMDATA)]
      upd_dataCur[1,c(sindexSVMDATA:eindexSVMDATA)]
      
      
      trainStart.time <- Sys.time()
      
      # # **********************
      # # **********************
      
      SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(tunedSVM4AL, upd_dataCurFeatsub)), objInfoNames)
      
      sampledResult <- self_learn_AL(
        SVtotal, # NOT USED
        objInfoNames, SVL_variables,
        upd_dataCurFeatsub,upd_dataCurLabels,
        plot_flag = FALSE
      )
      sampled_data <- sampledResult$sampled_data
      reference_label <- sampledResult$best_updCur_Labels
      
      # trainDataCurRemaining4AL_IT[samplesRemaining$ID_unit[1],c(sindexSVMDATA:eindexSVMDATA)]
      # sampled_data[1,1:(ncol(sampled_data)-2)]
      
      cat("computing samples distance required ", round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1),"sec\n",sep="")
      
      # # **********************
      # # **********************
      
      
      cat("\n") ###############################  AL MS + t-SNE #######################################
      model_name_AL_MS = "AL_MS"
      
      cat("active labeling ",model_name_AL_MS," | realization [",realization,"/",nR,"] | train samples: ",length(trainLabels4AL_IT)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS],"\n",sep="")
      
      # get the new size for the active labeling
      newSize = round(sampleSizePor[sample_size]/2)
      
      clusterSizes = newSize+1 # c(round(max(classPor/40,newSize+1)))
      newSize_for_iter = newSize
      
      # result <- add_AL_samples(sampled_data,
      #                          sampled_data[,1:numFeat], reference_label,
      #                          trainFeat4AL_IT, trainLabels4AL_IT,
      #                          newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
      #                          upd_dataCur$ID_unit,
      #                          tSNE_flag = TRUE, flag_cluster = TRUE, newSize2=b[bb]*nclass) #,
      
      result <- add_AL_samples(
        sampled_data,
        sampled_data[, 1:numFeat], reference_label,
        trainFeat4AL_IT, trainLabels4AL_IT,
        newSize_for_iter, clusterSizes[cS],
        ID_unit = upd_dataCur$ID_unit,
        tSNE_flag = TRUE, flag_cluster = TRUE,
        newSize2 = b[bb] * nclass
      )

      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL

      
      # **********************
      # get al train set portion
      
      trainFeat4AL <- rbind(setNames(trainFeat4AL_IT, names), setNames(new_trainFeat, names))
      trainLabels4AL <- unlist(list(trainLabels4AL_IT[], new_trainLabels))
      # SVtotal = setNames(cbind(trainFeat4AL_IT, trainLabels4AL_IT),c(objInfoNames[-length(objInfoNames)],"REF"))

      # **********************
      
      
      
      
      # **********************
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(trainFeat4AL)
      indexTrainData = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(trainFeat4AL,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(trainLabels4AL, testLabels))
      
      AL_MS_tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
      tmp_pred = predict(AL_MS_tunedSVM, validateFeatsub)
      cm_AL_MS  = confusionMatrix(tmp_pred, validateLabels)
      cat(model_name_AL_MS," accuracy: ",round(cm_AL_MS$overall["Accuracy"],5),"\n",sep="")
      # **********************

      
      AccuracyAL_MS[realization,sample_size] = as.numeric((cm_AL_MS$overall["Accuracy"]))
      KappaAL_MS[realization,sample_size] = as.numeric((cm_AL_MS$overall["Kappa"]))
      SVsAL_MS[realization,sample_size] = as.numeric(length(AL_MS_tunedSVM$finalModel@SVindex))

      
      if(cm_AL_MS$overall["Accuracy"]>best_acc){
        best_acc <- cm_AL_MS$overall["Accuracy"]
        best_model_name <- model_name_AL_MS
        new_bestModel <- AL_MS_tunedSVM
      }
      
      
      cat("\n") ###############################  Active Query 1IT #####################################

      # classSize=c(25000)            
      # 
      # cat("sampling ", classSize," unlabeled data\n",sep="")
      # samplingStart.time <- Sys.time()
      # 
      # # Definition of sampling configuration (strata:random sampling without replacement)
      # stratSampRemaining = strata(trainDataCurRemaining4AL_IT, size = classSize[clS], method = "srswor")
      # samplesRemaining_1IT = getdata(trainDataCurRemaining4AL_IT, stratSampRemaining)
      # 
      # sampling.time = round(as.numeric((Sys.time() - samplingStart.time), units = "secs"), 1)
      # 
      # # Final check for duplicates
      # final_duplicate_count <- sum(duplicated(samplesRemaining_1IT[, c(sindexSVMDATA:eindexSVMDATA)]))
      # cat("final unlabeled pool size: ",nrow(samplesRemaining_1IT)," | duplicates: ", final_duplicate_count,"\n",sep="")
      # cat("using currently with ",length(trainLabels4AL_IT)," train samples | accuracy: ",cm_SVM4AL$overall["Accuracy"],"\n",sep="")
  
      
      cat("\n") ###############################  AL MS + t-SNE 1IT #######################################

      model_name_AL_MS_1IT = "AL_MS_1IT"
      
      cat("active labeling ",model_name_AL_MS_1IT," | realization [",realization,"/",nR,"] | samples: ",length(trainLabels4AL_IT)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS],"\n",sep="")

      
      newSize = round(sampleSizePor[sample_size]/4)
      clusterSizes = newSize+1 
      newSize_for_iter = newSize

      # upd_dataCur <- samplesRemaining_1IT[,1:(ncol(trainDataCur)+1)]
      # upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
      # upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
      
      # trainStart.time <- Sys.time()
      # 
      # # # **********************
      # # # **********************
      # 
      # SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(tunedSVM4AL, upd_dataCurFeatsub)), objInfoNames)
      # 
      # sampledResult <- self_learn_AL(
      #   SVtotal, # NOT USED
      #   objInfoNames, SVL_variables,
      #   upd_dataCurFeatsub,upd_dataCurLabels,
      #   plot_flag = FALSE
      # )
      # sampled_data <- sampledResult$sampled_data
      # reference_label <- sampledResult$best_updCur_Labels
      # cat("computing samples distance required ", round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1),"sec\n",sep="")
      # 
      # # # **********************
      # # # **********************

      
      # result <- add_AL_samples(sampled_data,
      #                          sampled_data[,1:numFeat], reference_label,
      #                          trainFeat4AL_IT, trainLabels4AL_IT,
      #                          newSize_for_iter, clusterSizes[cS], 
      #                          upd_dataCur$ID_unit,tSNE_flag = TRUE, flag_cluster = TRUE, newSize2=b[bb]*nclass) 
      
      result <- add_AL_samples(
        sampled_data,
        sampled_data[, 1:numFeat], reference_label,
        trainFeat4AL_IT, trainLabels4AL_IT,
        newSize_for_iter, clusterSizes[cS],
        ID_unit = upd_dataCur$ID_unit,
        tSNE_flag = TRUE, flag_cluster = TRUE,
        newSize2 = b[bb] * nclass
      )
      
      # upd_SVindex = upd_dataCur$ID_unit %in% result$IDunit
      # upd_SVindex = result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL
      
      
      
      
      # trainDataCurRemaining4AL_IT[upd_dataCur$ID_unit[1],c(sindexSVMDATA:eindexSVMDATA)]
      # upd_dataCur[1,c(sindexSVMDATA:eindexSVMDATA)]
      # new_trainFeat
      # trainDataCurRemaining4AL_IT[result$IDunit, c(sindexSVMDATA:eindexSVMDATA)]
      
      # remove new_trainFeat from trainDataCurRemaining4AL_IT
      trainDataCurRemaining4AL_IT <- trainDataCurRemaining4AL_IT[-(result$IDunit), ]

      # add new_trainFeat allLevel to trainDataCur4AL_IT
      trainDataCur4AL_IT = rbind(trainDataCur4AL_IT,upd_dataCur[upd_dataCur$ID_unit %in% result$IDunit, 1:(ncol(trainDataPoolAllLev))])
      
      
      
      # ## ADD AQ-S3VSVM
      # 
      # bb = 1 # for (bb in seq(along=b)) {
      # 
      # # Definition of sampling configuration (strata:random sampling without replacement)
      # stratSampRemaining_b = strata(trainDataCurRemaining4AL_IT, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      # 
      # # get samples of trainDataCurRemaining4AL_IT and set trainDataCurRemaining4AL_IT new
      # samplesRemaining_b = getdata(trainDataCurRemaining4AL_IT, stratSampRemaining_b)
      # 
      # trainDataCurRemaining4AL_IT <- trainDataCurRemaining4AL_IT[-c(samplesRemaining_b$ID_unit), ]
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
      
      trainFeat4AL_IT <- rbind(setNames(trainFeat4AL_IT, names), setNames(new_trainFeat, names))
      trainLabels4AL_IT <- unlist(list(trainLabels4AL_IT[], new_trainLabels))
      # SVtotal = setNames(cbind(trainFeat4AL_IT, trainLabels4AL_IT),c(objInfoNames[-length(objInfoNames)],"REF"))
      
      # **********************
      
      
      
      
      # **********************
      # trainData index to split between train and test in svmFit
      countTrainData = nrow(trainFeat4AL_IT)
      indexTrainData = list(c(1:countTrainData))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat = rbind(setNames(trainFeat4AL_IT,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub4AL_2IT,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel = unlist(list(trainLabels4AL_IT, testLabels4AL_2IT))
      
      AL_MS_tunedSVM_1IT = svmFit(tuneFeat, tuneLabel, indexTrainData)
      tmp_pred = predict(AL_MS_tunedSVM_1IT, validateFeatsub)
      cm_AL_MS_1IT  = confusionMatrix(tmp_pred, validateLabels)
      cat(model_name_AL_MS_1IT," accuracy: ",round(cm_AL_MS_1IT$overall["Accuracy"],5),"\n",sep="")
      # **********************
      
      
      
      cat("\n") ###############################  Active Query 2IT #####################################
      
      
      # get the new size for the active labeling
      newSize = round(sampleSizePor[sample_size]/4)
      
      clusterSizes = newSize+1 # c(round(max(classPor/40,newSize+1)))
      
      classSize=c(25000)            

      cat("sampling ", classSize," unlabeled data\n",sep="")
      samplingStart.time <- Sys.time()
      
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining = strata(trainDataCurRemaining4AL_IT, size = classSize[clS], method = "srswor")
      samplesRemaining = getdata(trainDataCurRemaining4AL_IT, stratSampRemaining)
      
      sampling.time = round(as.numeric((Sys.time() - samplingStart.time), units = "secs"), 1)
      
      
      # Final check for duplicates
      final_duplicate_count <- sum(duplicated(samplesRemaining[, c(sindexSVMDATA:eindexSVMDATA)]))
      cat("final unlabeled pool size: ",nrow(samplesRemaining)," | duplicates: ", final_duplicate_count,"\n",sep="")
      cat("using currently SVM with ",length(trainLabels4AL_IT)," train samples | accuracy: ",cm_AL_MS_1IT$overall["Accuracy"],"\n",sep="")

      
      cat("\n") ###############################  AL MS + t-SNE 2IT #######################################
      model_name_AL_MS_2IT = "AL_MS_2IT"
      
      cat("active labeling ",model_name_AL_MS_2IT," | realization [",realization,"/",nR,"] | samples: ",length(trainLabels4AL_IT)," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      cat("adding ",newSize," active samples | pool size: ",
          nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS],"\n",sep="")
      

      upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
      upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
      upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
      
      newSize_for_iter = newSize
      trainStart.time <- Sys.time()
      
      # # **********************
      # # **********************
      
      SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(tunedSVM4AL, upd_dataCurFeatsub)), objInfoNames)
      
      sampledResult <- self_learn_AL(
        SVtotal, # NOT USED
        objInfoNames, SVL_variables,
        upd_dataCurFeatsub,upd_dataCurLabels,
        plot_flag = FALSE
      )
      sampled_data <- sampledResult$sampled_data
      reference_label <- sampledResult$best_updCur_Labels
      cat("computing samples distance required ", round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1),"sec\n",sep="")
      
      # # **********************
      # # **********************
      
      
      
      
      trainStart.time <- Sys.time()
      
      
      ALSamplesStart.time <- Sys.time()
      result <- add_AL_samples(sampled_data,
                               sampled_data[,1:numFeat], reference_label,
                               trainFeat4AL_IT, trainLabels4AL_IT,
                               newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
                               upd_dataCur$ID_unit,tSNE_flag = TRUE, flag_cluster = TRUE, newSize2=b[bb]*nclass) #,
      ALS.time <- round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1)
      # cat("getting active-labeled samples and updated datasets required ", ALS.time,"sec\n",sep="")

      # upd_SVindex = upd_dataCur$ID_unit %in% result$IDunit
      
      new_trainFeat <- result$new_trainFeat_AL
      new_trainLabels <- result$new_trainLabels_AL
      

      # remove new_trainFeat from trainDataCurRemaining4AL_IT
      trainDataCurRemaining4AL_IT <- trainDataCurRemaining4AL_IT[-(result$IDunit), ]
      
      # add new_trainFeat allLevel to trainDataCur4AL_IT
      trainDataCur4AL_IT = rbind(trainDataCur4AL_IT,upd_dataCur[upd_dataCur$ID_unit %in% result$IDunit, 1:(ncol(trainDataPoolAllLev))])
      
      
      # **********************
      # get al train set portion
      
      trainFeat4AL_IT <- rbind(setNames(trainFeat4AL_IT, names), setNames(new_trainFeat, names))
      trainLabels4AL_IT <- unlist(list(trainLabels4AL_IT[], new_trainLabels))
      SVtotal = setNames(cbind(trainFeat4AL_IT, trainLabels4AL_IT),c(objInfoNames[-length(objInfoNames)],"REF"))
    
      # **********************
      
      
      # **********************

      countTrainData = nrow(trainFeat4AL_IT)
      indexTrainData = list(c(1:countTrainData))
      
      # tuneFeat = rbind(setNames(trainFeat4AL_IT,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
      # tuneLabel = unlist(list(trainLabels4AL_IT, testLabels))
      
      # join of train and test test data (separable through indexTrainData in svmFit)
      tuneFeat4AL_2AL = rbind(setNames(trainFeat4AL_IT,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub4AL_2IT,objInfoNames[1:(length(objInfoNames)-1)]))
      tuneLabel4AL_2AL = unlist(list(trainLabels4AL_IT, testLabels4AL_2IT))
      
      AL_MS_tunedSVM_2IT = svmFit(tuneFeat4AL_2AL, tuneLabel4AL_2AL, indexTrainData)
      tmp_pred = predict(AL_MS_tunedSVM_2IT, validateFeatsub)
      cm_AL_MS_2IT= confusionMatrix(tmp_pred, validateLabels)
      cat(model_name_AL_MS_2IT," accuracy: ",round(cm_AL_MS_2IT$overall["Accuracy"],5),"\n",sep="")
      
      # **********************
      
      
      
      AccuracyAL_MS_2IT[realization,sample_size] = as.numeric((cm_AL_MS_2IT$overall["Accuracy"]))
      KappaAL_MS_2IT[realization,sample_size] = as.numeric((cm_AL_MS_2IT$overall["Kappa"]))
      SVsAL_MS_2IT[realization,sample_size] = as.numeric(length(AL_MS_tunedSVM_2IT$finalModel@SVindex))

      
      # trainDataCurRemaining_AL <- trainDataCurRemaining4AL_IT[!upd_SVindex, ] # IT IS REQUIRED FOR AL-SVM models
      # trainDataCur4AL_IT = rbind(trainDataCur4AL_IT,upd_dataCur[upd_SVindex,1:(ncol(trainDataPoolAllLev))])
      

      cat("\n") ###############################  AL-SVM EQUAL TO AL_MS  #####################################
      # "AL_SVM" IS EQUAL TO "AL_MS"

      # get original SVs of base SVM ************************* prevent removing an entire class label
      ALSVindex = AL_MS_tunedSVM_2IT$finalModel@SVindex   # indices 1:(sample size per class) ; values
      ALSVtotal = trainDataCur4AL_IT[ALSVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur4AL_IT))]
      freq_table <- table(ALSVtotal$REF)
      zero_label_classes <- names(freq_table[freq_table == 0])
      if (length(zero_label_classes) > 0) {
        cat("info: prevent an entire class label to be removed | realization [",realization,"/",nR,"] | [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
        for (class in zero_label_classes) {
          # Find the indices of rows in train DataCur with the zero label class
          class_indices <- which(trainLabels4AL_IT == class)
          # Extract the corresponding rows from trainFeat and trainLabels
          class_feat <- trainFeat4AL_IT[class_indices, , drop = FALSE]
          class_labels <- trainLabels4AL_IT[class_indices, drop = FALSE]
          # Create a data frame with the same structure as SVtotal
          class_data <- cbind(class_feat, class_labels)
          # Append the class_data to SVtotal
          ALSVtotal <- rbind(setNames(ALSVtotal,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")), setNames(class_data,c(objInfoNames[1:(length(objInfoNames)-1)],"REF")))
          ALSVindex <- c(ALSVindex, class_indices)
        }
      }
      # ******************************************************
      binaryClassProblem = list()
      for (jj in seq_along(AL_MS_tunedSVM_2IT$finalModel@xmatrix)) { # COMPARE EVERY COUPLE COMBINATION OF CLASSES
        binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur4AL_IT[AL_MS_tunedSVM_2IT$finalModel@alphaindex[[jj]], ncol(trainDataCur4AL_IT)]))
      }
      # ******************************************************
      names = objInfoNames[1:(length(objInfoNames)-1)]
      
      cat("\n") ###############################  AL-SVM-SL + semi-labeled samples #####################################
      model_name_AL_SVMUn = "AL_SVM_SLUn"
      
      trainStart.time <- Sys.time()
      # Definition of sampling configuration (strata:random sampling without replacement)
      stratSampRemaining_b = strata(trainDataCurRemaining_AL, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      

      
  
      # get samples of trainDataCurRemaining_AL and set trainDataCurRemaining_AL new
      samplesRemaining_b = getdata(trainDataCurRemaining_AL, stratSampRemaining_b)
      # trainDataCurRemaining_AL <- trainDataCurRemaining_AL[-c(samplesRemaining_b$ID_unit), ]
      
      trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]
      
      
      
      
      REFALSVM = predict(AL_MS_tunedSVM_2IT, trainDataCurRemainingsub_SL)
      
      # get SV of unlabeled samples
      SVindexALSVMUn = 1:nrow(trainDataCurRemainingsub_SL)
      SVtotalALSVMUn = trainDataCurRemaining_SL[SVindexALSVMUn ,c(sindexSVMDATA:eindexSVMDATA)]
      SVtotalALSVMUn = cbind(SVtotalALSVMUn, REFALSVM)
      
      cat("evaluation of SVM with self learning and semi-labeled samples| realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
      SVL_variables = list(
        list(SVtotalALSVMUn, SVL2SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL3SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM)),
        list(SVtotalALSVMUn, SVL5SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL6SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL7SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL8SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL9SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL10SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFALSVM)),
        list(SVtotalALSVMUn, SVL11SVMUn = cbind(trainDataCurRemaining_SL[SVindexALSVMUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFALSVM))
      )
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_SVMUn, ALSVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables, AL_MS_tunedSVM_2IT$finalModel)
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
      cat("ALSVM_SL_Un accuracy: ",round(cm_AL_SVM_SL_Un$overall["Accuracy"],5),"\n",sep="")
      
      AccuracyALSVM_SL_Un_2IT[realization,sample_size] = as.numeric(cm_AL_SVM_SL_Un$overall["Accuracy"])
      KappaALSVM_SL_Un_2IT[realization,sample_size] = as.numeric(cm_AL_SVM_SL_Un$overall["Kappa"])
      SVsALSVM_SL_Un_2IT[realization,sample_size] = as.numeric(length(bestFittingALModelSVMUn$finalModel@SVindex))
      if (cm_AL_SVM_SL_Un$overall["Accuracy"]>best_acc) {
        best_acc <- cm_AL_SVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingALModelSVMUn
        best_model_name <- model_name_AL_SVMUn
      }
      
      cat("\n") ###############################  AL-VSVM-SL ################################################
      model_name_AL_VSVM_SL = "AL_VSVM_SL"
      
      trainStart.time <- Sys.time()
      
      SVL2 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCur))]
      SVL3 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCur))]
      
      SVL5 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCur))]
      SVL6 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCur))]
      SVL7 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCur))]
      SVL8 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCur))]
      SVL9 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCur))]
      SVL10 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
      SVL11 = trainDataCur4AL_IT[ALSVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]
      
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
      
      cat("evaluation of VSVM with self learning| realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
      
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
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_VSVM_SL, ALSVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables, AL_MS_tunedSVM_2IT$finalModel)
      bestFittingALModel <- SLresult$bestFittingModel
      best_trainFeatALVSVM <- SLresult$best_trainFeatVSVM
      best_trainLabelsALVSVM <- SLresult$best_trainLabelsVSVM
      best_bound_SL = SLresult$best_bound
      best_boundMargin_SL = SLresult$best_boundMargin
      t.timeALSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+trainSVM.time, 1)
      
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsALVSVMsum = predict(bestFittingALModel, validateFeatsub)
      cm_AL_VSVM_SL = confusionMatrix(predLabelsALVSVMsum, validateLabels)
      cat("AL VSVM_SL accuracy: ",round(cm_AL_VSVM_SL$overall["Accuracy"],5),"\n",sep="")
      
      AccuracyALVSVM_SL_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL$overall["Accuracy"])
      KappaALVSVM_SL_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL$overall["Kappa"])
      SVsALVSVM_SL_2IT[realization,sample_size] = as.numeric(length(bestFittingALModel$finalModel@SVindex))
      if (cm_AL_VSVM_SL$overall["Accuracy"]>best_acc) {
        best_acc <- cm_AL_VSVM_SL$overall["Accuracy"]
        new_bestModel <- bestFittingALModel
        best_model_name <- model_name_AL_VSVM_SL
      }
      
      
      cat("\n") ###############################  AL-VSVM-SL + semi-labeled samples #####################################
      model_name_AL_VSVM_SLUn = "AL_VSVM_SLUn"
      
      trainStart.timeUn <- Sys.time()
      actAcc_ALvUn = -1e-6
      bb = 1 # for (bb in seq(along=b)) {
      
      # # Definition of sampling configuration (strata:random sampling without replacement)
      # stratSampRemaining_b = strata(trainDataCurRemaining_AL, c("REF"), size = c(b[bb],b[bb],b[bb],b[bb],b[bb],b[bb]), method = "srswor")
      # 
      # # get samples of trainDataCurRemaining_AL and set trainDataCurRemaining_AL new
      # samplesRemaining_b = getdata(trainDataCurRemaining_AL, stratSampRemaining_b)
      # # trainDataCurRemaining_AL <- trainDataCurRemaining_AL[-c(samplesRemaining_b$ID_unit), ]
      # 
      # trainDataCurRemaining_SL = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
      # trainDataCurRemainingsub_SL = trainDataCurRemaining_SL[sindexSVMDATA:eindexSVMDATA]
      
      REF_b = predict(bestFittingALModel, trainDataCurRemainingsub_SL)
      
      # get SV of unlabeled samples
      indexUn = 1:nrow(trainDataCurRemainingsub_SL)
      totalUn = trainDataCurRemaining_SL[indexUn ,c(sindexSVMDATA:eindexSVMDATA)]
      totalUn = cbind(totalUn, REF_b)
      
      cat("evaluation of VSVM SL with ",b[bb]," semi-labeled samples | realization [",realization,"/",nR,"] | samples: ",sampleSizePor[sample_size]," [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="")
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
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_Un, ALSVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables, AL_MS_tunedSVM_2IT$finalModel)
      bestFittingALModelUn <- SLresult$bestFittingModel
      best_trainFeatALVSVMUn <- SLresult$best_trainFeatVSVM
      best_trainLabelsALVSVMUn <- SLresult$best_trainLabelsVSVM
      # best_bound_SL_Un = SLresult$best_bound
      best_boundMargin_SL_Un = SLresult$best_boundMargin
      trainALUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      predLabelsALVSVMsumUn = predict(bestFittingALModelUn, validateFeatsub)
      cm_AL_VSVM_SL_Un = confusionMatrix(predLabelsALVSVMsumUn, validateLabels)
      cat("AL VSVM_SL_Un accuracy: ",round(cm_AL_VSVM_SL_Un$overall["Accuracy"],5),"\n",sep="")
      
      AccuracyALVSVM_SL_Un_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL_Un$overall["Accuracy"])
      KappaALVSVM_SL_Un_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL_Un$overall["Kappa"])
      SVsALVSVM_SL_Un_2IT[realization,sample_size] = as.numeric(length(bestFittingALModelUn$finalModel@SVindex))
      if (cm_AL_VSVM_SL_Un$overall["Accuracy"] > best_acc) {
        best_acc <- cm_AL_VSVM_SL_Un$overall["Accuracy"]
        new_bestModel <- bestFittingALModelUn
        best_model_name <- model_name_AL_VSVM_SLUn
      }
      
      
      cat("\n") ###############################  AL-VSVM-SL + virtual semi-labeled samples ##################################
      model_name_AL_VSVM_SLvUn = "AL_VSVM_SLvUn"
      
      REF_v = predict(bestFittingALModelUn, trainDataCurRemainingsub_SL)
      
      # get SV of unlabeled samples
      indexvUn_v = 1:nrow(trainDataCurRemainingsub_SL) #  bestFittingModelUn$finalModel@SVindex
      SVtotalvUn_v = trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA)] #na.omit(trainDataCurRemaining_SL[indexvUn_v ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_SL))])
      SVtotalvUn_v = cbind(SVtotalvUn_v, REF_v)
      
      # extracting previously assigned reference column
      SVtotalvUn_vFeat = SVtotalvUn_v[,1:(ncol(SVtotalvUn_v)-1)] # -1)]
      REF_v = SVtotalvUn_v[,(ncol(SVtotalvUn_v))]
      SVtotalvUn_v = cbind(SVtotalvUn_vFeat, REF_v)
      
      cat("evaluation of AL VSVM SL with ",b[bb]," virtual semi-labeled samples | realization [",realization,"/",nR,"] | samples: [",(sample_size),"/",round((length(sampleSizePor))),"]\n",sep="") #  [",bb,"/",length(b),"]","
      
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
      
      SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_vUn, ALSVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                             SVL_variables, AL_MS_tunedSVM_2IT$finalModel)
      bestFittingALModelvUn <- SLresult$bestFittingModel

      new_best_bound_SLvUn = SLresult$best_bound
      trainALvUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
      # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
      new_predLabelsALVSVMvUnsum = predict(bestFittingALModelvUn, validateFeatsub)
      cm_AL_VSVM_SL_vUn = confusionMatrix(new_predLabelsALVSVMvUnsum, validateLabels)
      
      cat("AL VSVM_SL_vUn accuracy: ",round(cm_AL_VSVM_SL_vUn$overall["Accuracy"],5),"\n",sep="")
      
      AccuracyALVSVM_SL_vUn_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL_vUn$overall["Accuracy"])
      KappaALVSVM_SL_vUn_2IT[realization,sample_size] = as.numeric(cm_AL_VSVM_SL_vUn$overall["Kappa"])
      SVsALVSVM_SL_vUn_2IT[realization,sample_size] = as.numeric(length(bestFittingALModelvUn$finalModel@SVindex))
      if (cm_AL_VSVM_SL_vUn$overall["Accuracy"] > best_acc){
        best_acc <- cm_AL_VSVM_SL_vUn$overall["Accuracy"]
        new_bestModel <- bestFittingALModelvUn
        best_model_name <- model_name_AL_VSVM_SLvUn
      }
      #######################################################################################################
      
      
    
    


    cat("\n") #################################  End sample portion #########################################
    if (sample_size==length(sampleSizePor)) {
      saveRDS(tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelSVMUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingModelvUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(bestFittingALModelSVMUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModelUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_VSVM_SLUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(bestFittingALModelvUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_VSVM_SLvUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      

      saveRDS(AL_MS_tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MS,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(AL_MS_tunedSVM_2IT, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_MS_2IT,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      
      saveRDS(cm_SVM,paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_SVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_VSVM_SL_vUn, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(cm_AL_MS, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MS,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_MS_2IT, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_MS_2IT,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      
      saveRDS(cm_AL_SVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL_Un, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_VSVM_SLUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
      saveRDS(cm_AL_VSVM_SL_vUn, paste0(format(Sys.time(),"%Y%m%d"),"_confusionMatrix_",model_name_AL_VSVM_SLvUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b[bb],"Unl_realiz",realization,"_",seed,"seed.rds"))
    }
  }
  cat("\n") ###################################  End realization ############################################
  # Store hyperparameters 
  best_model_oa=c(best_model_oa,best_model_name,": ",as.numeric(best_acc),"\n")
  time.taken_iter = c(time.taken_iter, c("Realization ",realization," | seed: ",seed," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h"),"\n")
  cat("Realization ",realization," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h\n\n")
} 
if (length(sampleSizePor)>=2) {
  time.taken_oa <- round(as.numeric((Sys.time() - start.time_oa), units = "hours"), 2)
  setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city,"/",model_prob,"/",invariance))
  save(AccuracySVM, 
       AccuracySVM_SL_Un,
       AccuracyVSVM_SL,
       AccuracyVSVM_SL_Un,
       AccuracyVSVM_SL_vUn,
       
       AccuracyAL_MS,
       AccuracyAL_MS_2IT,
       
       AccuracyALSVM_SL_Un_2IT,
       AccuracyALVSVM_SL_2IT,
       AccuracyALVSVM_SL_Un_2IT,
       AccuracyALVSVM_SL_vUn_2IT,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_acc_",script,"_",b[bb],"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  save(KappaSVM, 
       KappaSVM_SL_Un,
       KappaVSVM_SL,
       KappaVSVM_SL_Un,
       KappaVSVM_SL_vUn,
       
       KappaAL_MS,             
       KappaAL_MS_2IT,             

       KappaALSVM_SL_Un_2IT,
       KappaALVSVM_SL_2IT,
       KappaALVSVM_SL_Un_2IT,
       KappaALVSVM_SL_vUn_2IT,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_Kappa_",script,"_",b[bb],"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  save(SVsSVM, 
       SVsSVM_SL_Un,
       SVsVSVM_SL,
       SVsVSVM_SL_Un,
       SVsVSVM_SL_vUn,

       SVsAL_MS,
       SVsAL_MS_2IT,
       
       SVsALSVM_SL_Un_2IT,
       SVsALVSVM_SL_2IT,
       SVsALVSVM_SL_Un_2IT,
       SVsALVSVM_SL_vUn_2IT,
       
       file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_SVs_",script,"_",b[bb],"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData")
       )
  
  cat("OA Execution time: ", time.taken_oa, "h\n", time.taken_iter,"\n",best_model_oa,
      

      
      "\nNumber of ", model_name_tunedSVM ," SVs: ",length(tunedSVM$finalModel@SVindex),
      "\nNumber of ", model_name_SVMUn ," SVs: ",length(bestFittingModelSVMUn$finalModel@SVindex),
      "\nNumber of ", model_name_VSVM_SL ," SVs: ",length(bestFittingModel$finalModel@SVindex),
      "\nNumber of ", model_name_Un ," SVs: ",length(bestFittingModelUn$finalModel@SVindex),
      "\nNumber of ", model_name_vUn ," SVs: ",length(bestFittingModelvUn$finalModel@SVindex),
      
      "\nNumber of ", model_name_AL_MS," SVs: ",length(AL_MS_tunedSVM$finalModel@SVindex),
      "\nNumber of ", model_name_AL_MS_2IT," SVs: ",length(AL_MS_tunedSVM_2IT$finalModel@SVindex),
      
      "\nNumber of final train Labels AL: ",length(trainLabels4AL_IT),
      sep = "", file = paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_metadata_",script,"_",city,"_",model_prob,"_",invariance,"_",b[bb],"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.txt")
      )
  print(confusionMatrix(new_trainLabels,predict(bestFittingModel, new_trainFeat)))
  cat("Number of ", model_name_AL_MS ," SVs: ",length(AL_MS_tunedSVM_2IT$finalModel@SVindex),"\nNumber of final train Labels AL: ",length(trainLabels4AL_IT),"\n\n",sep="")
  cat("performance results: acquired\n\n\n")
}








ExCsvMSD = function (datadase, filename = NA){
  
  datadase = as.matrix(datadase)
  n = ncol(datadase)
  if(n>1){
    MSDdata = matrix(data = NA, nrow = 2, ncol = n)
    for (l in seq(along=1:n)){
      MSDdata[1,l] = mean(datadase[,l])
      MSDdata[2,l] = sd(datadase[,l])
    }
    MSDdata_final = rbind(datadase, MSDdata) 
    #export final kappa value table to .csv-file
    if(!missing(filename)){ write.csv(MSDdata_final, filename) }
    
  }else{
    MSDdata = matrix(data = NA, nrow = 2, ncol = length(datadase))
    MSDdata[1,] = datadase
    MSDdata[2,] = 0
  }
  rownames(MSDdata) = c("Mean","Std Dev")
  return(MSDdata)
}

nclass=6
if(model_prob=="binary"){ nclass=2 }

column_names <- colnames(AccuracySVM)

x <- 2*as.integer(column_names)/nclass # we consider also test/validate samples 

lenged_names = c("SVM single-level L4", 
                 "SVM-SL + semi-labeled", 
                 "VSVM-SL",
                 "VSVM-SL + semi-labeled",
                 "VSVM-SL + virtual semi-labeled",
                 
                 "AL MS",
                 "AL MS semi AL",

                 "AL SVM-SL + semi-labeled", 
                 "AL VSVM-SL",
                 "AL VSVM-SL + semi-labeled",
                 "AL VSVM-SL + virtual semi-labeled"
)

# ===== Colors by family =====
SVM_col        <- 1   # black
SVM_SL_col     <- 2   # red
VSVM_SL_col    <- 3   # blue

AL_MS_col      <- 5   # purple
AL_SVM_SL_col  <- 6   # orange
AL_VSVM_SL_col <- 7   # brown

legend_col = c(SVM_col, SVM_SL_col, VSVM_SL_col, VSVM_SL_col, VSVM_SL_col,   # VSVM-SL family shares blue
               
               AL_MS_col, AL_MS_col, AL_MS_col,          # AL MS family shares purple
               AL_SVM_SL_col,                            # AL SVM-SL
               AL_VSVM_SL_col, AL_VSVM_SL_col, AL_VSVM_SL_col # AL VSVM-SL family shares brown
)

# ===== Line types per variant =====
SVM_lty        <- 1    # solid
SVM_SL_lty     <- 2    # dashed
VSVM_SL_lty    <- 1    # solid
VSVM_SL_Un_lty <- 2    # dashed
VSVM_SL_vUn_lty<- 3    # dotted

AL_MS_lty      <- 1    # solid
AL_MS_semiAL_lty<- 2   # dashed
AL_MS_2IT_lty <- 3 # dotted

AL_SVM_SL_lty  <- 2    # dashed
AL_VSVM_SL_lty <- 1    # solid
AL_VSVM_SL_Un_lty<- 2  # dashed
AL_VSVM_SL_vUn_lty<- 3 # dotted

lenged_lty = c(SVM_lty, SVM_SL_lty, VSVM_SL_lty, VSVM_SL_Un_lty, VSVM_SL_vUn_lty,

               AL_MS_lty, AL_MS_semiAL_lty, AL_MS_2IT_lty,
               
               AL_SVM_SL_lty,
               AL_VSVM_SL_lty, AL_VSVM_SL_Un_lty, AL_VSVM_SL_vUn_lty
)

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","images/",city))

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.80 # 0.76
    ylowerBound = 0.48 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}

type = "l"

######################################## Accuracy ##########################################
file_name_acc = "20251009_cologne_multiclass_scale_acc_AQS3VSVM_20Unl_20nR_7SizePor"
file_name_kappa = "20251009_cologne_multiclass_scale_Kappa_AQS3VSVM_20Unl_20nR_7SizePor"


png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=24,
    res=96)

# # ******************************************************************************************************

  
  avgSVM           = ExCsvMSD(AccuracySVM)[1,]
  avgSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un)[1,]
  avgVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL)[1,]
  avgVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un)[1,]
  avgVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn)[1,]

  avgAL_MS      = ExCsvMSD(AccuracyAL_MS)[1,]
  avgAL_MS_2IT      = ExCsvMSD(AccuracyAL_MS_2IT)[1,]
  
  avgALSVM_SL_Un_b   = ExCsvMSD(AccuracyALSVM_SL_Un_1IT)[1,]
  avgALVSVM_SL       = ExCsvMSD(AccuracyALVSVM_SL_1IT)[1,]
  avgALVSVM_SL_Un_b  = ExCsvMSD(AccuracyALVSVM_SL_Un_1IT)[1,]
  avgALVSVM_SL_vUn_b = ExCsvMSD(AccuracyALVSVM_SL_vUn_1IT)[1,]
  
  # *********************************************
  
  msdSVMPlot = plot(x, avgSVM,log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,       col = SVM_col, lwd = 2,lty = SVM_lty,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  
  lines(x, avgSVM_SL_Un_b,   type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
  lines(x, avgVSVM_SL,       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
  lines(x, avgVSVM_SL_Un_b,  type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
  lines(x, avgVSVM_SL_vUn_b, type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)
  
  lines(x, avgAL_MS,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_lty)
  lines(x, avgAL_MS_2IT,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_2IT_lty)
  
  lines(x, avgALSVM_SL_Un_b,   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
  lines(x, avgALVSVM_SL,       type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
  lines(x, avgALVSVM_SL_Un_b,  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
  lines(x, avgALVSVM_SL_vUn_b, type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)
  

  # # ******************************************************************************************************

legend("bottomright", 
       lenged_names,
       lty = lenged_lty,
       col= legend_col
) 

dev.off()

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.81 # 0.76
    ylowerBound = 0.45 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}

# ===== Accuracy +/- std dev =====
if(nrow(AccuracySVM) > 1){
  
  sdSVM           = ExCsvMSD(AccuracySVM)[2,]
  sdSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un)[2,]
  sdVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL)[2,]
  sdVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un)[2,]
  sdVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn)[2,]
  
  sdAL_MS  = ExCsvMSD(AccuracyAL_MS)[2,]  
  sdAL_MS_2IT  = ExCsvMSD(AccuracyAL_MS_2IT)[2,]  
  
  sdALSVM_SL_Un_b    = ExCsvMSD(AccuracyALSVM_SL_Un_1IT)[2,]
  sdALVSVM_SL        = ExCsvMSD(AccuracyALVSVM_SL_1IT)[2,]
  sdALVSVM_SL_Un_b   = ExCsvMSD(AccuracyALVSVM_SL_Un_1IT)[2,]
  sdALVSVM_SL_vUn_b  = ExCsvMSD(AccuracyALVSVM_SL_vUn_1IT)[2,]
  
  png(filename=paste0(file_name_acc,"_sd.png"),
      units="in", width=20, height=16,
      pointsize=24, res=96)
  
  msdSVMPlot = plot(x, avgSVM, log="x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type=type, col=SVM_col, lwd=2, lty=SVM_lty,
                    xlab="number of labeled samples per class",
                    ylab="accuracy (%) +/- std dev",
                    main=paste(city,"-", model_prob,"classification problem -", invariance,"invariance"))
  
  # Families
  lines(x, avgSVM_SL_Un_b,   type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
  lines(x, avgVSVM_SL,       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
  lines(x, avgVSVM_SL_Un_b,  type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
  lines(x, avgVSVM_SL_vUn_b, type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)
  
  lines(x, avgAL_MS,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_lty)
  lines(x, avgAL_MS_2IT,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_2IT_lty)
  
  lines(x, avgALSVM_SL_Un_b,   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
  lines(x, avgALVSVM_SL,       type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
  lines(x, avgALVSVM_SL_Un_b,  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
  lines(x, avgALVSVM_SL_vUn_b, type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)
  
  # Std dev bars (arrows)
  arrows(x, avgVSVM_SL - sdVSVM_SL,     x, avgVSVM_SL + sdVSVM_SL,     length=0.075, angle=90, code=3, col=VSVM_SL_col, lty=VSVM_SL_lty)
  arrows(x, avgVSVM_SL_vUn_b - sdVSVM_SL_vUn_b, x, avgVSVM_SL_vUn_b + sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3, col=VSVM_SL_col, lty=VSVM_SL_vUn_lty)
  arrows(x, avgVSVM_SL_Un_itTSL - sdVSVM_SL_Un_itTSL, x, avgVSVM_SL_Un_itTSL + sdVSVM_SL_Un_itTSL, length=0.075, angle=90, code=3, col=AL_MS_col, lty=AL_MS_semiAL_lty)
  
  legend("bottomright", lenged_names, lty=lenged_lty, col=legend_col) 
  dev.off()
}

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.72 # 0.76
    ylowerBound = 0.35 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}

# ===== Kappa =====
png(filename=paste0(file_name_kappa,".png"),
    units="in", width=20, height=16,
    pointsize=24, res=96)

if(nrow(KappaSVM) > 1){
  msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,], log="x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type=type, col=SVM_col, lwd=2, lty=SVM_lty,
                    xlab="number of labeled samples per class",
                    ylab="Kappa-score",
                    main=paste(city,"-", model_prob,"classification problem -", invariance,"invariance"))
  
  lines(x, ExCsvMSD(KappaSVM_SL_Un)[1,],     type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL)[1,],       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un)[1,],    type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL_vUn)[1,],   type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)
  
  lines(x, ExCsvMSD(KappaAL_MS)[1,],         type=type, col=AL_MS_col,       lwd=2, lty=AL_MS_lty)
  lines(x, ExCsvMSD(KappaAL_MS_2IT)[1,],         type=type, col=AL_MS_col,       lwd=2, lty=AL_MS_2IT_lty)
  
  lines(x, ExCsvMSD(KappaALSVM_SL_Un_1IT)[1,],   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL_1IT)[1,],     type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL_Un_1IT)[1,],  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL_vUn_1IT)[1,], type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)
}

legend("bottomright", lenged_names, lty=lenged_lty, col=legend_col) 
dev.off()

