library(caret)
library(kernlab)
library(sampling)
library(progress) # for progress bar visualization
library(foreach)    # for parallel processing
library(doParallel) # for multiple CPU core
library(stats) # for k-means clustering
# Adjust the number of cores depending on the system capabilities
num_cores <- parallel::detectCores()

# Define the class sample size 
sample_size = 2 # 41 label per class

# Decide if train the SVM or load from dir the saved ones if present, default it tries to load them
train = TRUE

# Choose between Binary classification or Multiclass 
binary = FALSE

# Define the size of unlabeled samples in each class
b = 20 # balanced_unlabeled_samples

#path = "D:/tunc_oz/apply_model"
path = '/home/rsrg9/Documents/tunc_oz/apply_model/'
model_path = "/home/rsrg9/Documents/GitHub/active-learning-virtual-SVM/"
########################################  Utils  ########################################

# Coarse and Narrow grid search for SVM parameters tuning
# showPrg = TRUE
# classProb = FALSE
svmFit = function(x, y, indexTrain, classProb = FALSE, showPrg = TRUE){ #x = training descriptors, y = class labels
  
  #expand coarse grid
  coarseGrid = expand.grid(sigma = 2^seq(-5,3,by=2), C = 2^seq(-4,12,by=2))
  
  #set seed
  set.seed(13)
  if(showPrg){print("Running coarse grid search...")}
  
  svmFitCoarse = train(x,    
                       y, 
                       method = "svmRadial",
                       metric = "Kappa",
                       maximize = TRUE,
                       tuneGrid = coarseGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]],
                                                  classProbs =  classProb),
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
  svmFitNarrow = train(x, 
                       y, 
                       method = "svmRadial",
                       metric = "Kappa",
                       maximize = TRUE,
                       tuneGrid = narrowGrid,
                       trControl = trainControl ( method = "cv",
                                                  #verboseIter=T,
                                                  index = indexTrain,
                                                  indexFinal= indexTrain[[1]],
                                                  classProbs =  classProb),
                       
                       scaled = FALSE)

  return(svmFitNarrow)  
}

# ***********************************

rbf_kernel <- function(x, y, sigma) {
  # Calculate the squared Euclidean distance between x and y
  distance_squared <- sum((x - y)^2)
  
  # Compute the RBF kernel value using the squared distance and sigma
  kernel_value <- exp(-distance_squared / (2 * sigma^2))
  
  return(kernel_value)
}

# Kernel distance between two point lying in the hyperspace
kern_dis = function(a, b){
  a  <- unlist(a)
  b  <- unlist(b)
  dk <- sqrt( rbf_kernel(a,a,1)+rbf_kernel(b,b,1)-2*rbf_kernel(a,b,1))
  return(dk)
}

# rem_extrem_kerneldist(SVtotal, SVL7, bound[jj])
rem_extrem_kerneldist = function(org, VSV1, a){

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
    distance[l,2] = kern_dis(org[l,-ncol(org)],VSV1[l,-ncol(VSV1)])
  }

  boundClass = list()

  # Compute the distance threshold boundClass for each class
  for(f in seq(along = c(1:length(SVClass)))){
    distanceSVC1 = c()
    if(nrow(SVClass[[f]])>0){
      for(n in seq(along = 1:(nrow(SVClass[[f]])-1))){
        for(nn in seq(along = c(n:(nrow(SVClass[[f]])-1)))){
          distanceSVC1[length(distanceSVC1)+1] = kern_dis(SVClass[[f]][n,-ncol(SVClass[[f]])], SVClass[[f]][(n+nn),-ncol(SVClass[[f]])])
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
    }
    #if(!tmp_cond){VSV1[k,]=NA}
  }
  return(VSV1)
}
# # FOR DEBUGGING SAKE
# distance = data.frame(matrix(nrow=nrow(SVtotalUn_b),ncol=2))
# distanceSVC1 = c()
# distanceSVC2 = c()
# 
# numClass = nlevels(SVtotalUn_b$REF)
# SVClass = list()
# 
# # split SV according to its classes
# for(f in seq(along = c(1:numClass))){
#   SVClass[[f]]=SVtotalUn_b[which(SVtotalUn_b$REF==levels(SVtotalUn_b$"REF")[[f]]),]
# }
# 
# # save label of sample and the distance between SV and VSV in distance for each pair of SV and VSV
# for(l in seq(along = c(1:nrow(SVtotalUn_b)))){
#   distance[l,1] = as.character( SVtotalUn_b[l,ncol(SVtotalUn_b)])
#   distance[l,2] = kern_dis(SVtotalUn_b[l,-ncol(SVtotalUn_b)],SVL3Un_b[l,-ncol(SVL3Un_b)])
# }
# 
# boundClass = list()
# 
# for(f in seq(along = c(1:length(SVClass)))){
#   distanceSVC1 = c()
#   if(nrow(SVClass[[f]])>0){
#     for(n in seq(along = 1:(nrow(SVClass[[f]])-1))){
#       for(nn in seq(along = c(n:(nrow(SVClass[[f]])-1)))){
#         distanceSVC1[length(distanceSVC1)+1] = kern_dis(SVClass[[f]][n,-ncol(SVClass[[f]])], SVClass[[f]][(n+nn),-ncol(SVClass[[f]])])
#       }
#     }
#     disClass1mean = mean(distanceSVC1)
#     boundClass[[f]] = disClass1mean*bound[jj]
#   }
# }

pred_one = function(modelfin, dataPoint, dataPointLabels, binaryClassProblem=binaryClassProblem){
  
  smallestDistance = 9999
  
  for(ll in seq(along = dataPointLabels)){
    
    #print(dataPointLabels[ll])
    for(l in seq(along = binaryClassProblem)){
      #print(binaryClassProblem[[l]])

      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProblem[[l]])){
        #print(paste("vero", pred))
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
  
  # for (k in seq_along(1:nrow(samp))) { # SEQUENTIAL OLD IMPLEMETATION 
  #   distance[k, "distance"] <- pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), samp[k, ncol(samp)])
  # }
  
  registerDoParallel(num_cores)
  distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), samp[k, ncol(samp)])
  }
  registerDoSEQ()
  scaled_distances <- apply(distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  distance$distance <- scaled_distances
  
  samp <- cbind(samp, distance)
  return(samp)
  #return(distance)
}
# # FOR DEBUGGING SAKE
# probabilities <- predict(bestFittingModelUn_b, predLabelsVSVMsumUn_unc[1,1:ncol(predLabelsVSVMsumUn_unc) - 1],type="prob")
# 
# smallestDistance = 9999
# # binaryNameClasses = names(probabilities)[1]
# for(l in seq(along = binaryNameClasses)){
#   if(unlist(predLabelsVSVMsumUn_unc[1, length(predLabelsVSVMsumUn_unc)]) %in% binaryNameClasses[l]){
#     print("veroo")
#     print(binaryNameClasses[l])
#     pred = sum(sapply(1:nrow(bestFittingModelUn_b$finalModel@xmatrix[[1]]), function(j)
#       bestFittingModelUn_b$finalModel@kernelf(xmatrix(bestFittingModelUn_b$finalModel)[[l]][j,], unlist(predLabelsVSVMsumUn_unc[1,1:(length(predLabelsVSVMsumUn_unc)-1)]))*bestFittingModelUn_b$finalModel@coef[[l]][j]))-bestFittingModelUn_b$finalModel@b[l]
#     print(pred)
#     if(abs(pred) < abs(smallestDistance))
#       smallestDistance = abs(pred)
#     }
#   }
# smallestDistance

# # Evaluate Margin Sampling (MS) # SEQUENTIAL PROCESSING (OLD IMPLEMENTATION)
# margin_sampling <- function(org, samp) {
#   # Initialize data frame to store margin distance for each sample
#   margin_distance <- data.frame(control_label = as.character(samp[, ncol(samp)]), margin_distance = numeric(nrow(samp)))
# 
#   # Progress bar for tracking computation
#   pb <- progress_bar$new(
#     format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
#     total = nrow(samp),
#     clear = FALSE
#   )
# 
#   for (k in seq_along(1:nrow(samp))) {
#     # Get prediction probabilities for the current sample
#     probabilities <- predict(org, newdata = samp[k, -ncol(samp)], type = "prob")
# 
#     # Find the class with the maximal confidence
#     max_confidence_class <- which.max(probabilities)
# 
#     # Get the distance to the hyperplane for each class
#     distances <- rep(0, length(probabilities))
#     for (i in 1:length(probabilities)) {
#       distances[factor(names(probabilities))[i]] <- pred_one(org$finalModel, unlist(samp[k,-ncol(samp)]),factor(names(probabilities))[i])
#     }
# 
#     # Calculate the margin distance as the difference between the distance to the hyperplane of the max confidence class and the distance to the hyperplane of the other classes
#     margin_distance[k, "margin_distance"] <- distances[max_confidence_class] - max(distances[-which.max(probabilities)])
# 
#     pb$tick()
#   }
# 
#   preProc <- preProcess(margin_distance, method = "range")
#   normdistance <- predict(preProc, margin_distance)
# 
#   merged_data <- cbind(samp, normdistance)
# 
#   #return(margin_distance)
#   return(merged_data)
# }

# # FOR DEBUGGING SAKE
# margin_distance <- data.frame(control_label = as.character(predLabelsVSVMsumUn_unc[1, ncol(predLabelsVSVMsumUn_unc)]), margin_distance = numeric(nrow(predLabelsVSVMsumUn_unc[1,])))
# probabilities <- predict(bestFittingModelUn_b, predLabelsVSVMsumUn_unc[1,-ncol(predLabelsVSVMsumUn_unc)],type="prob")
# max_confidence_class <- which.max(probabilities)
# distances <- rep(0, length(probabilities))
# for (i in 1:length(probabilities)) {
#   distances[factor(names(probabilities))[i]] <- pred_one(bestFittingModelUn_b$finalModel, unlist(predLabelsVSVMsumUn_unc[1,-ncol(predLabelsVSVMsumUn_unc)]),factor(names(probabilities))[i])
# }
# 
# margin_distance[1, "margin_distance"] <- distances[max_confidence_class] - max(distances[-which.max(probabilities)])

# Evaluate Margin Sampling (MS) WITH MULTICORES CPU - PARALLEL COMPUTING
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
  # cl <- makeCluster(num_cores)
  registerDoParallel(num_cores)
  
  # Use foreach for parallel processing
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_margin_distance(k)
  }
  registerDoSEQ()
  # margin_distance$distance <- margin_distances
  # preProc <- preProcess(margin_distance, method = "range")
  # normdistance <- predict(preProc, margin_distance)
  
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
  # uncertainty$distance <- mclu_distances
  # preProc <- preProcess(uncertainty, method = "range")
  # normdistance <- predict(preProc, uncertainty)
  
  mclu_distances <- apply(mclu_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  uncertainty$distance <- mclu_distances
  
  merged_data <- cbind(samp, uncertainty)
  
  return(merged_data)
}

# mclp_sampled_data <- mclp_sampling(bestFittingModelUn_b, predLabelsVSVMsumUn_unc)
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

alter_labels = function(distance_data, ref, newSize){
  
  #merge features and original labels
  ref_added = cbind(distance_data,ref)
  #order by most uncertain samples
  ref_added_or = ref_added[order(ref_added$distance),]
  #re-label most uncertain n number of samples
  ref_added_or[1:newSize,]$label = ref_added_or[1:newSize,]$ref
  ref_added_or[1:newSize,]$distance = 1.0000000000
  #re-order data set by its index
  ref_added_or$index = as.numeric(row.names(ref_added_or))
  ref_added_reor = ref_added_or [order(ref_added_or$index),]
  
  #extract labels for prediction
  labels= ref_added_reor[,(ncol(ref_added_reor)-4)]
  #uncertainty= ref_added_reor[,(ncol(ref_added_reor)-2)]
  return(labels)
}

#   # CHECK IF ref_added_or AND ref_added_or HAVE THE SAME COLUMNS
#   
#   return(list(features = features, 
#               labels = ref, 
#               new_trainFeatVSVM = new_trainFeatVSVM, 
#               new_trainLabelsVSVM = new_trainLabelsVSVM))
# }

# result <- add_new_samples(sampled_data, 
#                           upd_trainDataCurFeatsub, 
#                           upd_trainDataCurLabels, 
#                           new_trainFeatVSVM, 
#                           new_trainLabelsVSVM, 
#                           newSize=newSize_for_iter,
#                           cluster=120 )

add_new_samples = function(distance_data=sampled_data,
                           features=upd_trainDataCurFeatsub,
                           ref=upd_trainDataCurLabels,
                           new_trainFeatVSVM=new_trainFeatVSVM,
                           new_trainLabelsVSVM=new_trainLabelsVSVM,
                           newSize=newSize_for_iter,
                           cluster=120){
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
      # ref_added_or[sample,]$distance <- 1.0000000000
      
      selected_indices <- c(selected_indices, as.numeric(rownames(ref_added_or[sample,])))
    }
  }
  # # If not enough clusters, select additional samples from the remaining
  # if (length(selected_indices) < newSize) {
  #   remaining_indices <- setdiff(1:nrow(ref_added_or), selected_indices)
  #   additional_indices <- remaining_indices[1:(newSize - length(selected_indices))]
  #   selected_indices <- c(selected_indices, additional_indices)
  # }
  ref_added_reor = ref_added_or[order(as.numeric(rownames(ref_added_or))),]
  
  # Remove relabeled samples from validateLabels
  reor_idx <- which(rownames(ref_added_reor) %in% selected_indices)
  features <- features[!(rownames(features) %in% selected_indices), ]
  ref <- ref[-reor_idx]
  # reor_idx <- match(selected_indices, rownames(ref_added_reor))
  # ref <- data.frame(index = as.numeric(rownames(ref_added_reor)), refLabels = as.character(ref))
  # ref <- as.factor(ref[!(ref$index %in% rownames(ref_added_reor[reor_idx,])), ncol(ref)])
  
  # Add relabeled samples to new_trainFeatVSVM and new_trainLabelsVSVM
  new_trainFeatVSVM <- rbind(new_trainFeatVSVM, ref_added_reor[reor_idx, 1:(ncol(ref_added_reor)-5)])
  new_trainLabelsVSVM <- c(new_trainLabelsVSVM, ref_added_reor[reor_idx, (ncol(ref_added_reor)-4)])
  
  return(list(features = features, 
              labels = ref, 
              new_trainFeatVSVM = new_trainFeatVSVM, 
              new_trainLabelsVSVM = new_trainLabelsVSVM))
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
bound = c(0.636,0.68,0.812,0.813,0.845,0.9)

boundMargin  = c(1.5,1.0,0.5)                             # distance on positive side of hyperplane threshold 
boundMargin = c(0.315,0.355,0.5,0.65,0.75,1,1.5)

sampleSizePor = c(3,4,6,8,10,12,16,25,40,60)                       # vector with % of max          # c(40,25,16,12,10,8,6,4,3,2,1) 
colheader = c("3","4","6","8","10","12","16","25","40","60")       # corresponding column names    # c("40","25","16","12","10","8","6","4","3","2","1")
sampleSizePor = c(10,20,40,60,90,140,200)  
colheader = c("10","20","40","60","90","140","200")
sindexSVMDATA = 37                                                # start of baseline model with one segmentation scale data
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

setwd(paste0(path, "csv_data_r_import/cologne/scale"))

# import data
generalDataPool = read.csv2(inputPath,header = T, sep =";",colClasses = columnClass)

# exclude unclassified and delete level of factor
generalDataPool = subset(generalDataPool, REF != "unclassified")
generalDataPool$REF <- factor(generalDataPool$REF)

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
# WHY DO WE USE THIS DATA AT THE END? IS IT CORRECT TO USE THEM INSTEAD OF THE TRAIN TEST VALID SETS?
rm(data)
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

# current training data-set, updated (refreshed) after each iteration 
# WHAT DOES IT MEAN?

# set randomized seed for the random sampling procedure
seed = 72

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
# in 2Class Setting only first two records are used BY DEFAULT?
pA = 1/6
pB = 1/6
pC = 1/6
pD = 1/6
pE = 1/6
pF = 1/6

nR=1 # Number of Realizations

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
AccuracyVSVM_SL_Un_b_ms = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_b_ms) = colheader   
AccuracyVSVM_SL_Un_mclp_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_Un_mclp_it) = colheader   
AccuracyVSVM_SL_vUn_b_ms = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_b_ms) = colheader   
AccuracyVSVM_SL_vUn_mclp_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
colnames(AccuracyVSVM_SL_vUn_mclp_it) = colheader   

for(realization in seq(along = c(1:length(nR)))){#}

  print(paste0("Realization: ",nR[realization]," -> ",realization,"/",length(nR)))  
  # initial seed value for randomized sampling
  # if(train){seed = seed + sample(-71:33, 1)}
  
for(sample_size in seq(along = c(1:length(sampleSizePor)))){#}
  trainDataCur = trainDataPoolAllLev
  testDataCur = testDataAllLev
print(paste0("Sample size: ",sampleSizePor[sample_size]," -> ",sample_size,"/",length(sampleSizePor)))
# definition of training sample set sizes S [% of max. sample size]
sCur = sMax*(sampleSizePor[sample_size]/100)
# definition of sample shares
nA = round(sCur*pA)
nB = round(sCur*pB)
nC = round(sCur*pC)
nD = round(sCur*pD)
nE = round(sCur*pE)
nF = round(sCur*pF)
shares = c(nA,nB,nC,nD,nE,nF)
nClass = sampleSizePor[sample_size] 
shares = c(nClass,nClass,nClass,nClass,nClass,nClass)

# set randomized seed for the random sampling procedure
set.seed(seed)

# definition of sampling configuration (strata:random sampling without replacement)
stratSamp = strata(trainDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

# get samples of trainDataCur and set trainDataCur new
samples = getdata(trainDataCur, stratSamp)

samplesID = samples$ID_unit
trainDataCurRemaining <- trainDataCur[-c(samplesID), ]

trainDataCur = samples[,1:ncol(trainDataPoolAllLev)]
trainFeat = trainDataCur[,1:(ncol(trainDataPoolAllLev)-1)]
trainLabels = trainDataCur[,ncol(trainDataPoolAllLev)]

# subset for each outer iteration test data to speed up computing
testDataCur = testDataCur[order(testDataCur[,ncol(testDataCur)]),]

stratSamp = strata(testDataCur, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

samples = getdata(testDataCur, stratSamp)
testDataCur = samples[,1:ncol(testDataCur)]

# split test feat from test label for later join with trainData
testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
testLabels = testDataCur[,ncol(testDataCur)]

# subset on base level
testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]

# trainData index to split between train and test in svmFit
countTrainData = nrow(trainFeat)
indexTrainData = list(c(1:countTrainData))

# SVM base for invariants

# subset on L_4 ********************************** key passage *******************************************
trainFeat = trainFeat[sindexSVMDATA:eindexSVMDATA] # ALL the preprocessing made before is still required for test and valid set
# ************************************************ *******************************************************
# join of train and test test data (separable through indexTrainData in svmFit)
tuneFeat = rbind(trainFeat, testFeatsub)
tuneLabel = unlist(list(trainLabels, testLabels))

########################################  SVM parameter tuning  #########################################
setwd(paste0(model_path, "saved_models"))
if (file.exists("tunedSVM.rds") && !train) {
  tunedSVM <- readRDS("tunedSVM.rds")
  print("Model already exists!")
} else {
  tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
  # saveRDS(tunedSVM, "tunedSVM.rds")
}

# run classification and accuracy assessment for unmodified SV
# predict labels of test data
predLabelsSVM = predict(tunedSVM, validateFeatsub)

# accuracy assessment
accSVM = confusionMatrix(predLabelsSVM, validateLabels)
print(accSVM)
#########################################################################################################

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
if (file.exists("tunedVSVM.rds") && !train) {
  tunedVSVM <- readRDS("tunedVSVM.rds")
  print("Model already exists!")
} else {
  tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData)
  # saveRDS(tunedVSVM, "tunedVSVM.rds")
}

tunedVSVM_apply = tunedVSVM

# run classification and accuracy assessment for modified SV
# predict labels of test data
predLabelsVSVM = predict(tunedVSVM, validateFeatsub)

# accuracy assessment
accVSVM = confusionMatrix(predLabelsVSVM, validateLabels)
print(accVSVM)
##########################################################################################################

##################################### VSVM - EVALUATION (SL) of all Level ################################

# **********************
# records which 2 classes are involved in 2 class problems
binaryClassProblem = list()
# CHECK WHY DOES IT HAS TO BE BUILD IN THIS WAY
for(jj in seq(along = c(1:length(tunedSVM$finalModel@xmatrix)))){
  binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
}
# **********************
if (file.exists("bestFittingModel.rds") && !train) {
  bestFittingModel <- readRDS("bestFittingModel.rds")
  actKappa = bestFittingModel$resample$Kappa
  print("Model already exists!")
} else {
  actKappa = 0
  # Iteration over bound to test different bound thresholds determining the radius of acceptation
  for(jj in seq(along = c(1:length(bound)))){
    # remove VSVs which are not located within certain distance to original SVs;
    # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM: tunedSVM$finalModel@kernelf
    # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
    print("Removing VSVs out of boundaries...")
    registerDoParallel(num_cores)
    # Define a function to process SVL variables
    process_SVL <- function(SVtotal, SVL, bound, objInfoNames) {
      setNames(rem_extrem_kerneldist(SVtotal, SVL, bound), objInfoNames)
    }
    # Remove VSVs out of bound
    SVinvarRadi <- foreach(SVL = list(SVL2, SVL3, SVL5, SVL6, SVL7, SVL8, SVL9, SVL10, SVL11), .combine = rbind) %dopar% {
                             process_SVL(SVtotal, SVL, bound[[jj]], objInfoNames)
    }
    registerDoSEQ()
    # SVinvarRadi = rbind(setNames(rem_extrem_kerneldist(SVtotal, SVL2, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL3, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL5, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL6, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL7, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL8, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL9, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL10, bound[jj]),objInfoNames),
    #                     setNames(rem_extrem_kerneldist(SVtotal, SVL11, bound[jj]),objInfoNames)
    # )
    # # SVinvarRadi = rbind(setNames(rem_extrem(SVtotal, SVL2, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL3, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL5, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL6, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL7, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL8, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL9, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL10, bound[jj]),objInfoNames),
    # #                     setNames(rem_extrem(SVtotal, SVL11, bound[jj]),objInfoNames)
    # #                     #setNames(rem_extrem(SVtotal, SVL12, bound[jj]),objInfoNames)
    # #                     #setNames(rem_extrem(SVtotal, SVL13, bound[jj]),objInfoNames)
    # # )
    SVinvarRadi = na.omit(SVinvarRadi) # remove NAs

    # iterating over
    for (kk in seq(along = c(1:length(boundMargin)))){
      print(paste0("Testing bound margin: ",kk,"/",length(boundMargin)," and radius threshold: ",jj,"/",length(bound)))

      # remove VSV which are not located in certain distance to desicion function
      # data.frame to store elected VSV within the margin
      SVinvar=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)

      # Progress bar for tracking computation
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
      tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData, classProb=TRUE)

      # of all Different bound settings get the one with best Kappa ans save its model
      if(actKappa < tunedVSVM$resample$Kappa){
        bestFittingModel = tunedVSVM
        actKappa = tunedVSVM$resample$Kappa
        best_trainFeatVSVM = trainFeatVSVM
        best_trainLabelsVSVM = trainLabelsVSVM
        best_bound = bound[jj]
        best_boundMargin = boundMargin[kk]
      }
    }
  }
  # registerDoParallel(num_cores)
  # # Define a function to process SVL variables
  # process_SVL <- function(SVtotal, SVL, bound, boundMargin, kk, objInfoNames) {
  #   setNames(rem_extrem_kerneldist(SVtotal, SVL, bound), objInfoNames)
  # }
  # 
  # # Iterate over bound and boundMargin
  # foreach(jj = seq_along(bound_dense), .combine = rbind) %:%
  #   foreach(kk = seq_along(boundMargin_dense), .combine = rbind) %dopar% {
  #     # Remove VSVs out of bound
  #     SVinvarRadi <- foreach(SVL = list(SVL2, SVL3, SVL5, SVL6, SVL7, SVL8, SVL9, SVL10, SVL11),
  #                            .combine = rbind) %do% {
  #                              process_SVL(SVtotal, SVL, bound[[jj]], boundMargin[[kk]], kk, objInfoNames)
  #                            }
  # 
  #     # Remove NAs
  #     SVinvarRadi <- na.omit(SVinvarRadi)
  # 
  #     # Initialize SVinvar
  #     SVinvar <- setNames(data.frame(matrix(ncol = numFeat + 1)), objInfoNames)
  # 
  #     foreach(m = seq_along(nrow(SVinvarRadi)), .combine = rbind) %do% {
  #       signa <- as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
  #       if ((signa < boundMargin[[kk]]) && (signa > -boundMargin[[kk]])) {
  #         SVinvarRadi[m, ]
  #       } else {
  #         NULL
  #       }
  #     }
  #   }
  # }
  # saveRDS(bestFittingModel, "bestFittingModel.rds")
}

# run classification and accuracy assessment for the best bound setting
# predict labels of test data
predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)

# accuracy assessment
accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
print(accVSVM_SL)

########################################### unlabeled samples ############################################

# Definition of sampling configuration (strata:random sampling without replacement)
stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = c(b,b,b,b,b,b), method = "srswor")
#stratSampRemaining = strata(trainDataCurRemaining, size = 6*b, method = "srswor")
#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)

# get samples of trainDataCurRemaining and set trainDataCurRemaining new
samplesRemaining_b = getdata(trainDataCurRemaining, stratSampRemaining)

trainDataCurRemaining_b = samplesRemaining_b[,1:ncol(trainDataPoolAllLev)]
trainDataCurRemainingsub_b = trainDataCurRemaining_b[sindexSVMDATA:eindexSVMDATA]

REF_b = predict(tunedVSVM, trainDataCurRemainingsub_b)
# get SV of unlabeled samples
SVindexUn_b = 1:nrow(trainDataCurRemainingsub_b)
SVtotalUn_b = trainDataCurRemaining_b[SVindexUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
SVtotalUn_b = cbind(SVtotalUn_b, REF_b)
# get VSs, means rows of SV but with subset on different level
SVL2Un_b = cbind(trainDataCurRemaining[SVindexUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)
SVL3Un_b = cbind(trainDataCurRemaining[SVindexUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)

SVL5Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)
SVL6Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)
SVL7Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)
SVL8Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)
SVL9Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b)
SVL10Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)
SVL11Un_b = cbind(trainDataCurRemaining_b[SVindexUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b)
# bind original SV with modified to new train data set
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
)
SVinvarUn_b = rbind(setNames(SVinvar,objInfoNames),
                    setNames(SVinvarUn_b,objInfoNames)
)

############## VSVM with Balanced Unlabeled samples - EVALUATION (SL)

if (file.exists("bestFittingModelUn_b.rds") && !train) {
  bestFittingModelUn_b <- readRDS("bestFittingModelUn_b.rds")
  actKappa = bestFittingModelUn_b$resample$Kappa
  print("Model already exists!")
} else {
  actKappa = 0
  
  # iteration over bound to test different bound thresholds determining the radius of acception
  for(jj in seq(along = c(1:length(bound)))){
    print("Removing VSVs out of boundaries...")
    # remove VSV which are not located within certain distance to org.SV;
    # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
    # # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
    # SVinvarRadiUn_b = rbind(setNames(rem_extrem_kerneldist(SVtotal, SVL2, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL3, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL5, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL6, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL7, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL8, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL9, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL10, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotal, SVL11, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL2Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL3Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL5Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL6Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL7Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL8Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL9Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL10Un_b, bound[jj]),objInfoNames),
    #                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL11Un_b, bound[jj]),objInfoNames)
    # )

    # Define the SVL variables
    SVL_variables <- list(
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

    registerDoParallel(num_cores)
    # Apply foreach loop to process each SVL variable and bind the results
    SVinvarRadiUn_b <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
      setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
    }
    # remove NAs
    SVinvarRadiUn_b = na.omit(SVinvarRadiUn_b)
    registerDoSEQ()
    # iterating over boundMargin to test different threshold on margin distance
    for (kk in seq(along = c(1:length(boundMargin)))){
      print(paste0("Testing bound margin: ",kk,"/",length(boundMargin)," and radius threshold: ",jj,"/",length(bound)))

      # remove VSV which are not located in certain distance to desicion function
      # data.frame to store elected VSV within the margin
      SVinvarUn_b=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)

      pb <- progress_bar$new(
        format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
        total = nrow(SVinvarRadiUn_b),
        clear = FALSE
      )
      # iterate over SVinvarRadi and evaluate distance to hyperplane
      # implementation checks class membership for case that each class should be evaluate on different bound
      for(m in seq(along = c(1:nrow(SVinvarRadiUn_b)))){
        signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadiUn_b[m,-ncol(SVinvarRadiUn_b)]),SVinvarRadiUn_b[m,ncol(SVinvarRadiUn_b)]))

        if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
          SVinvarUn_b = rbind(SVinvarUn_b, SVinvarRadiUn_b[m,])
        }
        pb$tick()
      }

      # merge elected VSV with original SV
      SVinvar_orgUn_b = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarUn_b,objInfoNames))

      SVinvar_orgUn_b=na.omit(SVinvar_orgUn_b)

      # split for training to feature and label
      trainFeatVSVMUn_b = SVinvar_orgUn_b[,1:(ncol(SVinvar_orgUn_b)-1)]
      trainLabelsVSVMUn_b = SVinvar_orgUn_b[,ncol(SVinvar_orgUn_b)]

      # get list with index of trainData to split between train and test in svmFit
      countTrainDataUn_b = nrow(SVinvar_orgUn_b)
      indexTrainDataUn_b = list(c(1:countTrainDataUn_b))

      # join of train and test data (through indesTrainData in svmFit seperable)
      names = objInfoNames[1:length(objInfoNames)-1]
      tuneFeatVSVMUn_b = rbind(trainFeatVSVMUn_b, setNames(testFeatsub, names))
      tuneLabelsVSVMUn_b = unlist(list(trainLabelsVSVMUn_b, testLabels))

      ######################################## VSVM control parameter tuning ########################################
      tunedVSVMUn_b = svmFit(tuneFeatVSVMUn_b, tuneLabelsVSVMUn_b, indexTrainDataUn_b)

      # of all Different bound settings get the one with best Kappa ans save its model
      if(actKappa < tunedVSVMUn_b$resample$Kappa){
        bestFittingModelUn_b = tunedVSVMUn_b
        actKappa = tunedVSVMUn_b$resample$Kappa
        best_trainFeatVSVMUn_b = trainFeatVSVMUn_b
        best_trainLabelsVSVMUn_b = trainLabelsVSVMUn_b
        best_bound_b = bound[jj]
        best_boundMargin_b = boundMargin[kk]
      }
    }
  }
  # saveRDS(bestFittingModelUn_b, "bestFittingModelUn_b.rds")
}

# Run classification: predict labels of validate data
predLabelsVSVMsumUn_b = predict(bestFittingModelUn_b, validateFeatsub)
# summary(predLabelsVSVMsumUn_b)

# Accuracy assessment
accVSVM_SL_Un_b = confusionMatrix(predLabelsVSVMsumUn_b, validateLabels)
print(accVSVM_SL_Un_b)
######################################## UNCERTAINTY function on VSVM-SL  #########################################
resampledSize=100
# ****** #
print("Computing samples margin distance using Iterative Active Learning Procedure...")
classSize = 2700 # number of samples for each class # 250, 500, 750, 1000, 1500, 3000, 5803
stratSampSize = c(classSize,classSize,classSize,classSize,classSize,classSize)
# Definition of sampling configuration (strata:random sampling without replacement)
stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
# Get new samples from trainDataCurRemaining 
samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
samplesID_Remaining = samplesRemaining$ID_unit
trainDataCurRemaining <- trainDataCurRemaining[-c(samplesID_Remaining), ]

upd_trainDataCurFeatsub = samplesRemaining[sindexSVMDATA:eindexSVMDATA]
upd_trainDataCurLabels = samplesRemaining$REF
new_trainFeatVSVM <- best_trainFeatVSVM
new_trainLabelsVSVM <- best_trainLabelsVSVM
new_tunedVSVM <- bestFittingModel

newSize_for_iter = 4 # 2, 3, 5, 8, 10, 20, 60
num_iters = round(resampledSize/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100

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
  sampled_data <- mclu_sampling(tunedVSVM, predLabelsVSVM_unc)
  # print(paste0("Relabeling samples..."))
  # Get new labels and updated datasets
  result <- add_new_samples(# sampled_data, 
                            # upd_trainDataCurFeatsub, 
                            # upd_trainDataCurLabels, 
                            # new_trainFeatVSVM, 
                            # new_trainLabelsVSVM, 
                            newSize=newSize_for_iter,
                            cluster=120 ) # always greater than newSize_for_iter, # 60, 80, 100, 120
  # CHECK IF THE NEW DISTANCE SET TO 1.000000 IS ACTUALLY USEFUL IN THE NEXT ITERATIONS -> DON'T THINK SO
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
  new_tunedVSVM = svmFit(tuneFeatVSVMUn_it, tuneLabelsVSVMUn_it, indexTrainDataUn_it, showPrg = FALSE)
  pb$tick()
}
fin_predLabelsVSVM = predict(new_tunedVSVM, validateFeatsub)
accVSVM_SL_Un_mclp_it  = confusionMatrix(fin_predLabelsVSVM, validateLabels)
print(accVSVM_SL_Un_mclp_it)
# saveRDS(new_tunedVSVM, "bestFittingModel_Un_mclp_it.rds")
# ************************************************************************************************

# predLabelsVSVMsumUn_b = predict(bestFittingModel, validateFeatsub)
# Add predicted labels to the features data set
predLabelsVSVMsumUn_unc = cbind(validateFeatsub, predLabelsVSVMsumUn_b)
predLabelsVSVMsumUn_unc = setNames(predLabelsVSVMsumUn_unc, objInfoNames)
# predict(bestFittingModelUn_b, predLabelsVSVMsumUn_unc[1,1:ncol(predLabelsVSVMsumUn_unc) - 1])

# ******
print("Computing samples margin distance using Uncertainty distance...")
#calculate uncertainty of the samples by selecting SV's and data set
normdistvsvm_sl_un = uncertainty_dist_v2_2(bestFittingModel, predLabelsVSVMsumUn_unc)
# Extract labels for prediction
predlabels_vsvm_Slu = alter_labels(normdistvsvm_sl_un, validateLabels, resampledSize)
accVSVM_SL_Un_b_ud = confusionMatrix(predlabels_vsvm_Slu, validateLabels)
print(accVSVM_SL_Un_b_ud)

# ****** # 
print("Computing samples margin distance using Marging Samples MS...")
# margin_sampled_data <- margin_sampling(bestFittingModelUn_b, predLabelsVSVMsumUn_unc)
ms_data_multicore <- margin_sampling_multicore(bestFittingModel, predLabelsVSVMsumUn_unc)
predlabels_vsvm_ms = alter_labels(ms_data_multicore, validateLabels, resampledSize)
accVSVM_SL_Un_b_ms = confusionMatrix(predlabels_vsvm_ms, validateLabels)
print(accVSVM_SL_Un_b_ms)

# ****** # 
print("Computing samples margin distance using Multiclass Level Uncertainty...")
mclu_sampled_data <- mclu_sampling(bestFittingModel, predLabelsVSVMsumUn_unc)
predlabels_vsvm_mclu = alter_labels(mclu_sampled_data, validateLabels, resampledSize)
accVSVM_SL_Un_b_mclu = confusionMatrix(predlabels_vsvm_mclu, validateLabels)
print(accVSVM_SL_Un_b_mclu)

# ****** # 
print("Computing samples margin distance using Multiclass Level Probability...")
mclp_sampled_data <- mclp_sampling(bestFittingModel, predLabelsVSVMsumUn_unc)
predlabels_vsvm_mclp <- alter_labels(mclp_sampled_data, validateLabels, resampledSize)
accVSVM_SL_Un_b_mclp = confusionMatrix(predlabels_vsvm_mclp, validateLabels)
print(accVSVM_SL_Un_b_mclp)

################################## VSVM-SL + VIRTUAL (Balanced) Unlabeled Samples ####################################

REF_v = predict(bestFittingModelUn_b, trainDataCurRemainingsub_b)

# get SV of unlabeled samples
SVindexvUn_b = 1:nrow(trainDataCurRemainingsub_b)
SVtotalvUn_b = trainDataCurRemaining_b[SVindexvUn_b ,c(sindexSVMDATA:eindexSVMDATA)]
SVtotalvUn_b = cbind(SVtotalUn_b, REF_v)

# extracting previously assigned reference column
SVtotalvUn_bFeat = SVtotalvUn_b[,1:(ncol(SVtotalvUn_b)-2)]
REF_v = SVtotalvUn_b[,(ncol(SVtotalvUn_b))]
SVtotalvUn_b = cbind(SVtotalvUn_bFeat,REF_v)

# get VSs, means rows of SV but with subset on different level
SVL2vUn_b = cbind(trainDataCurRemaining[SVindexvUn_b,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)
SVL3vUn_b = cbind(trainDataCurRemaining[SVindexvUn_b,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)

SVL5vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)
SVL6vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)
SVL7vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)
SVL8vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)
SVL9vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v)
SVL10vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)
SVL11vUn_b = cbind(trainDataCurRemaining_b[SVindexvUn_b,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v)

# bind original SV with modified to new train data set
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
)

SVinvarvUn_b = rbind(setNames(SVinvar,objInfoNames),
                     setNames(SVinvarvUn_b,objInfoNames)
)

######################### VSVM - EVALUATION of all Level VSV + (Balanced) Unlabeled Samples ##########################

if (file.exists("bestFittingModelvUn_b.rds") && !train) {
  bestFittingModelvUn_b <- readRDS("bestFittingModelvUn_b.rds")
  actKappa = bestFittingModelvUn_b$resample$Kappa
  print("Luckily, model already exists!")
} else {
  actKappa = 0
  
  # iteration over bound to test different bound thresholds determining the radius of acception
  for(jj in seq(along = c(1:length(bound)))){
    print("Removing VSVs out of boundaries...")
    # # remove VSV which are not located within certain distance to org.SV;
    # # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
    # # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
    # SVinvarRadivUn = rbind(setNames(rem_extrem_kerneldist(SVtotal, SVL2, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL3, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL5, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL6, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL7, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL8, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL9, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL10, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotal, SVL11, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL2vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL3vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL5vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL6vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL7vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL8vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL9vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL10vUn_b, bound[jj]),objInfoNames),
    #                        setNames(rem_extrem_kerneldist(SVtotalvUn_b, SVL11vUn_b, bound[jj]),objInfoNames)
    # )
    # Define the SVL variables
    SVL_variables <- list(
      list(SVtotal, SVL2),
      list(SVtotal, SVL3),
      list(SVtotal, SVL5),
      list(SVtotal, SVL6),
      list(SVtotal, SVL7),
      list(SVtotal, SVL8),
      list(SVtotal, SVL9),
      list(SVtotal, SVL10),
      list(SVtotal, SVL11),
      list(SVtotalvUn_b, SVL2Un_b),
      list(SVtotalvUn_b, SVL3Un_b),
      list(SVtotalvUn_b, SVL5Un_b),
      list(SVtotalvUn_b, SVL6Un_b),
      list(SVtotalvUn_b, SVL7Un_b),
      list(SVtotalvUn_b, SVL8Un_b),
      list(SVtotalvUn_b, SVL9Un_b),
      list(SVtotalvUn_b, SVL10Un_b),
      list(SVtotalvUn_b, SVL11Un_b)
    )
    
    registerDoParallel(num_cores)
    # Apply foreach loop to process each SVL variable and bind the results
    SVinvarRadivUn <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
      setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
    }
    registerDoSEQ()
    # remove NAs
    SVinvarRadivUn = na.omit(SVinvarRadivUn)

    # iterating over boundMargin to test different threshold on margin distance
    for (kk in seq(along = c(1:length(boundMargin)))){
      print(paste0("Testing bound margin: ",kk,"/",length(boundMargin)," and radius threshold: ",jj,"/",length(bound)))

      # remove VSV which are not located in certain distance to decision function
      # data.frame to store elected VSV within the margin
      SVinvarvUn_b=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)

      pb <- progress_bar$new(
        format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
        total = nrow(SVinvarRadiUn_b),
        clear = FALSE
      )
      # iterate over SVinvarRadi and evaluate distance to hyperplane
      # implementation checks class membership for case that each class should be evaluate on different bound
      for(m in seq(along = c(1:nrow(SVinvarRadiUn_b)))){
        signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadivUn[m,-ncol(SVinvarRadivUn)]), SVinvarRadivUn[m, ncol(SVinvarRadivUn)]))

        if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
          SVinvarvUn_b = rbind(SVinvarvUn_b, SVinvarRadivUn[m,])
        }
        pb$tick()
      }

      # merge elected VSV with original SV
      SVinvar_orgvUn_b = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarvUn_b,objInfoNames))

      SVinvar_orgvUn_b=na.omit(SVinvar_orgvUn_b)

      # split for training to feature and label
      trainFeatVSVMvUn_b = SVinvar_orgvUn_b[,1:(ncol(SVinvar_orgvUn_b)-1)]
      trainLabelsVSVMvUn_b = SVinvar_orgvUn_b[,ncol(SVinvar_orgvUn_b)]

      # get list with index of trainData to split between train and test in svmFit
      countTrainDatavUn_b = nrow(SVinvar_orgvUn_b)
      indexTrainDatavUn_b = list(c(1:countTrainData))

      # join of train and test data (through indesTrainData in svmFit seperable)
      names = objInfoNames[1:length(objInfoNames)-1]
      tuneFeatVSVMvUn_b = rbind(trainFeatVSVMvUn_b, setNames(testFeatsub, names))
      tuneLabelsVSVMvUn_b = unlist(list(trainLabelsVSVMvUn_b, testLabels))

      ######################################## VSVM control parameter tuning ########################################
      tunedVSVMvUn_b = svmFit(tuneFeatVSVMvUn_b, tuneLabelsVSVMvUn_b, indexTrainDatavUn_b, classProb = TRUE)

      # of all Different bound settings get the one with best Kappa ans save its model
      if(actKappa < tunedVSVMvUn_b$resample$Kappa){
        bestFittingModelvUn_b = tunedVSVMvUn_b
        actKappa = tunedVSVMvUn_b$resample$Kappa
        best_trainFeatVSVMvUn_b = trainFeatVSVMvUn_b
        best_trainLabelsVSVMvUn_b = trainLabelsVSVMvUn_b
        best_bound_b_v = bound[jj]
        best_boundMargin_b_v = boundMargin[kk]
      }
    }
  }
  # saveRDS(bestFittingModelvUn_b, "bestFittingModelvUn_b.rds")
}

# run classification and accuracy assessment for the best bound setting
# predict labels of test data
predLabelsVSVMvUn_bsum = predict(bestFittingModelvUn_b, validateFeatsub)

##accuracy assessment
accVSVM_SL_vUn_b = confusionMatrix(predLabelsVSVMvUn_bsum, validateLabels)
print(accVSVM_SL_vUn_b)
######################################## UNCERTAINTY function on VSVM-SL-VUNL ########################################

#add predicted labels to the features data set
predLabelsVSVMsumVUn_unc= cbind(validateFeatsub, predLabelsVSVMvUn_bsum)
predLabelsVSVMsumVUn_unc = setNames(predLabelsVSVMsumVUn_unc, objInfoNames)

# ****** # 
print("Computing samples margin distance using Marging Sampling MS...")
ms_data_vsvm_Slvu <- margin_sampling_multicore(bestFittingModelvUn_b, predLabelsVSVMsumVUn_unc)
predlabels_vsvm_ms_Slvu = alter_labels(ms_data_vsvm_Slvu, validateLabels, resampledSize)
accVSVM_SL_vUn_b_ms = confusionMatrix(predlabels_vsvm_ms_Slvu, validateLabels)
print(accVSVM_SL_vUn_b_ms)

# #########################################################################################################
# #                                                                                                       #
# #                                       VSVM-SL-Unl + Virtual Unl                                       #
# #                                                                                                       #
# #########################################################################################################
# # IN THIS CASE WE DO NOT DISTIGUISH BETWEEN RANDOM & BALANCED UNLABELED SAMPLES ANYMORE (trainDataCurRemaining)
# 
# # get SV of tunedVSVMUn_b
# SVindexvUn = tunedVSVMUn_b$finalModel@SVindex   # indices 1:(sample size per class) ; values
# SVtotalvUn = trainDataCurRemaining[SVindexvUn ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining))]
# 
# # get VSs, means rows of SV but with subset on different level
# SVL2vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1), ncol(trainDataCurRemaining))]
# SVL3vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1), ncol(trainDataCurRemaining))]
# 
# SVL5vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL6vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL7vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL8vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL9vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL10vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCurRemaining))]
# SVL11vUn = trainDataCurRemaining[SVindexvUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCurRemaining))]
# 
# # bind original SV with modified to new train data set
# SVinvarvUn = rbind(setNames(SVtotalvUn,objInfoNames),
#                    setNames(SVL2vUn,objInfoNames),
#                    setNames(SVL3vUn,objInfoNames),
#                    setNames(SVL5vUn,objInfoNames), 
#                    setNames(SVL6vUn,objInfoNames),
#                    setNames(SVL7vUn,objInfoNames),
#                    setNames(SVL8vUn,objInfoNames),
#                    setNames(SVL9vUn,objInfoNames),
#                    setNames(SVL10vUn,objInfoNames),
#                    setNames(SVL11vUn,objInfoNames)
# )
# 
# # VSVM with evaluation of VSV with all Level 
# # Lu et al.(2016): A Novel Synergetic Classification Approach for  Hyperspectral and Panchromatic Images Based on Self-Learning
# 
# if (file.exists("bestFittingModelvUn.rds") && !train) {
#   bestFittingModelvUn_b <- readRDS("bestFittingModelvUn.rds")
#   actKappa = bestFittingModelvUn_b$resample$Kappa
#   print("Model already exists!")
# } else {
# 
#   # iteration over bound to test different bound thresholds determining the radius of acseption
#   actKappa = 0
#   
#   # iteration over bound to test different bound thresholds determining the radius of acception
#   for(jj in seq(along = c(1:length(bound)))){
#     print("Removing VSVs out of boundaries...")
#     # # remove VSV which are not located within certain distance to org.SV;
#     # # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
#     # # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
#     # SVinvarRadivUn = rbind(setNames(rem_extrem_kerneldist(SVtotal, SVL2, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL3, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL5, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL6, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL7, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL8, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL9, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL10, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotal, SVL11, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL2vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL3vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL5vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL6vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL7vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL8vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL9vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL10vUn, bound[jj]),objInfoNames),
#     #                        setNames(rem_extrem_kerneldist(SVtotalvUn, SVL11vUn, bound[jj]),objInfoNames)
#     # )
# 
#     # Define the SVL variables
#     SVL_variables <- list(
#       list(SVtotal, SVL2),
#       list(SVtotal, SVL3),
#       list(SVtotal, SVL5),
#       list(SVtotal, SVL6),
#       list(SVtotal, SVL7),
#       list(SVtotal, SVL8),
#       list(SVtotal, SVL9),
#       list(SVtotal, SVL10),
#       list(SVtotal, SVL11),
#       list(SVtotalvUn, SVL2vUn),
#       list(SVtotalvUn, SVL3vUn),
#       list(SVtotalvUn, SVL5vUn),
#       list(SVtotalvUn, SVL6vUn),
#       list(SVtotalvUn, SVL7vUn),
#       list(SVtotalvUn, SVL8vUn),
#       list(SVtotalvUn, SVL9vUn),
#       list(SVtotalvUn, SVL10vUn),
#       list(SVtotalvUn, SVL11vUn)
#     )
#     registerDoParallel(num_cores)
#     # Apply foreach loop to process each SVL variable and bind the results
#     SVinvarRadivUn <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
#       setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
#     }
#     registerDoSEQ()
#     # remove NAs
#     SVinvarRadivUn = na.omit(SVinvarRadivUn)
# 
#     # iterating over boundMargin to test different threshold on margin distance
#     for (kk in seq(along = c(1:length(boundMargin)))){
#       print(paste0("Testing bound margin: ",kk,"/",length(boundMargin)," and radius threshold: ",jj,"/",length(bound)))
#       # remove VSV which are not located in certain distance to desicion function
#       # data.frame to store elected VSV within the margin
#       SVinvarvUn_b=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
#       
#       pb <- progress_bar$new(
#         format = "[:bar] :percent [elapsed time: :elapsedfull | remaining: :eta]",
#         total = nrow(SVinvarRadivUn),
#         clear = FALSE
#       )
#       # iterate over SVinvarRadi and evaluate distance to hyperplane
#       # implementation checks class membership for case that each class should be evaluate on different bound
#       for(m in seq(along = c(1:nrow(SVinvarRadivUn)))){
#         signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadivUn[m,-ncol(SVinvarRadivUn)]), SVinvarRadivUn[m, ncol(SVinvarRadivUn)]) )
#         
#           if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
#             SVinvarvUn_b = rbind(SVinvarvUn_b, SVinvarRadivUn[m,])
#           }
#         pb$tick()
#       }
# 
#       # merge elected VSV with original SV
#       SVinvar_orgvUn_b = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarvUn_b,objInfoNames))
# 
#       SVinvar_orgvUn_b=na.omit(SVinvar_orgvUn_b)
# 
#       # split for training to feature and label
#       trainFeatVSVMvUn_b = SVinvar_orgvUn_b[,1:(ncol(SVinvar_orgvUn_b)-1)]
#       trainLabelsVSVMvUn_b = SVinvar_orgvUn_b[,ncol(SVinvar_orgvUn_b)]
# 
#       # get list with index of trainData to split between train and test in svmFit
#       countTrainDatavUn_b = nrow(SVinvar_orgvUn_b)
#       indexTrainDatavUn_b = list(c(1:countTrainData))
# 
#       # join of train and test data (through indesTrainData in svmFit separable)
#       names = objInfoNames[1:length(objInfoNames)-1]
#       tuneFeatVSVMvUn_b = rbind(trainFeatVSVMvUn_b, setNames(testFeatsub, names))
#       tuneLabelsVSVMvUn_b = unlist(list(trainLabelsVSVMvUn_b, testLabels))
# 
#       ######################################## VSVM control parameter tuning ########################################
#       tunedVSVMvUn_b = svmFit(tuneFeatVSVMvUn_b, tuneLabelsVSVMvUn_b, indexTrainDatavUn_b)
# 
#       # of all Different bound settings get the one with best Kappa ans save its model
#       if(actKappa < tunedVSVMvUn_b$resample$Kappa){
#         bestFittingModelvUn_b = tunedVSVMvUn_b
#         actKappa = tunedVSVMvUn_b$resample$Kappa
#       }
#     }
#   }
# }
# # run classification and accuracy assessment for the best bound setting
# # predict labels of test data
# predLabelsVSVMvUn_bsum = predict(bestFittingModelvUn_b, validateFeatsub)
# 
# # accuracy assessment
# accVSVM_SL_vUn = confusionMatrix(predLabelsVSVMvUn_bsum, validateLabels)
# print(accVSVM_SL_vUn)


#############################################  MultiScale  ##############################################
print("Computing Multiscale SVM...")
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

######  MultiScale

trainDataCurMS = trainDataPoolAllLevMS
testDataCurMS = testDataAllLevMS

stratSamp = strata(trainDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
samples = getdata(trainDataCurMS, stratSamp)
trainDataCurMS = samples[,1:ncol(trainDataPoolAllLevMS)]
trainFeatMS = trainDataCurMS[,1:(ncol(trainDataPoolAllLevMS)-1)]
trainLabelsMS = trainDataCurMS[,ncol(trainDataPoolAllLevMS)]

# subset for each outer iteration test data to speed up computing
testDataCurMS = testDataCurMS[order(testDataCurMS[,ncol(testDataCurMS)]),]

stratSamp = strata(testDataCurMS, c("REF"), size = shares, method = "srswor")#size --> vector of stratum sample sizes (in the order in which the strata are given in the input data set)
samples = getdata(testDataCurMS, stratSamp)
testDataCurMS = samples[,1:ncol(testDataCurMS)]

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

# run classification and accuracy assessment for unmodified SV
# predict labels of test data
predLabelsSVMmultiScale = predict(tunedSVM_MS, validateFeatAllLevMS)

##accuracy assessment
accSVM_M = confusionMatrix(predLabelsSVMmultiScale, validateLabelsMS)
print(accSVM_M)
########################################### Save Accuracies ###############################################

#write current kappa and accuracy value in Kappas matrix
# KappaSVM[i,u] = as.numeric(accSVM$overall["Kappa"])
AccuracySVM[realization,sample_size] = as.numeric(accSVM$overall["Accuracy"])
AccuracyVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Accuracy"])
AccuracyVSVM_SL_Un_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_Un_b_ms$overall["Accuracy"])
AccuracyVSVM_SL_Un_mclp_it[realization,sample_size] = as.numeric(accVSVM_SL_Un_mclp_it$overall["Accuracy"])
AccuracySVM_M[realization,sample_size] = as.numeric(accSVM_M$overall["Accuracy"])
AccuracyVSVM_SL_vUn_b_ms[realization,sample_size] = as.numeric(accVSVM_SL_vUn_b_ms$overall["Accuracy"])
# AccuracyVSVM_SL_vUn_mclp_it[realization,sample_size] = as.numeric(accVSVM_SL_vUn_mclp_it$overall["Accuracy"])
}}
print("Acquiring accuracy results: DONE")
############################################################################################################
setwd(paste0(model_path,"results"))
save(AccuracySVM,AccuracySVM_M,AccuracyVSVM_SL,AccuracyVSVM_SL_Un_b_ms,AccuracyVSVM_SL_Un_mclp_it,AccuracyVSVM_SL_vUn_b_ms,file =paste("ColScaleMulticlass_accuracy_",b,"UnlSamples.RData",sep=""))
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################




# 
# ############### SVM_MS - evaluation of SL with L4 AND BALANCED UNLABELED SAMPLE (SVM-SL + Unl Samples)  #############
# 
# # Multiscale
# 
# # get SV of tunedSVM
# SVindexMultiScale = tunedSVM_MS$finalModel@SVindex   # indices 1:(sample size per class) ; values
# SVtotalMultiScale = trainDataCur[SVindexMultiScale ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
# 
# # get SV of tunedSVM
# SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
# SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
# 
# # iteration over bound to test different bound thresholds determining the radius of acceptation
# actKappa = 0
# 
# # iteration over bound to test different bound thresholds determining the radius of acceptation
# for(jj in seq(along = c(1:length(bound)))){
#   
#   
#   # remove VSV which are not located within certain distance to org.SV; 
#   # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
#   # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
#   
#   SVinvarRadiUn = rbind(setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL2Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL3Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL5Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL6Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL7Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL8Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL9Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL10Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem_kerneldist(SVtotalUn_b, SVL11Un_b, bound[jj]),objInfoNames)
#   )
#   
#   # remove NAs 
#   SVinvarRadiUn = na.omit(SVinvarRadiUn)
#   
#   # iterating over boundMargin to test different threshold on margin distance
#   for (kk in seq(along = c(1:length(boundMargin)))){
#     
#     # remove VSV which are not located in certain distance to desicion function
#     # data.frame to store elected VSV within the margin
#     SVinvarUn=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
#     
#     # iterate over SVinvarRadi and evaluate distance to hyperplane
#     # implementation checks class membership for case that each class should be evaluate on different bound
#     for(m in seq(along = c(1:nrow(SVinvarRadiUn)))){
#       signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadiUn[m,])))
#       if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
#         SVinvarUn = rbind(SVinvarUn, SVinvarRadiUn[m,])
#       }
#     }
#     
#     # merge elected VSV with original SV
#     SVinvar_orgUn = rbind(setNames(SVtotalMultiScale,objInfoNames), setNames(SVinvarUn,objInfoNames))
#     
#     SVinvar_orgUn=na.omit(SVinvar_orgUn)
#     
#     # split for training to feature and label
#     trainFeatSVMUn = SVinvar_orgUn[,1:(ncol(SVinvar_orgUn)-1)]
#     trainLabelsSVMUn = SVinvar_orgUn[,ncol(SVinvar_orgUn)]
#     
#     # get list with index of trainData to split between train and test in svmFit
#     countTrainDataUn = nrow(SVinvar_orgUn)
#     indexTrainDataUn = list(c(1:countTrainDataUn))
#     
#     # join of train and test data (through indesTrainData in svmFit seperable)
#     names = objInfoNames[1:length(objInfoNames)-1]
#     tuneFeatSVMUn = rbind(trainFeatSVMUn, setNames(testFeatsub, names))
#     tuneLabelsSVMUn = unlist(list(trainLabelsSVMUn, testLabels))
#     
#     ######################################## VSVM control parameter tuning ########################################
#     tunedSVMUn = svmFit(tuneFeatSVMUn, tuneLabelsSVMUn, indexTrainDataUn)
#     
#     # of all Different bound settings get the one with best Kappa ans save its model
#     if(actKappa < tunedSVMUn$resample$Kappa){
#       bestFittingModelUn = tunedSVMUn
#       actKappa = tunedSVMUn$resample$Kappa
#     }
#   }
# }
# 
# # run classification and accuracy assessment for the best bound setting
# # predict labels of test data
# predLabelsSVMsumUn = predict(bestFittingModelUn, validateFeatsub)
# 
# # accuracy assessment
# accSVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)

# ############## VSVM_MS - EVALUATION of VSV with all Level AND BALANCED UNLABELLED SAMPLES #############
# # actually it can't exist a VSVM_MS
# # get SV of tunedSVM
# SVindex = tunedSVM$finalModel@SVindex   # indices 1:(sample size per class) ; values
# SVtotal = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
# 
# # iteration over bound to test different bound thresholds determining the radius of acception
# actKappa = 0
# 
# # iteration over bound to test different bound thresholds determining the radius of acception
# for(jj in seq(along = c(1:length(bound)))){
#   
#   
#   # remove VSV which are not located within certain distance to org.SV; 
#   # done by "rem_extrem_kerneldist()" which evaluates the kernel distance (in hyperspace) under consideration of the kernelfunction used in baseSVM
#   # to use euclidian distance (in inputspace) instead of kernel distance use "rem_extrem()"
#   
#   SVinvarRadiUn = rbind(setNames(rem_extrem(SVtotalUn_b, SVL2Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL3Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL5Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL6Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL7Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL8Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL9Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL10Un_b, bound[jj]),objInfoNames),
#                         setNames(rem_extrem(SVtotalUn_b, SVL11Un_b, bound[jj]),objInfoNames)
#   ) 
#   
#   # remove NAs 
#   SVinvarRadiUn = na.omit(SVinvarRadiUn)
#   
#   # iterating over boundMargin to test different threshold on margin distance
#   for (kk in seq(along = c(1:length(boundMargin)))){
#     
#     # remove VSV which are not located in certain distance to desicion function
#     # data.frame to store elected VSV within the margin
#     SVinvarUn=setNames(data.frame(matrix(ncol = numFeat+1)), objInfoNames)
#     
#     # iterate over SVinvarRadi and evaluate distance to hyperplane
#     # implementation checks class membership for case that each class should be evaluate on different bound
#     for(m in seq(along = c(1:nrow(SVinvarRadiUn)))){
#       signa = as.numeric(pred_one(tunedSVM$finalModel, unlist(SVinvarRadiUn[m,-ncol(SVinvarRadiUn)])))
#       
#       if(SVinvarRadiUn[m,ncol(SVinvarRadiUn)] == levels(generalDataPool$REF)[1]){
#         if((signa < boundMargin[kk]) && (signa > -boundMargin[kk])){
#           SVinvarUn = rbind(SVinvarUn, SVinvarRadiUn[m,])
#         }
#       }else{
#         if(SVinvarRadiUn[m,ncol(SVinvarRadiUn)] == levels(generalDataPool$REF)[2]){
#           if((signa > -boundMargin[kk])&& (signa < boundMargin[kk])){
#             SVinvarUn = rbind(SVinvarUn, SVinvarRadiUn[m,])
#             
#           }
#         }
#       }
#     }
#     
#     # merge elected VSV with original SV
#     SVinvar_orgUn = rbind(setNames(SVtotal,objInfoNames), setNames(SVinvarUn,objInfoNames))
#     
#     SVinvar_orgUn=na.omit(SVinvar_orgUn)
#     
#     # split for training to feature and label
#     trainFeatSVMUn = SVinvar_orgUn[,1:(ncol(SVinvar_orgUn)-1)]
#     trainLabelsSVMUn = SVinvar_orgUn[,ncol(SVinvar_orgUn)]
#     
#     # get list with index of trainData to split between train and test in svmFit
#     countTrainDataUn = nrow(SVinvar_orgUn)
#     indexTrainDataUn = list(c(1:countTrainDataUn))
#     
#     # join of train and test data (through indesTrainData in svmFit seperable)
#     names = objInfoNames[1:length(objInfoNames)-1]
#     tuneFeatSVMUn = rbind(trainFeatSVMUn, setNames(testFeatsub, names))
#     tuneLabelsSVMUn = unlist(list(trainLabelsSVMUn, testLabels))
#     
#     ######################################## VSVM control parameter tuning ########################################
#     tunedSVMUn = svmFit(tuneFeatSVMUn, tuneLabelsSVMUn, indexTrainDataUn)
#     
#     # of all Different bound settings get the one with best Kappa ans save its model
#     if(actKappa < tunedSVMUn$resample$Kappa){
#       bestFittingModelUn = tunedSVMUn
#       actKappa = tunedSVMUn$resample$Kappa
#     }
#   }
# }
# 
# # run classification and accuracy assesment for the best bound setting
# # predict labels of test data
# predLabelsSVMsumUn = predict(bestFittingModelUn, validateFeatsub)
# 
# # accuracy assessment
# accSVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)

#################################### UNCERTAINTY function on svm_MS-sl-un ###################################
# 
# # add predicted labels to the features data set
# predLabelsSVMsumUn_unc= cbind(validateFeatsub, predLabelsSVMsumUn)
# refLabelsSVMsumUn_unc = cbind(validateFeatsub, validateLabels)
# 
# predLabelsSVMsumUn_unc = setNames(predLabelsSVMsumUn_unc, objInfoNames)
# 
# # get SV of tunedSVM-SL-Un if necessary
# SVindex = tunedSVMUn$finalModel@SVindex   # indices 1:(sample size per class) ; values
# SVtotalUn = trainDataCur[SVindex ,c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCur))]
# 
# # calculate uncertainty of the samples by selecting SV's and data set
# normdistsvm_sl_un_v2 = uncertainty_dist_v2_2(tunedSVMUn, predLabelsSVMsumUn_unc)
# 
# # add real labels
# normdistsvm_sl_un_v2_refad = cbind(normdistsvm_sl_un_v2,validateLabels )
# 
# # select most uncertain 100 samples and relabel them
# normdistsvm_sl_un_sub = normdistsvm_sl_un_v2_refad[order(normdistsvm_sl_un_v2_refad$distance),]
# normdistsvm_sl_un_sub[1:100,]$label = normdistsvm_sl_un_sub[1:100,]$validateLabels
# normdistsvm_sl_un_sub[1:100,]$distance = 1.0000000000
# 
# # reorder data frame by its index
# normdistsvm_sl_un_sub$index = as.numeric(row.names(normdistsvm_sl_un_sub))
# normdistsvm_sl_un_sub_or = normdistsvm_sl_un_sub [order(normdistsvm_sl_un_sub$index),]
# 
# # extract labels for prediction
# predLabelsSVMsumUn_un= normdistsvm_sl_un_sub_or[,(ncol(normdistsvm_sl_un_sub_or)-4)]
# 
# # with function
# predlabels_svm_Slu = alter_labels(normdistsvm_sl_un_v2,validateLabels, 250)
# 
# accSVM_SL_Un_Ad = confusionMatrix(predlabels_svm_Slu, validateLabels)
# 
# # # using function instead of using the codes above
# # deneme = alter_labels(normdistsvm_sl_un_v2, validateLabels)
# # 
# # # recalculate accuracy 
# # accSVM_SL_Un_Ad = confusionMatrix(deneme, validateLabels)

# ####################################### apply model and export data #####################################
# 
# setwd(paste0(path,"results/uncertainty/final"))
# 
# # save mean and sd of each kappa matrices for all variations ##
# save(accSVM,accSVM_M,accVSVM,accVSVM_SL,accVSVM_SL_Un_b,accVSVM_SL_vUn_b,accSVM_SL_Un,file =paste("ColScaleBinary_accuracy_",r,"unlabledsamples.RData",sep=""))
# save(accSVM,accSVM_M,accVSVM,accVSVM_SL,accVSVM_SL_Un_b_ad,accVSVM_SL_vUn_b_ad,accSVM_SL_Un_Ad,file =paste("ColScaleBinary_accuracy_",r,"unlabledsamplesReLabeled.RData",sep=""))
# 
# ############################################################
# #                                                          #
# #               apply VSVM-SL-Unl_b-V_Unl_b                #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply_vu = predict(bestFittingModelvUn_b, normalized_data)
# tunedVSVM_SL_VUn_bv = bestFittingModelvUn_b
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_VUn_b",b,"unl", shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply_vu, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# uncert_vsvm_Sl_vunb = setNames(cbind(normalized_data,predLabels_data_modell_apply_vu),objInfoNames)
# uncert_vsvm_Sl_vunb_dist_v2= uncertainty_dist_v2(tunedVSVM_SL_VUn_bv,uncert_vsvm_Sl_vunb)
# uncert_vsvm_Sl_vunb_dist_v2_col = uncert_vsvm_Sl_vunb_dist_v2[,ncol(uncert_vsvm_Sl_vunb_dist_v2):ncol(uncert_vsvm_Sl_vunb_dist_v2)]
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_VUn_uncertainty",shares[1],"samples.csv",sep="")
# write.csv2(uncert_vsvm_Sl_vunb_dist_v2_col, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# # add real labels
# uncert_vsvm_Sl_vunb_dist_v2_refad = cbind(uncert_vsvm_Sl_vunb_dist_v2,data_label )
# 
# # select most uncertain 100 samples and relabel them
# uncert_vsvm_Sl_vunb_sub = uncert_vsvm_Sl_vunb_dist_v2_refad[order(uncert_vsvm_Sl_vunb_dist_v2_refad$distance),]
# uncert_vsvm_Sl_vunb_sub[1:250,]$label = uncert_vsvm_Sl_vunb_sub[1:250,]$data_label
# uncert_vsvm_Sl_vunb_sub[1:250,]$distance = 1.0000000000
# 
# # normalize distances
# preProc = preProcess(uncert_vsvm_Sl_vunb_sub, method = "range")
# uncert_vsvm_Sl_vunb_sub_n = predict(preProc, uncert_vsvm_Sl_vunb_sub)
# 
# # reorder dataframe by its index
# uncert_vsvm_Sl_vunb_sub_n$index = as.numeric(row.names(uncert_vsvm_Sl_vunb_sub_n))
# uncert_vsvm_Sl_vunb_sub_sub_or = uncert_vsvm_Sl_vunb_sub_n [order(uncert_vsvm_Sl_vunb_sub_n$index),]
# 
# lab = uncert_vsvm_Sl_vunb_sub_sub_or[,ncol(uncert_vsvm_Sl_vunb_sub_sub_or)-4]
# dist = uncert_vsvm_Sl_vunb_sub_sub_or[,ncol(uncert_vsvm_Sl_vunb_sub_sub_or)-2]
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_VUn_uncertainty_recalculated_lab",shares[1],"samples.csv",sep="")
# write.csv2(lab, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                 apply SVM_MS-SL-Unl_b                    #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply_slu = predict(bestFittingModelUn, normalized_data)
# 
# tunedVSVM_SL_Un_b = bestFittingModelUn
# 
# outputfile = paste("ColScaleBinary_SVM_SL_Un_b",b,"unl", shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply_slu, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# uncert_svm_Sl_unb = setNames(cbind(normalized_data,predLabels_data_modell_apply_slu),objInfoNames)
# uncert_svm_Sl_unb_dist_v2= uncertainty_dist_v2(tunedVSVM_SL_Un_b,uncert_svm_Sl_unb)
# uncert_svm_Sl_unb_dist_v2_col = uncert_svm_Sl_unb_dist_v2[,ncol(uncert_svm_Sl_unb_dist_v2):ncol(uncert_svm_Sl_unb_dist_v2)]
# 
# outputfile = paste("ColScaleBinary_SVM_SL_Un_b_uncertainty",shares[1],"samples.csv",sep="")
# write.csv2(uncert_svm_Sl_unb_dist_v2_col, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# # add real labels
# uncert_svm_Sl_unb_dist_v2_refad = cbind(uncert_svm_Sl_unb_dist_v2,data_label )
# 
# 
# # select most uncertain 100 samples and relabel them
# uncert_svm_Sl_unb_sub = uncert_svm_Sl_unb_dist_v2_refad[order(uncert_svm_Sl_unb_dist_v2_refad$distance),]
# uncert_svm_Sl_unb_sub[1:250,]$label = uncert_svm_Sl_unb_sub[1:250,]$data_label
# uncert_svm_Sl_unb_sub[1:250,]$distance = 1.0000000000
# 
# # normalize distances
# preProc = preProcess(uncert_svm_Sl_unb_sub, method = "range")
# uncert_svm_Sl_unb_sub_n = predict(preProc, uncert_svm_Sl_unb_sub)
# 
# 
# # reorder dataframe by its index
# uncert_svm_Sl_unb_sub_n$index = as.numeric(row.names(uncert_svm_Sl_unb_sub_n))
# uncert_svm_Sl_unb_sub_or = uncert_svm_Sl_unb_sub_n [order(uncert_svm_Sl_unb_sub_n$index),]
# 
# 
# lab = uncert_svm_Sl_unb_sub_or[,ncol(uncert_svm_Sl_unb_sub_or)-4]
# dist = uncert_svm_Sl_unb_sub_or[,ncol(uncert_svm_Sl_unb_sub_or)-2]
# 
# outputfile = paste("ColScaleBinary_SVM_SL_Un_uncertainty_recalculated_lab_1",shares[1],"samples.csv",sep="")
# write.csv2(lab, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                  apply VSVM-SL-Unl_b                     #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply_u = predict(bestFittingModelUn_b, normalized_data)
# 
# tunedVSVM_SL_Un = bestFittingModelUn_b
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_Un_b",b,"unl", shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply_u, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# uncert_vsvm_Sl_un = setNames(cbind(normalized_data,predLabels_data_modell_apply_u),objInfoNames)
# uncert_vsvm_Sl_un_dist_v2= uncertainty_dist_v2(tunedVSVM_SL_Un ,uncert_vsvm_Sl_un)
# uncert_vsvm_Sl_un_dist_v2_col = uncert_vsvm_Sl_un_dist_v2[,ncol(uncert_vsvm_Sl_un_dist_v2):ncol(uncert_vsvm_Sl_un_dist_v2)]
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_Un_b_uncertainty",shares[1],"samples.csv",sep="")
# write.csv2(uncert_vsvm_Sl_un_dist_v2_col, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# # add real labels
# uncert_vsvm_Sl_un_dist_v2_refad = cbind(uncert_vsvm_Sl_un_dist_v2,data_label )
# 
# # select most uncertain 100 samples and relabel them
# uncert_vsvm_Sl_unb_sub = uncert_vsvm_Sl_un_dist_v2_refad[order(uncert_vsvm_Sl_un_dist_v2_refad$distance),]
# uncert_vsvm_Sl_unb_sub[1:250,]$label = uncert_vsvm_Sl_unb_sub[1:250,]$data_label
# uncert_vsvm_Sl_unb_sub[1:250,]$distance = 1.0000000000
# 
# # normalize distances
# preProc = preProcess(uncert_vsvm_Sl_unb_sub, method = "range")
# uncert_vsvm_Sl_unb_sub_n = predict(preProc, uncert_vsvm_Sl_unb_sub)
# 
# # reorder dataframe by its index
# uncert_vsvm_Sl_unb_sub_n$index = as.numeric(row.names(uncert_vsvm_Sl_unb_sub_n))
# uncert_vsvm_Sl_unb_sub_or = uncert_vsvm_Sl_unb_sub_n [order(uncert_vsvm_Sl_unb_sub_n$index),]
# 
# lab = uncert_vsvm_Sl_unb_sub_or[,ncol(uncert_vsvm_Sl_unb_sub_or)-4]
# dist = uncert_vsvm_Sl_unb_sub_or[,ncol(uncert_vsvm_Sl_unb_sub_or)-2]
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_Un_b_uncertainty_recalculated_lab_1",shares[1],"samples.csv",sep="")
# write.csv2(lab, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ####################################### uncertainty visualization #######################################
# 
# # add predicted labels to the features
# uncert_vsvm_Sl_unb = setNames(cbind(normalized_data,predLabels_data_modell_apply),objInfoNames)
# # calculate distances to the hyperplane with uncertainty function
# uncert_vsvm_Sl_unb_dist_v2= uncertainty_dist_v2(tunedVSVM_SL_Un_b,uncert_vsvm_Sl_unb)
# # extract uncertainty coloumn for visualizing 
# uncert_vsvm_Sl_unb_dist_v2_cols = uncert_vsvm_Sl_unb_dist_v2[,ncol(uncert_vsvm_Sl_unb_dist_v2):ncol(uncert_vsvm_Sl_unb_dist_v2)]
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_Un_b_uncertainty_colonly",b,"samples.csv",sep="")
# write.csv2(uncert_vsvm_Sl_unb_dist_v2_cols, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                  apply VSVM-SL-Unl_r                     #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply = predict(bestFittingModelUn_r, normalized_data)
# tunedVSVM_SL_Un_r = bestFittingModelUn_r
# 
# outputfile = paste("ColScaleBinary_VSVM_SL_Un_r",r,"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                     apply VSVM-SL                        #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply = predict(bestFittingModel, normalized_data)
# tunedVSVM_SL = bestFittingModel
# 
# accoutputfile = paste("ColScaleBinary_VSVM_SL_",shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                       apply SVM                          #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply = predict(tunedSVM, normalized_data)
# outputfile = paste("ColScaleBinary_SVM_",shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                       apply SVM-MS                       #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply = predict(tunedSVM_MS, normalized_data_MS,probability = TRUE, decision.values = TRUE)
# outputfile = paste("ColScaleBinary_SVM_M_",shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# ############################################################
# #                                                          #
# #                       apply VSVM                         #
# #                                                          #
# ############################################################
# 
# predLabels_data_modell_apply = predict(tunedVSVM_apply, normalized_data)
# tunedVSVM = tunedVSVM_apply
# 
# outputfile = paste("ColScaleBinary_VSVM_",shares[1],"samples.csv",sep="")
# write.csv2(predLabels_data_modell_apply, file = outputfile, sep=";",row.names = T ,col.names = F)
# 
# # save accuracy matrices for all variations ##
# save(accSVM,accSVM_M,accVSVM,accVSVM_SL,accVSVM_SL_Un_b,accVSVM_SL_vUn_b,accSVM_SL_Un, file =paste("ColScaleBinary_accuracyMatrix_image_",shares[1],"samples.RData",sep=""))
# save(tunedSVM,tunedSVM_MS,tunedVSVM,tunedVSVM_SL,tunedVSVM_SL_Un_b,tunedVSVM_SL_Un_r,file =paste("ColScaleBinary_modelObjects_image_",shares[1],"samples.RData",sep=""))
# 
# 


model_name = "bestFittingModelvUn_b.rds"
# Define the SVL variables
SVL_variables <- list(
  list(SVtotal, SVL2),
  list(SVtotal, SVL3),
  list(SVtotal, SVL5),
  list(SVtotal, SVL6),
  list(SVtotal, SVL7),
  list(SVtotal, SVL8),
  list(SVtotal, SVL9),
  list(SVtotal, SVL10),
  list(SVtotal, SVL11),
  list(SVtotalvUn_b, SVL2Un_b),
  list(SVtotalvUn_b, SVL3Un_b),
  list(SVtotalvUn_b, SVL5Un_b),
  list(SVtotalvUn_b, SVL6Un_b),
  list(SVtotalvUn_b, SVL7Un_b),
  list(SVtotalvUn_b, SVL8Un_b),
  list(SVtotalvUn_b, SVL9Un_b),
  list(SVtotalvUn_b, SVL10Un_b),
  list(SVtotalvUn_b, SVL11Un_b)
)

SL = function(model_name=model_name,
              SVL_variables=SVL_variables, 
              train=TRUE, 
              rem_extrem_kerneldist=rem_extrem_kerneldist, 
              pred_one=pred_one,
              tunedSVM=tunedSVM,
              num_cores=num_cores,
              boundMargin=boundMargin, 
              bound=bound,
              objInfoNames=objInfoNames,
              numFeat=numFeat,
              countTrainData=countTrainData,
              SVtotal=SVtotal,
              indexTrainData=indexTrainData
              ){
  
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
      SVinvarRadi <- foreach(variable = SVL_variables, .combine = rbind) %dopar% {
        setNames(rem_extrem_kerneldist(variable[[1]], variable[[2]], bound[jj]), objInfoNames)
      }
      registerDoSEQ()
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
        for(m in seq(along = c(1:nrow(SVinvarRadiUn_b)))){
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
        tunedVSVM = svmFit(tuneFeatVSVM, tuneLabelsVSVM, indexTrainData, classProb = TRUE)
        
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
    # saveRDS(bestFittingModelvUn_b, "bestFittingModelvUn_b.rds")
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa, 
                best_trainFeatVSVM = best_trainFeatVSVM, 
                best_trainLabelsVSVM = best_trainLabelsVSVM, 
                best_bound = best_bound, 
                best_boundMargin = best_boundMargin))
  }
}