script = "ALTSLv3"  # -> New train samples and Active labeled samples are distinct data
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

nR = 10                   # number of realizations
cities = c("hagadera")    # cologne or hagadera location
invariances = c("scale")   # scale or shape invariance
model_probs = c("binary")  # multiclass or binary problem

b = c(20)                     # size of balanced_unlabeled_samples per class
bound = c(0.3, 0.6, 0.9)      # radius around SV - threshold       
boundMargin = c(1.5, 1, 0.5)  # distance from hyperplane - threshold   
sampleSizePor = c(25,30, 40,40, 60,60, 100,100, 180,180, 340,340, 500,500)

path = '/home/data1/Lorenzo/'
#####################################################  Utils  ####################################################
# ************************************************************************************************************** #
#                                       lorenzo.carlassara98@gmail.com                                           #
#                                       linkedin.com/in/lorenzo-carlassara/                                      #
#                                       feel free to reach me out for any question                               #
# ************************************************************************************************************** #
# sampleSizePor = c(5,10,20,32,46,62,80,100) # Class sample size: round(250/6) label per class i.e. 42 # c(100,80,62,46,32,20,10,5)
lgtS=TRUE
train  = TRUE              # if TRUE, train the models otherwise load them from dir 
num_cores <- 48 #parallel::detectCores() # Numbers of CPU cores for parallel processing

if(!dir.exists(path)){path = "D:/"}

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
  smallestDistance <- 9999
  distance <- c()
  for(ll in seq(along=dataPointLabels)){ #print(dataPointLabels[ll])
    for(l in seq(along=binaryClassProb)){ #print(binaryClassProb[[l]])
      if(as.integer(dataPointLabels[ll]) %in% as.integer(binaryClassProb[[l]])){ #print(paste("vero", pred))
        pred <- sum(sapply(1:nrow(modelfin@xmatrix[[l]]), function(j) 
          modelfin@kernelf(xmatrix(modelfin)[[l]][j,], dataPoint[1:length(dataPoint)])*modelfin@coef[[l]][j]))-modelfin@b[l]
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
#   registerDoParallel(num_cores)
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
  
  # Set up parallel backend
  registerDoParallel(num_cores)
  margin_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    # calculate_margin_distance(k)
    pred_one(org$finalModel, unlist(samp[k, -ncol(samp)]), classes, binaryClassProblem)
  }
  registerDoSEQ()
  
  # scale distances
  scaled_distances <- apply(margin_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # Assign scaled distances to probability dataframe
  # margin_distance$distance <- scaled_distances
  margin_distance$distance <- log1p(scaled_distances * (exp(1) - 1))
  merged_data <- cbind(samp, margin_distance)
  
  # ***********************************************************************************
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) {
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MSUD_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=12,
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
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
  }
  
  # ****************************************************************************************
  
  return(merged_data)
}

# mclu_sampling(new_tunedSVM, predLabelsVSVM_unc)
# mclu_sampling(org=new_tunedVSVM_v1, samp=predLabelsVSVM_unc, pred_all, binaryClassProblem, classes=NA) 
# Evaluate Multiclass Level Uncertainty (MCLU)
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
  
  # Use foreach for parallel processing
  registerDoParallel(num_cores)
  mclu_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
    calculate_mclu_distance(k)
  }
  registerDoSEQ()
  
  mclu_scaled_distances <- apply(mclu_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # uncertainty$distance <- mclu_distances
  uncertainty$distance <- log1p(mclu_scaled_distances * (exp(1) - 1))
  merged_data <- cbind(samp, uncertainty)
  
  # ***********************************************************************************
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) { 
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MS_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=12,
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
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
  }
  
  # ****************************************************************************************
  
  return(merged_data)
}

# mclp_sampling <- function(org, samp) {
#   
#   # Initialize data frame to store PROBABILITY for each sample
#   probability <- data.frame(control_label = as.character(samp[, ncol(samp)]), distance = numeric(nrow(samp)))
#   
#   # Define the function to calculate margin distance for a single sample
#   calculate_mclp_distance <- function(k) {
#     probabilities <- predict(org, newdata = samp[k, -ncol(samp)], type = "prob")
#     
#     # Get the two most probable classes
#     top_classes <- (sort(unlist(probabilities), decreasing = TRUE))[1:2]
#     
#     # Calculate the difference between the probabilities for the two most probable classes
#     return(abs(top_classes[[1]] - top_classes[[2]]))
#   }
#   registerDoParallel(num_cores)
#   
#   # Use foreach for parallel processing with " %dopar% "
#   mclp_distances <- foreach(k = 1:nrow(samp), .combine = rbind) %dopar% {
#     calculate_mclp_distance(k)
#   }
#   registerDoSEQ()
#   # Apply "range" normalization to mclp_distances
#   scaled_distances <- apply(mclp_distances, 2, function(x) (x - min(x)) / (max(x) - min(x)))
#   
#   # Assign scaled distances to probability dataframe
#   probability$distance <- scaled_distances
#   merged_data <- cbind(samp, probability)
#   
#   return(merged_data)
# }
# result <- add_AL_samples(distance_data=sampled_data,
# ref=reference_label, features=sampled_data[,1:numFeat]
# ref=upd_dataCurLabels, features=upd_dataCurFeatsub, 
#                          new_trainFeatVSVM, new_trainLabelsVSVM,
#                          newSize=newSize_for_iter, cluster=round(min(clusterSizes[cS],nrow(sampled_data)/20)), 
#                          ID_unit=upd_dataCur$ID_unit, nFeat=numFeat, PCA_flag=FALSE, tSNE_flag=TRUE, 
#                          realiz=realization, s_size=sample_size, newSize2=b[1]*nclass, plot_flag=model_name_ALTrainSL_VSVMSL) 
add_AL_samples = function(distance_data,
                          features=NULL, ref, 
                          new_trainFeatVSVM=NULL, new_trainLabelsVSVM=NULL,
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

    # Display the first pairs
    for (i in seq_len(min(1, num_duplicates))) {
      original_index <- duplicate_indices[i]
      duplicate_index <- which(apply(ref_added_or[, 1:nFeat], 1, function(row) all(row == ref_added_or[original_index, 1:nFeat])))[1]
      cat("Duplicate pair", i, ":\n")
      print(ref_added_or[c(duplicate_index, original_index), ])
    }
    
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
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) {
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
    
    # Define colors for clusters
    cluster_colors <- rainbow(cluster)
    
    if(PCA_flag){
      pca_data_with_distance$Cluster <- as.factor(km_pca$cluster)
      
      png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"), plot_flag, "_cluster_PCA_distance_", script, "_", city, "_", model_prob, "_", invariance, ".png"),
          units="in", 
          width=16, 
          height=20, 
          pointsize=12,
          res=96)
      par(mfrow = c(2, 1), mar = c(5, 4, 4, 8), xpd = TRUE)
      
      # Plot PCA
      plot(pca_data$PC1, pca_data$PC2, col = cluster_colors[pca_data_with_distance$Cluster],
           pch = 20, cex = 2, main = "PCA with K-means Clustering",
           xlab = "Principal Component 1", ylab = "Principal Component 2")
      # legend("topright", inset = c(-0.2, 0), legend = levels(pca_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      
      # Plot PCA with Distance
      plot(pca_data_with_distance$PC1, ref_added_or$distance, col = cluster_colors[pca_data_with_distance$Cluster],
           pch = 20, cex = 2, main = "K-means Clustering on PCA + Distance",
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
          pointsize=12,
          res=96)
      par(mfrow = c(2, 1), mar = c(5, 4, 4, 8), xpd = TRUE)
      
      # Plot t-SNE
      plot(tsne_data$tSNE1, tsne_data$tSNE2, col = cluster_colors[tsne_data_with_distance$Cluster],
           pch = 20, cex = 2, main = c("t-SNE with K-means Clustering ", cluster),
           xlab = "t-SNE 1", ylab = "t-SNE 2")
      # legend("topright", inset = c(-0.2, 0), legend = levels(tsne_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      
      # Plot t-SNE with Distance
      plot(tsne_data_with_distance$tSNE1, ref_added_or$distance, col = cluster_colors[tsne_data_with_distance$Cluster],
           pch = 20, cex = 2, main = "K-means Clustering on t-SNE + Distance",
           xlab = "t-SNE 1", ylab = "Distance")
      # legend("topright", inset = c(-0.2, 0), legend = levels(tsne_data_with_distance$Cluster),
      #        col = cluster_colors, pch = 20, title = "Cluster", bty = "n")
      dev.off()
    }
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
  }
  # ***********************************************************************************
  
  # # Initialize a vector to store selected sample indices
  # selected_indices <- c()
  # cluster_samples <- c()
  # selected_indices_semi <- c()
  # label_samples_semi <- c()
  # labels_in_semi <- c()
  # tmpSize = 0
  # # Iterate over clusters and select one sample from each cluster
  # for (sample in seq_len(nrow(ref_added_or))) {
  #   if (!( ref_added_or[sample,]$cluster %in% cluster_samples) && tmpSize < newSize){
  #     if(flag_cluster){
  #       cluster_samples <- c(cluster_samples, ref_added_or[sample,]$cluster)
  #     }
  #     tmpSize = tmpSize+1
  #     
  #     ref_added_or[sample,]$label <- ref_added_or[sample,]$ref
  #     selected_indices <- c(selected_indices, as.numeric(rownames(ref_added_or[sample,])))
  #   }
  # 
  #   # if (tmpSize >= newSize && tmpSize < newSize + semi_size - nclass +1) {
  #   #     tmpSize <- tmpSize + 1
  #   #     selected_indices_semi <- c(selected_indices_semi, as.numeric(rownames(ref_added_or[sample,])))
  #   #     if (!(ref_added_or[sample,]$label %in% labels_in_semi) ) {
  #   #       labels_in_semi <- c(labels_in_semi, ref_added_or[sample,]$label)
  #   #     }
  #   # }
  #   # if(tmpSize < newSize + semi_size && length(labels_in_semi) < nclass && !(ref_added_or[sample,]$label %in% labels_in_semi)){
  #   #       labels_in_semi <- c(labels_in_semi, ref_added_or[sample,]$label)
  #   #       tmpSize <- tmpSize + 1
  #   #       selected_indices_semi <- c(selected_indices_semi, as.numeric(rownames(ref_added_or[sample,])))
  #   # }
  # 
  #   
  # }
  
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
      if (class_sample_count[class_label] < samples_per_class || !flag_class) {
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
      }
      
      # Check if the first selection is done
      if (sum(class_sample_count) >= newSize) {
        first_selection_done <- TRUE
        
        # Reset class_sample_count 
        class_sample_count <- setNames(rep(0, length(class_labels)), class_labels)
        selected_clusters <- list()
        for (label in class_labels) { selected_clusters[[label]] <- c()  }
      }
    } else {  # Start selecting for the second set of selected_indices2
      
      # Check if the current class has not exceeded the allowed number of samples
      if (class_sample_count[class_label] < samples_per_class2 || !flag_class ) {
        # Check if the cluster has not been selected for this class
        if (!(cluster_id %in% selected_clusters[[class_label]]) || !flag_cluster || length(unique(ref_added_or[ref_added_or$label==class_label,"cluster"]))<samples_per_class2) {
          
          # Add the sample's cluster to the selected clusters for this class
          selected_clusters[[class_label]] <- c(selected_clusters[[class_label]], cluster_id)
          
          # Add the index of the selected sample to the second selection
          selected_indices2 <- c(selected_indices2, as.numeric(rownames(ref_added_or[sample,])))
          
          # Increment the count of selected samples for this class
          class_sample_count[class_label] <- class_sample_count[class_label] + 1
        }
      }
      
      # Stop if we have reached the desired total number of samples for the second selection
      if (sum(class_sample_count) >= newSize2) { break }
    }
  }
  ref_added_reor = ref_added_or[order(as.numeric(rownames(ref_added_or))),]
  
  # Add relabeled samples to new_trainFeatVSVM and new_trainLabelsVSVM
  if(length(features)>1){
    # Remove relabeled samples from validateLabels
    features <- features[!(rownames(features) %in% selected_indices), ]
    reor_idx <- which(rownames(ref_added_reor) %in% selected_indices)
    semi_idx <- which(rownames(ref_added_reor) %in% selected_indices2)
    
    if(length(ID_unit)>100){
      new_trainFeatVSVM <- ref_added_reor[reor_idx, 1:nFeat]
      new_trainLabelsVSVM <- ref_added_reor[reor_idx, nFeat+1]
      return(list(IDunit=ID_unit[reor_idx], semiIDunit=ID_unit[semi_idx], 
                  semi_samples = ref_added_reor[semi_idx, 1:(nFeat+1)],
                  # features = features, labels = ref[-reor_idx], 
                  new_trainFeatVSVM = new_trainFeatVSVM, 
                  new_trainLabelsVSVM = new_trainLabelsVSVM))
    } 
    cat("Not returning ID_unit\n")
    new_trainFeatVSVM <- rbind(new_trainFeatVSVM, ref_added_reor[reor_idx, 1:nFeat])
    new_trainLabelsVSVM <- c(new_trainLabelsVSVM, ref_added_reor[reor_idx, nFeat+1])
    return(list(features = features, labels = ref[-reor_idx],
                new_trainFeatVSVM = new_trainFeatVSVM, 
                new_trainLabelsVSVM = new_trainLabelsVSVM))
  } 
  return(ref_added_reor[, nFeat+1])
}

# upd_SLresult <- self_learn(testFeatsub, testLabels, bound = c(0.01, 0.1), boundMargin = c(1.5, 0.5), model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables, tmp_new_tunedSVM$finalModel)
# upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
#                            SVL_variables=list(list(SVtotal_ud, S01C09=(cbind(upd_dataCur[upd_SVindex_ud,c((numFeat+1):(2*numFeat))],REF_ud)))))
self_learn = function(testFeatsub, testLabels, bound, boundMargin, model_name, SVtotal, objInfoNames, rem_extrem, rem_extrem_kerneldist, SVL_variables, SVMfinModel=tunedSVM$finalModel, train=TRUE, classProb = FALSE) {
  if (file.exists(paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl_",seed,"seed.rds")) && !train) {
    bestFittingModel <- readRDS(paste0(format(Sys.time(),"%Y%m%d"),model_name,"_",city,"_",model_prob,"_",invariance,"_",sampleSizePor[sample_size],"Size_",b,"Unl_",seed,"seed.rds"))
    actKappa = bestFittingModel$resample$Kappa
    cat("Luckily, model already exists!\n")
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa))
  } else {
    actKappa = -1e-6
    cat("applying constraints to VSVs candidates\n")
    # iteration over bound to test different bound thresholds determining the radius of acception
    for(jj in seq(along=bound)){
      
      registerDoParallel(num_cores)
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
      registerDoSEQ() # print("step 3")
      
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
    return(list(bestFittingModel = bestFittingModel, 
                actKappa = actKappa, 
                best_trainFeatVSVM = best_trainFeatVSVM, 
                best_trainLabelsVSVM = best_trainLabelsVSVM, 
                best_bound = best_bound, 
                best_boundMargin = best_boundMargin))
  }
}

# self_learn_AL(testFeatsub, testLabels, SVtotal, objInfoNames,
#                                SVinvarRadi=SVL_variables, plot_flag = model_name_ALTrainSL_VSVMSL)
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
      signa = as.numeric(pred_one(new_bestTunedVSVM$finalModel, unlist(SVinvarRadi[m, -ncol(SVinvarRadi)]), SVinvarRadi[m, ncol(SVinvarRadi)]))
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
  
  if (!(is.logical(plot_flag)) && realiz==1 && s_size==3) { 
    setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
    # Plotting the histogram
    png(filename=paste0(format(Sys.time(),"%Y%m%d_%H%M"),plot_flag,"_MSSL_AL_Distances_",script,"_",city,"_",model_prob,"_",invariance,".png"),
        units="in", 
        width=20, 
        height=9, 
        pointsize=12,
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
    setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
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
##################################################################################################################
for (model_prob in model_probs) { 
  for (invariance in invariances) {
    for (city in cities) {
      
      cat("preprocessing",city,model_prob,invariance,"\n")
      if(city=="cologne"){ 
        sampleSizePor = c(30,36, 48,48, 72,72, 120,120, 216,216, 408,408, 600,600)
      } 
      if(model_prob=="binary"){ 
      # sampleSizePor = c(10,12, 20,20, 40,40, 64,64, 92,92, 124,124, 160,160, 200,200)
        sampleSizePor = c(8,10, 14,14, 22,22, 38,38, 70,70, 134,134, 200,200)
      }
      if (lgtS) { 
        sampleSizePor = sampleSizePor[1:(length(sampleSizePor)-2)]
        if(model_prob=="multiclass"){ 
          # sampleSizePor = sampleSizePor[1:(length(sampleSizePor)-2)]
          }
        bound = c(0.3, 0.6)
        boundMargin = c(1.5, 0.5)
      }
      colheader = as.character(sampleSizePor) # corresponding column names
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
          # rm(validateDataAllLev)
          
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
          #########################################################################################
        } else { # seed 47
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
          # rm(validateDataAllLev)
          
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
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
                    # rm(validateDataAllLev)
          
          # order train datapool by class label in alphabetical order:
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
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
                    # rm(validateDataAllLev)
          
          trainDataPoolAllLev = trainDataPoolAllLev[order(trainDataPoolAllLev[,ncol(trainDataPoolAllLev)]),]
          
          ##################################################################################################################
        }
      }
      
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
      
      AccuracyVSVM_SL_Un_random_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_random_it) = colheader
      
      AccuracyVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_it) = colheader
      AccuracyVSVM_SL_Un_itSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_itSL) = colheader
      AccuracyVSVM_SL_Un_itTSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_itTSL) = colheader
      AccuracyVSVM_SL_Un_itSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_itSL2) = colheader
      AccuracyVSVM_SL_Un_itTSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(AccuracyVSVM_SL_Un_itTSL2) = colheader
      
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
      
      KappaVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_it) = colheader
      KappaVSVM_SL_Un_itSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_itSL) = colheader
      KappaVSVM_SL_Un_itTSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_itTSL) = colheader      
      KappaVSVM_SL_Un_itSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_itSL2) = colheader
      KappaVSVM_SL_Un_itTSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_itTSL2) = colheader
      
      KappaVSVM_SL_Un_random_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(KappaVSVM_SL_Un_random_it) = colheader
      
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
      
      SVsVSVM_SL_Un_it = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(SVsVSVM_SL_Un_it) = colheader
      SVsVSVM_SL_Un_itSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(SVsVSVM_SL_Un_itSL) = colheader
      SVsVSVM_SL_Un_itTSL = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(SVsVSVM_SL_Un_itTSL) = colheader
      SVsVSVM_SL_Un_itSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(SVsVSVM_SL_Un_itSL2) = colheader
      SVsVSVM_SL_Un_itTSL2 = matrix(data = NA, nrow = nR, ncol = length(colheader))
      colnames(SVsVSVM_SL_Un_itTSL2) = colheader
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
      if(city=="hagadera"){ nclass=5
      }else if(model_prob=="binary"){ nclass=2  }
      
      # set randomized seed for the random sampling procedure
      seed = 5 # 5, 73, 20, 98, 133
      
      ###############################################  Training  #################################################

      start.time_oa <- Sys.time()

      for (realization in seq(1,nR)) {
        
        cat("\n","CPU cores: ",num_cores,sep="")
        start.time <- Sys.time()

        for (sample_size in seq(1, length(sampleSizePor), by=2)) {
          cat("\n") ################################# Sampling train and test data #####################################
          
          # initial seed value for randomized sampling
          if (train) {seed = seed + sample(100, 1)}
          cat(city," ",model_prob ," ",invariance," | realization [",realization,"/",nR,"] | labeled samples: ",sampleSizePor[sample_size]," [",(sample_size+1)/2,"/",round(length(sampleSizePor)/2),"] | seed: ",seed,"\n",sep="")
          
          sampleSize = round(sampleSizePor[sample_size]/nclass)
          shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)
          
          # set randomized seed for the random sampling procedure
          set.seed(seed)
          
          if(sample_size==1){
            
            trainDataCurBeg = trainDataPoolAllLev
            testDataCurBeg = testDataAllLev
            # subset for each outer iteration test data to speed up computing
            testDataCurBeg = testDataCurBeg[order(testDataCurBeg[,ncol(testDataCurBeg)]),]
            
            if(realization==11){
              # *************
              if (lgtS) {
                # set.seed(seed)        
                # validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
                # validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
                # 
                # validateData <- cbind(validateFeatsub, validateLabels)
                # finalFeatsub <- data.frame()  # Container for final feature samples
                # finalLabels <- factor()  # Initialize with factor levels
                # 
                # # Sort labels by the number of instances (start with the smallest)
                # label_order <- levels(validateLabels)[order(table(validateLabels))]
                # 
                # for (label in label_order) {
                #   lightS <- sum(validateLabels == label)
                #   
                #   samplesRemaining <- data.frame()  # DataFrame to store unique samples
                #   valDataCurRemaining_sampl <- validateData[validateData$validateLabels == label, ]
                #   
                #   # Stratified sampling without replacement
                #   stratSampSize <- min(lightS, nrow(valDataCurRemaining_sampl))
                #   val_stratSamp <- strata(valDataCurRemaining_sampl, c("validateLabels"), size = stratSampSize, method = "srswor")
                #   validateData_sampl <- getdata(valDataCurRemaining_sampl, val_stratSamp)
                #   
                #   # Remove duplicates within the current sample
                #   # unique_new_samples <- validateData_sampl[!duplicated(validateData_sampl[, 1:ncol(validateFeatsub)]), ]
                #   unique_new_samples <- validateData_sampl[]
                #   
                #   # Check for duplicates against all previously collected samples
                #   if (nrow(finalFeatsub) > 0) {
                #     duplicate_indices <- duplicated(rbind(finalFeatsub[, 1:ncol(validateFeatsub)], unique_new_samples[, 1:ncol(validateFeatsub)]))
                #     unique_new_samples <- unique_new_samples[!duplicate_indices[(nrow(finalFeatsub) + 1):length(duplicate_indices)], ]
                #   }
                #   
                #   # Add unique rows to the cumulative dataframe
                #   samplesRemaining <- rbind(samplesRemaining, unique_new_samples)
                #   samplesRemaining <- samplesRemaining[!duplicated(samplesRemaining[, 1:ncol(validateFeatsub)]), ]
                #   cat("[ ", label, " 1 ] Number of samples: ", nrow(samplesRemaining),"\n", sep = "")
                #   
                #   # Append the unique samples to the final containers
                #   finalFeatsub <- rbind(finalFeatsub, samplesRemaining[, 1:ncol(validateFeatsub)])
                #   finalLabels <- factor(c(as.character(finalLabels), as.character(samplesRemaining$validateLabels)),
                #                         levels = levels(validateLabels))
                #   
                #   # ********************************************************************************************************************
                #   
                #   samplesRemaining <- data.frame()  # DataFrame to store unique samples
                #   light_factor<- 40
                #   if(city=="hagadera"){ light_factor<- 40 } # 16 # 20 # 25 # 35 # 40 # 60 # 80
                #   if(model_prob=="binary"){ light_factor<- 40 }
                #   # print(paste(lightS/light_factor,nrow(valDataCurRemaining_sampl)))
                #   stratSampSize <- min(lightS/light_factor, nrow(valDataCurRemaining_sampl))  
                #   val_stratSamp <- strata(valDataCurRemaining_sampl, c("validateLabels"), size = stratSampSize, method = "srswor")
                #   validateData_sampl <- getdata(valDataCurRemaining_sampl, val_stratSamp)
                #   
                #   samplesRemaining <- rbind(samplesRemaining, validateData_sampl)
                #   cat("[ ", label, " 2 ] Number of samples: ", nrow(samplesRemaining), "\n", sep = "")
                #   
                #   finalFeatsub <- rbind(finalFeatsub, samplesRemaining[, 1:ncol(validateFeatsub)])
                #   finalLabels <- factor(c(as.character(finalLabels), as.character(samplesRemaining$validateLabels)),
                #                         levels = levels(validateLabels))
                # }
                # 
                # # Replace original data with the final sampled data
                # validateFeatsub <- finalFeatsub
                # validateLabels <- finalLabels
                # print(length(validateLabels))
                # rm(valDataCurRemaining_sampl, validateData_sampl, unique_new_samples, val_stratSamp,finalFeatsub,finalLabels)
              }
              # *************
              
              if (lgtS) {
                # set.seed(seed)
                # validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
                # validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
                # 
                # validateData <- cbind(validateFeatsub, validateLabels)
                # finalFeatsub <- data.frame()  # Container for final feature samples
                # finalLabels <- factor()  # Initialize with factor levels
                # 
                # # Sort labels by the number of instances (start with the smallest)
                # label_order <- levels(validateLabels)[order(table(validateLabels))]
                # 
                # for (label in label_order) {
                #   lightS <- sum(validateLabels == label)
                # 
                #   samplesRemaining <- data.frame()  # DataFrame to store unique samples
                #   iteration <- 0  # Initialize the iteration counter
                #   valDataCurRemaining_sampl <- validateData[validateData$validateLabels == label, ]
                # 
                #   while (nrow(samplesRemaining) < lightS && iteration < 1) {
                #     iteration <- iteration + 1
                # 
                #     # Stratified sampling without replacement
                #     stratSampSize <- min(lightS, nrow(valDataCurRemaining_sampl))
                #     val_stratSamp <- strata(valDataCurRemaining_sampl, c("validateLabels"), size = stratSampSize, method = "srswor")
                #     validateData_sampl <- getdata(valDataCurRemaining_sampl, val_stratSamp)
                # 
                #     # Remove duplicates within the current sample
                #     # unique_new_samples <- validateData_sampl[!duplicated(validateData_sampl[, 1:ncol(validateFeatsub)]), ]
                #     unique_new_samples <- validateData_sampl[]
                # 
                #     # Check for duplicates against all previously collected samples
                #     if (nrow(finalFeatsub) > 0) {
                #       duplicate_indices <- duplicated(rbind(finalFeatsub[, 1:ncol(validateFeatsub)], unique_new_samples[, 1:ncol(validateFeatsub)]))
                #       unique_new_samples <- unique_new_samples[!duplicate_indices[(nrow(finalFeatsub) + 1):length(duplicate_indices)], ]
                #     }
                # 
                #     # Add unique rows to the cumulative dataframe
                #     samplesRemaining <- rbind(samplesRemaining, unique_new_samples)
                #     samplesRemaining <- samplesRemaining[!duplicated(samplesRemaining[, 1:ncol(validateFeatsub)]), ]
                # 
                #     # If more samples are needed, update the remaining pool
                #     if (nrow(samplesRemaining) < lightS) {
                #       valDataCurRemaining_sampl <- valDataCurRemaining_sampl[!rownames(valDataCurRemaining_sampl) %in% rownames(unique_new_samples), ]
                #     }
                # 
                #     cat("[ ", label, " ] Number of samples: ", nrow(samplesRemaining)," Number of labels: ", length(as.character(samplesRemaining$validateLabels)), "\n", sep = "")
                #   }
                # 
                #   # Append the unique samples to the final containers
                #   finalFeatsub <- rbind(finalFeatsub, samplesRemaining[, 1:ncol(validateFeatsub)])
                #   finalLabels <- factor(c(as.character(finalLabels), as.character(samplesRemaining$validateLabels)),
                #                         levels = levels(validateLabels))
                # }
                # 
                # # Replace original data with the final sampled data
                # validateFeatsub <- finalFeatsub
                # validateLabels <- finalLabels
                # print(length(validateLabels))
                # rm(valDataCurRemaining_sampl, validateData_sampl, unique_new_samples, val_stratSamp,finalFeatsub,finalLabels)
              }
              if (lgtS) {
                # set.seed(seed)
                # # lightS=as.numeric(min(table(validateLabels)))
                # # lightS=c(lightS,lightS,lightS,lightS,lightS,lightS)
                # # validateData = cbind(validateFeatsub,validateLabels)
                # # val_stratSamp = strata(validateData, c("validateLabels"), size = lightS, method = "srswor")
                # # validateData = getdata(validateData, val_stratSamp)
                # # validateFeatsub = validateData[,1:ncol(validateFeatsub)]
                # # validateLabels = validateData[,ncol(validateFeatsub)+1]
                # # rm(validateData, val_stratSamp)
                # 
                # validateLabels = validateDataAllLev[,(ncol(validateDataAllLev))]
                # validateFeatsub = validateDataAllLev[sindexSVMDATA:eindexSVMDATA]
                # 
                # validateData <- cbind(validateFeatsub,validateLabels)
                # finalFeatsub <- data.frame()  # Container for final feature samples
                # finalLabels <- factor()  # Initialize with factor levels
                # 
                # # Sort labels by the number of instances (start with the smallest)
                # label_order <- levels(validateLabels)[order(table(validateLabels))]
                # for (label in label_order) {
                # 
                #   lightS <- sum(validateLabels == label)/80
                #   samplesRemaining <- data.frame()  # DataFrame to store unique samples
                # 
                #   valDataCurRemaining_sampl <- validateData[validateData$validateLabels == label, ]
                # 
                #   # Stratified sampling without replacement
                #   stratSampSize <- min(lightS, nrow(valDataCurRemaining_sampl))
                #   val_stratSamp <- strata(valDataCurRemaining_sampl, c("validateLabels"), size = stratSampSize, method = "srswor")
                #   validateData_sampl <- getdata(valDataCurRemaining_sampl, val_stratSamp)
                # 
                #   samplesRemaining <- rbind(samplesRemaining, validateData_sampl)
                # 
                #   cat("[ ", label, " ] Number of samples: ", nrow(samplesRemaining)," Number of labels: ", length(as.character(samplesRemaining$validateLabels)), "\n", sep = "")
                # 
                #   # Append the unique samples to the final containers
                #   finalFeatsub <- rbind(finalFeatsub, samplesRemaining[, 1:ncol(validateFeatsub)])
                #   finalLabels <- factor(c(as.character(finalLabels), as.character(samplesRemaining$validateLabels)),
                #                         levels = levels(validateLabels))
                # }
                # 
                # # Extract relevant columns for uniqueness check
                # validateFeatsub <- finalFeatsub
                # validateLabels = finalLabels
                # print(length(validateLabels))
                # rm(valDataCurRemaining_sampl, val_stratSamp, samplesRemaining,finalFeatsub,finalLabels)
              }
            }
          }
          
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

          # trainData index to split between train and test in svmFit
          countTrainData = nrow(trainFeat)
          indexTrainData = list(c(1:countTrainData))
            
          # join of train and test test data (separable through indexTrainData in svmFit)
          tuneFeat = rbind(setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
          tuneLabel = unlist(list(trainLabels, testLabels))
          
          setwd(paste0(path, "GitHub/active-learning-virtual-SVM/saved_models/",city))
          
          cat("\n") ################################# SVM #####################################
          model_name_tunedSVM = "SVM"
          
          cat("training SVM\n")
          if (file.exists(model_name_tunedSVM) && !train) {
            tunedSVM <- readRDS(model_name_tunedSVM)
            cat("Luckily, model already exists!\n")
          } else { trainStart.time <- Sys.time()
          tunedSVM = svmFit(tuneFeat, tuneLabel, indexTrainData)
          trainSVM.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
          }
          # run classification and accuracy assessment for unmodified SV and predict labels of test data
          predLabelsSVM = predict(tunedSVM, validateFeatsub)
          accSVM = confusionMatrix(predLabelsSVM, validateLabels)
          cat("SVM accuracy: ",round(accSVM$overall["Accuracy"],5)," | execution time: ",trainSVM.time,"sec\n",sep="")
          
          AccuracySVM[realization,sample_size] = as.numeric(accSVM$overall["Accuracy"])
          KappaSVM[realization,sample_size] = as.numeric(accSVM$overall["Kappa"])
          SVsSVM[realization,sample_size] = as.numeric(length(tunedSVM$finalModel@SVindex))
          
          if(sample_size==1 || accSVM$overall["Accuracy"]>best_acc){
            best_acc <- accSVM$overall["Accuracy"]
            best_model <- model_name_tunedSVM
            new_bestTunedVSVM <- tunedSVM
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
          for (jj in 1:length(tunedSVM$finalModel@xmatrix)) { # COMPARE EVERY COUPLE COMBINATION OF CLASSES
            binaryClassProblem[[length(binaryClassProblem)+1]] = c(unique(trainDataCur[tunedSVM$finalModel@alphaindex[[jj]], ncol(trainDataCur)]))
          }
          # ******************************************************
          names = objInfoNames[1:(length(objInfoNames)-1)]

          cat("\n") ################################# SVM-SL + semi-labeled samples #####################################
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

          cat("evaluation of SVM with self learning and semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="")
          if (invariance=="scale") {
            SVL_variables = list(
              list(SVtotalSVMUn, SVL2SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REFSVM)),
              list(SVtotalSVMUn, SVL3SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REFSVM)),
              list(SVtotalSVMUn, SVL5SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REFSVM)),
              list(SVtotalSVMUn, SVL6SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REFSVM)),
              list(SVtotalSVMUn, SVL7SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REFSVM)),
              list(SVtotalSVMUn, SVL8SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REFSVM)),
              list(SVtotalSVMUn, SVL9SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REFSVM))
            )
            if (city=="cologne") {# get VSs, means rows of SV but with subset on different level
              cologne_vars = list(
                list(SVtotalSVMUn, SVL10SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REFSVM)),
                list(SVtotalSVMUn, SVL11SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REFSVM))
              )
              SVL_variables = c(SVL_variables, cologne_vars)
            }
          } else {
            SVL_variables = list(
              list(SVtotalSVMUn, S01C09SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c((numFeat+1):(2*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S03C05SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((2*numFeat)+1):(3*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S03C07SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((3*numFeat)+1):(4*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S05C03SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((4*numFeat)+1):(5*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S05C05SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((5*numFeat)+1):(6*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S05C07SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((6*numFeat)+1):(7*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S07C03SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((7*numFeat)+1):(8*numFeat))], REFSVM)),
              list(SVtotalSVMUn, S09C01SVMUn = cbind(trainDataCurRemainingSVM_Un[SVindexSVMUn,c(((8*numFeat)+1):(9*numFeat))], REFSVM))
            )
          }
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
          accSVM_SL_Un = confusionMatrix(predLabelsSVMsumUn, validateLabels)
          cat("SVM_SL_Un accuracy: ",round(accSVM_SL_Un$overall["Accuracy"],5)," | execution time: ",t.timeSVMUn,"sec\n",sep="")

          AccuracySVM_SL_Un[realization,sample_size] = as.numeric(accSVM_SL_Un$overall["Accuracy"])
          KappaSVM_SL_Un[realization,sample_size] = as.numeric(accSVM_SL_Un$overall["Kappa"])
          SVsSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelSVMUn$finalModel@SVindex))
          if (accSVM_SL_Un$overall["Accuracy"]>best_acc) {
            best_acc <- accSVM_SL_Un$overall["Accuracy"]
            new_bestTunedSVM <- bestFittingModelSVMUn
            new_bestTrainFeatSVM <- best_trainFeatSVMUn
            new_bestTrainLabelsSVM <- best_trainLabelsSVMUn
            best_boundMargin <- best_boundMarginSVMUn
            best_model <- model_name_SVMUn
            best_train.time <- t.timeSVMUn
          }

          cat("\n") ################################# VSVM-SL ################################################
          model_name_VSVM_SL = "VSVM_SL"

          trainStart.time <- Sys.time()
          if (invariance=="scale") {
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
            if (city=="cologne") {
              SVL10 = trainDataCur[SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1),ncol(trainDataCur))]
              SVL11 = trainDataCur[SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1),ncol(trainDataCur))]

              # bind original SV with modified to new train data set
              SVinvar = rbind(setNames(SVinvar,objInfoNames),
                              setNames(SVL10,objInfoNames),
                              setNames(SVL11,objInfoNames)
              ) # The new Train Data Set is made by SVs and VSVs only
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
          cat("evaluation of VSVM with self learning | ",sampleSizePor[sample_size]," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="")
          if (invariance=="scale") {
            SVL_variables = list(
              list(SVtotal, SVL2),
              list(SVtotal, SVL3),
              list(SVtotal, SVL5),
              list(SVtotal, SVL6),
              list(SVtotal, SVL7),
              list(SVtotal, SVL8),
              list(SVtotal, SVL9)
            )
            if (city=="cologne") {
              cologne_vars = list(
                list(SVtotal, SVL10),
                list(SVtotal, SVL11)
              )
              SVL_variables = c(SVL_variables, cologne_vars)
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
          t.timeSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+trainSVM.time, 1)

          # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
          predLabelsVSVMsum = predict(bestFittingModel, validateFeatsub)
          accVSVM_SL = confusionMatrix(predLabelsVSVMsum, validateLabels)
          cat("VSVM_SL accuracy: ",round(accVSVM_SL$overall["Accuracy"],5)," | execution time: ",t.timeSL,"sec\n",sep="")

          AccuracyVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Accuracy"])
          KappaVSVM_SL[realization,sample_size] = as.numeric(accVSVM_SL$overall["Kappa"])
          SVsVSVM_SL[realization,sample_size] = as.numeric(length(bestFittingModel$finalModel@SVindex))
          if (accVSVM_SL$overall["Accuracy"]>best_acc) {
            best_acc <- accVSVM_SL$overall["Accuracy"]
            new_bestTunedVSVM <- bestFittingModel
            new_bestTrainFeatVSVM <- best_trainFeatVSVM
            new_bestTrainLabelsVSVM <- best_trainLabelsVSVM
            best_boundMargin <- best_boundMargin_SL
            best_model <- model_name_VSVM_SL
            best_train.time <- t.timeSL
          }


          cat("\n") ################################# VSVM-SL + semi-labeled samples #####################################
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

            cat("evaluation of VSVM SL with ",b[bb]," semi-labeled samples | ",sampleSizePor[sample_size]," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="") #  [",bb,"/",length(b),"]","
            if (invariance=="scale") { # get VSs, means rows of SV but with subset on different level
              SVL_variables = list(
                list(SVtotal, SVL2),
                list(SVtotal, SVL3),
                list(SVtotal, SVL5),
                list(SVtotal, SVL6),
                list(SVtotal, SVL7),
                list(SVtotal, SVL8),
                list(SVtotal, SVL9),
                list(totalUn, L2Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_b)),
                list(totalUn, L3Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_b)),
                list(totalUn, L5Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_b)),
                list(totalUn, L6Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_b)),
                list(totalUn, L7Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_b)),
                list(totalUn, L8Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_b)),
                list(totalUn, L9Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_b))
              )
              if (city=="cologne") {
                cologne_vars = list(
                  list(SVtotal, SVL10),
                  list(SVtotal, SVL11),
                  list(totalUn, L10Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_b)),
                  list(totalUn, L11Un = cbind(trainDataCurRemaining_SL[indexUn,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_b))
                )
                SVL_variables = c(SVL_variables, cologne_vars)
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
                list(totalUn, S01C09Un = cbind(trainDataCurRemaining_SL[indexUn,c((numFeat+1):(2*numFeat))], REF_b)),
                list(totalUn, S03C05Un = cbind(trainDataCurRemaining_SL[indexUn,c(((2*numFeat)+1):(3*numFeat))], REF_b)),
                list(totalUn, S03C07Un = cbind(trainDataCurRemaining_SL[indexUn,c(((3*numFeat)+1):(4*numFeat))], REF_b)),
                list(totalUn, S05C03Un = cbind(trainDataCurRemaining_SL[indexUn,c(((4*numFeat)+1):(5*numFeat))], REF_b)),
                list(totalUn, S05C05Un = cbind(trainDataCurRemaining_SL[indexUn,c(((5*numFeat)+1):(6*numFeat))], REF_b)),
                list(totalUn, S05C07Un = cbind(trainDataCurRemaining_SL[indexUn,c(((6*numFeat)+1):(7*numFeat))], REF_b)),
                list(totalUn, S07C03Un = cbind(trainDataCurRemaining_SL[indexUn,c(((7*numFeat)+1):(8*numFeat))], REF_b)),
                list(totalUn, S09C01Un = cbind(trainDataCurRemaining_SL[indexUn,c(((8*numFeat)+1):(9*numFeat))], REF_b))
              )
            }
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
            accVSVM_SL_Un = confusionMatrix(predLabelsVSVMsumUn, validateLabels)
            cat("VSVM_SL_Un accuracy: ",round(accVSVM_SL_Un$overall["Accuracy"],5)," | execution time: ",trainUn.time,"sec","\n",sep="")

            AccuracyVSVM_SL_Un[realization,sample_size] = as.numeric(accVSVM_SL_Un$overall["Accuracy"])
            KappaVSVM_SL_Un[realization,sample_size] = as.numeric(accVSVM_SL_Un$overall["Kappa"])
            SVsVSVM_SL_Un[realization,sample_size] = as.numeric(length(bestFittingModelUn$finalModel@SVindex))
            if (accVSVM_SL_Un$overall["Accuracy"] > best_acc) {
              best_acc <- accVSVM_SL_Un$overall["Accuracy"]
              new_bestTunedVSVM <- bestFittingModelUn
              new_bestTrainFeatVSVM <- best_trainFeatVSVMUn
              new_bestTrainLabelsVSVM <- best_trainLabelsVSVMUn
              best_boundMargin <- best_boundMargin_SL_Un
              best_model <- model_name_Un
              best_train.time <- trainUn.time
            }


          cat("\n") ################################# VSVM-SL + virtual semi-labeled samples ##################################
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

            cat("evaluation of VSVM SL with ",b[bb]," virtual semi-labeled samples\n",sep="") #  [",bb,"/",length(b),"]","
            if (invariance=="scale") {
              SVL_variables = list(
                list(SVtotal, SVL2),
                list(SVtotal, SVL3),
                list(SVtotal, SVL5),
                list(SVtotal, SVL6),
                list(SVtotal, SVL7),
                list(SVtotal, SVL8),
                list(SVtotal, SVL9),
                list(SVtotalvUn_v, SVL2vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))], REF_v)),
                list(SVtotalvUn_v, SVL3vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))], REF_v)),
                list(SVtotalvUn_v, SVL5vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))], REF_v)),
                list(SVtotalvUn_v, SVL6vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))], REF_v)),
                list(SVtotalvUn_v, SVL7vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))], REF_v)),
                list(SVtotalvUn_v, SVL8vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))], REF_v)),
                list(SVtotalvUn_v, SVL9vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))], REF_v))
              )
              if (city=="cologne") { # get VSs, means rows of SV but with subset on different level
                cologne_vars = list(
                  list(SVtotal, SVL10),
                  list(SVtotal, SVL11),
                  list(SVtotalvUn_v, SVL10vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat)-1))], REF_v)),
                  list(SVtotalvUn_v, SVL11vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat)-1))], REF_v))
                )
                SVL_variables = c(SVL_variables, cologne_vars)
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
                list(SVtotalvUn_v, S01C09vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c((numFeat+1):(2*numFeat))], REF_v)),
                list(SVtotalvUn_v, S03C05vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((2*numFeat)+1):(3*numFeat))], REF_v)),
                list(SVtotalvUn_v, S03C07vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((3*numFeat)+1):(4*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C03vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((4*numFeat)+1):(5*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C05vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((5*numFeat)+1):(6*numFeat))], REF_v)),
                list(SVtotalvUn_v, S05C07vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((6*numFeat)+1):(7*numFeat))], REF_v)),
                list(SVtotalvUn_v, S07C03vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((7*numFeat)+1):(8*numFeat))], REF_v)),
                list(SVtotalvUn_v, S09C01vUn = cbind(trainDataCurRemaining_SL[indexvUn_v,c(((8*numFeat)+1):(9*numFeat))], REF_v))
              )
            }
            SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin, model_name_vUn, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                   SVL_variables)
            new_bestFittingModelvUn <- SLresult$bestFittingModel
            new_best_trainFeatVSVMvUn <- SLresult$best_trainFeatVSVM
            new_best_trainLabelsVSVMvUn <- SLresult$best_trainLabelsVSVM
            # new_best_bound_SLvUn = SLresult$best_bound
            new_best_boundMargin_SLvUn = SLresult$best_boundMargin
            trainvUn.time <- round(as.numeric((Sys.time() - trainStart.timeUn), units = "secs")+trainSVM.time, 1)
            # predict labels of test data i.e. run classification and accuracy assessment for the best bound setting
            new_predLabelsVSVMvUnsum = predict(new_bestFittingModelvUn, validateFeatsub)
            new_accVSVM_SL_vUn = confusionMatrix(new_predLabelsVSVMvUnsum, validateLabels)

            if (new_accVSVM_SL_vUn$overall["Accuracy"] > actAcc_vUn) {
              actAcc_vUn <- new_accVSVM_SL_vUn$overall["Accuracy"]
              bestFittingModelvUn <- new_bestFittingModelvUn
              accVSVM_SL_vUn <- new_accVSVM_SL_vUn
              best_trainFeatVSVMvUn <- new_best_trainFeatVSVMvUn
              best_trainLabelsVSVMvUn <- new_best_trainLabelsVSVMvUn
              # best_bound_SLvUn = new_best_bound_SLvUn
              best_boundMargin_SLvUn = new_best_boundMargin_SLvUn
              best_train.time <- trainvUn.time
            }
          # }
          cat("VSVM_SL_vUn accuracy: ",round(accVSVM_SL_vUn$overall["Accuracy"],5)," | execution time: ",trainvUn.time,"sec","\n",sep="")

          AccuracyVSVM_SL_vUn[realization,sample_size] = as.numeric(accVSVM_SL_vUn$overall["Accuracy"])
          KappaVSVM_SL_vUn[realization,sample_size] = as.numeric(accVSVM_SL_vUn$overall["Kappa"])
          SVsVSVM_SL_vUn[realization,sample_size] = as.numeric(length(bestFittingModelvUn$finalModel@SVindex))
          if (accVSVM_SL_vUn$overall["Accuracy"] > best_acc){
            best_acc <- accVSVM_SL_vUn$overall["Accuracy"]
            new_bestTunedVSVM <- bestFittingModelvUn
            new_bestTrainFeatVSVM <- best_trainFeatVSVMvUn
            new_bestTrainLabelsVSVM <- best_trainLabelsVSVMvUn
            best_boundMargin <- best_boundMargin_SLvUn
            best_model <- model_name_vUn
          }
          ########################################################################################################
          if (num_cores>=10 && sample_size<length(sampleSizePor)) {
            cat("\n") ############################### Sampling unlabeled data #####################################

            # resampledSize = c(60)    # total number of relabeled samples # 20, 40, 60, 120
            # newSizes = c(8) # = resampledSize[rS]       # number of samples picked per iteration # 4, 5, 10, 20, resampledSize
            # classSize = c(2000) #1200 # number of samples per class # 25, 50, 75, 100, 150, 300, 580 for multiclass #  min(2000,as.numeric(min(table(trainDataCurRemaining$REF)))/3)
            # clusterSizes = c(120) #1200 # number of clusters used to pick samples from different groups # 40, 60, 80, 100, 120, 300

            # get the new size for the active labeling
            newSize = sampleSizePor[sample_size+1]-sampleSizePor[sample_size-1]
            if(sample_size==1){ 

              # Update test set
              sampleSize = round(sampleSizePor[sample_size+1]/nclass) # length(sampleSizePor)  
              shares = c(sampleSize,sampleSize,sampleSize,sampleSize,sampleSize,sampleSize)
              stratSamp = strata(testDataCurBeg, c("REF"), size = shares, method = "srswor")
              samples = getdata(testDataCurBeg, stratSamp)
              testDataCur = samples[,1:ncol(testDataAllLev)]
              # split test feat from test label for later join with trainData
              testFeat = testDataCur[,1:(ncol(testDataCur)-1)]
              testLabels = testDataCur[,ncol(testDataCur)]
              # subset on base level
              testFeatsub = testFeat[sindexSVMDATA:eindexSVMDATA]
                                # trainFeat_AL = setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)])
                                # trainLabels_AL = trainLabels
                                # # distinguish active train set from random train set
                                # trainFeat_rand = setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)])
                                # trainLabels_rand = trainLabels
                                # sampleSize = round(sampleSizePor[sample_size+2]/nclass) # length(sampleSizePor)  
              newSize = sampleSizePor[sample_size+1]-sampleSizePor[sample_size] # +1
            } 
            trainFeat_AL = setNames(trainFeat,objInfoNames[1:(length(objInfoNames)-1)])
            trainLabels_AL = trainLabels
  

            clusterSizes = newSize+1 # c(round(max(classPor/40,newSize+1)))
            
            # classSize=c(round(min(1800,as.numeric(min(table(trainDataCurRemaining$REF))))))  
            # if(city=="cologne"){ 
            #   classSize=c(round(min(1500,as.numeric(min(table(trainDataCurRemaining$REF)))))) } 
            # if(model_prob=="binary"){ 
            #   classSize=c(round(min(4500,as.numeric(min(table(trainDataCurRemaining$REF))))))}

            # if(realization==1){
            # classSize=c(round(min(30000,as.numeric(min(table(trainDataCurRemaining$REF))))))
            classSize=c(25000)            
            clS=1
            cat("sampling ", classSize," unlabeled data\n",sep="")
            samplingStart.time <- Sys.time()
            
            # Definition of sampling configuration (strata:random sampling without replacement)
            stratSampRemaining = strata(trainDataCurRemaining, size = classSize[clS], method = "srswor")
            # Get new samples from trainDataCurRemaining
            samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
            
            sampling.time = round(as.numeric((Sys.time() - samplingStart.time), units = "secs"), 1)
            # }
            
            # # Create an empty dataframe to store unique samples
            # samplesRemaining <- data.frame()
            # # Initialize the iteration counter
            # iteration <- 0
            # trainDataCurRemaining_sampl <- trainDataCurRemaining
            # while (nrow(samplesRemaining) < classSize[clS]*nclass && iteration < 10) {
            #   iteration <- iteration + 1
            #   # Determine how many more samples are needed
            #   remaining_needed <- as.numeric(min(table(trainDataCurRemaining_sampl$REF)))
            #   # remaining_needed <- round((classSize[clS]*nclass - nrow(samplesRemaining))/nclass)
            #   # Sample additional rows
            #   stratSampSize <- c(remaining_needed, remaining_needed, remaining_needed, remaining_needed, remaining_needed, remaining_needed)
            #   stratSampRemaining <- strata(trainDataCurRemaining_sampl[, c(sindexSVMDATA:eindexSVMDATA,ncol(trainDataCurRemaining_sampl))], c("REF"), size = stratSampSize, method = "srswor")
            #   newsamplesRemaining <- getdata(trainDataCurRemaining_sampl, stratSampRemaining)
            #   # Extract relevant columns for uniqueness check
            #   new_samples <- newsamplesRemaining[, 1:(ncol(trainDataCur) + 1)]
            #   
            #   # Remove duplicates based on the specified column range
            #   unique_new_samples <- new_samples[!duplicated(new_samples[, c(sindexSVMDATA:eindexSVMDATA)]), ]
            #   # Add unique rows to the cumulative dataframe
            #   samplesRemaining <- rbind(samplesRemaining, unique_new_samples)
            #   samplesRemaining <- samplesRemaining[!duplicated(samplesRemaining[, c(sindexSVMDATA:eindexSVMDATA)]), ]
            #   samplesRemaining <- samplesRemaining[!duplicated(samplesRemaining$ID_unit), ]
            #   
            #   cat("[iteration ",iteration,"/10] number of samples: ",nrow(samplesRemaining), "\n",sep="")
            #   # If more samples are needed, update trainDataCurRemaining_sampl by excluding the already sampled rows
            #   if (nrow(samplesRemaining) < classSize[clS]*nclass) {
            #     # Remove sampled rows from the remaining pool
            #     trainDataCurRemaining_sampl <- trainDataCurRemaining_sampl[!rownames(trainDataCurRemaining_sampl) %in% rownames(unique_new_samples), ]
            #   }
            # }
            # rm(trainDataCurRemaining_sampl,new_samples,newsamplesRemaining,stratSampRemaining)
            # 
            # # If necessary, trim the dataframe to exactly the desired number of rows
            # if (nrow(samplesRemaining) > classSize[clS]*nclass) {
            #   samplesRemaining <- samplesRemaining[1:(classSize[clS]*nclass), ]
            # }
            # 
            
            # Final check for duplicates
            final_duplicate_count <- sum(duplicated(samplesRemaining[, c(sindexSVMDATA:eindexSVMDATA)]))
            cat("final unlabeled pool size: ",nrow(samplesRemaining)," | duplicates: ", final_duplicate_count," | sampling required ", sampling.time,"sec\n",sep="")
            cat("using currently best model: ",best_model," | accuracy: ",best_acc,"\n",sep="")
            cS=1  
            cat("\n") ############################### Random AL SVM #######################################

            #   model_name_AL_VSVMSL_r = "AL_random_VSVM-SL-vUn"
            # 
            #   cat("random active labeling | ",sampleSizePor[sample_size]," [",(sample_size+1)/2,"/",round(length(sampleSizePor)/2),"]\n",sep="")
            #   # actAcc = -1e-6
            #   # for (nS4it in 1:length(newSizes)) {
            #       # for (rS in 1:length(resampledSize)) {
            #         cat("adding ",newSize," active samples | pool size: ",
            #         # cat("tot samples: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | pool size: ",
            #             nrow(samplesRemaining)," [",clS,"/",length(classSize),"]\n",sep="")
            # 
            #         upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
            # 
            #         tmp_new_tunedSVM_r <- new_bestTunedVSVM
            # 
            #         # newSize_for_iter = newSize #sampleSize/10 # or just 4
            #         # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
            #         trainStart.time <- Sys.time()
            # 
            #         # Extract new RANDOM datasets
            #         # randStratSampRemaining = strata(upd_dataCur, size = newSize, method = "srswor")
            #         randStratSampRemaining = strata(upd_dataCur, size = newSize, method = "srswor")
            # 
            #         # Get new samples from trainDataCurRemaining
            #         random_pick = getdata(upd_dataCur, randStratSampRemaining)
            # 
            #         new_trainFeat <- random_pick[c(sindexSVMDATA:eindexSVMDATA)]
            #         new_trainLabels <- random_pick[,ncol(trainDataCur)]
            #         # **********************
            # 
            #         # **********************
            #         # get original SVs of base SVM
            #         # SVindex_ud = tmp_new_tunedSVM_r$finalModel@SVindex
            # 
            #         # get new rand train set portion
            #         trainFeat_rand <- rbind(trainFeat[,], setNames(new_trainFeat, names))
            #         trainLabels_rand <- unlist(list(trainLabels[], new_trainLabels))
            # 
            #         # SVtotal = setNames(cbind(trainFeat_rand, trainLabels_rand),c(objInfoNames[-length(objInfoNames)],"REF"))
            #         # # **********************
            #         #
            #         # REF_ud = new_trainLabels # real label
            #         # SVtotal_ud = cbind(new_trainFeat, REF_ud)
            #         #
            #         # if (invariance=="scale") {
            #         #   SVL_variables = list(
            #         #     list(SVtotal_ud, SVL2=cbind(random_pick[c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL3=cbind(random_pick[c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL5=cbind(random_pick[c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL6=cbind(random_pick[c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL7=cbind(random_pick[c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL8=cbind(random_pick[c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
            #         #     list(SVtotal_ud, SVL9=cbind(random_pick[c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud))
            #         #   )
            #         #   if (city == "cologne") {
            #         #     cologne_vars = list(
            #         #       list(SVtotal_ud, SVL10=cbind(random_pick[c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))],REF_ud)),
            #         #       list(SVtotal_ud, SVL11=cbind(random_pick[c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))],REF_ud))
            #         #     )
            #         #     SVL_variables = c(SVL_variables, cologne_vars)
            #         #   }
            #         # } else {
            #         #   SVL_variables = list(
            #         #     list(SVtotal_ud, S01C09=cbind(random_pick[c((numFeat+1):(2*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S03C05=cbind(random_pick[c(((2*numFeat)+1):(3*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S03C07=cbind(random_pick[c(((3*numFeat)+1):(4*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S05C03=cbind(random_pick[c(((4*numFeat)+1):(5*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S05C05=cbind(random_pick[c(((5*numFeat)+1):(6*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S05C07=cbind(random_pick[c(((6*numFeat)+1):(7*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S07C03=cbind(random_pick[c(((7*numFeat)+1):(8*numFeat))],REF_ud)),
            #         #     list(SVtotal_ud, S09C01=cbind(random_pick[c(((8*numFeat)+1):(9*numFeat))],REF_ud))
            #         #   )
            #         # } #    =c(0.01, 0.3, 0.9)      =c(1.5, 1, 0.5)
            #         # upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin=c(1.5, 0.5), model_name_AL_VSVMSL_r, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
            #         #                            SVL_variables, tmp_new_tunedSVM_r$finalModel)
            #         # tmp_new_tunedSVM_r2 <- upd_SLresult$bestFittingModel
            #         # # new_trainFeatVSVM <- upd_SLresult$best_trainFeatVSVM
            #         # # new_trainLabelsVSVM <- upd_SLresult$best_trainLabelsVSVM
            #         # # upd_dataCur <- upd_dataCur[!rownames(upd_dataCur) %in% rownames(random_pick), ]
            # 
            # 
            #         # **********************
            #         # trainData index to split between train and test in svmFit
            #         countTrainData = nrow(trainFeat_rand)
            #         indexTrainData = list(c(1:countTrainData))
            # 
            #         # join of train and test test data (separable through indexTrainData in svmFit)
            #         tuneFeat = rbind(setNames(trainFeat_rand,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
            #         tuneLabel = unlist(list(trainLabels_rand, testLabels))
            # 
            #         tmp_new_tunedSVM_r2 = svmFit(tuneFeat, tuneLabel, indexTrainData)
            #         # **********************
            # 
            # 
            #         t.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+t.timeSL+sampling.time, 1)
            #         tmp_pred = predict(tmp_new_tunedSVM_r2, validateFeatsub)
            #         tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
            #         # # if(actAcc < tmp_new_tunedSVM_r$resample$Kappa){ print(paste0("current best kappa: ",round(tmp_new_tunedSVM_r$resample$Kappa,4)))
            #         # if (actAcc < tmp_acc$overall["Accuracy"]) { cat("current best accuracy: ",round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",round(tmp_new_tunedSVM_r2$resample$Kappa,4),"\n",sep="")
            #         tmp_new_tunedSVM_r = tmp_new_tunedSVM_r2
            #         actAcc = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM_r$resample$Kappa #
            #         accVSVM_SL_AL_random = tmp_acc
            #         train.time = t.time
            #         # } else { cat("discarded accuracy: ",round(tmp_acc$overall["Accuracy"],5),"\n",sep="") }
            #     # }
            #   # }
            # # }
            # cat("VSVM_SL - AL random accuracy: ",round(accVSVM_SL_AL_random$overall["Accuracy"],5)," | execution time: ",train.time,"sec\n",sep="")
            # 
            # AccuracyVSVM_SL_Un_random_it[realization,sample_size+1] = as.numeric(accVSVM_SL_AL_random$overall["Accuracy"])
            # KappaVSVM_SL_Un_random_it[realization,sample_size+1] = as.numeric(accVSVM_SL_AL_random$overall["Kappa"])

            cat("\n") ############################### AL MCLU + t-SNE&Class SVM #######################################
            model_name_AL_VSVMSL ="AL_MCLU+kmeans_SVM"

            cat("active labeling ",model_name_AL_VSVMSL," | ",length(trainLabels_AL)," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="")
            # actAcc = -1e-6
            # classSize=c(min(classPor,round(as.numeric(min(table(trainDataCurRemaining$REF)))/1)))
            # if (model_prob=="multiclass") { if (city=="hagadera"){classSize=round(classSize/2.5)} else {classSize=round(classSize/3)}}
            # for (clS in 1:length(classSize)) {
            #   stratSampSize = c(classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS])
            #   # Definition of sampling configuration (strata:random sampling without replacement)
            #   stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
            #   # Get new samples from trainDataCurRemaining
            #   samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
            #   # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining$ID_unit), ]
              # for (nS4it in 1:length(newSizes)) {
                #for (cS in 1:length(clusterSizes)) {
                  # for (rS in 1:length(resampledSize)) {
                    # cat("tot samples: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | per iter: ",newSize," [",nS4it,"/",length(newSizes),"] | pool size: ",
                    cat("adding ",newSize," active samples | pool size: ",
                        nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")

                    upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
                    upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
                    upd_dataCurLabels  <- upd_dataCur[,ncol(trainDataCur)]

                    new_trainFeatVSVM <- setNames(trainFeat_AL, names)
                    new_trainLabelsVSVM <- trainLabels_AL

                    tmp_new_tunedSVM <- new_bestTunedVSVM

                    newSize_for_iter = newSize #sampleSize/10 # or just 4
                    # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100

                    trainStart.time <- Sys.time()

                      predLabelsVSVM = predict(tmp_new_tunedSVM, upd_dataCurFeatsub)
                      # Add predicted labels to the features data set
                      predLabelsVSVM_unc = cbind(upd_dataCurFeatsub, predLabelsVSVM)
                      predLabelsVSVM_unc = setNames(predLabelsVSVM_unc, objInfoNames)

                      if (model_prob=="binary") { sampled_data <- margin_sampling(tmp_new_tunedSVM, predLabelsVSVM_unc, pred_one, binaryClassProblem, plot_flag = model_name_AL_VSVMSL)
                      } else {                    sampled_data <- mclu_sampling(  tmp_new_tunedSVM, predLabelsVSVM_unc, pred_all, binaryClassProblem, plot_flag = model_name_AL_VSVMSL) }
                      cat("computing distances required ", round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1),"sec\n",sep="")
                      ALSamplesStart.time <- Sys.time()
                      result <- add_AL_samples(sampled_data,
                                               upd_dataCurFeatsub, upd_dataCurLabels,
                                               new_trainFeatVSVM, new_trainLabelsVSVM,
                                               newSize_for_iter, cluster=round(min(clusterSizes[cS],nrow(sampled_data)/20)), # always greater than newSize_for_iter, # 60, 80, 100, 120
                                               upd_dataCur$ID_unit, tSNE_flag = FALSE, flag_cluster = TRUE)
                      cat("getting active-labeled samples and updated datasets required ", round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1),"sec\n",sep="")
                      # Extract new datasets
                      # upd_dataCurFeatsub <- result$features
                      # upd_dataCurLabels <- result$labels
                      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit

                      new_trainFeat <- result$new_trainFeatVSVM
                      new_trainLabels <- result$new_trainLabelsVSVM
                      semiAL_tot <- result$semi_samples
                      semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit

                      # **********************
                      # get original SVs of base SVM
                      # SVindex_ud = tmp_new_tunedSVM$finalModel@SVindex # SVs OF THIS MODEL ARE NOT IN new_trainFeatVSVM

                      # get new al train set portion
                      new_trainFeatVSVM <- rbind(new_trainFeatVSVM[,], setNames(new_trainFeat, names))
                      new_trainLabelsVSVM <- unlist(list(new_trainLabelsVSVM[], new_trainLabels))

                      # SVtotal = setNames(cbind(new_trainFeatVSVM, new_trainLabelsVSVM),c(objInfoNames[-length(objInfoNames)],"REF"))
                      # # **********************
                      # 
                      # REF_ud = new_trainLabels
                      # SVtotal_ud = cbind(new_trainFeat, REF_ud)
                      # 
                      # if (invariance=="scale") {
                      #   SVL_variables = list(
                      #     list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
                      #     list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
                      #     list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
                      #     list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
                      #     list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
                      #     list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
                      #     list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud))#,
                      #     # list(semiAL_tot, sAL2=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL3=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL5=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL6=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL7=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL8=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, sAL9=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],semiAL_tot[,length(semiAL_tot)]))
                      # 
                      #   )
                      #   if (city == "cologne") {
                      #     cologne_vars = list(
                      #       list(SVtotal_ud, SVL10 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], REF_ud)),
                      #       list(SVtotal_ud, SVL11 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], REF_ud))#,
                      #       # list(semiAL_tot, sAL10 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)])),
                      #       # list(semiAL_tot, sAL11 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)]))
                      #     )
                      #     SVL_variables = c(SVL_variables, cologne_vars)
                      #   }
                      # } else {
                      #   SVL_variables = list(
                      #     list(SVtotal_ud, S01C09=cbind(upd_dataCur[upd_SVindex_ud,c((numFeat+1):(2*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S03C05=cbind(upd_dataCur[upd_SVindex_ud,c(((2*numFeat)+1):(3*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S03C07=cbind(upd_dataCur[upd_SVindex_ud,c(((3*numFeat)+1):(4*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S05C03=cbind(upd_dataCur[upd_SVindex_ud,c(((4*numFeat)+1):(5*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S05C05=cbind(upd_dataCur[upd_SVindex_ud,c(((5*numFeat)+1):(6*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S05C07=cbind(upd_dataCur[upd_SVindex_ud,c(((6*numFeat)+1):(7*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S07C03=cbind(upd_dataCur[upd_SVindex_ud,c(((7*numFeat)+1):(8*numFeat))],REF_ud)),
                      #     list(SVtotal_ud, S09C01=cbind(upd_dataCur[upd_SVindex_ud,c(((8*numFeat)+1):(9*numFeat))],REF_ud))#,
                      #     # list(semiAL_tot, s01C09=cbind(upd_dataCur[semiAL_SVindex,c((numFeat+1):(2*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s03C05=cbind(upd_dataCur[semiAL_SVindex,c(((2*numFeat)+1):(3*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s03C07=cbind(upd_dataCur[semiAL_SVindex,c(((3*numFeat)+1):(4*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s05C03=cbind(upd_dataCur[semiAL_SVindex,c(((4*numFeat)+1):(5*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s05C05=cbind(upd_dataCur[semiAL_SVindex,c(((5*numFeat)+1):(6*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s05C07=cbind(upd_dataCur[semiAL_SVindex,c(((6*numFeat)+1):(7*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s07C03=cbind(upd_dataCur[semiAL_SVindex,c(((7*numFeat)+1):(8*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                      #     # list(semiAL_tot, s09C01=cbind(upd_dataCur[semiAL_SVindex,c(((8*numFeat)+1):(9*numFeat))],semiAL_tot[,length(semiAL_tot)]))
                      #   )
                      # } #    =c(0.01, 0.3, 0.9)      =c(1.5, 1, 0.5)
                      # upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin=c(1), model_name_AL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                      #                            SVL_variables, tmp_new_tunedSVM$finalModel)
                      # tmp_new_tunedSVM2 <- upd_SLresult$bestFittingModel
                      # tmp_pred = predict(tmp_new_tunedSVM2, validateFeatsub)
                      # tmp_acc_sl  = confusionMatrix(tmp_pred, validateLabels)
                      
                      # **********************
                      # trainData index to split between train and test in svmFit
                      countTrainData = nrow(new_trainFeatVSVM)
                      indexTrainData = list(c(1:countTrainData))

                      # join of train and test test data (separable through indexTrainData in svmFit)
                      tuneFeat = rbind(setNames(new_trainFeatVSVM,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
                      tuneLabel = unlist(list(new_trainLabelsVSVM, testLabels))

                      tmp_new_tunedSVM2 = svmFit(tuneFeat, tuneLabel, indexTrainData)
                      train.timeALv1_tSNE_VSVMSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time, 1)
                      
                      tmp_pred = predict(tmp_new_tunedSVM2, validateFeatsub)
                      tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
                      # cat(model_name_AL_VSVMSL," accuracy: ",round(tmp_acc$overall["Accuracy"],5),"\n",sep="")
                      
                      # **********************
                      # upd_dataCur <- upd_dataCur[!upd_SVindex_ud, ]

                      # if(actAcc < tmp_new_tunedSVM$resample$Kappa){ print(paste0("current best kappa: ",round(tmp_new_tunedSVM$resample$Kappa,4)))
                      # if (actAcc < tmp_acc$overall["Accuracy"]) {  # cat("current best accuracy: ",sep="")
                      tmp_new_tunedSVM = tmp_new_tunedSVM2
                      accVSVM_SL_itAL = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
                      # best_resample = resampledSize[rS]
                      # best_newSize4iter = newSize_for_iter
                      # best_classSize = classSize[clS]
                      # best_cluster = clusterSizes[cS]
                      # train.timeALv1_tSNE_VSVMSL = t.time
                      # } # else { cat("discarded accuracy: ",sep="")}
                      # cat(round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",round(tmp_new_tunedSVM2$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="")
                  # }
                # }
              # }
            # }
            cat(model_name_AL_VSVMSL," accuracy: ",accVSVM_SL_itAL," | execution time: ",train.timeALv1_tSNE_VSVMSL,"sec\n",sep="")

            AccuracyVSVM_SL_Un_it[realization,sample_size+1] = as.numeric(tmp_acc$overall["Accuracy"])
            KappaVSVM_SL_Un_it[realization,sample_size+1] = as.numeric(tmp_acc$overall["Kappa"])
            SVsVSVM_SL_Un_it[realization,sample_size+1] = as.numeric(length(tmp_new_tunedSVM2$finalModel@SVindex))
            
            cat("\n") ############################### AL MS + t-SNE&Class + SL SVM #######################################
            model_name_ALSL_VSVMSL = "AL_MS+tSNE+SL_SVM"
            model_name_ALSL_VSVMSL2 = "AL_MS+tSNE_SVM"
            
            cat("active labeling ",model_name_ALSL_VSVMSL," | ",length(trainLabels_AL)," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="")
            # actAcc = -1e-6
            # classSize=c(min(classPor,round(as.numeric(min(table(trainDataCurRemaining$REF)))/1)))
            # if (model_prob=="multiclass") { if (city=="hagadera"){classSize=round(classSize/2.5)} else {classSize=round(classSize/3)}}
            # for (clS in 1:length(classSize)) {
            #   stratSampSize = c(classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS])
            #   # Definition of sampling configuration (strata:random sampling without replacement)
            #   stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
            #   # Get new samples from trainDataCurRemaining
            #   samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
            #   # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining$ID_unit), ]
              # for (nS4it in 1:length(newSizes)) {
                # for (cS in 1:length(clusterSizes)) {
                  # for (rS in 1:length(resampledSize)) {
                    # cat("tot samples: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | per iter: ",newSize," [",nS4it,"/",length(newSizes),"] | pool size: ",
                    cat("adding ",newSize," active samples | pool size: ",
                        nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")

                    upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
                    upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
                    upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]

                    new_trainFeatVSVM <- setNames(trainFeat_AL, names)
                    new_trainLabelsVSVM <- trainLabels_AL

                    tmp_new_tunedSVM_SL <- new_bestTunedVSVM

                    newSize_for_iter = newSize #sampleSize/10 # or just 4
                    # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100

                    trainStart.time <- Sys.time()

                      # # **********************
                      # # **********************

                      # Add predicted labels to the features data set
                      SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(tmp_new_tunedSVM_SL, upd_dataCurFeatsub)), objInfoNames)

                      sampledResult <- self_learn_AL(
                        # testFeatsub, testLabels, boundMargin=c(best_boundMargin),
                        SVtotal, objInfoNames, SVL_variables,
                        # validateFeatsub,validateLabels,
                        upd_dataCurFeatsub,upd_dataCurLabels,
                        # realiz=1, s_size=3, 
                        plot_flag = model_name_ALSL_VSVMSL
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
                                               new_trainFeatVSVM, new_trainLabelsVSVM,
                                               newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
                                               upd_dataCur$ID_unit,tSNE_flag = TRUE, flag_cluster = TRUE, plot_flag  = model_name_ALSL_VSVMSL)
                      ALS.time <- round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1)
                      cat("getting active-labeled samples and updated datasets required ", ALS.time,"sec\n",sep="")
                      # Extract new datasets
                      # upd_dataCurFeatsub <- result$features
                      # upd_dataCurLabels <- result$labels
                      upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit

                      new_trainFeat <- result$new_trainFeatVSVM
                      new_trainLabels <- result$new_trainLabelsVSVM
                      semiAL_tot <- result$semi_samples
                      semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit

                      # **********************
                      # get al train set portion
                      new_trainFeatVSVM <- rbind(new_trainFeatVSVM[,], setNames(new_trainFeat, names))
                      new_trainLabelsVSVM <- unlist(list(new_trainLabelsVSVM[], new_trainLabels))
                      
                      SVtotal = setNames(cbind(new_trainFeatVSVM, new_trainLabelsVSVM),c(objInfoNames[-length(objInfoNames)],"REF"))
                      # **********************

                      REF_ud = new_trainLabels
                      SVtotal_ud = cbind(new_trainFeat, REF_ud)

                      if (invariance=="scale") {
                        SVL_variables = list(
                          list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
                          list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
                          list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
                          list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
                          list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
                          list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
                          list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud))#,
                          # list(semiAL_tot, sAL2=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL3=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL5=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL6=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL7=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL8=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, sAL9=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],semiAL_tot[,length(semiAL_tot)]))

                        )
                        if (city == "cologne") {
                          cologne_vars = list(
                            list(SVtotal_ud, SVL10 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], REF_ud)),
                            list(SVtotal_ud, SVL11 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], REF_ud))#,
                            # list(semiAL_tot, sAL10 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)])),
                            # list(semiAL_tot, sAL11 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)]))
                          )
                          SVL_variables = c(SVL_variables, cologne_vars)
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
                          list(SVtotal_ud, S09C01=cbind(upd_dataCur[upd_SVindex_ud,c(((8*numFeat)+1):(9*numFeat))],REF_ud))#,
                          # list(semiAL_tot, s01C09=cbind(upd_dataCur[semiAL_SVindex,c((numFeat+1):(2*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s03C05=cbind(upd_dataCur[semiAL_SVindex,c(((2*numFeat)+1):(3*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s03C07=cbind(upd_dataCur[semiAL_SVindex,c(((3*numFeat)+1):(4*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s05C03=cbind(upd_dataCur[semiAL_SVindex,c(((4*numFeat)+1):(5*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s05C05=cbind(upd_dataCur[semiAL_SVindex,c(((5*numFeat)+1):(6*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s05C07=cbind(upd_dataCur[semiAL_SVindex,c(((6*numFeat)+1):(7*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s07C03=cbind(upd_dataCur[semiAL_SVindex,c(((7*numFeat)+1):(8*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                          # list(semiAL_tot, s09C01=cbind(upd_dataCur[semiAL_SVindex,c(((8*numFeat)+1):(9*numFeat))],semiAL_tot[,length(semiAL_tot)]))
                        )
                      } #    =c(0.01, 0.3, 0.9)      =c(1.5, 1, 0.5)
                      upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin=c(1), model_name_ALSL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                                 SVL_variables, tmp_new_tunedSVM_SL$finalModel)
                      tmp_new_tunedSVM2 <- upd_SLresult$bestFittingModel
                      train.timeALv2_tSNE_VSVMSL <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time, 1)
                      # upd_dataCur <- upd_dataCur[!upd_SVindex_ud, ]
                      tmp_pred = predict(tmp_new_tunedSVM2, validateFeatsub)
                      tmp_acc_sl  = confusionMatrix(tmp_pred, validateLabels)
                      
                      # **********************
                      # trainData index to split between train and test in svmFit
                      countTrainData = nrow(new_trainFeatVSVM)
                      indexTrainData = list(c(1:countTrainData))
                      
                      # join of train and test test data (separable through indexTrainData in svmFit)
                      tuneFeat = rbind(setNames(new_trainFeatVSVM,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
                      tuneLabel = unlist(list(new_trainLabelsVSVM, testLabels))
                      
                      tmp_new_tunedSVM_SL2 = svmFit(tuneFeat, tuneLabel, indexTrainData)
                      tmp_pred = predict(tmp_new_tunedSVM_SL2, validateFeatsub)
                      tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
                      cat(model_name_ALSL_VSVMSL2," accuracy: ",round(tmp_acc$overall["Accuracy"],5),"\n",sep="")
                      # **********************


                      # if (actAcc < tmp_acc$overall["Accuracy"]) { cat("current best accuracy: ",sep="")
                        tmp_new_tunedSVM_SL = tmp_new_tunedSVM2
                        # actAcc = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
                        accVSVM_ALv2SL_tSNE2 = (tmp_acc$overall["Accuracy"])
                        accVSVM_ALv2SL_tSNE = (tmp_acc_sl$overall["Accuracy"])
                        
                        # best_resample = resampledSize[rS]
                        best_newSize4iter = newSize_for_iter
                        best_classSize = classSize[clS]
                        best_cluster = clusterSizes[cS]
                      # } # else { cat("discarded accuracy: ",sep="")}
                      # cat(round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",round(tmp_new_tunedSVM2$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="")
                  # }
                # }
              # }
            # }
            cat(model_name_ALSL_VSVMSL," accuracy: ",round(tmp_acc_sl$overall["Accuracy"],5)," | execution time: ",train.timeALv2_tSNE_VSVMSL,"sec\n",sep="")

            AccuracyVSVM_SL_Un_itSL[realization,sample_size+1] = as.numeric((tmp_acc$overall["Accuracy"]))
            KappaVSVM_SL_Un_itSL[realization,sample_size+1] = as.numeric((tmp_acc$overall["Kappa"]))
            SVsVSVM_SL_Un_itSL[realization,sample_size+1] = as.numeric(length(tmp_new_tunedSVM_SL2$finalModel@SVindex))
            
            AccuracyVSVM_SL_Un_itSL2[realization,sample_size+1] = as.numeric((tmp_acc_sl$overall["Accuracy"]))
            KappaVSVM_SL_Un_itSL2[realization,sample_size+1] = as.numeric((tmp_acc_sl$overall["Kappa"]))
            SVsVSVM_SL_Un_itSL2[realization,sample_size+1] = as.numeric(length(tmp_new_tunedSVM_SL$finalModel@SVindex))
            
            cat("\n") ############################### AL MS + semi-SL + Train SVM #######################################
            model_name_ALTrainSL_VSVMSL = "AL_MS+kmeans+semiSL_SVM"
            model_name_ALTrainSL_VSVMSL2 = "AL_MS+kmeans+Train_SVM"
            
            cat("active labeling ",model_name_ALTrainSL_VSVMSL," | ",length(trainLabels_AL)," [",(sample_size+1)/2,"/",length(sampleSizePor)/2,"]\n",sep="")
            # actAcc = -1e-6
            # classSize=c(min(classPor,round(as.numeric(min(table(trainDataCurRemaining$REF)))/1)))
            # if (model_prob=="multiclass") { if (city=="hagadera"){classSize=round(classSize/2.5)} else {classSize=round(classSize/3)}}
            # for (clS in 1:length(classSize)) {
            #   stratSampSize = c(classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS],classSize[clS])
            #   # Definition of sampling configuration (strata:random sampling without replacement)
            #   stratSampRemaining = strata(trainDataCurRemaining, c("REF"), size = stratSampSize, method = "srswor")
            #   # Get new samples from trainDataCurRemaining
            #   samplesRemaining = getdata(trainDataCurRemaining, stratSampRemaining)
            #   # trainDataCurRemaining <- trainDataCurRemaining[-c(samplesRemaining$ID_unit), ]
            # for (nS4it in 1:length(newSizes)) {
            # for (cS in 1:length(clusterSizes)) {
            # for (rS in 1:length(resampledSize)) {
            # cat("tot samples: ",resampledSize[rS]," [",rS,"/",length(resampledSize),"] | per iter: ",newSize," [",nS4it,"/",length(newSizes),"] | pool size: ",
            cat("adding ",newSize," active samples | pool size: ",
                nrow(samplesRemaining)," [",clS,"/",length(classSize),"] | clusters: ",clusterSizes[cS]," [",cS,"/",length(clusterSizes),"]\n",sep="")
            
            trainStart.time <- Sys.time()

            new_trainFeatVSVM <- setNames(trainFeat_AL, names)
            new_trainLabelsVSVM <- trainLabels_AL
            
            # upd_dataCur <- samplesRemaining[,1:(ncol(trainDataCur)+1)]
            # upd_dataCurFeatsub <- upd_dataCur[,c(sindexSVMDATA:eindexSVMDATA)]
            # upd_dataCurLabels <- upd_dataCur[,ncol(trainDataCur)]
            # 
            # tmp_new_tunedSVM_ALT <- new_bestTunedVSVM
            # 
            # newSize_for_iter = newSize #sampleSize/10 # or just 4
            # # num_iters = round(resampledSize[rS]/newSize_for_iter) # 1, 3, 5, 10, 16, 24, 50, 100
            # 
            # 
            # # # **********************
            # # # **********************
            # 
            # # Add predicted labels to the features data set
            # SVL_variables<-setNames(cbind(upd_dataCurFeatsub, predict(tmp_new_tunedSVM_ALT, upd_dataCurFeatsub)), objInfoNames)
            # 
            # sampledResult <- self_learn_AL(
            #   # testFeatsub, testLabels, boundMargin=c(best_boundMargin),
            #   SVtotal, objInfoNames, SVL_variables,
            #   # validateFeatsub,validateLabels,
            #   upd_dataCurFeatsub,upd_dataCurLabels,
            #   # realiz=1,s_size=2, 
            #   # plot_flag = model_name_ALTrainSL_VSVMSL #
            # )
            # sampled_data <- sampledResult$sampled_data
            # reference_label <- sampledResult$best_updCur_Labels
            # 
            # # # **********************
            # # # **********************
            # 
            # d.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs"), 1)
            # cat("computing distances required ", d.time,"sec\n",sep="")
            ALSamplesStart.time <- Sys.time()
            result <- add_AL_samples(sampled_data,
                                     sampled_data[,1:numFeat], reference_label,
                                     new_trainFeatVSVM, new_trainLabelsVSVM,
                                     newSize_for_iter, clusterSizes[cS], # always greater than newSize_for_iter, # 60, 80, 100, 120
                                     upd_dataCur$ID_unit, flag_cluster = TRUE, newSize2=b[bb]*nclass) #,
            ALS.time <- round(as.numeric((Sys.time() - ALSamplesStart.time), units = "secs"), 1)
            cat("getting active-labeled samples and updated datasets required ", ALS.time,"sec\n",sep="")
            # Extract new datasets
            # upd_dataCurFeatsub <- result$features
            # upd_dataCurLabels <- result$labels
            upd_SVindex_ud = upd_dataCur$ID_unit %in% result$IDunit

            new_trainFeat <- result$new_trainFeatVSVM
            new_trainLabels <- result$new_trainLabelsVSVM
            semiAL_tot <- result$semi_samples
            semiAL_SVindex <- upd_dataCur$ID_unit %in% result$semiIDunit

            # **********************
            # get al train set portion
            new_trainFeatVSVM <- rbind(new_trainFeatVSVM[,], setNames(new_trainFeat, names))
            new_trainLabelsVSVM <- unlist(list(new_trainLabelsVSVM[], new_trainLabels))

            SVtotal = setNames(cbind(new_trainFeatVSVM, new_trainLabelsVSVM),c(objInfoNames[-length(objInfoNames)],"REF"))
            # **********************

            REF_ud = new_trainLabels
            SVtotal_ud = cbind(new_trainFeat, REF_ud)

            if (invariance=="scale") {
              SVL_variables = list(
                list(SVtotal_ud, SVL2=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],REF_ud)),
                list(SVtotal_ud, SVL3=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],REF_ud)),
                list(SVtotal_ud, SVL5=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],REF_ud)),
                list(SVtotal_ud, SVL6=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],REF_ud)),
                list(SVtotal_ud, SVL7=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],REF_ud)),
                list(SVtotal_ud, SVL8=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],REF_ud)),
                list(SVtotal_ud, SVL9=cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],REF_ud)),
                list(semiAL_tot, sAL2=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - 2*numFeat):(sindexSVMDATA - numFeat - 1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL3=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA - numFeat):(sindexSVMDATA -1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL5=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + numFeat):((sindexSVMDATA + 2*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL6=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 2*numFeat):((sindexSVMDATA + 3*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL7=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 3*numFeat):((sindexSVMDATA + 4*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL8=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 4*numFeat):((sindexSVMDATA + 5*numFeat)-1))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, sAL9=cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 5*numFeat):((sindexSVMDATA + 6*numFeat)-1))],semiAL_tot[,length(semiAL_tot)]))

              )
              if (city == "cologne") {
                cologne_vars = list(
                  list(SVtotal_ud, SVL10 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], REF_ud)),
                  list(SVtotal_ud, SVL11 = cbind(upd_dataCur[upd_SVindex_ud,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], REF_ud)),
                  list(semiAL_tot, sAL10 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 6*numFeat):((sindexSVMDATA + 7*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)])),
                  list(semiAL_tot, sAL11 = cbind(upd_dataCur[semiAL_SVindex,c((sindexSVMDATA + 7*numFeat):((sindexSVMDATA + 8*numFeat) - 1))], semiAL_tot[,length(semiAL_tot)]))
                )
                SVL_variables = c(SVL_variables, cologne_vars)
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
                list(SVtotal_ud, S09C01=cbind(upd_dataCur[upd_SVindex_ud,c(((8*numFeat)+1):(9*numFeat))],REF_ud)),
                list(semiAL_tot, s01C09=cbind(upd_dataCur[semiAL_SVindex,c((numFeat+1):(2*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s03C05=cbind(upd_dataCur[semiAL_SVindex,c(((2*numFeat)+1):(3*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s03C07=cbind(upd_dataCur[semiAL_SVindex,c(((3*numFeat)+1):(4*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s05C03=cbind(upd_dataCur[semiAL_SVindex,c(((4*numFeat)+1):(5*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s05C05=cbind(upd_dataCur[semiAL_SVindex,c(((5*numFeat)+1):(6*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s05C07=cbind(upd_dataCur[semiAL_SVindex,c(((6*numFeat)+1):(7*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s07C03=cbind(upd_dataCur[semiAL_SVindex,c(((7*numFeat)+1):(8*numFeat))],semiAL_tot[,length(semiAL_tot)])),
                list(semiAL_tot, s09C01=cbind(upd_dataCur[semiAL_SVindex,c(((8*numFeat)+1):(9*numFeat))],semiAL_tot[,length(semiAL_tot)]))
              )
            } #    =c(0.01, 0.3, 0.9)      =c(1.5, 1, 0.5)
            upd_SLresult <- self_learn(testFeatsub, testLabels, bound, boundMargin=c(1), model_name_ALTrainSL_VSVMSL, SVtotal, objInfoNames,rem_extrem,rem_extrem_kerneldist, #classProb=TRUE,
                                       SVL_variables, tmp_new_tunedSVM_SL$finalModel)
            tmp_new_tunedSVM_ALSL2 <- upd_SLresult$bestFittingModel
            tmp_pred = predict(tmp_new_tunedSVM_ALSL2, validateFeatsub)
            tmp_acc_sl  = confusionMatrix(tmp_pred, validateLabels)
            t.time <- round(as.numeric((Sys.time() - trainStart.time), units = "secs")+best_train.time+sampling.time+d.time, 1)

            # **********************
            # trainData index to split between train and test in svmFit
            countTrainData = nrow(new_trainFeatVSVM)
            indexTrainData = list(c(1:countTrainData))

            # join of train and test test data (separable through indexTrainData in svmFit)
            tuneFeat = rbind(setNames(new_trainFeatVSVM,objInfoNames[1:(length(objInfoNames)-1)]), setNames(testFeatsub,objInfoNames[1:(length(objInfoNames)-1)]))
            tuneLabel = unlist(list(new_trainLabelsVSVM, testLabels))

            tmp_new_tunedSVM_ALT2 = svmFit(tuneFeat, tuneLabel, indexTrainData)
            tmp_pred = predict(tmp_new_tunedSVM_ALT2, validateFeatsub)
            tmp_acc  = confusionMatrix(tmp_pred, validateLabels)
            cat(model_name_ALTrainSL_VSVMSL2," accuracy: ",round(tmp_acc$overall["Accuracy"],5),"\n",sep="")
            # **********************

            # if(actAcc < tmp_new_tunedSVM_SL2$resample$Kappa){ print(paste0("current best kappa: ",round(tmp_new_tunedSVM_SL2$resample$Kappa,4)))
            # if (actAcc < tmp_acc$overall["Accuracy"]) { #cat("current best accuracy: ",sep="")
            tmp_new_tunedSVM_ALT = tmp_new_tunedSVM_ALSL2
            # actAcc = tmp_acc$overall["Accuracy"] # tmp_new_tunedSVM$resample$Kappa #
            accVSVM_ALv2_TSL2 = tmp_acc$overall["Accuracy"]
            accVSVM_ALv2_TSL = tmp_acc_sl$overall["Accuracy"]
            # best_resample = resampledSize[rS]
            # best_newSize4iter = newSize_for_iter
            # best_classSize = classSize[clS]
            # best_cluster = clusterSizes[cS]
            train.timeALv2_SEMI_VSVMSL = t.time
            # } #else { cat("discarded accuracy: ",sep="")}
            #cat(round(tmp_acc$overall["Accuracy"],5)," | related kappa: ",round(tmp_new_tunedSVM_SL2$resample$Kappa,4)," | execution time: ",t.time,"sec\n",sep="")
            # }
            # }
            # }
            # }
            cat(model_name_ALTrainSL_VSVMSL," accuracy: ",round(tmp_acc_sl$overall["Accuracy"],5)," | execution time: ",train.timeALv2_SEMI_VSVMSL,"sec\n",sep="")
            
            AccuracyVSVM_SL_Un_itTSL[realization,sample_size+1] = as.numeric((tmp_acc$overall["Accuracy"]))
            KappaVSVM_SL_Un_itTSL[realization,sample_size+1] = as.numeric((tmp_acc$overall["Kappa"]))
            SVsVSVM_SL_Un_itTSL[realization,sample_size+1] = as.numeric(length(tmp_new_tunedSVM_ALT2$finalModel@SVindex))
            
            AccuracyVSVM_SL_Un_itTSL2[realization,sample_size+1] = as.numeric((tmp_acc_sl$overall["Accuracy"]))
            KappaVSVM_SL_Un_itTSL2[realization,sample_size+1] = as.numeric((tmp_acc_sl$overall["Kappa"]))
            SVsVSVM_SL_Un_itTSL2[realization,sample_size+1] = as.numeric(length(tmp_new_tunedSVM_ALT$finalModel@SVindex))
            
            # *********************************************************************
            # get original SVs of base SVM
            SVindex_ud = tmp_new_tunedSVM_ALT2$finalModel@SVindex

            # get new al train set portion
            trainFeat_AL <- new_trainFeatVSVM[SVindex_ud,]
            trainLabels_AL <- new_trainLabelsVSVM[SVindex_ud]

            trainDataCur <- rbind(trainDataCur, upd_dataCur[upd_SVindex_ud, 1:ncol(trainDataCur)])
            
            trainDataCurBeg <- trainDataCurRemaining[!upd_SVindex_ud, ]
            # samplesRemaining <- samplesRemaining[!upd_SVindex_ud, ]
            
            if(accVSVM_SL_itAL>best_acc){
              best_acc <- accVSVM_SL_itAL
              best_model <- model_name_AL_VSVMSL
              new_bestTunedVSVM <- tmp_new_tunedSVM
              best_train.time <- train.timeALv1_tSNE_VSVMSL-best_train.time
            }
            
            if(accVSVM_ALv2SL_tSNE2>best_acc){ 
              best_acc <- accVSVM_ALv2SL_tSNE2
              best_model <- model_name_ALSL_VSVMSL2
              new_bestTunedVSVM <- tmp_new_tunedSVM_SL2
              best_train.time <- train.timeALv2_tSNE_VSVMSL-best_train.time
            }
            
            if(accVSVM_ALv2SL_tSNE>best_acc){ 
              best_acc <- accVSVM_ALv2SL_tSNE
              best_model <- model_name_ALSL_VSVMSL
              new_bestTunedVSVM <- tmp_new_tunedSVM_SL
              best_train.time <- train.timeALv2_tSNE_VSVMSL-best_train.time
            }
            
            if(accVSVM_ALv2_TSL2>best_acc){
              best_acc <- accVSVM_ALv2_TSL2
              best_model <- model_name_ALTrainSL_VSVMSL2
              new_bestTunedVSVM <- tmp_new_tunedSVM_ALT2
              best_train.time <- train.timeALv2_SEMI_VSVMSL-best_train.time
            }
            
            if(accVSVM_ALv2_TSL>best_acc){
              best_acc <- accVSVM_ALv2_TSL
              best_model <- model_name_ALTrainSL_VSVMSL
              new_bestTunedVSVM <- tmp_new_tunedSVM_ALT
              best_train.time <- train.timeALv2_SEMI_VSVMSL-best_train.time
            }
            # *********************************************************************
            # upd_dataCur <- upd_dataCur[!upd_SVindex_ud, ]

          }
          cat("\n") ################################# End sample portion #########################################
          if (realization==1 && sample_size==5) {
            saveRDS(tunedSVM, paste0(format(Sys.time(),"%Y%m%d"),model_name_tunedSVM,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(bestFittingModelSVMUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_SVMUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(bestFittingModel, paste0(format(Sys.time(),"%Y%m%d"),model_name_VSVM_SL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(bestFittingModelUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_Un,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(bestFittingModelvUn, paste0(format(Sys.time(),"%Y%m%d"),model_name_vUn,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(tmp_new_tunedSVM2, paste0(format(Sys.time(),"%Y%m%d"),model_name_AL_VSVMSL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(tmp_new_tunedSVM_SL2, paste0(format(Sys.time(),"%Y%m%d"),model_name_ALSL_VSVMSL2,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(tmp_new_tunedSVM_SL, paste0(format(Sys.time(),"%Y%m%d"),model_name_ALSL_VSVMSL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(tmp_new_tunedSVM_ALT2, paste0(format(Sys.time(),"%Y%m%d"),model_name_ALTrainSL_VSVMSL2,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
            saveRDS(tmp_new_tunedSVM_ALT, paste0(format(Sys.time(),"%Y%m%d"),model_name_ALTrainSL_VSVMSL,"_",city,"_",model_prob,"_",invariance,"_",script,"_",sampleSizePor[sample_size],"sampleSizePor_",b,"Unl_",seed,"seed.rds"))
          }
        }
        cat("\n") ################################### End realization ############################################
        # Store hyperparameters 
        best_model_oa=c(best_model_oa,best_model,": ",as.numeric(best_acc),"\n")
        trainSVMUn.time_oa = trainSVMUn.time_oa+t.timeSVMUn
        trainSL.time_oa = trainSL.time_oa+t.timeSL
        trainUn.time_oa = trainUn.time_oa+trainUn.time
        trainvUn.time_oa = trainvUn.time_oa+trainvUn.time
        train.timeALv1_VSVMSL_oa = train.timeALv1_VSVMSL_oa+train.timeALv1_tSNE_VSVMSL
        train.timeALv2_SEMI_VSVMSL_oa = train.timeALv2_SEMI_VSVMSL_oa+train.timeALv2_SEMI_VSVMSL
        train.timeALv2_tSNE_VSVMSL_oa = train.timeALv2_tSNE_VSVMSL_oa+train.timeALv2_tSNE_VSVMSL
        time.taken_iter = c(time.taken_iter, c("Realization ",realization," | seed: ",seed," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h"),"\n")
        cat("Realization ",realization," execution time: ",round(as.numeric((Sys.time() - start.time), units = "hours"), 2),"h\n")
      } 
      time.taken_oa <- round(as.numeric((Sys.time() - start.time_oa), units = "hours"), 2)
      if (length(sampleSizePor)>=4) {
        if(nR>realization){
          AccuracySVM=AccuracySVM[1:(realization-1),]
          AccuracySVM_SL_Un=AccuracySVM_SL_Un[1:(realization-1),]
          AccuracyVSVM_SL=AccuracyVSVM_SL[1:(realization-1),]
          AccuracyVSVM_SL_Un=AccuracyVSVM_SL_Un[1:(realization-1),]
          AccuracyVSVM_SL_vUn=AccuracyVSVM_SL_vUn[1:(realization-1),]
          AccuracyVSVM_SL_Un_it[1:(realization-1),]
          # AccuracyVSVM_SL_Un_random_it=AccuracyVSVM_SL_Un_random_it[1:(realization-1),]
          AccuracyVSVM_SL_Un_itSL=AccuracyVSVM_SL_Un_itSL[1:(realization-1),]
          AccuracyVSVM_SL_Un_itTSL=AccuracyVSVM_SL_Un_itTSL[1:(realization-1),]
          AccuracyVSVM_SL_Un_itSL2=AccuracyVSVM_SL_Un_itSL2[1:(realization-1),]
          AccuracyVSVM_SL_Un_itTSL2=AccuracyVSVM_SL_Un_itTSL2[1:(realization-1),]
          KappaSVM=KappaSVM[1:(realization-1),]
          KappaSVM_SL_Un=KappaSVM_SL_Un[1:(realization-1),]
          KappaVSVM_SL=KappaVSVM_SL[1:(realization-1),]
          KappaVSVM_SL_Un=KappaVSVM_SL_Un[1:(realization-1),]
          KappaVSVM_SL_vUn=KappaVSVM_SL_vUn[1:(realization-1),]
          KappaVSVM_SL_Un_it[1:(realization-1),]
          # KappaVSVM_SL_Un_random_it=KappaVSVM_SL_Un_random_it[1:(realization-1),]
          KappaVSVM_SL_Un_itSL=KappaVSVM_SL_Un_itSL[1:(realization-1),]
          KappaVSVM_SL_Un_itTSL=KappaVSVM_SL_Un_itTSL[1:(realization-1),]
          KappaVSVM_SL_Un_itSL2=KappaVSVM_SL_Un_itSL2[1:(realization-1),]
          KappaVSVM_SL_Un_itTSL2=KappaVSVM_SL_Un_itTSL2[1:(realization-1),]
        }
        setwd(paste0(path,"GitHub/active-learning-virtual-SVM/results/",city))
        save(AccuracySVM, 
             AccuracySVM_SL_Un,
             AccuracyVSVM_SL,
             AccuracyVSVM_SL_Un,
             AccuracyVSVM_SL_vUn,
             AccuracyVSVM_SL_Un_it,
             # AccuracyVSVM_SL_Un_random_it,
             AccuracyVSVM_SL_Un_itSL,
             AccuracyVSVM_SL_Un_itTSL,
             AccuracyVSVM_SL_Un_itSL2,
             AccuracyVSVM_SL_Un_itTSL2,
             file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_acc_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData"))
        save(KappaSVM, 
             KappaSVM_SL_Un,
             KappaVSVM_SL,
             KappaVSVM_SL_Un,
             KappaVSVM_SL_vUn,
             KappaVSVM_SL_Un_it,
             # KappaVSVM_SL_Un_random_it,
             KappaVSVM_SL_Un_itSL,
             KappaVSVM_SL_Un_itTSL,             
             KappaVSVM_SL_Un_itSL2,
             KappaVSVM_SL_Un_itTSL2,
             file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_Kappa_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData"))
        save(SVsSVM, SVsSVM_SL_Un,SVsVSVM_SL,
             SVsVSVM_SL_Un,
             SVsVSVM_SL_vUn,
             SVsVSVM_SL_Un_it,
             SVsVSVM_SL_Un_itSL,
             SVsVSVM_SL_Un_itTSL,
             SVsVSVM_SL_Un_itSL2,
             SVsVSVM_SL_Un_itTSL2,
             file=paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_",city,"_",model_prob,"_",invariance,"_SVs_",script,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.RData"))
        cat("OA Execution time: ", time.taken_oa, "h\n", time.taken_iter,"\n",best_model_oa,
            "\n",model_name_SVMUn," training time: ",trainSVMUn.time_oa/realization, "sec",
            "\n",model_name_VSVM_SL," training time: ",trainSL.time_oa/realization, "sec",
            "\n",model_name_Un," training time: ",trainUn.time_oa/realization, "sec",
            "\n",model_name_vUn," training time: ",trainvUn.time_oa/realization, "sec",
            "\n",model_name_AL_VSVMSL," training time: ",train.timeALv1_VSVMSL_oa/realization, "sec",
            "\n",model_name_ALSL_VSVMSL," training time: ",train.timeALv2_tSNE_VSVMSL_oa/realization, "sec",
            "\n",model_name_ALTrainSL_VSVMSL," training time: ",train.timeALv2_SEMI_VSVMSL_oa/realization, "sec\n",
            # "\nbest_resample_oa: ", best_resample_oa, "\nbest_newSize_oa: ", best_newSize_oa,"\nbest_classSize_oa: ", best_classSize_oa,  "\nbest_cluster_oa: ",best_cluster_oa,
            "\nNumber of ", model_name_tunedSVM ," SVs: ",length(tunedSVM$finalModel@SVindex),
            "\nNumber of ", model_name_SVMUn ," SVs: ",length(bestFittingModelSVMUn$finalModel@SVindex),
            "\nNumber of ", model_name_VSVM_SL ," SVs: ",length(bestFittingModel$finalModel@SVindex),
            "\nNumber of ", model_name_Un ," SVs: ",length(bestFittingModelUn$finalModel@SVindex),
            "\nNumber of ", model_name_vUn ," SVs: ",length(bestFittingModelvUn$finalModel@SVindex),
            "\nNumber of ", model_name_ALSL_VSVMSL2 ," SVs: ",length(tmp_new_tunedSVM_SL2$finalModel@SVindex),
            "\nNumber of ", model_name_ALSL_VSVMSL ," SVs: ",length(tmp_new_tunedSVM_SL$finalModel@SVindex),
            "\nNumber of ", model_name_ALTrainSL_VSVMSL2 ," SVs: ",length(tmp_new_tunedSVM_ALT2$finalModel@SVindex),
            "\nNumber of ", model_name_ALTrainSL_VSVMSL ," SVs: ",length(tmp_new_tunedSVM_ALT$finalModel@SVindex),
            "\nNumber of ", model_name_AL_VSVMSL ," SVs: ", length(tmp_new_tunedSVM2$finalModel@SVindex),
            "\nNumber of final train Labels AL: ",length(trainLabels_AL),
            sep = "", file = paste0(format(Sys.time(),"%Y%m%d_%H%M"),"_metadata_",script,"_",city,"_",model_prob,"_",invariance,"_",b,"Unl_",realization,"nR_",length(sampleSizePor),"SizePor.txt"))
        print(confusionMatrix(new_trainLabels,predict(bestFittingModel, new_trainFeat)))
        cat("Number of ", model_name_ALTrainSL_VSVMSL ," SVs: ",length(tmp_new_tunedSVM_ALT$finalModel@SVindex),"\nNumber of ", model_name_AL_VSVMSL ," SVs: ", length(tmp_new_tunedSVM2$finalModel@SVindex),"\nNumber of final train Labels AL: ",length(trainLabels_AL),"\n\n",sep="")
        cat("performance results: acquired\n\n\n")
      }
    }
  }
}