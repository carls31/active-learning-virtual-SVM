library(scales)


setwd("D:/GitHub/active-learning-virtual-SVM/results")

# file_name = "20240527_1813_Col_scale_binary_accuracy_20UnlSamples"
# file_name = "20240528_0826_Col_scale_binary_accuracy_20UnlSamples"
# file_name = "20240530_1759_Col_scale_binary_accuracy_20Unl_9nR"
file_name = "20240601_0826_Col_scale_multiclass_acc_20Unl_3nR"

load(paste0(file_name,".RData"))
setwd("D:/GitHub/active-learning-virtual-SVM/images")

ExCsvMSD = function (datadase, filename = NA){{
  
  datadase = as.matrix(datadase)
  n = ncol(datadase)
  MSDdata = matrix(data = NA, nrow = 2, ncol = n)
  
  rownames(MSDdata) = c("Mean","Std Dev")
  
  for (l in seq(along=1:n)){
    
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


column_names <- colnames(AccuracySVM)

x <- 6*as.integer(column_names)


#get maximum and minimum values for plotting
maxi = 0
mini = 1000

yUpperBound = max(c(0.40,maxi))
ylowerBound = min(c(0.79,mini))

######################################## acc ##########################################

###plot basemodel
png(filename=paste0(file_name,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

#par(mar=c(4.2,4,1,1) )

msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)), 
                  pch=20, type="l",                      col = 1, lwd = 2,lty = 2, 
                  xlab="Number of Training Samples", 
                  ylab="Accuracy",
                  main = "Cologne Scale Multiclass"
)
# lines(x, ExCsvMSD(AccuracySVM_M)[1,], type="l" ,         col = 1, lwd = 2,lty = 5)
lines(x, ExCsvMSD(AccuracySVM_SL_Un_b)[1,], type="l" ,   col = 4, lwd = 2,lty = 4)

lines(x, ExCsvMSD(AccuracyVSVM)[1,], type="l" ,       col = 2, lwd = 2,lty = 2)
lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type="l" ,       col = 2, lwd = 2,lty = 3)
lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,], type="l" ,  col = 4, lwd = 2,lty = 5)
lines(x, ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,], type="l" , col = 2, lwd = 2)

# lines(x, AccuracyVSVM_SL_Un_b_mclp, type="l" , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type="l" , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type="l" , col = 6, lwd=2)

lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type="l" , col = 3, lwd = 2)

# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b_ud)[1,], type="l" , col = 4, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_mclp, type="l" , col = 8, lwd=2)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],ylowerBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM-SL + semi-labeled",
         "VSVM","VSVM-SL","VSVM-SL + semi-labeled", "VSVM-SL + virtual semi-labeled",
         "VSVM-SL ITerative AL"),
       lty=c(2,4,2,3,5,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,4,2,2,4,2,3)  # gives the legend lines the correct color and width
       ) 

dev.off()


######################################## acc +/- std dev ##########################################

avgSVM=ExCsvMSD(AccuracySVM)[1,]
sdSVM=ExCsvMSD(AccuracySVM)[2,]

avgSVM_M=ExCsvMSD(AccuracySVM_M)[1,]
sdSVM_M=ExCsvMSD(AccuracySVM_M)[2,]

avgSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[1,]
sdSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[2,]

avgVSVM=ExCsvMSD(AccuracyVSVM)[1,]
sdVSVM=ExCsvMSD(AccuracyVSVM)[2,]

avgVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[1,]
sdVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[2,]

avgVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,]
sdVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un_b)[2,]

avgVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,]
sdVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[2,]

avgVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,]
sdVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn_b)[2,]

###plot basemodel
png(filename=paste0(file_name,"_sd.png"),
    units="in",
    width=20,
    height=16,
    pointsize=12,
    res=96)

#par(mar=c(4.2,4,1,1) )

msdSVMPlot = plot(x, avgSVM,log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type="l",   col = 1, lwd = 2,lty = 2,
                  xlab="Number of Training Samples",
                  ylab="Accuracy +/- Std Dev",
                  main = "Cologne Scale Multiclass"
)
# lines(x, avgSVM_M, type="l" ,         col = 2, lwd = 2,lty = 2)
lines(x, avgSVM_SL_Un_b, type="l" ,   col = 3, lwd = 2,lty = 2)

lines(x, avgVSVM, type="l" ,          col = 1, lwd = 2,lty = 3)
lines(x, avgVSVM_SL, type="l" ,       col = 4, lwd = 2,lty = 3)
lines(x, avgVSVM_SL_Un_b, type="l" ,  col = 3, lwd = 2,lty = 3)
lines(x, avgVSVM_SL_vUn_b, type="l" , col = 6, lwd = 2,lty = 3)

# lines(x, AccuracyVSVM_SL_Un_b_mclp, type="l" , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type="l" , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type="l" , col = 6, lwd=2)

lines(x, avgVSVM_SL_Un_it, type="l" , col = 7, lwd=2)
# lines(x, avgVSVM_SL_Un_b_ud, type="l" , col = 4, lwd=2)


# lines(x, AccuracyVSVM_SL_vUn_b, type="l" , col = 7, lwd=2)
# lines(x, AccuracyVSVM_SL_vUn_mclp, type="l" , col = 8, lwd=2)

# arrows(x, avgSVM-sdSVM, x, avgSVM+sdSVM, length=0.075, angle=90, code=3 ,col = 1,lty=1)
# arrows(x, avgSVM_M-sdSVM_M, x, avgSVM_M+sdSVM_M, length=0.075, angle=90, code=3 ,col = 2,lty=2)

arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 4,lty=3)
# arrows(x, avgSVM_SL_Un_b-sdSVM_SL_Un_b, x, avgSVM_SL_Un_b+sdSVM_SL_Un_b, length=0.075, angle=90, code=3 ,col = 1,lty=4)
# arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 1,lty=3)
# arrows(x, avgVSVM_SL_vUn_b-sdVSVM_SL_vUn_b, x, avgVSVM_SL_vUn_b+sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3 ,col = 5)
arrows(x, avgVSVM_SL_Un_it-sdVSVM_SL_Un_it, x, avgVSVM_SL_Un_it+sdVSVM_SL_Un_it, length=0.075, angle=90, code=3 ,col = 7)
# arrows(x, avgVSVM_SL_Un_b_ud-sdVSVM_SL_Un_b_ud, x, avgVSVM_SL_Un_b_ud+sdVSVM_SL_Un_b_ud, length=0.075, angle=90, code=3 ,col = 4)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],ylowerBound, # places a legend at the appropriate place
       c("SVM single-level L4","SVM-SL + semi-labeled",
         "VSVM","VSVM-SL", "VSVM-SL + semi-labeled", "VSVM-SL + virtual semi-labeled",
         "VSVM-SL ITerative AL"),
       lty=c(2,2,3,3,3,3,1), # gives the legend appropriate symbols (lines)
       col=c(1,3,1,4,3,6,7)  # gives the legend lines the correct color and width
)

dev.off()



##########################################################################
# KAPPA
##########################################################################

setwd("D:/GitHub/active-learning-virtual-SVM/results")

file_name = "20240530_1759_Col_scale_binary_kappa_20Unl_9nR"
file_name = "20240601_0826_Col_scale_multiclass_kappa_20Unl_3nR"

# load("D:/GitHub/active-learning-virtual-SVM/results/FIRST_TRY_ColScaleMulticlass_accuracy_20UnlSamples.RData")
load(paste0(file_name,".RData"))
setwd("D:/GitHub/active-learning-virtual-SVM/images")

yUpperBound = 0.3
ylowerBound = 0.66

###plot basemodel
png(filename=paste0(file_name,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)), 
                  pch=20, type="l",                   col = 1, lwd=2, 
                  xlab="Number of Training Samples", 
                  ylab="Kappa-score",
                  main = "Cologne Scale Multiclass"
)
lines(x, ExCsvMSD(KappaSVM_M)[1,], type="l" ,         col = 1, lwd=2,lty = 2)
lines(x, ExCsvMSD(KappaSVM_SL_Un_b)[1,], type="l" ,   col = 1, lwd=2,lty = 3)

lines(x, ExCsvMSD(KappaVSVM)[1,], type="l" ,          col = 2, lwd=2,lty = 2)
lines(x, ExCsvMSD(KappaVSVM_SL)[1,], type="l" ,       col = 2, lwd=2)
lines(x, ExCsvMSD(KappaVSVM_SL_Un_b)[1,], type="l" ,  col = 2, lwd=2,lty = 3)
lines(x, ExCsvMSD(KappaVSVM_SL_vUn_b)[1,], type="l" , col = 3, lwd=2)

lines(x, ExCsvMSD(KappaVSVM_SL_Un_it)[1,], type="l" , col = 4, lwd=2)

# lines(x, ExCsvMSD(KappaVSVM_SL_Un_b_ud)[1,], type="l" , col = 4, lwd=2)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],ylowerBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM multi-level","SVM-SL + semi-labeled",
         "VSVM","VSVM-SL","VSVM-SL + semi-labeled", "VSVM-SL + virtual semi-salabeled",
         "VSVM-SL ITerative AL"),
       lty=c(1,2,3,2,1,3,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,1,1,2,2,2,3,4)  # gives the legend lines the correct color and width
) 

dev.off()