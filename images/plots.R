library(scales)


setwd("D:/GitHub/active-learning-virtual-SVM/results")
# load("D:/GitHub/active-learning-virtual-SVM/results/FIRST_TRY_ColScaleMulticlass_accuracy_20UnlSamples.RData")
load("TRY5_Col_scale_binary_accuracy_20UnlSamples.RData")
setwd("D:/GitHub/active-learning-virtual-SVM/images")

column_names <- colnames(AccuracySVM)
 
#create and save plot, plot of results 

# x-axis

x <- as.integer(column_names)


#get maximum and minimum values for plotting
maxi = 0
mini = 1000

yUpperBound = max(c(0.83,maxi))
ylowerBound = min(c(0.95,mini))

###plot basemodel
png(filename="TRY5_Cologne_Scale_Binary.png",
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

#par(mar=c(4.2,4,1,1) )

msdSVMPlot = plot(x, AccuracySVM,log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)), 
                  pch=20, type="l", col = 1, lwd=2, 
                  xlab="Number of Labeled Samples", 
                  ylab="Accuracy",
                  main = "Cologne Scale Binary"
)
lines(x, AccuracySVM_M, type="l" , col = 1, lwd=2,lty=2)
lines(x, AccuracyVSVM_SL, type="l" , col = 1, lwd=2,lty=3)


#pseudo line for highlighting 
lines(x, AccuracyVSVM_SL_Un_b, type="l" , col = 3, lwd = 2)

lines(x, AccuracyVSVM_SL_Un_b_mclp, type="l" , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type="l" , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type="l" , col = 6, lwd=2)
lines(x, AccuracyVSVM_SL_Un_b_ud, type="l" , col = 4, lwd=2)

lines(x, AccuracyVSVM_SL_Un_it, type="l" , col = 8, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_b, type="l" , col = 7, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_mclp, type="l" , col = 8, lwd=2)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],ylowerBound, # places a legend at the appropriate place 
       c("SVM L4","SVM_M","VSVM_SL",
         "VSVM_SL 20_Unl","VSVM_SL 20_Unl mclp","VSVM_SL 20_Unl ud",
          "VSVM_SL Iterative"), # puts text in the legend
       lty=c(1,2,3,1,2,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,1,1,3,4,4,8)  # gives the legend lines the correct color and width
       ) 

dev.off()
