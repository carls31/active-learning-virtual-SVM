library(scales)


setwd("D:/GitHub/active-learning-virtual-SVM/results")
load("D:/GitHub/active-learning-virtual-SVM/results/FIRST_TRY_ColScaleMulticlass_accuracy_20UnlSamples.RData")
setwd("D:/GitHub/active-learning-virtual-SVM/images")

#create and save plot, plot of results 

# x-axis

x = c(10,20,30)   


#get maximum and minimum values for plotting
maxi = 0
mini = 1000

yUpperBound = max(c(0.75,maxi))
ylowerBound = min(c(0.5,mini))

###plot basemodel
png(filename="FIRST_TRY_Cologne_Scale_Multiclass.png",
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
                  main = "Cologne Scale Multiclass"
)

# lines(x, AccuracySVM, type="l" , col = 1, lty=2, lwd=2)
#pseudo line for highlighting 
# lines(x, AccuracySVM_M, type="l" , col =alpha("yellow",0.3), lwd = 4)
lines(x, AccuracySVM_M, type="l" , col = 2, lwd=2)

# lines(x, AccuracyVSVM_SL, type="l" , col =alpha("yellow",0.3), lwd = 4)
lines(x, AccuracyVSVM_SL, type="l" , col = 3 , lwd=2)

#pseudo line for highlighting 
# lines(x, AccuracyVSVM_SL_Un_b, type="l" , col = alpha(6,0.2), lwd=5)
lines(x, AccuracyVSVM_SL_Un_b, type="l" , col = 4, lwd = 2)


legend(x[1],ylowerBound+0.015, # places a legend at the appropriate place 
       c("Basis SVM L4","SVM_M","VSVM_SL","VSVM_SL 20 Unlabeled Balanced Samples"), # puts text in the legend
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,2,3,4)  # gives the legend lines the correct color and width
       ) 

dev.off()
