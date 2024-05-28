library(scales)


setwd("D:/GitHub/active-learning-virtual-SVM/results")
# load("D:/GitHub/active-learning-virtual-SVM/results/FIRST_TRY_ColScaleMulticlass_accuracy_20UnlSamples.RData")
load("20240527_1813_Col_scale_binary_accuracy_20UnlSamples.RData")
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
png(filename="20240527_1813_Col_scale_binary_accuracy_20UnlSamples.png",
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

#par(mar=c(4.2,4,1,1) )

msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)), 
                  pch=20, type="l", col = 1, lwd=2, 
                  xlab="Number of Samples", 
                  ylab="Accuracy",
                  main = "Cologne Scale Binary"
)
lines(x, ExCsvMSD(AccuracySVM_M)[1,], type="l" , col = 1, lwd=2,lty=2)
lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type="l" , col = 1, lwd=2,lty=3)


#pseudo line for highlighting 
lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,], type="l" , col = 3, lwd = 2)

# lines(x, AccuracyVSVM_SL_Un_b_mclp, type="l" , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type="l" , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type="l" , col = 6, lwd=2)
lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b_ud)[1,], type="l" , col = 4, lwd=2)

lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type="l" , col = 8, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_b, type="l" , col = 7, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_mclp, type="l" , col = 8, lwd=2)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],ylowerBound, # places a legend at the appropriate place 
       c("SVM L4","SVM_M","VSVM_SL",
         "VSVM_SL 20_Unl","VSVM_SL 20_Unl ud",
          "VSVM_SL Iterative"), # puts text in the legend
       lty=c(1,2,3,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,1,1,3,4,8)  # gives the legend lines the correct color and width
       ) 

dev.off()


##########################################################################

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

MSDAccuracySVM = ExCsvMSD(AccuracySVM)

avg = MSDAccuracySVM[1,]
sdev = MSDAccuracySVM[2,]


#plot basemodel

png(filename="AMean_03_09_Hyp_05_15_L2L13dis-40_BushesTrees_Kappa_msdbase.png",
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)


# par(mar=c(4.2,4,1,1) )

plot(x, avg,log = "x",
      ylim=range(c(ylowerBound,yUpperBound)), 
      pch=20, type="l", col = 2,  
      xlab="Training samples", 
      ylab="Mean Accuracy +/- Std Dev",
      main = "Cologne Scale Binary"
                  
)

lines(x, avgVSVM2, type="l" , col = 3)
lines(x, avgVSVM, type="l" , col = 4)
lines(x, avgSVMmultiScale, type="l" , col = 5)

#hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3 ,col = 2)
arrows(x, avgVSVM2-sdevVSVM2, x, avgVSVM2+sdevVSVM2, length=0.05, angle=90, code=3, col = 3)
arrows(x, avgVSVM-sdevVSVM, x, avgVSVM+sdevVSVM, length=0.05, angle=90, code=3, col = 4)
arrows(x, avgSVMmultiScale - sdevSVMmultiScale, x, avgSVMmultiScale+sdevSVMmultiScale, length=0.05, angle=90, code=3, col = 5)

legend(x[3],ylowerBound+0.1, # places a legend at the appropriate place 
       c("Basis SVM L4","VSVM L3 L5","VSVM alle Level", "Multiscale baseline"), # puts text in the legend
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(2,3,4,5)) # gives the legend lines the correct color and width

dev.off()

