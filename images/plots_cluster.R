library(scales)

location = "cologne"
invariance = "scale"
class = "multiclass"

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",location))

file_name_acc = "20240614_1743_cluster_Col_scale_binary_acc_20Unl_1nR_1SizePor"
file_name_acc = "20240614_1944_cluster_Col_scale_binary_acc_20Unl_1nR_1SizePor"
file_name_acc = "20240617_0006_cluster_Col_scale_multiclass_acc_20Unl_2nR_1SizePor"

# ********************************************************************

file_name_kappa = "20240614_1743_cluster_Col_scale_binary_Kappa_20Unl_1nR_1SizePor"
file_name_kappa = "20240614_1944_cluster_Col_scale_binary_Kappa_20Unl_1nR_1SizePor"
file_name_kappa = "20240617_0006_cluster_Col_scale_multiclass_Kappa_20Unl_2nR_1SizePor"


load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))


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
x <- as.integer(column_names)

setwd(paste0(path,"images/",location))

if(class == "multiclass"){

  yUpperBound = 0.80
  ylowerBound = 0.445
  }else{

  yUpperBound = 0.95
  ylowerBound = 0.91
  }

type = "l"


######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                  xlab= "number of clusters",
                  ylab= "accuracy (%)",
                  main = paste(location,"-", class,"classification problem -", invariance,"invariance")
)
lines(x, ExCsvMSD(AccuracySVM_M)[1,], type= type ,         col = 8, lwd = 2,lty = 3)
lines(x, ExCsvMSD(AccuracySVM_SL_Un_b)[1,], type= type ,   col = 1, lwd = 2,lty = 4)

lines(x, ExCsvMSD(AccuracyVSVM)[1,], type= type ,          col = 3, lwd = 2,lty = 1)
lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type= type ,       col = 3, lwd = 2,lty = 2)
lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,], type= type ,  col = 4, lwd = 2,lty = 1)
lines(x, ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,], type= type , col = 5, lwd = 2,lty = 1)


lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type= type , col = 7, lwd = 2,lty = 1)


legend(x[1],yUpperBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM multi-level","SVM-SL + Unlabeled",
         "VSVM","VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL + ITerative AL"),
       lty=c(1,3,4,1,2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,8,1,3,3,4,5,7)  # gives the legend lines the correct color and width
       ) 

dev.off()


##########################################################################
# KAPPA
##########################################################################

if(class == "multiclass"){
  yUpperBound = 0.715
  ylowerBound = 0.405
}else{
  yUpperBound = 0.83
  ylowerBound = 0.76
}

# *********************************************
png(filename=paste0(file_name_kappa,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)), 
                  pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                  xlab= "number of clusters", 
                  ylab="Kappa-score",
                  main = paste(location,"-", class,"classification problem -", invariance,"invariance")
)
lines(x, ExCsvMSD(KappaSVM_M)[1,], type= type ,         col = 8, lwd=2,lty = 3)
lines(x, ExCsvMSD(KappaSVM_SL_Un_b)[1,], type= type ,   col = 1, lwd=2,lty = 4)

lines(x, ExCsvMSD(KappaVSVM)[1,], type= type ,          col = 3, lwd=2,lty = 1)
lines(x, ExCsvMSD(KappaVSVM_SL)[1,], type= type ,       col = 3, lwd=2,lty = 2)
lines(x, ExCsvMSD(KappaVSVM_SL_Un_b)[1,], type= type ,  col = 4, lwd=2,lty = 1)
lines(x, ExCsvMSD(KappaVSVM_SL_vUn_b)[1,], type= type , col = 5, lwd=2,lty = 1)

lines(x, ExCsvMSD(KappaVSVM_SL_Un_it)[1,], type= type , col = 7, lwd=2,lty = 1)

legend(x[1],yUpperBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM multi-level","SVM-SL + Unlabeled",
         "VSVM","VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL + ITerative AL"),
       lty=c(1,3,4,1,2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,8,1,3,3,4,5,7)  # gives the legend lines the correct color and width
) 

dev.off()