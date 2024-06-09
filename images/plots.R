library(scales)

col = TRUE
scale = FALSE
multiclass = TRUE

if(col){location = "cologne"}else{location = "hagadera"}

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",location))

# file_name_acc = "20240527_1813_Col_scale_binary_acc_20UnlSamples"
# file_name_acc = "20240528_0826_Col_scale_binary_acc_20UnlSamples"
file_name_acc = "20240530_1759_Col_scale_binary_acc_20Unl_9nR"
file_name_acc = "20240601_0826_Col_scale_multiclass_acc_20Unl_3nR"
file_name_acc = "20240603_1314_Col_scale_multiclass_acc_20Unl_10nR"
file_name_acc = "20240605_1201_Col_shape_binary_acc_20Unl_10nR"
file_name_acc = "20240605_2246_Col_shape_multiclass_acc_20Unl_10nR"

# ********************************************************************

file_name_kappa = "20240530_1759_Col_scale_binary_kappa_20Unl_9nR"
file_name_kappa = "20240601_0826_Col_scale_multiclass_kappa_20Unl_3nR"
file_name_kappa = "20240603_1314_Col_scale_multiclass_kappa_20Unl_10nR"
file_name_kappa = "20240605_1201_Col_shape_binary_Kappa_20Unl_10nR"
file_name_kappa = "20240605_2246_Col_shape_multiclass_Kappa_20Unl_10nR"

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
x <- 2*as.integer(column_names)

setwd(paste0(path,"images/",location))

if(scale){invariance = "scale"}else{invariance = "shape"}
if(multiclass){
  class = "multiclass"
  yUpperBound = 0.80
  ylowerBound = 0.445
  }else{
  class = "binary"
  yUpperBound = 0.94
  ylowerBound = 0.73
  }

type = "o"

# KappaVSVM_SL_Un_it=KappaVSVM_SL_Un_it[nrow(KappaVSVM_SL_Un_it),]
# 
# save(AccuracySVM,
#      AccuracySVM_M,
#      AccuracySVM_SL_Un_b,
#      AccuracyVSVM,
#      AccuracyVSVM_SL,
#      AccuracyVSVM_SL_Un_b,
#      AccuracyVSVM_SL_vUn_b,
#      AccuracyVSVM_SL_Un_it,
#      file=paste0(file_name_acc,".RData"))
# save(KappaSVM,
#      KappaSVM_M,
#      KappaSVM_SL_Un_b,
#      KappaVSVM,
#      KappaVSVM_SL,
#      KappaVSVM_SL_Un_b,
#      KappaVSVM_SL_vUn_b,
#      KappaVSVM_SL_Un_it,
#      file=paste0(file_name_kappa,".RData"))
######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

# # ******************************************************************************************************
# x <- 2*as.integer(names(AccuracySVM))
# msdSVMPlot = plot(x, (AccuracySVM),log = "x",
#                   ylim=range(c(ylowerBound,yUpperBound)), 
#                   pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
#                   xlab= "number of labeled samples per class", 
#                   ylab= "accuracy (%)",
#                   main = paste(location,"-", class,"classification problem -", invariance,"invariance")
# )
# lines(x, (AccuracySVM_M), type= type ,         col = 8, lwd = 2,lty = 3)
# lines(x, (AccuracySVM_SL_Un_b), type= type ,   col = 1, lwd = 2,lty = 4)
# 
# lines(x, (AccuracyVSVM), type= type ,          col = 3, lwd = 2,lty = 1)
# lines(x, (AccuracyVSVM_SL), type= type ,       col = 3, lwd = 2,lty = 2)
# lines(x, (AccuracyVSVM_SL_Un_b), type= type ,  col = 4, lwd = 2,lty = 1)
# lines(x, (AccuracyVSVM_SL_vUn_b), type= type , col = 5, lwd = 2,lty = 1)
# 
# 
# lines(x, (AccuracyVSVM_SL_Un_it), type= type , col = 7, lwd = 2,lty = 1)
# # ******************************************************************************************************

msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                  xlab= "number of labeled samples per class",
                  ylab= "accuracy (%)",
                  main = paste(location,"-", class,"classification problem -", invariance,"invariance")
)
lines(x, ExCsvMSD(AccuracySVM_M)[1,], type= type ,         col = 8, lwd = 2,lty = 3)
lines(x, ExCsvMSD(AccuracySVM_SL_Un_b)[1,], type= type ,   col = 1, lwd = 2,lty = 4)

lines(x, ExCsvMSD(AccuracyVSVM)[1,], type= type ,          col = 3, lwd = 2,lty = 1)
lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type= type ,       col = 3, lwd = 2,lty = 2)
lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,], type= type ,  col = 4, lwd = 2,lty = 1)
lines(x, ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,], type= type , col = 5, lwd = 2,lty = 1)

# lines(x, AccuracyVSVM_SL_Un_b_mclp, type= type , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type= type , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type= type , col = 6, lwd=2)

lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type= type , col = 7, lwd = 2,lty = 1)

# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b_ud)[1,], type= type , col = 4, lwd=2)

# lines(x, AccuracyVSVM_SL_vUn_mclp, type= type , col = 8, lwd=2)

legend(x[1],yUpperBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM multi-level","SVM-SL + Unlabeled",
         "VSVM","VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL ITerative AL"),
       lty=c(1,3,4,1,2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,8,1,3,3,4,5,7)  # gives the legend lines the correct color and width
       ) 

dev.off()

######################################## Accuracy +/- std dev ##########################################

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

# *********************************************
png(filename=paste0(file_name_acc,"_sd.png"),
    units="in",
    width=20,
    height=16,
    pointsize=12,
    res=96)

msdSVMPlot = plot(x, avgSVM,log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,   col = 1, lwd = 2,lty = 1,
                  xlab= "number of labeled samples per class", 
                  ylab="accuracy (%) +/- std dev",
                  main = paste(location,"-", class,"classification problem -", invariance,"invariance")
)
lines(x, avgSVM_M, type= type ,         col = 8, lwd = 2,lty = 3)
lines(x, avgSVM_SL_Un_b, type= type ,   col = 1, lwd = 2,lty = 4)

lines(x, avgVSVM, type= type ,          col = 3, lwd = 2,lty = 1)
lines(x, avgVSVM_SL, type= type ,       col = 3, lwd = 2,lty = 2)
lines(x, avgVSVM_SL_Un_b, type= type ,  col = 4, lwd = 2,lty = 1)
lines(x, avgVSVM_SL_vUn_b, type= type , col = 5, lwd = 2,lty = 1)

# lines(x, AccuracyVSVM_SL_Un_b_mclp, type= type , col = 4, lwd=2,lty=2)
# lines(x, AccuracyVSVM_SL_Un_b_mclu, type= type , col = 5, lwd=2)
# lines(x, AccuracyVSVM_SL_Un_b_ms, type= type , col = 6, lwd=2)
 
lines(x, avgVSVM_SL_Un_it, type= type , col = 7, lwd = 2, lty = 1)
# lines(x, avgVSVM_SL_Un_b_ud, type= type , col = 4, lwd=2)
# lines(x, AccuracyVSVM_SL_vUn_mclp, type= type , col = 8, lwd=2)

# arrows(x, avgSVM-sdSVM, x, avgSVM+sdSVM, length=0.075, angle=90, code=3 ,col = 1,lty=1)
# arrows(x, avgSVM_M-sdSVM_M, x, avgSVM_M+sdSVM_M, length=0.075, angle=90, code=3 ,col = 2,lty=2)
# arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 3,lty=2)
# arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 1,lty=3)
# arrows(x, avgSVM_SL_Un_b-sdSVM_SL_Un_b, x, avgSVM_SL_Un_b+sdSVM_SL_Un_b, length=0.075, angle=90, code=3 ,col = 1,lty=4)
arrows(x, avgVSVM_SL_vUn_b-sdVSVM_SL_vUn_b, x, avgVSVM_SL_vUn_b+sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3 ,col = 5)
arrows(x, avgVSVM_SL_Un_it-sdVSVM_SL_Un_it, x, avgVSVM_SL_Un_it+sdVSVM_SL_Un_it, length=0.075, angle=90, code=3 ,col = 7)

legend(x[1],yUpperBound, # places a legend at the appropriate place
       c("SVM single-level L4","SVM multi-level","SVM-SL + Unlabeled",
         "VSVM","VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL ITerative AL"),
       lty=c(1,3,4,1,2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,8,1,3,3,4,5,7)  # gives the legend lines the correct color and width
)

dev.off()



##########################################################################
# KAPPA
##########################################################################

if(multiclass){
  yUpperBound = 0.66
  ylowerBound = 0.33
}else{
  yUpperBound = 0.83
  ylowerBound = 0.43
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
                  xlab= "number of labeled samples per class", 
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

# lines(x, ExCsvMSD(KappaVSVM_SL_Un_b_ud)[1,], type= type , col = 4, lwd=2)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend(x[1],yUpperBound, # places a legend at the appropriate place 
       c("SVM single-level L4","SVM multi-level","SVM-SL + Unlabeled",
         "VSVM","VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL ITerative AL"),
       lty=c(1,3,4,1,2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,8,1,3,3,4,5,7)  # gives the legend lines the correct color and width
) 

dev.off()