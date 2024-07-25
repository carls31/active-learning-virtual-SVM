library(scales)

city = "cologne"    # cologne or hagadera
invariance = "scale"     # scale or shape
class = "multiclass"     # multiclass or binary

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",city))

file_name_acc = "20240724_1814_cologne_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240725_1256_cologne_multiclass_scale_acc_benchmark_20Unl_1nR_8SizePor"


# ********************************************************************

file_name_kappa = "20240724_1814_cologne_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240725_1256_cologne_multiclass_scale_Kappa_benchmark_20Unl_1nR_8SizePor"


load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))

# # **********************************************************************************
# tmp_AccuracySVM = AccuracySVM
# tmp_AccuracySVM_M = AccuracySVM_M
# tmp_AccuracySVM_SL_Un_b = AccuracySVM_SL_Un_b
# tmp_AccuracyVSVM = AccuracyVSVM
# tmp_AccuracyVSVM_SL = AccuracyVSVM_SL
# tmp_AccuracyVSVM_SL_Un_b = AccuracyVSVM_SL_Un_b
# tmp_AccuracyVSVM_SL_vUn_b = AccuracyVSVM_SL_vUn_b
# tmp_AccuracyVSVM_SL_Un_it = AccuracyVSVM_SL_Un_it
# 
# tmp_KappaSVM = KappaSVM
# tmp_KappaSVM_M = KappaSVM_M
# tmp_KappaSVM_SL_Un_b = KappaSVM_SL_Un_b
# tmp_KappaVSVM = KappaVSVM
# tmp_KappaVSVM_SL = KappaVSVM_SL
# tmp_KappaVSVM_SL_Un_b = KappaVSVM_SL_Un_b
# tmp_KappaVSVM_SL_vUn_b = KappaVSVM_SL_vUn_b
# tmp_KappaVSVM_SL_Un_it = KappaVSVM_SL_Un_it
# # **********************************************************************************
# tmp_AccuracySVM=rbind(tmp_AccuracySVM,AccuracySVM)
# tmp_AccuracySVM_M=rbind(tmp_AccuracySVM_M,AccuracySVM_M)
# tmp_AccuracySVM_SL_Un_b=rbind(tmp_AccuracySVM_SL_Un_b,AccuracySVM_SL_Un_b)
# tmp_AccuracyVSVM=rbind(tmp_AccuracyVSVM,AccuracyVSVM)
# tmp_AccuracyVSVM_SL=rbind(tmp_AccuracyVSVM_SL,AccuracyVSVM_SL)
# tmp_AccuracyVSVM_SL_Un_b=rbind(tmp_AccuracyVSVM_SL_Un_b,AccuracyVSVM_SL_Un_b)
# tmp_AccuracyVSVM_SL_vUn_b=rbind(tmp_AccuracyVSVM_SL_vUn_b,AccuracyVSVM_SL_vUn_b)
# tmp_AccuracyVSVM_SL_Un_it=rbind(tmp_AccuracyVSVM_SL_Un_it,AccuracyVSVM_SL_Un_it)
# 
# tmp_KappaSVM=rbind(tmp_KappaSVM,KappaSVM)
# tmp_KappaSVM_M=rbind(tmp_KappaSVM_M,KappaSVM_M)
# tmp_KappaSVM_SL_Un_b=rbind(tmp_KappaSVM_SL_Un_b,KappaSVM_SL_Un_b)
# tmp_KappaVSVM=rbind(tmp_KappaVSVM,KappaVSVM)
# tmp_KappaVSVM_SL=rbind(tmp_KappaVSVM_SL,KappaVSVM_SL)
# tmp_KappaVSVM_SL_Un_b=rbind(tmp_KappaVSVM_SL_Un_b,KappaVSVM_SL_Un_b)
# tmp_KappaVSVM_SL_vUn_b=rbind(tmp_KappaVSVM_SL_vUn_b,KappaVSVM_SL_vUn_b)
# tmp_KappaVSVM_SL_Un_it=rbind(tmp_KappaVSVM_SL_Un_it,KappaVSVM_SL_Un_it)
# # **********************************************************************************
# AccuracySVM=tmp_AccuracySVM
# AccuracySVM_M=tmp_AccuracySVM_M
# AccuracySVM_SL_Un_b=tmp_AccuracySVM_SL_Un_b
# AccuracyVSVM=tmp_AccuracyVSVM
# AccuracyVSVM_SL=tmp_AccuracyVSVM_SL
# AccuracyVSVM_SL_Un_b=tmp_AccuracyVSVM_SL_Un_b
# AccuracyVSVM_SL_vUn_b=tmp_AccuracyVSVM_SL_vUn_b
# AccuracyVSVM_SL_Un_it=tmp_AccuracyVSVM_SL_Un_it
# 
# KappaSVM=tmp_KappaSVM
# KappaSVM_M=tmp_KappaSVM_M
# KappaSVM_SL_Un_b=tmp_KappaSVM_SL_Un_b
# KappaVSVM=tmp_KappaVSVM
# KappaVSVM_SL=tmp_KappaVSVM_SL
# KappaVSVM_SL_Un_b=tmp_KappaVSVM_SL_Un_b
# KappaVSVM_SL_vUn_b=tmp_KappaVSVM_SL_vUn_b
# KappaVSVM_SL_Un_it=tmp_KappaVSVM_SL_Un_it
# 
# file_name_acc = "20240711_cologne_multiclass_shape_acc_20Unl_5nR_8SizePor"
# file_name_kappa = "20240711_cologne_multiclass_shape_Kappa_20Unl_5nR_8SizePor"
# # **********************************************************************************
# AccuracySVM=AccuracySVM[1,]
# AccuracySVM_M=AccuracySVM_M[1,]
# AccuracySVM_SL_Un_b=AccuracySVM_SL_Un_b[1,]
# AccuracyVSVM=AccuracyVSVM[1,]
# AccuracyVSVM_SL=AccuracyVSVM_SL[1,]
# AccuracyVSVM_SL_Un_b=AccuracyVSVM_SL_Un_b[1,]
# AccuracyVSVM_SL_vUn_b=AccuracyVSVM_SL_vUn_b[1,]
# AccuracyVSVM_SL_Un_it=AccuracyVSVM_SL_Un_it[1,]
# 
# KappaSVM=KappaSVM[1,]
# KappaSVM_M=KappaSVM_M[1,]
# KappaSVM_SL_Un_b=KappaSVM_SL_Un_b[1,]
# KappaVSVM=KappaVSVM[1,]
# KappaVSVM_SL=KappaVSVM_SL[1,]
# KappaVSVM_SL_Un_b=KappaVSVM_SL_Un_b[1,]
# KappaVSVM_SL_vUn_b=KappaVSVM_SL_vUn_b[1,]
# KappaVSVM_SL_Un_it=KappaVSVM_SL_Un_it[1,]
# # **********************************************************************************
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
# # **********************************************************************************

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

column_names <- colnames(AccuracySVM)
if(is.null(column_names)){column_names <- names(AccuracySVM)}
x <- 2*as.integer(column_names)

setwd(paste0(path,"images/",city))

if(class == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.97
      ylowerBound = 0.775
    }
    if(invariance=="shape"){
      yUpperBound = 0.97
      ylowerBound = 0.80
      }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.942
      ylowerBound = 0.56
    }
    if(invariance=="shape"){
      yUpperBound = 0.79
      ylowerBound = 0.545
      }
  }
}
if(class == "binary"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.985
      ylowerBound = 0.67
    }
    if(invariance=="shape"){
      yUpperBound = 0.985
      ylowerBound = 0.70
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.95
      ylowerBound = 0.78
    }
    if(invariance=="shape"){
      yUpperBound = 0.941
      ylowerBound = 0.79
    }
  }
}

type = "l"


######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

# # ******************************************************************************************************
# type = "o"
msdSVMPlot = plot(x, (AccuracySVM),log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                  xlab= "number of labeled samples per class",
                  ylab= "accuracy (%)",
                  main = paste(city,"-", class,"classification problem -", invariance,"invariance")
)
lines(x, (AccuracyVSVM_SL_Un_it), type= type ,       col = 7, lwd = 2,lty = 1)
lines(x, (AccuracyVSVM_SL_Un_random_it), type= type ,  col = 4, lwd = 2,lty = 1)
lines(x, (AccuracyVSVM_SL_Un_AL_v1), type= type , col = 5, lwd = 2,lty = 1)
lines(x, (AccuracyVSVM_SL_Un_AL_v2), type= type , col = 3, lwd = 2,lty = 1)

# # ******************************************************************************************************

# msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
#                   ylim=range(c(ylowerBound,yUpperBound)),
#                   pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
#                   xlab= "number of labeled samples per class",
#                   ylab= "accuracy (%)",
#                   main = paste(city,"-", class,"classification problem -", invariance,"invariance")
# )
# 
# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type= type ,       col = 7, lwd = 2,lty = 1)
# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_random_it)[1,], type= type ,  col = 4, lwd = 2,lty = 1)
# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_AL_v1)[1,], type= type , col = 5, lwd = 2,lty = 1)
# lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_AL_v2)[1,], type= type , col = 3, lwd = 2,lty = 1)


legend("bottomright", 
       c("SVM single-level L4",
         "VSVM-SL AL",
         "VSVM-SL AL random",
         "VSVM-SL AL v1",
         "VSVM-SL AL v2"),
       lty=c(1,
             1,
             1,
             1,
             1), # gives the legend appropriate symbols (lines)
       col=c(1,
             7,
             4,
             5,
             3)  # gives the legend lines the correct color and width
       ) 

dev.off()

# ######################################## Accuracy +/- std dev ##########################################
# 
# avgSVM=ExCsvMSD(AccuracySVM)[1,]
# sdSVM=ExCsvMSD(AccuracySVM)[2,]
# 
# avgSVM_M=ExCsvMSD(AccuracySVM_M)[1,]
# sdSVM_M=ExCsvMSD(AccuracySVM_M)[2,]
# 
# avgSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[1,]
# sdSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[2,]
# 
# avgVSVM=ExCsvMSD(AccuracyVSVM)[1,]
# sdVSVM=ExCsvMSD(AccuracyVSVM)[2,]
# 
# avgVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[1,]
# sdVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[2,]
# 
# avgVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,]
# sdVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un_b)[2,]
# 
# avgVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,]
# sdVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[2,]
# 
# avgVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,]
# sdVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn_b)[2,]



##########################################################################
# KAPPA
##########################################################################


if(class == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.955
      ylowerBound = 0.695
    }
    if(invariance=="shape"){
      yUpperBound = 0.955
      ylowerBound = 0.715
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.705
      ylowerBound = 0.40
    }
    if(invariance=="shape"){
      yUpperBound = 0.71
      ylowerBound = 0.41
    }
  }
}
if(class == "binary"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.96
      ylowerBound = 0.40
    }
    if(invariance=="shape"){
      yUpperBound = 0.963
      ylowerBound = 0.47
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.827
      ylowerBound = 0.46
    }
    if(invariance=="shape"){
      yUpperBound = 0.825
      ylowerBound = 0.42
    }
  }
}


# *********************************************
png(filename=paste0(file_name_kappa,".png"),
    units="in",
    width=20,
    height=16,
    pointsize=12,
    res=96)

msdSVMPlot = plot(x, (KappaSVM),log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                  xlab= "number of labeled samples per class",
                  ylab="Kappa-score",
                  main = paste(city,"-", class,"classification problem -", invariance,"invariance")
)

lines(x, (KappaVSVM_SL_Un_it), type= type ,          col = 7, lwd=2,lty = 1)
lines(x, (KappaVSVM_SL_Un_random_it), type= type ,  col = 4, lwd=2,lty = 1)
lines(x, (KappaVSVM_SL_Un_AL_v1), type= type , col = 5, lwd=2,lty = 1)
lines(x, (KappaVSVM_SL_Un_AL_v2), type= type , col = 3, lwd=2,lty = 1)

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend("bottomright",
       c("SVM single-level L4",
         "VSVM-SL AL",
         "VSVM-SL AL random",
         "VSVM-SL AL v1",
         "VSVM-SL AL v2"),
       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,7,4,5,3)  # gives the legend lines the correct color and width
)

dev.off()