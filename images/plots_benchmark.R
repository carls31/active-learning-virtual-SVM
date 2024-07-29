library(scales)

city = "cologne"    # cologne or hagadera
class = "multiclass"     # multiclass or binary
invariance = "shape"     # scale or shape

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",city))

file_name_acc = "20240724_1814_cologne_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240725_1256_cologne_multiclass_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240726_1015_cologne_binary_shape_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240725_2127_hagadera_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240726_0628_hagadera_binary_shape_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240725_1936_hagadera_multiclass_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240726_1628_cologne_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240726_1740_hagadera_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240728_1221_hagadera_multiclass_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240728_0421_cologne_multiclass_shape_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240728_0046_cologne_binary_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240727_2309_cologne_multiclass_shape_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240727_1310_cologne_multiclass_scale_acc_benchmark_20Unl_1nR_8SizePor"
file_name_acc = "20240728_cologne_multiclass_shape_acc_benchmark_20Unl_2nR_8SizePor"

# ********************************************************************

file_name_kappa = "20240724_1814_cologne_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240725_1256_cologne_multiclass_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240726_1015_cologne_binary_shape_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240725_2127_hagadera_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240726_0628_hagadera_binary_shape_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240725_1936_hagadera_multiclass_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240726_1628_cologne_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240726_1740_hagadera_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240728_1221_hagadera_multiclass_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240728_0421_cologne_multiclass_shape_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240728_0046_cologne_binary_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240727_2309_cologne_multiclass_shape_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240727_1310_cologne_multiclass_scale_Kappa_benchmark_20Unl_1nR_8SizePor"
file_name_kappa = "20240728_cologne_multiclass_shape_Kappa_benchmark_20Unl_2nR_8SizePor"



load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))

# # **********************************************************************************
# tmp_AccuracySVM = AccuracySVM
# tmp_AccuracyVSVM_SL = AccuracyVSVM_SL
# tmp_AccuracyVSVM_SL_Un_AL_v1 = AccuracyVSVM_SL_Un_AL_v1
# tmp_AccuracyVSVM_SL_Un_AL_v2 = AccuracyVSVM_SL_Un_AL_v2
# tmp_AccuracyVSVM_SL_Un_random_it = AccuracyVSVM_SL_Un_random_it
# tmp_AccuracyVSVM_SL_Un_it = AccuracyVSVM_SL_Un_it
# 
# tmp_KappaSVM = KappaSVM
# tmp_KappaVSVM_SL = KappaVSVM_SL
# tmp_KappaVSVM_SL_Un_AL_v1 = KappaVSVM_SL_Un_AL_v1
# tmp_KappaVSVM_SL_Un_AL_v2 = KappaVSVM_SL_Un_AL_v2
# tmp_KappaVSVM_SL_Un_random_it = KappaVSVM_SL_Un_random_it
# tmp_KappaVSVM_SL_Un_it = KappaVSVM_SL_Un_it

# # **********************************************************************************
# tmp_AccuracySVM=rbind(tmp_AccuracySVM,AccuracySVM)
# tmp_AccuracyVSVM_SL=rbind(tmp_AccuracyVSVM_SL,AccuracyVSVM_SL)
# tmp_AccuracyVSVM_SL_Un_AL_v1=rbind(tmp_AccuracyVSVM_SL_Un_AL_v1,AccuracyVSVM_SL_Un_AL_v1)
# tmp_AccuracyVSVM_SL_Un_AL_v2=rbind(tmp_AccuracyVSVM_SL_Un_AL_v2,AccuracyVSVM_SL_Un_AL_v2)
# tmp_AccuracyVSVM_SL_Un_random_it=rbind(tmp_AccuracyVSVM_SL_Un_random_it,AccuracyVSVM_SL_Un_random_it)
# tmp_AccuracyVSVM_SL_Un_it=rbind(tmp_AccuracyVSVM_SL_Un_it,AccuracyVSVM_SL_Un_it)
# 
# tmp_KappaSVM=rbind(tmp_KappaSVM,KappaSVM)
# tmp_KappaVSVM_SL=rbind(tmp_KappaVSVM_SL,KappaVSVM_SL)
# tmp_KappaVSVM_SL_Un_AL_v1=rbind(tmp_KappaVSVM_SL_Un_AL_v1,KappaVSVM_SL_Un_AL_v1)
# tmp_KappaVSVM_SL_Un_AL_v2=rbind(tmp_KappaVSVM_SL_Un_AL_v2,KappaVSVM_SL_Un_AL_v2)
# tmp_KappaVSVM_SL_Un_random_it=rbind(tmp_KappaVSVM_SL_Un_random_it,KappaVSVM_SL_Un_random_it)
# tmp_KappaVSVM_SL_Un_it=rbind(tmp_KappaVSVM_SL_Un_it,KappaVSVM_SL_Un_it)
# # **********************************************************************************
# AccuracySVM=tmp_AccuracySVM
# AccuracyVSVM_SL=tmp_AccuracyVSVM_SL
# AccuracyVSVM_SL_Un_AL_v1=tmp_AccuracyVSVM_SL_Un_AL_v1
# AccuracyVSVM_SL_Un_AL_v2=tmp_AccuracyVSVM_SL_Un_AL_v2
# AccuracyVSVM_SL_Un_random_it=tmp_AccuracyVSVM_SL_Un_random_it
# AccuracyVSVM_SL_Un_it=tmp_AccuracyVSVM_SL_Un_it
# 
# KappaSVM=tmp_KappaSVM
# KappaVSVM_SL=tmp_KappaVSVM_SL
# KappaVSVM_SL_Un_AL_v1=tmp_KappaVSVM_SL_Un_AL_v1
# KappaVSVM_SL_Un_AL_v2=tmp_KappaVSVM_SL_Un_AL_v2
# KappaVSVM_SL_Un_random_it=tmp_KappaVSVM_SL_Un_random_it
# KappaVSVM_SL_Un_it=tmp_KappaVSVM_SL_Un_it
# 
# file_name_acc = "20240728_cologne_multiclass_shape_acc_benchmark_20Unl_2nR_8SizePor"
# file_name_kappa = "20240728_cologne_multiclass_shape_Kappa_benchmark_20Unl_2nR_8SizePor"
# # **********************************************************************************
# AccuracySVM=AccuracySVM[1,]

# AccuracyVSVM_SL=AccuracyVSVM_SL[1,]

# AccuracyVSVM_SL_Un_it=AccuracyVSVM_SL_Un_it[1,]
# 
# KappaSVM=KappaSVM[1,]

# KappaVSVM_SL=KappaVSVM_SL[1,]

# KappaVSVM_SL_Un_it=KappaVSVM_SL_Un_it[1,]
# # **********************************************************************************
# 
# save(AccuracySVM,
#      AccuracyVSVM_SL,
#      AccuracyVSVM_SL_Un_AL_v1,
#      AccuracyVSVM_SL_Un_AL_v2,
#      AccuracyVSVM_SL_Un_random_it,
#      AccuracyVSVM_SL_Un_it,
#      file=paste0(file_name_acc,".RData"))
# save(KappaSVM,
#      KappaVSVM_SL,
#      KappaVSVM_SL_Un_AL_v1,
#      KappaVSVM_SL_Un_AL_v2,
#      KappaVSVM_SL_Un_random_it,
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
      yUpperBound = 0.78
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
      ylowerBound = 0.86
    }
    if(invariance=="shape"){
      yUpperBound = 0.985
      ylowerBound = 0.70
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.965
      ylowerBound = 0.83
    }
    if(invariance=="shape"){
      yUpperBound = 0.941
      ylowerBound = 0.79
    }
  }
}

type = "l"
# # type = "o"


######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

# # ******************************************************************************************************

if(nrow(AccuracySVM)>1){
  msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  
  lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type= type , col = 4, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_random_it)[1,], type= type ,  col = 2, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_AL_v1)[1,], type= type , col = 5, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_AL_v2)[1,], type= type , col = 3, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type= type ,       col = 7, lwd = 2,lty = 1)
}else{
  msdSVMPlot = plot(x, (AccuracySVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  lines(x, (AccuracyVSVM_SL), type= type , col = 4, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_random_it), type= type ,  col = 2, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_AL_v1), type= type , col = 5, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_AL_v2), type= type , col = 3, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_it), type= type ,       col = 7, lwd = 2,lty = 1)
}

# # ******************************************************************************************************

legend("bottomright", 
       c("SVM single-level L4",
         "VSVM-SL",
         "VSVM-SL AL random",
         "VSVM-SL AL v1",
         "VSVM-SL AL v2",
         "VSVM-SL AL"
       ),
       lty=c(1,
             1,
             1,
             1,
             1,
             1), # gives the legend appropriate symbols (lines)
       col=c(1,
             4,
             2,
             5,
             3,
             7)  # gives the legend lines the correct color and width
       ) 

dev.off()

# ######################################## Accuracy +/- std dev ##########################################
# 
# avgSVM=ExCsvMSD(AccuracySVM)[1,]
# sdSVM=ExCsvMSD(AccuracySVM)[2,]
# 
# avgVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[1,]
# sdVSVM_SL=ExCsvMSD(AccuracyVSVM_SL)[2,]
# 
# avgVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,]
# sdVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it)[2,]
# 
# avgVSVM_SL_Un_random_it=ExCsvMSD(AccuracyVSVM_SL_Un_random_it)[1,]
# sdVSVM_SL_Un_random_it=ExCsvMSD(AccuracyVSVM_SL_Un_random_it)[2,]
# 
# avgVSVM_SL_Un_AL_v1=ExCsvMSD(AccuracyVSVM_SL_Un_AL_v1)[1,]
# sdVSVM_SL_Un_AL_v1=ExCsvMSD(AccuracyVSVM_SL_Un_AL_v1)[2,]
# 
# avgVSVM_SL_Un_AL_v2=ExCsvMSD(AccuracyVSVM_SL_Un_AL_v2)[1,]
# sdVSVM_SL_Un_AL_v2=ExCsvMSD(AccuracyVSVM_SL_Un_AL_v2)[2,]

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
      ylowerBound = 0.61
    }
    if(invariance=="shape"){
      yUpperBound = 0.963
      ylowerBound = 0.47
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.935
      ylowerBound = 0.395
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

# *********************************************

if(nrow(KappaSVM)){
  msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  
  lines(x, ExCsvMSD(KappaVSVM_SL)[1,], type= type , col = 4, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_random_it)[1,], type= type ,  col = 2, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_AL_v1)[1,], type= type , col = 5, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_AL_v2)[1,], type= type , col = 3, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_it)[1,], type= type ,    col = 7, lwd=2,lty = 1)
}else{
  msdSVMPlot = plot(x, (KappaSVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  
  lines(x, (KappaVSVM_SL), type= type , col = 4, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_random_it), type= type ,  col = 2, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_AL_v1), type= type , col = 5, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_AL_v2), type= type , col = 3, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_it), type= type ,    col = 7, lwd=2,lty = 1)
}

# *********************************************

legend("bottomright",
       c("SVM single-level L4",
         "VSVM-SL",
         "VSVM-SL AL random",
         "VSVM-SL AL v1",
         "VSVM-SL AL v2",
         "VSVM-SL AL"
       ),
       lty=c(1,
             1,
             1,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,
             4,
             2,5,3,7)  # gives the legend lines the correct color and width
)

dev.off()