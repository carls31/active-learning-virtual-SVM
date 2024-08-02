library(scales)

city = "hagadera"    # cologne or hagadera
invariance = "scale"     # scale or shape
class = "binary"     # multiclass or binary

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",city))

file_name_acc = "20240801_1504_cologne_binary_scale_acc_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_acc = "20240801_1137_cologne_binary_scale_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240801_1651_cologne_binary_scale_acc_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_acc = "20240801_1030_hagadera_binary_scale_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240802_1549_hagadera_multiclass_scale_acc_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_acc = "20240802_2253_hagadera_binary_scale_acc_ALTSL_20Unl_1nR_8SizePor"



# ********************************************************************

file_name_kappa = "20240801_1504_cologne_binary_scale_Kappa_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_kappa = "20240801_1137_cologne_binary_scale_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240801_1651_cologne_binary_scale_Kappa_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_kappa = "20240801_1030_hagadera_binary_scale_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240802_1549_hagadera_multiclass_scale_Kappa_AL+Trainv1_20Unl_1nR_8SizePor"
file_name_kappa = "20240802_2253_hagadera_binary_scale_Kappa_ALTSL_20Unl_1nR_8SizePor"







load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))


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
      ylowerBound = 0.82
    }
    if(invariance=="shape"){
      yUpperBound = 0.97
      ylowerBound = 0.80
      }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.76
      ylowerBound = 0.50
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
      yUpperBound = 0.99
      ylowerBound = 0.85
    }
    if(invariance=="shape"){
      yUpperBound = 0.985
      ylowerBound = 0.85
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
# type = "o"

######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=12,
    res=96)

# # ******************************************************************************************************

  msdSVMPlot = plot(x, (AccuracySVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  lines(x, AccuracySVM[2,], type= type ,         col = 1, lwd = 2,lty = 2)
  
  lines(x, AccuracyVSVM_SL[1,], type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x, AccuracyVSVM_SL[2,], type= type ,       col = 3, lwd = 2,lty = 2)
  
  lines(x, (AccuracyVSVM_SL_Un_random_it)[1,], type= type , col = 4, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_random_it)[2,], type= type , col = 4, lwd = 2,lty = 2)
  
  lines(x, (AccuracyVSVM_SL_Un_it)[1,], type= type , col = 7, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_it)[2,], type= type , col = 7, lwd = 2,lty = 2)

  lines(x, (AccuracyVSVM_SL_Un_itSL)[1,], type= type , col = 5, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itSL)[2,], type= type , col = 5, lwd = 2,lty = 2)
  
  lines(x, (AccuracyVSVM_SL_Un_itTSL)[1,], type= type , col = 6, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itTSL)[2,], type= type , col = 6, lwd = 2,lty = 2)
  


# # ******************************************************************************************************
  legend("bottomright", 
         c("SVM single-level L4","SVM single-level L4 retrain",
           "VSVM-SL","VSVM-SL retrain", 
           "VSVM-SL + random AL ","VSVM-SL + random AL retrain",
           "VSVM-SL + AL","VSVM-SL + AL retrain",
           "VSVM-SL + AL T","VSVM-SL + AL T retrain",
           "VSVM-SL + AL TSL","VSVM-SL + AL TSL retrain"
           
         ),
         lty=c(1,
               2,
               1,
               2,
               1,
               2,
               1,
               2,
               1,
               2,
               1,
               2), 
         col=c(1,
               1,
               3,
               3,
               4,
               4,
               7,
               7,
               5,
               5,
               6,
               6)  
  ) 
  
  dev.off()

##########################################################################
# KAPPA
##########################################################################


if(class == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.955
      ylowerBound = 0.74
    }
    if(invariance=="shape"){
      yUpperBound = 0.955
      ylowerBound = 0.715
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.68
      ylowerBound = 0.35
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
      ylowerBound = 0.57
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

  msdSVMPlot = plot(x, (KappaSVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  lines(x, KappaSVM[2,], type= type , col = 1, lwd = 2,lty = 2)
  
  lines(x, KappaVSVM_SL[1,], type= type , col = 3, lwd = 2,lty = 1)
  lines(x, KappaVSVM_SL[2,], type= type , col = 3, lwd = 2,lty = 2)
  
  lines(x, (KappaVSVM_SL_Un_random_it)[1,], type= type , col = 4, lwd = 2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_random_it)[2,], type= type , col = 4, lwd = 2,lty = 2)
  
  lines(x, (KappaVSVM_SL_Un_it)[1,], type= type , col = 7, lwd = 2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_it)[2,], type= type , col = 7, lwd = 2,lty = 2)

  lines(x, (KappaVSVM_SL_Un_itSL)[1,], type= type , col = 5, lwd = 2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itSL)[2,], type= type , col = 5, lwd = 2,lty = 2)
  
  lines(x, (KappaVSVM_SL_Un_itTSL)[1,], type= type , col = 6, lwd = 2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itTSL)[2,], type= type , col = 6, lwd = 2,lty = 2)
  
  legend("bottomright", 
         c("SVM single-level L4","SVM single-level L4 retrain",
           "VSVM-SL","VSVM-SL retrain", 
           "VSVM-SL + random AL ","VSVM-SL + random AL retrain",
           "VSVM-SL + AL","VSVM-SL + AL retrain",
           "VSVM-SL + AL T","VSVM-SL + AL T retrain",
           "VSVM-SL + AL TSL","VSVM-SL + AL TSL retrain"
           
         ),
         lty=c(1,
               2,
               1,
               2,
               1,
               2,
               1,
               2,
               1,
               2,
               1,
               2), 
         col=c(1,
               1,
               3,
               3,
               4,
               4,
               7,
               7,
               5,
               5,
               6,
               6)  
  ) 
  
  dev.off()