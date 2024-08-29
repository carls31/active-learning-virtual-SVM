############################################
# Author: Lorenzo Carlassara               #
# lorenzo.carlassara98@gmail.com           #
# linkedin.com/in/lorenzo-carlassara/      #
# feel free to contact me for any question #
############################################
library(scales)

city = "cologne"    # cologne or hagadera
invariance = "scale"     # scale or shape
class = "multiclass"     # multiclass or binary

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","results/",city))

file_name_acc = "20240809_1322_hagadera_binary_scale_acc_ALTSLf_20Unl_1nR_7SizePor"
file_name_acc = "20240809_1706_hagadera_multiclass_scale_acc_ALTSLf_20Unl_1nR_9SizePor"
file_name_acc = "20240817_0730_hagadera_multiclass_scale_acc_ALTSLf_20Unl_10nR_9SizePor"
file_name_acc = "20240819_1244_cologne_multiclass_scale_acc_ALTSLf_20Unl_10nR_9SizePor"
file_name_acc = "20240819_1904_hagadera_binary_scale_acc_ALTSLf_20Unl_8nR_16SizePor"
file_name_acc = "20240821_0601_cologne_binary_scale_acc_ALTSLf_20Unl_8nR_16SizePor"
file_name_acc = "20240821_2337_cologne_binary_scale_acc_ALTSLv3_20Unl_8nR_16SizePor"
file_name_acc = "20240824_1141_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240825_1346_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240824_2023_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240825_1615_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240825_1959_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240825_2358_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_6SizePor"
file_name_acc = "20240826_0827_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_10SizePor"
file_name_acc = "20240827_1123_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_8SizePor"
file_name_acc = "20240827_2213_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_8SizePor"



# ********************************************************************

file_name_kappa = "20240809_1322_hagadera_binary_scale_Kappa_ALTSLf_20Unl_1nR_7SizePor"
file_name_kappa = "20240809_1706_hagadera_multiclass_scale_Kappa_ALTSLf_20Unl_1nR_9SizePor"
file_name_kappa = "20240817_0730_hagadera_multiclass_scale_Kappa_ALTSLf_20Unl_10nR_9SizePor"
file_name_kappa = "20240819_1244_cologne_multiclass_scale_Kappa_ALTSLf_20Unl_10nR_9SizePor"
file_name_kappa = "20240819_1904_hagadera_binary_scale_Kappa_ALTSLf_20Unl_8nR_16SizePor"
file_name_kappa = "20240821_0601_cologne_binary_scale_Kappa_ALTSLf_20Unl_8nR_16SizePor"
file_name_kappa = "20240821_2337_cologne_binary_scale_Kappa_ALTSLv3_20Unl_8nR_16SizePor"
file_name_kappa = "20240824_1141_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240825_1346_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240824_2023_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240825_1615_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240825_1959_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240825_2358_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_6SizePor"
file_name_kappa = "20240826_0827_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_10SizePor"
file_name_kappa = "20240827_1123_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_10SizePor"
file_name_kappa = "20240827_2213_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_8SizePor"








load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))

# # **********************************************************************************
# tmp_AccuracySVM = AccuracySVM
# tmp_AccuracySVM_M = AccuracySVM_M
# tmp_AccuracySVM_SL_Un = AccuracySVM_SL_Un
# tmp_AccuracyVSVM = AccuracyVSVM
# tmp_AccuracyVSVM_SL = AccuracyVSVM_SL
# tmp_AccuracyVSVM_SL_Un = AccuracyVSVM_SL_Un
# tmp_AccuracyVSVM_SL_vUn = AccuracyVSVM_SL_vUn
# tmp_AccuracyVSVM_SL_Un_it = AccuracyVSVM_SL_Un_it
# 
# tmp_KappaSVM = KappaSVM
# tmp_KappaSVM_M = KappaSVM_M
# tmp_KappaSVM_SL_Un = KappaSVM_SL_Un
# tmp_KappaVSVM = KappaVSVM
# tmp_KappaVSVM_SL = KappaVSVM_SL
# tmp_KappaVSVM_SL_Un = KappaVSVM_SL_Un
# tmp_KappaVSVM_SL_vUn = KappaVSVM_SL_vUn
# tmp_KappaVSVM_SL_Un_it = KappaVSVM_SL_Un_it
# # **********************************************************************************
# tmp_AccuracySVM=rbind(tmp_AccuracySVM,AccuracySVM)
# tmp_AccuracySVM_M=rbind(tmp_AccuracySVM_M,AccuracySVM_M)
# tmp_AccuracySVM_SL_Un=rbind(tmp_AccuracySVM_SL_Un,AccuracySVM_SL_Un)
# tmp_AccuracyVSVM=rbind(tmp_AccuracyVSVM,AccuracyVSVM)
# tmp_AccuracyVSVM_SL=rbind(tmp_AccuracyVSVM_SL,AccuracyVSVM_SL)
# tmp_AccuracyVSVM_SL_Un=rbind(tmp_AccuracyVSVM_SL_Un,AccuracyVSVM_SL_Un)
# tmp_AccuracyVSVM_SL_vUn=rbind(tmp_AccuracyVSVM_SL_vUn,AccuracyVSVM_SL_vUn)
# tmp_AccuracyVSVM_SL_Un_it=rbind(tmp_AccuracyVSVM_SL_Un_it,AccuracyVSVM_SL_Un_it)
# 
# tmp_KappaSVM=rbind(tmp_KappaSVM,KappaSVM)
# tmp_KappaSVM_M=rbind(tmp_KappaSVM_M,KappaSVM_M)
# tmp_KappaSVM_SL_Un=rbind(tmp_KappaSVM_SL_Un,KappaSVM_SL_Un)
# tmp_KappaVSVM=rbind(tmp_KappaVSVM,KappaVSVM)
# tmp_KappaVSVM_SL=rbind(tmp_KappaVSVM_SL,KappaVSVM_SL)
# tmp_KappaVSVM_SL_Un=rbind(tmp_KappaVSVM_SL_Un,KappaVSVM_SL_Un)
# tmp_KappaVSVM_SL_vUn=rbind(tmp_KappaVSVM_SL_vUn,KappaVSVM_SL_vUn)
# tmp_KappaVSVM_SL_Un_it=rbind(tmp_KappaVSVM_SL_Un_it,KappaVSVM_SL_Un_it)
# # **********************************************************************************
# AccuracySVM=tmp_AccuracySVM
# AccuracySVM_M=tmp_AccuracySVM_M
# AccuracySVM_SL_Un=tmp_AccuracySVM_SL_Un
# AccuracyVSVM=tmp_AccuracyVSVM
# AccuracyVSVM_SL=tmp_AccuracyVSVM_SL
# AccuracyVSVM_SL_Un=tmp_AccuracyVSVM_SL_Un
# AccuracyVSVM_SL_vUn=tmp_AccuracyVSVM_SL_vUn
# AccuracyVSVM_SL_Un_it=tmp_AccuracyVSVM_SL_Un_it
# 
# KappaSVM=tmp_KappaSVM
# KappaSVM_M=tmp_KappaSVM_M
# KappaSVM_SL_Un=tmp_KappaSVM_SL_Un
# KappaVSVM=tmp_KappaVSVM
# KappaVSVM_SL=tmp_KappaVSVM_SL
# KappaVSVM_SL_Un=tmp_KappaVSVM_SL_Un
# KappaVSVM_SL_vUn=tmp_KappaVSVM_SL_vUn
# KappaVSVM_SL_Un_it=tmp_KappaVSVM_SL_Un_it
# 
# file_name_acc = "20240711_cologne_multiclass_shape_acc_20Unl_5nR_8SizePor"
# file_name_kappa = "20240711_cologne_multiclass_shape_Kappa_20Unl_5nR_8SizePor"
# # **********************************************************************************
# AccuracySVM=AccuracySVM[1:5,]
# AccuracySVM_SL_Un=AccuracySVM_SL_Un[1:5,]
# AccuracyVSVM_SL=AccuracyVSVM_SL[1:5,]
# AccuracyVSVM_SL_Un=AccuracyVSVM_SL_Un[1:5,]
# AccuracyVSVM_SL_vUn=AccuracyVSVM_SL_vUn[1:5,]
# AccuracyVSVM_SL_Un_random_it=AccuracyVSVM_SL_Un_random_it[1:5,]
# AccuracyVSVM_SL_Un_it=AccuracyVSVM_SL_Un_it[1:5,]
# AccuracyVSVM_SL_Un_itSL=AccuracyVSVM_SL_Un_itSL[1:5,]
# AccuracyVSVM_SL_Un_itTSL=AccuracyVSVM_SL_Un_itTSL[1:5,]
# 
# KappaSVM=KappaSVM[1:5,]
# KappaSVM_SL_Un=KappaSVM_SL_Un[1:5,]
# KappaVSVM_SL=KappaVSVM_SL[1:5,]
# KappaVSVM_SL_Un=KappaVSVM_SL_Un[1:5,]
# KappaVSVM_SL_vUn=KappaVSVM_SL_vUn[1:5,]
# KappaVSVM_SL_Un_random_it=KappaVSVM_SL_Un_random_it[1:5,]
# KappaVSVM_SL_Un_it=KappaVSVM_SL_Un_it[1:5,]
# KappaVSVM_SL_Un_itSL=KappaVSVM_SL_Un_itSL[1:5,]
# KappaVSVM_SL_Un_itTSL=KappaVSVM_SL_Un_itTSL[1:5,]
# # **********************************************************************************
# 
# save(AccuracySVM,
#      AccuracySVM_SL_Un,
#      AccuracyVSVM_SL,
#      AccuracyVSVM_SL_Un,
#      AccuracyVSVM_SL_vUn,
#      AccuracyVSVM_SL_Un_random_it,
#      AccuracyVSVM_SL_Un_it,
#      AccuracyVSVM_SL_Un_itSL,
#      AccuracyVSVM_SL_Un_itTSL,
#      file=paste0(file_name_acc,".RData"))
# save(KappaSVM,
#      KappaSVM_SL_Un,
#      KappaVSVM_SL,
#      KappaVSVM_SL_Un,
#      KappaVSVM_SL_vUn,
#      KappaVSVM_SL_Un_random_it,
#      KappaVSVM_SL_Un_it,
#      KappaVSVM_SL_Un_itSL,
#      KappaVSVM_SL_Un_itTSL,
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

nclass=6
if(model_prob=="binary"){   nclass=2
}else if(city=="hagadera"){ nclass=5 }

column_names <- colnames(AccuracySVM)
if(is.null(column_names)){column_names <- names(AccuracySVM)}
x <- 2*as.integer(column_names)/nclass

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","images/",city))

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
      yUpperBound = 0.80
      ylowerBound = 0.45
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

if(nrow(AccuracySVM)>1){
  clms = seq(2,ncol(AccuracySVM),by=2)
  msdSVMPlot = plot(x[-clms[-length(clms)]], ExCsvMSD(AccuracySVM[,-clms[-length(clms)]])[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, ExCsvMSD(AccuracySVM_M)[1,], type= type ,         col = 8, lwd = 2,lty = 3)
  # lines(x[-clms], ExCsvMSD(AccuracySVM_SL_Un[,-clms])[1,], type= type ,   col = 1, lwd = 2,lty = 4)
  
  # lines(x, ExCsvMSD(AccuracyVSVM)[1,], type= type ,          col = 3, lwd = 2,lty = 1)
  lines(x[-clms], ExCsvMSD(AccuracyVSVM_SL[,-clms])[1,], type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x[-clms], ExCsvMSD(AccuracyVSVM_SL_Un[,-clms])[1,], type= type ,  col = 4, lwd = 2,lty = 1)
  # lines(x[-clms], ExCsvMSD(AccuracyVSVM_SL_vUn[,-clms])[1,], type= type , col = 5, lwd = 2,lty = 1)

  
  # lines(x[clms], ExCsvMSD(AccuracyVSVM_SL_Un_random_it[,clms])[1,], type= type , col = 7, lwd = 2,lty = 2)
  # lines(x[clms], ExCsvMSD(AccuracyVSVM_SL_Un_it[,clms])[1,], type= type , col = 7, lwd = 2,lty = 1)
  lines(x[clms], ExCsvMSD(AccuracyVSVM_SL_Un_itSL[,clms])[1,], type= type , col = 8, lwd = 2,lty = 1)
  lines(x[clms], ExCsvMSD(AccuracyVSVM_SL_Un_itTSL[,clms])[1,], type= type , col = 6, lwd = 2,lty = 1)
  
  }else{
  clms = seq(2,length(AccuracySVM),by=2)
  msdSVMPlot = plot(x, (AccuracySVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, (AccuracySVM_M), type= type ,         col = 8, lwd = 2,lty = 3)
  lines(x, (AccuracySVM_SL_Un), type= type ,   col = 1, lwd = 2,lty = 4)

  # lines(x, (AccuracyVSVM), type= type ,          col = 3, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL), type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un), type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_vUn), type= type , col = 5, lwd = 2,lty = 1)
  
  
  lines(x, (AccuracyVSVM_SL_Un_random_it), type= type , col = 7, lwd = 2,lty = 2)
  lines(x, (AccuracyVSVM_SL_Un_it), type= type , col = 7, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itSL), type= type , col = 8, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itTSL), type= type , col = 6, lwd = 2,lty = 1)
}

# # ******************************************************************************************************

legend("bottomright", 
       c("SVM single-level L4", # "SVM multi-level",
         # "SVM-SL + Unlabeled", # "VSVM",
         "VSVM-SL",
         "VSVM-SL + Unlabeled",
         # "VSVM-SL + Virtual Unlabeled",
         # "VSVM-SL + random AL",
         # "VSVM-SL + AL",
         "ALv2+tSNE+UnSL - VSVM-SL Un", "ALv2+Train+SL - VSVM-SL Un"
       ),
       lty=c(1,
             # 4,
             1,
             1,
             # 1,
             # 2,
             # 1,
             1, 1
             ), # gives the legend appropriate symbols (lines)
       col=c(1,
             # 1,
             3,
             4,
             # 5,
             # 7,
             # 7,
             8 ,6
             )  # gives the legend lines the correct color and width
       ) 

dev.off()

######################################## Accuracy +/- std dev ##########################################

if(nrow(AccuracySVM)>1){
  
  avgSVM=ExCsvMSD(AccuracySVM[,-clms[-length(clms)]])[1,]
  sdSVM=ExCsvMSD(AccuracySVM[,-clms[-length(clms)]])[2,]
  
  # avgSVM_M=ExCsvMSD(AccuracySVM_M)[1,]
  # sdSVM_M=ExCsvMSD(AccuracySVM_M)[2,]
  
  avgSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un[,-clms])[1,]
  sdSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un[,-clms])[2,]
  
  # avgVSVM=ExCsvMSD(AccuracyVSVM)[1,]
  # sdVSVM=ExCsvMSD(AccuracyVSVM)[2,]
  
  avgVSVM_SL=ExCsvMSD(AccuracyVSVM_SL[,-clms])[1,]
  sdVSVM_SL=ExCsvMSD(AccuracyVSVM_SL[,-clms])[2,]
  
  avgVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un[,-clms])[1,]
  sdVSVM_SL_Un_b=ExCsvMSD(AccuracyVSVM_SL_Un[,-clms])[2,]
  
  avgVSVM_SL_Un_random_it=ExCsvMSD(AccuracyVSVM_SL_Un_random_it[,clms])[1,]
  sdVSVM_SL_Un_random_it=ExCsvMSD(AccuracyVSVM_SL_Un_random_it[,clms])[2,] 
  
  avgVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it[,clms])[1,]
  sdVSVM_SL_Un_it=ExCsvMSD(AccuracyVSVM_SL_Un_it[,clms])[2,]  
  
  avgVSVM_SL_Un_itSL=ExCsvMSD(AccuracyVSVM_SL_Un_itSL[,clms])[1,]
  sdVSVM_SL_Un_itSL=ExCsvMSD(AccuracyVSVM_SL_Un_itSL[,clms])[2,]  
  
  avgVSVM_SL_Un_itTSL=ExCsvMSD(AccuracyVSVM_SL_Un_itTSL[,clms])[1,]
  sdVSVM_SL_Un_itTSL=ExCsvMSD(AccuracyVSVM_SL_Un_itTSL[,clms])[2,]
  
  avgVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn[,-clms])[1,]
  sdVSVM_SL_vUn_b=ExCsvMSD(AccuracyVSVM_SL_vUn[,-clms])[2,]
  
  # *********************************************
  png(filename=paste0(file_name_acc,"_sd.png"),
      units="in",
      width=20,
      height=16,
      pointsize=12,
      res=96)
  
  msdSVMPlot = plot(x[-clms[-length(clms)]], avgSVM,log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,   col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="accuracy (%) +/- std dev",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, avgSVM_M, type= type ,         col = 8, lwd = 2,lty = 3)
  lines(x[-clms], avgSVM_SL_Un_b, type= type ,   col = 1, lwd = 2,lty = 4)
  
  # lines(x, avgVSVM, type= type ,          col = 3, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL, type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_Un_b, type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_vUn_b, type= type , col = 5, lwd = 2,lty = 1)
  
  # lines(x, AccuracyVSVM_SL_Un_mclp, type= type , col = 4, lwd=2,lty=2)
  # lines(x, AccuracyVSVM_SL_Un_mclu, type= type , col = 5, lwd=2)
  # lines(x, AccuracyVSVM_SL_Un_ms, type= type , col = 6, lwd=2)
  
  lines(x[clms], avgVSVM_SL_Un_random_it, type= type , col = 7, lwd = 2, lty = 2)
  lines(x[clms], avgVSVM_SL_Un_it, type= type , col = 7, lwd = 2, lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itSL, type= type , col = 8, lwd = 2, lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itTSL, type= type , col = 6, lwd = 2, lty = 1)
  # lines(x, avgVSVM_SL_Un_b_ud, type= type , col = 4, lwd=2)
  # lines(x, AccuracyVSVM_SL_vUn_mclp, type= type , col = 8, lwd=2)
  
  # arrows(x, avgSVM-sdSVM, x, avgSVM+sdSVM, length=0.075, angle=90, code=3 ,col = 1,lty=1)
  # arrows(x, avgSVM_M-sdSVM_M, x, avgSVM_M+sdSVM_M, length=0.075, angle=90, code=3 ,col = 2,lty=2)
  # arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 3,lty=2)
  # arrows(x, avgVSVM_SL-sdVSVM_SL, x, avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 1,lty=3)
  # arrows(x, avgSVM_SL_Un_b-sdSVM_SL_Un_b, x, avgSVM_SL_Un_b+sdSVM_SL_Un_b, length=0.075, angle=90, code=3 ,col = 1,lty=4)
  arrows(x[-clms], avgVSVM_SL_vUn_b-sdVSVM_SL_vUn_b, x[-clms], avgVSVM_SL_vUn_b+sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3 ,col = 5)
  arrows(x[clms], avgVSVM_SL_Un_it-sdVSVM_SL_Un_it, x[clms], avgVSVM_SL_Un_it+sdVSVM_SL_Un_it, length=0.075, angle=90, code=3 ,col = 7)
  arrows(x[clms], avgVSVM_SL_Un_itSL-sdVSVM_SL_Un_itSL, x[clms], avgVSVM_SL_Un_itSL+sdVSVM_SL_Un_itSL, length=0.075, angle=90, code=3 ,col = 8)
  
  legend("bottomright", 
         c("SVM single-level L4",
           # "SVM multi-level",
           "SVM-SL + Unlabeled",
           # "VSVM",
           "VSVM-SL","VSVM-SL + Unlabeled",
           "VSVM-SL + Virtual Unlabeled",
           "VSVM-SL + random AL",
           "VSVM-SL + AL",
           "ALv2+tSNE+UnSL - VSVM-SL Un", "ALv2+Train+SL - VSVM-SL Un"
         ),
         lty=c(1,
               4,
               1,
               1,
               1,
               2,1,
               1,
               1), # gives the legend appropriate symbols (lines)
         col=c(1,
               # 8,
               1,
               # 3,
               3,4,
               5,
               7,7,
               8,6)  # gives the legend lines the correct color and width
  ) 
  
  dev.off()
}


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
      yUpperBound = 0.73
      ylowerBound = 0.30
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
if(nrow(KappaSVM)>1){
  msdSVMPlot = plot(x[-clms[-length(clms)]], ExCsvMSD(KappaSVM[,-clms[-length(clms)]])[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, ExCsvMSD(KappaSVM_M)[1,], type= type ,         col = 8, lwd=2,lty = 3)
  lines(x[-clms], ExCsvMSD(KappaSVM_SL_Un[,-clms])[1,], type= type ,   col = 1, lwd=2,lty = 4)

  # lines(x, ExCsvMSD(KappaVSVM)[1,], type= type ,          col = 3, lwd=2,lty = 1)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL[,-clms])[1,], type= type ,       col = 3, lwd=2,lty = 2)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL_Un[,-clms])[1,], type= type ,  col = 4, lwd=2,lty = 1)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL_vUn[,-clms])[1,], type= type , col = 5, lwd=2,lty = 1)

  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_random_it[,clms])[1,], type= type , col = 7, lwd=2,lty = 2)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_it[,clms])[1,], type= type , col = 7, lwd=2,lty = 1)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itSL[,clms])[1,], type= type , col = 8, lwd=2,lty = 1)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itTSL[,clms])[1,], type= type , col = 6, lwd=2,lty = 1)
  
  # lines(x, ExCsvMSD(KappaVSVM_SL_Un_ud)[1,], type= type , col = 4, lwd=2)
}else{
  msdSVMPlot = plot(x, (KappaSVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, (KappaSVM_M), type= type ,         col = 8, lwd=2,lty = 3)
  lines(x, (KappaSVM_SL_Un), type= type ,   col = 1, lwd=2,lty = 4)
  
  # lines(x, (KappaVSVM), type= type ,          col = 3, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL), type= type ,       col = 3, lwd=2,lty = 2)
  lines(x, (KappaVSVM_SL_Un), type= type ,  col = 4, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_vUn), type= type , col = 5, lwd=2,lty = 1)
  
  lines(x, (KappaVSVM_SL_Un_random_it), type= type , col = 7, lwd=2,lty = 2)
  lines(x, (KappaVSVM_SL_Un_it), type= type , col = 7, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itSL), type= type , col = 8, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itTSL), type= type , col = 6, lwd=2,lty = 1)
  
  # lines(x, (KappaVSVM_SL_Un_ud), type= type , col = 4, lwd=2)
}

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend("bottomright", 
       c("SVM single-level L4",
         # "SVM multi-level",
         "SVM-SL + Unlabeled",
         # "VSVM",
         "VSVM-SL","VSVM-SL + Unlabeled",
         "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL + random AL",
         "VSVM-SL + AL",
         "ALv2+tSNE+UnSL - VSVM-SL Un", "ALv2+Train+SL - VSVM-SL Un"
       ),
       lty=c(1,
             4,
             1,
             1,
             1,
             2,1,
             1,
             1), # gives the legend appropriate symbols (lines)
       col=c(1,
             # 8,
             1,
             # 3,
             3,4,
             5,
             7,7,
             8,6)  # gives the legend lines the correct color and width
) 

dev.off()