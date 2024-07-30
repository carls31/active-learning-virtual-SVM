library(scales)

city = "cologne"    # cologne or hagadera
invariance = "shape"     # scale or shape
class = "multiclass"     # multiclass or binary

path="D:/GitHub/active-learning-virtual-SVM/"

setwd(paste0(path,"results/",city))

file_name_acc = "20240527_1813_Col_scale_binary_acc_20UnlSamples"
# file_name_acc = "20240528_0826_Col_scale_binary_acc_20UnlSamples"
file_name_acc = "20240530_1759_Col_scale_binary_acc_20Unl_9nR"
file_name_acc = "20240601_0826_Col_scale_multiclass_acc_20Unl_3nR"
file_name_acc = "20240603_1314_Col_scale_multiclass_acc_20Unl_10nR"
file_name_acc = "20240605_1201_Col_shape_binary_acc_20Unl_10nR"
# file_name_acc = "20240605_2246_Col_shape_multiclass_acc_20Unl_10nR"
file_name_acc = "20240611_1332_Col_shape_multiclass_acc_20Unl_10nR"
file_name_acc = "20240618_1120_hagadera_scale_binary_acc_20Unl_10nR_8SizePor"
file_name_acc = "20240620_1034_hagadera_shape_binary_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240624_1813_hagadera_scale_multiclass_acc_20Unl_1nR_5SizePor"
file_name_acc = "20240626_1528_cologne_shape_multiclass_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240627_0631_cologne_shape_binary_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240627_0753_cologne_scale_binary_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240627_0505_cologne_scale_multiclass_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240627_1337_cologne_scale_binary_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240628_1251_cologne_scale_multiclass_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240701_2254_hagadera_scale_multiclass_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240702_1012_hagadera_scale_multiclass_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240703_1916_cologne_scale_binary_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240704_1551_cologne_scale_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240704_2214_hagadera_scale_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240705_0521_hagadera_shape_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240705_1832_hagadera_shape_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240706_0629_hagadera_shape_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240706_1237_hagadera_scale_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240707_1149_cologne_shape_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240708_1000_cologne_scale_multiclass_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240709_0142_cologne_shape_binary_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240709_0403_cologne_scale_binary_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240709_1058_hagadera_scale_binary_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240710_0330_cologne_binary_shape_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240710_0028_hagadera_binary_shape_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240710_hagadera_binary_shape_acc_20Unl_2nR_8SizePor"
file_name_acc = "20240710_hagadera_multiclass_shape_acc_20Unl_3nR_8SizePor"
file_name_acc = "20240710_hagadera_multiclass_scale_acc_20Unl_2nR_8SizePor"
file_name_acc = "20240710_hagadera_binary_scale_acc_20Unl_11nR_8SizePor"
file_name_acc = "20240710_cologne_multiclass_scale_acc_20Unl_8nR_8SizePor"
file_name_acc = "20240710_cologne_binary_scale_acc_20Unl_5nR_8SizePor"
file_name_acc = "20240710_cologne_multiclass_shape_acc_20Unl_4nR_8SizePor"
# file_name_acc = "20240710_1153_cologne_binary_shape_acc_20Unl_1nR_8SizePor"
# file_name_acc = "20240710_cologne_binary_shape_acc_20Unl_6nR_8SizePor"
# file_name_acc = "20240710_1347_cologne_binary_scale_acc_20Unl_1nR_8SizePor"
# file_name_acc = "20240710_cologne_binary_scale_acc_20Unl_6nR_8SizePor"
# file_name_acc = "20240711_1153_cologne_multiclass_shape_acc_20Unl_1nR_8SizePor"
file_name_acc = "20240711_cologne_multiclass_shape_acc_20Unl_5nR_8SizePor"

file_name_acc = "20240711_cologne_multiclass_shape_acc_20Unl_5nR_8SizePor"



# ********************************************************************

file_name_kappa = "20240530_1759_Col_scale_binary_kappa_20Unl_9nR"
file_name_kappa = "20240601_0826_Col_scale_multiclass_kappa_20Unl_3nR"
file_name_kappa = "20240603_1314_Col_scale_multiclass_kappa_20Unl_10nR"
file_name_kappa = "20240605_1201_Col_shape_binary_Kappa_20Unl_10nR"
# file_name_kappa = "20240605_2246_Col_shape_multiclass_Kappa_20Unl_10nR"
file_name_kappa = "20240611_1332_Col_shape_multiclass_Kappa_20Unl_10nR"
file_name_kappa = "20240618_1120_hagadera_scale_binary_Kappa_20Unl_10nR_8SizePor"
file_name_kappa = "20240620_1034_hagadera_shape_binary_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240624_1813_hagadera_scale_multiclass_Kappa_20Unl_1nR_5SizePor"
file_name_kappa = "20240626_1528_cologne_shape_multiclass_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240627_0631_cologne_shape_binary_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240627_0753_cologne_scale_binary_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240627_0505_cologne_scale_multiclass_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240627_1337_cologne_scale_binary_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240628_1251_cologne_scale_multiclass_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240701_2254_hagadera_scale_multiclass_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240702_1012_hagadera_scale_multiclass_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240703_1916_cologne_scale_binary_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240704_1551_cologne_scale_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240704_2214_hagadera_scale_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240705_0521_hagadera_shape_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240705_1832_hagadera_shape_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240706_0629_hagadera_shape_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240706_1237_hagadera_scale_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240707_1149_cologne_shape_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240708_1000_cologne_scale_multiclass_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240709_0142_cologne_shape_binary_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240709_0403_cologne_scale_binary_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240709_1058_hagadera_scale_binary_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240710_0330_cologne_binary_shape_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240710_0028_hagadera_binary_shape_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240710_hagadera_binary_shape_Kappa_20Unl_2nR_8SizePor"
file_name_kappa = "20240710_hagadera_multiclass_shape_Kappa_20Unl_3nR_8SizePor"
file_name_kappa = "20240710_hagadera_multiclass_scale_Kappa_20Unl_2nR_8SizePor"
file_name_kappa = "20240710_hagadera_binary_scale_Kappa_20Unl_11nR_8SizePor"
file_name_kappa = "20240710_cologne_multiclass_scale_Kappa_20Unl_8nR_8SizePor"
file_name_kappa = "20240710_cologne_binary_scale_Kappa_20Unl_5nR_8SizePor"
file_name_kappa = "20240710_cologne_multiclass_shape_Kappa_20Unl_4nR_8SizePor"
# file_name_kappa = "20240710_1153_cologne_binary_shape_Kappa_20Unl_1nR_8SizePor"
# file_name_kappa = "20240710_cologne_binary_shape_Kappa_20Unl_6nR_8SizePor"
# file_name_kappa = "20240710_1347_cologne_binary_scale_Kappa_20Unl_1nR_8SizePor"
# file_name_kappa = "20240710_cologne_binary_scale_Kappa_20Unl_6nR_8SizePor"
# file_name_kappa = "20240711_1153_cologne_multiclass_shape_Kappa_20Unl_1nR_8SizePor"
file_name_kappa = "20240711_cologne_multiclass_shape_Kappa_20Unl_5nR_8SizePor"

file_name_kappa = "20240711_cologne_multiclass_shape_Kappa_20Unl_5nR_8SizePor"


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
      yUpperBound = 0.785
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
  msdSVMPlot = plot(x, ExCsvMSD(AccuracySVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, ExCsvMSD(AccuracySVM_M)[1,], type= type ,         col = 8, lwd = 2,lty = 3)
  lines(x, ExCsvMSD(AccuracySVM_SL_Un_b)[1,], type= type ,   col = 1, lwd = 2,lty = 4)
  
  # lines(x, ExCsvMSD(AccuracyVSVM)[1,], type= type ,          col = 3, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL)[1,], type= type ,       col = 3, lwd = 2,lty = 2)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b)[1,], type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x, ExCsvMSD(AccuracyVSVM_SL_vUn_b)[1,], type= type , col = 5, lwd = 2,lty = 1)
  
  # lines(x, AccuracyVSVM_SL_Un_b_mclp, type= type , col = 4, lwd=2,lty=2)
  # lines(x, AccuracyVSVM_SL_Un_b_mclu, type= type , col = 5, lwd=2)
  # lines(x, AccuracyVSVM_SL_Un_b_ms, type= type , col = 6, lwd=2)
  
  lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_it)[1,], type= type , col = 7, lwd = 2,lty = 1)
  
  # lines(x, ExCsvMSD(AccuracyVSVM_SL_Un_b_ud)[1,], type= type , col = 4, lwd=2)
}else{
  msdSVMPlot = plot(x, (AccuracySVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, (AccuracySVM_M), type= type ,         col = 8, lwd = 2,lty = 3)
  lines(x, (AccuracySVM_SL_Un_b), type= type ,   col = 1, lwd = 2,lty = 4)
  
  # lines(x, (AccuracyVSVM), type= type ,          col = 3, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL), type= type ,       col = 3, lwd = 2,lty = 2)
  lines(x, (AccuracyVSVM_SL_Un_b), type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_vUn_b), type= type , col = 5, lwd = 2,lty = 1)
  
  
  lines(x, (AccuracyVSVM_SL_Un_it), type= type , col = 7, lwd = 2,lty = 1)
  
  # lines(x, AccuracyVSVM_SL_vUn_mclp, type= type , col = 8, lwd=2)
  
}

# # ******************************************************************************************************

legend("bottomright", 
       c("SVM single-level L4",
         # "SVM multi-level",
         "SVM-SL + Unlabeled",
         # "VSVM",
         "VSVM-SL","VSVM-SL + Unlabeled",
         "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL + AL"),
       lty=c(1,
             # 3,
             4,
             # 1,
             2,1,
             1,
             1), # gives the legend appropriate symbols (lines)
       col=c(1,
             # 8,
             1,
             # 3,
             3,4,
             5,
             7)  # gives the legend lines the correct color and width
       ) 

dev.off()

######################################## Accuracy +/- std dev ##########################################

if(nrow(AccuracySVM)>1){
  
  avgSVM=ExCsvMSD(AccuracySVM)[1,]
  sdSVM=ExCsvMSD(AccuracySVM)[2,]
  
  # avgSVM_M=ExCsvMSD(AccuracySVM_M)[1,]
  # sdSVM_M=ExCsvMSD(AccuracySVM_M)[2,]
  
  avgSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[1,]
  sdSVM_SL_Un_b=ExCsvMSD(AccuracySVM_SL_Un_b)[2,]
  
  # avgVSVM=ExCsvMSD(AccuracyVSVM)[1,]
  # sdVSVM=ExCsvMSD(AccuracyVSVM)[2,]
  
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
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, avgSVM_M, type= type ,         col = 8, lwd = 2,lty = 3)
  lines(x, avgSVM_SL_Un_b, type= type ,   col = 1, lwd = 2,lty = 4)
  
  # lines(x, avgVSVM, type= type ,          col = 3, lwd = 2,lty = 1)
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
  
  legend("bottomright",
         c("SVM single-level L4",
           # "SVM multi-level",
           "SVM-SL + Unlabeled",
           # "VSVM",
           "VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
           "VSVM-SL + AL"),
         lty=c(1,
               # 3,
               4,
               # 1,
               2,1,1,1), # gives the legend appropriate symbols (lines)
         col=c(1,
               # 8,
               1,
               # 3,
               3,4,5,7)  # gives the legend lines the correct color and width
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
if(nrow(KappaSVM)>1){
  msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)), 
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, ExCsvMSD(KappaSVM_M)[1,], type= type ,         col = 8, lwd=2,lty = 3)
  lines(x, ExCsvMSD(KappaSVM_SL_Un_b)[1,], type= type ,   col = 1, lwd=2,lty = 4)
  
  # lines(x, ExCsvMSD(KappaVSVM)[1,], type= type ,          col = 3, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL)[1,], type= type ,       col = 3, lwd=2,lty = 2)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_b)[1,], type= type ,  col = 4, lwd=2,lty = 1)
  lines(x, ExCsvMSD(KappaVSVM_SL_vUn_b)[1,], type= type , col = 5, lwd=2,lty = 1)
  
  lines(x, ExCsvMSD(KappaVSVM_SL_Un_it)[1,], type= type , col = 7, lwd=2,lty = 1)
  
  # lines(x, ExCsvMSD(KappaVSVM_SL_Un_b_ud)[1,], type= type , col = 4, lwd=2)
}else{
  msdSVMPlot = plot(x, (KappaSVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)), 
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="Kappa-score",
                    main = paste(city,"-", class,"classification problem -", invariance,"invariance")
  )
  # lines(x, (KappaSVM_M), type= type ,         col = 8, lwd=2,lty = 3)
  lines(x, (KappaSVM_SL_Un_b), type= type ,   col = 1, lwd=2,lty = 4)
  
  # lines(x, (KappaVSVM), type= type ,          col = 3, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL), type= type ,       col = 3, lwd=2,lty = 2)
  lines(x, (KappaVSVM_SL_Un_b), type= type ,  col = 4, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_vUn_b), type= type , col = 5, lwd=2,lty = 1)
  
  lines(x, (KappaVSVM_SL_Un_it), type= type , col = 7, lwd=2,lty = 1)
  
  # lines(x, (KappaVSVM_SL_Un_b_ud), type= type , col = 4, lwd=2)
}

# "VSVM_SL MCLU", , "VSVM_SL Virtual Unlabeled Balanced Samples MCLP"
legend("bottomright",
       c("SVM single-level L4",
         # "SVM multi-level",
         "SVM-SL + Unlabeled",
         # "VSVM",
         "VSVM-SL","VSVM-SL + Unlabeled", "VSVM-SL + Virtual Unlabeled",
         "VSVM-SL + AL"),
       lty=c(1,
             # 3,
             4,
             # 1,
             2,1,1,1), # gives the legend appropriate symbols (lines)
       col=c(1,
             # 8,
             1,
             # 3, 
             3,4,5,7)  # gives the legend lines the correct color and width
) 

dev.off()