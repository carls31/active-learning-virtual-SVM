library(scales)

city = "cologne"    # cologne or hagadera
model_prob = "binary"     # multiclass or binary
invariance = "scale"     # scale or shape


path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","results/",city))


file_name_acc = "20240827_1123_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_8SizePor"
file_name_acc = "20240827_2213_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_8SizePor"
file_name_acc = "20240829_1627_hagadera_multiclass_scale_acc_ALTSLv3_20Unl_10nR_10SizePor"
file_name_acc = "20240829_2345_hagadera_binary_scale_acc_ALTSLv3_20Unl_10nR_12SizePor"
## full benchmark
# file_name_acc = "20240901_0830_hagadera_multiclass_shape_acc_ALTSLv1_20Unl_8nR_10SizePor"
# file_name_acc = "20240901_2028_cologne_binary_shape_acc_ALTSLv1_20Unl_10nR_14SizePor"
file_name_acc = "20240903_1033_cologne_multiclass_shape_acc_ALTSLv1_20Unl_10nR_12SizePor"
file_name_acc = "20240904_1650_cologne_binary_scale_acc_ALTSLv1_20Unl_23nR_16SizePor"
# file_name_acc = "20240906_1619_hagadera_binary_shape_acc_ALTSLv1_20Unl_16nR_16SizePor"
file_name_acc = "20240907_1315_hagadera_multiclass_scale_acc_ALTSLv1_20Unl_12nR_14SizePor"
file_name_acc = "20240909_0607_cologne_multiclass_scale_acc_ALTSLv1_20Unl_12nR_15SizePor"
file_name_acc = "20240909_1656_cologne_binary_scale_acc_ALTSLv3_20Unl_10nR_14SizePor"
## back to v3
file_name_acc = "20240909_2329_cologne_binary_scale_acc_ALTSLv3_20Unl_10nR_12SizePor"
file_name_acc = "20240910_1114_cologne_multiclass_scale_acc_ALTSLv3_20Unl_10nR_10SizePor"
## back from v3
file_name_acc = "20240910_2128_cologne_multiclass_scale_acc_ALTSLv1_20Unl_10nR_11SizePor"
file_name_acc = "20240911_0402_cologne_binary_scale_acc_ALTSLv1_20Unl_10nR_13SizePor"
file_name_acc = "20240912_1339_hagadera_multiclass_shape_acc_ALTSLv1_20Unl_10nR_11SizePor"
file_name_acc = "20240913_1515_hagadera_binary_shape_acc_ALTSLv1_20Unl_10nR_13SizePor"
file_name_acc = "20240914_1031_hagadera_multiclass_scale_acc_ALTSLv1_20Unl_10nR_11SizePor"
file_name_acc = "20240915_0101_cologne_multiclass_shape_acc_ALTSLv1_20Unl_10nR_11SizePor"
file_name_acc = "20240915_0718_cologne_binary_shape_acc_ALTSLv1_20Unl_10nR_13SizePor"
file_name_acc = "20240915_1607_hagadera_binary_scale_acc_ALTSLv1_20Unl_10nR_13SizePor"

# file_name_acc = "20240916_1901_hagadera_multiclass_scale_acc_ALTSLv3_20Unl_10nR_12SizePor"


# ********************************************************************

file_name_kappa = "20240827_1123_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_10SizePor"
file_name_kappa = "20240827_2213_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_8SizePor"
file_name_kappa = "20240829_1627_hagadera_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_10SizePor"
file_name_kappa = "20240829_2345_hagadera_binary_scale_Kappa_ALTSLv3_20Unl_10nR_12SizePor"
## full benchmark
# file_name_kappa = "20240901_0833_hagadera_multiclass_shape_Kappa_ALTSLv1_20Unl_8nR_10SizePor"
# file_name_kappa = "20240901_2028_cologne_binary_shape_Kappa_ALTSLv1_20Unl_10nR_14SizePor"
file_name_kappa = "20240903_1033_cologne_multiclass_shape_Kappa_ALTSLv1_20Unl_10nR_12SizePor"
file_name_kappa = "20240904_1650_cologne_binary_scale_Kappa_ALTSLv1_20Unl_23nR_16SizePor"
# file_name_kappa = "20240906_1619_hagadera_binary_shape_Kappa_ALTSLv1_20Unl_16nR_16SizePor"
file_name_kappa = "20240907_1315_hagadera_multiclass_scale_Kappa_ALTSLv1_20Unl_12nR_14SizePor"
file_name_kappa = "20240909_0607_cologne_multiclass_scale_Kappa_ALTSLv1_20Unl_12nR_15SizePor"
file_name_kappa = "20240909_1656_cologne_binary_scale_Kappa_ALTSLv3_20Unl_10nR_14SizePor"
## back to v3
file_name_kappa = "20240909_2329_cologne_binary_scale_Kappa_ALTSLv3_20Unl_10nR_12SizePor"
file_name_kappa = "20240910_1114_cologne_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_10SizePor"
## back from v3
file_name_kappa = "20240910_2128_cologne_multiclass_scale_Kappa_ALTSLv1_20Unl_10nR_11SizePor"
file_name_kappa = "20240911_0402_cologne_binary_scale_Kappa_ALTSLv1_20Unl_10nR_13SizePor"
file_name_kappa = "20240912_1339_hagadera_multiclass_shape_Kappa_ALTSLv1_20Unl_10nR_11SizePor"
file_name_kappa = "20240913_1515_hagadera_binary_shape_Kappa_ALTSLv1_20Unl_10nR_13SizePor"
file_name_kappa = "20240914_1031_hagadera_multiclass_scale_Kappa_ALTSLv1_20Unl_10nR_11SizePor"
file_name_kappa = "20240915_0101_cologne_multiclass_shape_Kappa_ALTSLv1_20Unl_10nR_11SizePor"
file_name_kappa = "20240915_0718_cologne_binary_shape_Kappa_ALTSLv1_20Unl_10nR_13SizePor"
file_name_kappa = "20240915_1607_hagadera_binary_scale_Kappa_ALTSLv1_20Unl_10nR_13SizePor"

# file_name_kappa = "20240916_1901_hagadera_multiclass_scale_Kappa_ALTSLv3_20Unl_10nR_12SizePor"



load(paste0(file_name_acc,".RData"))
load(paste0(file_name_kappa,".RData"))

########################################################################################
# **************************************** #
# Author: Lorenzo Carlassara               #
# lorenzo.carlassara98@gmail.com           #
# linkedin.com/in/lorenzo-carlassara/      #
# feel free to contact me for any question #
# **************************************** #

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
# AccuracySVM=AccuracySVM[1:7,]
# AccuracySVM_SL_Un=AccuracySVM_SL_Un[1:7,]
# AccuracyVSVM_SL=AccuracyVSVM_SL[1:7,]
# AccuracyVSVM_SL_Un=AccuracyVSVM_SL_Un[1:7,]
# AccuracyVSVM_SL_vUn=AccuracyVSVM_SL_vUn[1:7,]
# AccuracyVSVM_SL_Un_random_it=AccuracyVSVM_SL_Un_random_it[1:7,]
# # AccuracyVSVM_SL_Un_it=AccuracyVSVM_SL_Un_it[1:7,]
# AccuracyVSVM_SL_Un_itSL=AccuracyVSVM_SL_Un_itSL[1:7,]
# AccuracyVSVM_SL_Un_itSL2=AccuracyVSVM_SL_Un_itSL2[1:7,]
# AccuracyVSVM_SL_Un_itTSL=AccuracyVSVM_SL_Un_itTSL[1:7,]
# AccuracyVSVM_SL_Un_itTSL2=AccuracyVSVM_SL_Un_itTSL2[1:7,]
# 
# KappaSVM=KappaSVM[1:7,]
# KappaSVM_SL_Un=KappaSVM_SL_Un[1:7,]
# KappaVSVM_SL=KappaVSVM_SL[1:7,]
# KappaVSVM_SL_Un=KappaVSVM_SL_Un[1:7,]
# KappaVSVM_SL_vUn=KappaVSVM_SL_vUn[1:7,]
# KappaVSVM_SL_Un_random_it=KappaVSVM_SL_Un_random_it[1:7,]
# # KappaVSVM_SL_Un_it=KappaVSVM_SL_Un_it[1:7,]
# KappaVSVM_SL_Un_itSL=KappaVSVM_SL_Un_itSL[1:7,]
# KappaVSVM_SL_Un_itSL2=KappaVSVM_SL_Un_itSL2[1:7,]
# KappaVSVM_SL_Un_itTSL=KappaVSVM_SL_Un_itTSL[1:7,]
# KappaVSVM_SL_Un_itTSL2=KappaVSVM_SL_Un_itTSL2[1:7,]
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
#      AccuracyVSVM_SL_Un_itSL2,
#      AccuracyVSVM_SL_Un_itTSL,
#      AccuracyVSVM_SL_Un_itTSL2,
#      file=paste0(file_name_acc,".RData"))
# save(KappaSVM,
#      KappaSVM_SL_Un,
#      KappaVSVM_SL,
#      KappaVSVM_SL_Un,
#      KappaVSVM_SL_vUn,
#      KappaVSVM_SL_Un_random_it,
#      KappaVSVM_SL_Un_it,
#      KappaVSVM_SL_Un_itSL,
#      KappaVSVM_SL_Un_itSL2,
#      KappaVSVM_SL_Un_itTSL,
#      KappaVSVM_SL_Un_itTSL2,
#      file=paste0(file_name_kappa,".RData"))
########################################################################################

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
if(city=="hagadera"){ nclass=5 }
if(model_prob=="binary"){ nclass=2 }

column_names <- colnames(AccuracySVM)
clms = seq(2,ncol(AccuracySVM),by=2)

if(grepl("v1", file_name_kappa) && as.integer(column_names[3])>as.integer(column_names[2])){
  column_names[-c(1,clms)]=column_names[clms[-length(clms)]]
  
  colnames(AccuracySVM)=column_names
  colnames(AccuracySVM_SL_Un)=column_names
  colnames(AccuracyVSVM_SL)=column_names
  colnames(AccuracyVSVM_SL_Un)=column_names
  colnames(AccuracyVSVM_SL_vUn)=column_names
  # colnames(AccuracyVSVM_SL_Un_it)=column_names
  colnames(AccuracyVSVM_SL_Un_random_it)=column_names
  colnames(AccuracyVSVM_SL_Un_itSL)=column_names
  colnames(AccuracyVSVM_SL_Un_itSL2)=column_names
  colnames(AccuracyVSVM_SL_Un_itTSL)=column_names
  colnames(AccuracyVSVM_SL_Un_itTSL2)=column_names
  
  colnames(KappaSVM)=column_names
  colnames(KappaSVM_SL_Un)=column_names
  colnames(KappaVSVM_SL)=column_names
  colnames(KappaVSVM_SL_Un)=column_names
  colnames(KappaVSVM_SL_vUn)=column_names
  # colnames(KappaVSVM_SL_Un_it)=column_names
  colnames(KappaVSVM_SL_Un_random_it)=column_names
  colnames(KappaVSVM_SL_Un_itSL)=column_names
  colnames(KappaVSVM_SL_Un_itSL2)=column_names
  colnames(KappaVSVM_SL_Un_itTSL)=column_names
  colnames(KappaVSVM_SL_Un_itTSL2)=column_names
  

  save(AccuracySVM,
       AccuracySVM_SL_Un,
       AccuracyVSVM_SL,
       AccuracyVSVM_SL_Un,
       AccuracyVSVM_SL_vUn,
       AccuracyVSVM_SL_Un_random_it,
       # AccuracyVSVM_SL_Un_it,
       AccuracyVSVM_SL_Un_itSL,
       AccuracyVSVM_SL_Un_itSL2,
       AccuracyVSVM_SL_Un_itTSL,
       AccuracyVSVM_SL_Un_itTSL2,
       file=paste0(file_name_acc,".RData"))
  save(KappaSVM,
       KappaSVM_SL_Un,
       KappaVSVM_SL,
       KappaVSVM_SL_Un,
       KappaVSVM_SL_vUn,
       KappaVSVM_SL_Un_random_it,
       # KappaVSVM_SL_Un_it,
       KappaVSVM_SL_Un_itSL,
       KappaVSVM_SL_Un_itSL2,
       KappaVSVM_SL_Un_itTSL,
       KappaVSVM_SL_Un_itTSL2,
       file=paste0(file_name_kappa,".RData"))
}

x <- 2*as.integer(column_names)/nclass

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","images/",city))

if(model_prob == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.972
      ylowerBound = 0.835
    }
    if(invariance=="shape"){
      yUpperBound = 0.975
      ylowerBound = 0.85
      }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.77
      ylowerBound = 0.54
    }
    if(invariance=="shape"){
      yUpperBound = 0.71
      ylowerBound = 0.565
      }
  }
}
if(model_prob == "binary"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.97
      ylowerBound = 0.72
    }
    if(invariance=="shape"){
      yUpperBound = 0.97
      ylowerBound = 0.79
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.935
      ylowerBound = 0.835
    }
    if(invariance=="shape"){
      yUpperBound = 0.93
      ylowerBound = 0.81
    }
  }
}

type = "l"

######################################## Accuracy ##########################################

png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=24,
    res=96)

# # ******************************************************************************************************

if(nrow(AccuracySVM)>1){

  avgSVM           = ExCsvMSD(AccuracySVM[,-clms[-length(clms)]])[1,]
  avgSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un[,-clms])[1,]
  avgVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL[,-clms])[1,]
  avgVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un[,-clms])[1,]
  avgVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn[,-clms])[1,]

  # avgVSVM_SL_Un_random_it = ExCsvMSD(AccuracyVSVM_SL_Un_random_it[,clms])[1,]
  avgVSVM_SL_Un_it        = ExCsvMSD(AccuracyVSVM_SL_Un_it[,clms])[1,]
  avgVSVM_SL_Un_itSL      = ExCsvMSD(AccuracyVSVM_SL_Un_itSL[,clms])[1,]
  avgVSVM_SL_Un_itTSL     = ExCsvMSD(AccuracyVSVM_SL_Un_itTSL[,clms])[1,]
  avgVSVM_SL_Un_itSL2     = ExCsvMSD(AccuracyVSVM_SL_Un_itSL2[,clms])[1,]
  avgVSVM_SL_Un_itTSL2    = ExCsvMSD(AccuracyVSVM_SL_Un_itTSL2[,clms])[1,]

  # *********************************************
  
  msdSVMPlot = plot(x[-clms[-length(clms)]], avgSVM,log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,       col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  lines(x[-clms], avgSVM_SL_Un_b, type= type ,   col = 1, lwd = 2,lty = 4)
  lines(x[-clms], avgVSVM_SL, type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_Un_b, type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_vUn_b, type= type , col = 5, lwd = 2,lty = 1)
  
  # lines(x[clms], avgVSVM_SL_Un_random_it, type= type , col = 7, lwd = 2,lty = 2)
  lines(x[clms], avgVSVM_SL_Un_it, type= type , col = 7, lwd = 2,lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itSL, type= type , col = 8, lwd = 2,lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itSL2, type= type , col = 8, lwd = 2,lty = 4)
  lines(x[clms], avgVSVM_SL_Un_itTSL, type= type , col = 6, lwd = 2,lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itTSL2, type= type , col = 6, lwd = 2,lty = 4)
  
  }else{
  clms = seq(2,length(AccuracySVM),by=2)
  msdSVMPlot = plot(x, (AccuracySVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                      col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab= "accuracy (%)",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  lines(x, (AccuracySVM_SL_Un), type= type ,   col = 1, lwd = 2,lty = 4)
  lines(x, (AccuracyVSVM_SL), type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un), type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_vUn), type= type , col = 5, lwd = 2,lty = 1)
  
  # lines(x, (AccuracyVSVM_SL_Un_random_it), type= type , col = 7, lwd = 2,lty = 2)
  lines(x, (AccuracyVSVM_SL_Un_it), type= type , col = 7, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itSL), type= type , col = 8, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itSL2), type= type , col = 8, lwd = 2,lty = 4)
  lines(x, (AccuracyVSVM_SL_Un_itTSL), type= type , col = 6, lwd = 2,lty = 1)
  lines(x, (AccuracyVSVM_SL_Un_itTSL2), type= type , col = 6, lwd = 2,lty = 4)
  }

# # ******************************************************************************************************

legend("bottomright", 
       c("SVM single-level L4", 
         "SVM-SL + semi-labeled", 
         "VSVM-SL",
         "VSVM-SL + semi-labeled",
         "VSVM-SL + virtual semi-labeled",
         # "Random AL SVM",
         "AL MCLU + t-SNE SVM",
         "AL MS + t-SNE SVM",
         "AL MS + t-SNE + SL SVM",
         "AL MS SVM (new train samples)", "AL MS + semi-labeled SL SVM"
       ),
       lty=c(1,
             4,
             1,
             1,
             1,
             # 2,
             1,
             1, 4,
             1, 4), # gives the legend appropriate symbols (lines)
       col=c(1, 
             1, 
             3, 4,
             5,
             # 7,
             7,
             8, 8,
             6, 6)  # gives the legend lines the correct color and width
) 

dev.off()

######################################## Accuracy +/- std dev ##########################################

if(model_prob == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.975
      ylowerBound = 0.82
    }
    if(invariance=="shape"){
      yUpperBound = 0.978
      ylowerBound = 0.813
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.78
      ylowerBound = 0.52
    }
    if(invariance=="shape"){
      yUpperBound = 0.785
      ylowerBound = 0.535
    }
  }
}
if(model_prob == "binary"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.99
      ylowerBound = 0.695
    }
    if(invariance=="shape"){
      yUpperBound = 0.99
      ylowerBound = 0.78
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.955
      ylowerBound = 0.81
    }
    if(invariance=="shape"){
      yUpperBound = 0.95
      ylowerBound = 0.79
    }
  }
}


if(nrow(AccuracySVM)>1){
  
  sdSVM           = ExCsvMSD(AccuracySVM[,-clms[-length(clms)]])[2,]
  sdSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un[,-clms])[2,]
  sdVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL[,-clms])[2,]
  sdVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un[,-clms])[2,]
  sdVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn[,-clms])[2,]
  
  # sdVSVM_SL_Un_random_it = ExCsvMSD(AccuracyVSVM_SL_Un_random_it[,clms])[2,]
  sdVSVM_SL_Un_it        = ExCsvMSD(AccuracyVSVM_SL_Un_it[,clms])[2,]
  sdVSVM_SL_Un_itSL      = ExCsvMSD(AccuracyVSVM_SL_Un_itSL[,clms])[2,]  
  sdVSVM_SL_Un_itTSL     = ExCsvMSD(AccuracyVSVM_SL_Un_itTSL[,clms])[2,]  
  sdVSVM_SL_Un_itSL2     = ExCsvMSD(AccuracyVSVM_SL_Un_itSL2[,clms])[2,]
  sdVSVM_SL_Un_itTSL2    = ExCsvMSD(AccuracyVSVM_SL_Un_itTSL2[,clms])[2,]
  
  # *********************************************
  png(filename=paste0(file_name_acc,"_sd.png"),
      units="in",
      width=20,
      height=16,
      pointsize=24,
      res=96)
  
  msdSVMPlot = plot(x[-clms[-length(clms)]], avgSVM,log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,   col = 1, lwd = 2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="accuracy (%) +/- std dev",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  lines(x[-clms], avgSVM_SL_Un_b, type= type ,   col = 1, lwd = 2,lty = 4)
  lines(x[-clms], avgVSVM_SL, type= type ,       col = 3, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_Un_b, type= type ,  col = 4, lwd = 2,lty = 1)
  lines(x[-clms], avgVSVM_SL_vUn_b, type= type , col = 5, lwd = 2,lty = 1)
  
  # lines(x[clms], avgVSVM_SL_Un_random_it, type= type , col = 7, lwd = 2, lty = 2)
  lines(x[clms], avgVSVM_SL_Un_it, type= type , col = 7, lwd = 2, lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itSL, type= type , col = 8, lwd = 2, lty = 1)
  lines(x[clms], avgVSVM_SL_Un_itTSL, type= type , col = 6, lwd = 2, lty = 1)  
  lines(x[clms], avgVSVM_SL_Un_itSL2, type= type , col = 8, lwd = 2, lty = 4)
  lines(x[clms], avgVSVM_SL_Un_itTSL2, type= type , col = 6, lwd = 2, lty = 4)
  
  # arrows(x[-clms], avgSVM-sdSVM, x[-clms], avgSVM+sdSVM, length=0.075, angle=90, code=3 ,col = 1,lty=1)
  # arrows(x[-clms], avgSVM_SL_Un_b-sdSVM_SL_Un_b, x[-clms], avgSVM_SL_Un_b+sdSVM_SL_Un_b, length=0.075, angle=90, code=3 ,col = 1,lty=4)
  arrows(x[-clms], avgVSVM_SL-sdVSVM_SL, x[-clms], avgVSVM_SL+sdVSVM_SL, length=0.075, angle=90, code=3 ,col = 3,lty=1)
  arrows(x[-clms], avgVSVM_SL_vUn_b-sdVSVM_SL_vUn_b, x[-clms], avgVSVM_SL_vUn_b+sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3 ,col = 5)
  
  arrows(x[clms], avgVSVM_SL_Un_it-sdVSVM_SL_Un_it, x[clms], avgVSVM_SL_Un_it+sdVSVM_SL_Un_it, length=0.075, angle=90, code=3 ,col = 7)
  # arrows(x[clms], avgVSVM_SL_Un_itSL-sdVSVM_SL_Un_itSL, x[clms], avgVSVM_SL_Un_itSL+sdVSVM_SL_Un_itSL, length=0.075, angle=90, code=3 ,col = 8)
  arrows(x[clms], avgVSVM_SL_Un_itTSL-sdVSVM_SL_Un_itTSL, x[clms], avgVSVM_SL_Un_itTSL+sdVSVM_SL_Un_itTSL, length=0.075, angle=90, code=3 ,col = 6)
  
  legend("bottomright", 
         c("SVM single-level L4", 
           "SVM-SL + semi-labeled", 
           "VSVM-SL",
           "VSVM-SL + semi-labeled",
           "VSVM-SL + virtual semi-labeled",
           # "Random AL SVM",
           "AL MCLU + t-SNE SVM",
           "AL MS + t-SNE SVM",
           "AL MS + t-SNE + SL SVM",
           "AL MS SVM (new train samples)", "AL MS + semi-labeled SL SVM"
         ),
         lty=c(1,
               4,
               1,
               1,
               1,
               # 2,
               1,
               1, 4,
               1, 4), # gives the legend appropriate symbols (lines)
         col=c(1, 
               1, 
               3, 4,
               5,
               # 7,
               7,
               8, 8,
               6, 6)  # gives the legend lines the correct color and width
  ) 
  
  dev.off()
}


##########################################################################
# KAPPA
##########################################################################


if(model_prob == "multiclass"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.96
      ylowerBound = 0.77
    }
    if(invariance=="shape"){
      yUpperBound = 0.96
      ylowerBound = 0.78
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.74
      ylowerBound = 0.41
    }
    if(invariance=="shape"){
      yUpperBound = 0.61
      ylowerBound = 0.44
    }
  }
}
if(model_prob == "binary"){
  if(city=="hagadera"){
    if(invariance=="scale"){
      yUpperBound = 0.94
      ylowerBound = 0.405
    }
    if(invariance=="shape"){
      yUpperBound = 0.93
      ylowerBound = 0.58
    }
  }
  if(city=="cologne"){
    if(invariance=="scale"){
      yUpperBound = 0.80
      ylowerBound = 0.51
    }
    if(invariance=="shape"){
      yUpperBound = 0.78
      ylowerBound = 0.47
    }
  }
}


# *********************************************
png(filename=paste0(file_name_kappa,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=24,
    res=96)
if(nrow(KappaSVM)>1){
  msdSVMPlot = plot(x[-clms[-length(clms)]], ExCsvMSD(KappaSVM[,-clms[-length(clms)]])[1,],log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class",
                    ylab="Kappa-score",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  lines(x[-clms], ExCsvMSD(KappaSVM_SL_Un[,-clms])[1,], type= type ,   col = 1, lwd=2,lty = 4)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL[,-clms])[1,], type= type ,       col = 3, lwd=2,lty = 1)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL_Un[,-clms])[1,], type= type ,  col = 4, lwd=2,lty = 1)
  lines(x[-clms], ExCsvMSD(KappaVSVM_SL_vUn[,-clms])[1,], type= type , col = 5, lwd=2,lty = 1)

  # lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_random_it[,clms])[1,], type= type , col = 7, lwd=2,lty = 2)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_it[,clms])[1,], type= type , col = 7, lwd=2,lty = 1)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itSL[,clms])[1,], type= type , col = 8, lwd=2,lty = 1)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itTSL[,clms])[1,], type= type , col = 6, lwd=2,lty = 1)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itSL2[,clms])[1,], type= type , col = 8, lwd=2,lty = 4)
  lines(x[clms], ExCsvMSD(KappaVSVM_SL_Un_itTSL2[,clms])[1,], type= type , col = 6, lwd=2,lty = 4)
  
  # lines(x, ExCsvMSD(KappaVSVM_SL_Un_ud)[1,], type= type , col = 4, lwd=2)
}else{
  msdSVMPlot = plot(x, (KappaSVM),log = "x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type= type,                   col = 1, lwd=2,lty = 1,
                    xlab= "number of labeled samples per class", 
                    ylab="Kappa-score",
                    main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
  )
  # lines(x, (KappaSVM_M), type= type ,         col = 8, lwd=2,lty = 3)
  lines(x, (KappaSVM_SL_Un), type= type ,   col = 1, lwd=2,lty = 4)
  
  # lines(x, (KappaVSVM), type= type ,          col = 3, lwd=2,lty = 2)
  lines(x, (KappaVSVM_SL), type= type ,       col = 3, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un), type= type ,  col = 4, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_vUn), type= type , col = 5, lwd=2,lty = 1)
  
  lines(x, (KappaVSVM_SL_Un_random_it), type= type , col = 7, lwd=2,lty = 2)
  lines(x, (KappaVSVM_SL_Un_it), type= type , col = 7, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itSL), type= type , col = 8, lwd=2,lty = 1)
  lines(x, (KappaVSVM_SL_Un_itTSL), type= type , col = 6, lwd=2,lty = 1)
  
  # lines(x, (KappaVSVM_SL_Un_ud), type= type , col = 4, lwd=2)
}

legend("bottomright", 
       c("SVM single-level L4", 
         "SVM-SL + semi-labeled", 
         "VSVM-SL",
         "VSVM-SL + semi-labeled",
         "VSVM-SL + virtual semi-labeled",
         # "Random AL SVM",
         "AL MCLU + t-SNE SVM",
         "AL MS + t-SNE SVM",
         "AL MS + t-SNE + SL SVM",
         "AL MS SVM (new train samples)", "AL MS + semi-labeled SL SVM"
       ),
       lty=c(1,
             4,
             1,
             1,
             1,
             # 2,
             1,
             1, 4,
             1, 4), # gives the legend appropriate symbols (lines)
       col=c(1, 
             1, 
             3, 4,
             5,
             # 7,
             7,
             8, 8,
             6, 6)  # gives the legend lines the correct color and width
) 

dev.off()