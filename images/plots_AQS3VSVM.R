library(scales)

city = "cologne"    # cologne or hagadera
model_prob = "multiclass"     # multiclass or binary
invariance = "shape"     # scale or shape

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}


# Load saved R objects
# load("D:/GitHub/active-learning-virtual-SVM/results/cologne/multiclass/scale/20250929_0613_cologne_multiclass_scale_acc_ALTSLv3_20Unl_20nR_9SizePor.RData")
# load("D:/GitHub/active-learning-virtual-SVM/results/cologne/multiclass/scale/20250929_0613_cologne_multiclass_scale_kappa_ALTSLv3_20Unl_20nR_9SizePor.RData")

load("D:/GitHub/active-learning-virtual-SVM/results/cologne/multiclass/shape/20251004_1646_cologne_multiclass_shape_acc_ALTSLv3_20Unl_20nR_9SizePor.RData")
load("D:/GitHub/active-learning-virtual-SVM/results/cologne/multiclass/shape/20251004_1646_cologne_multiclass_shape_Kappa_ALTSLv3_20Unl_20nR_9SizePor.RData")






ls()





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
if(model_prob=="binary"){ nclass=2 }

column_names <- colnames(AccuracySVM)

x <- 2*as.integer(column_names)/nclass # we consider also test/validate samples 

lenged_names = c("SVM single-level L4", 
                 "SVM-SL-semi-labeled", 
                 "VSVM-SL",
                 "VSVM-SL-semi-labeled",
                 "VSVM-SL-virtual-semi-labeled",
                 
                 # "AQ MCLU SVM",
                 "AQ MS SVM",
                 # "AQ MS semi-AL SVM",
                 # "AQ MS + tSNE SVM",
                 
                 "AQ SVM-SL-semi-active-labeled", 
                 "AQ VSVM-SL",
                 "AQ VSVM-SL-semi-labeled",
                 "AQ VSVM-SL-virtual-semi-labeled"
)

# ===== Colors by family =====
SVM_col        <- 1   # black
SVM_SL_col     <- 2   # red
VSVM_SL_col    <- 3   # blue
AL_MCLU_SVM_col<- 4   # dark green
AL_MS_col      <- 5   # purple
AL_SVM_SL_col  <- 4   # dark green # 6 orange
AL_VSVM_SL_col <- 7   # brown

legend_col = c(SVM_col, 
               SVM_SL_col, 
               VSVM_SL_col, VSVM_SL_col, VSVM_SL_col,   # VSVM-SL family shares blue
               # AL_MCLU_SVM_col,
               AL_MS_col, # AL_MS_col, # AL_MS_col,          # AL MS family shares purple
               AL_SVM_SL_col,                            # AL SVM-SL
               AL_VSVM_SL_col, AL_VSVM_SL_col, AL_VSVM_SL_col # AL VSVM-SL family shares brown
)

# ===== Line types per variant =====
SVM_lty        <- 1    # solid
SVM_SL_lty     <- 2    # dashed
VSVM_SL_lty    <- 1    # solid
VSVM_SL_Un_lty <- 2    # dashed
VSVM_SL_vUn_lty<- 3    # dotted

AL_MCLU_SVM_lty<- 1    # solid
AL_MS_lty      <- 1    # solid
AL_MS_semiAL_lty<- 2   # dashed
AL_MS_tSNE_lty <- 2    # dashed

AL_SVM_SL_lty  <- 3    # dotted
AL_VSVM_SL_lty <- 1    # solid
AL_VSVM_SL_Un_lty<- 2  # dashed
AL_VSVM_SL_vUn_lty<- 3 # dotted

lenged_lty = c(SVM_lty,
               SVM_SL_lty,
               VSVM_SL_lty, VSVM_SL_Un_lty, VSVM_SL_vUn_lty,
               # AL_MCLU_SVM_lty,
               AL_MS_lty, 
               # AL_MS_semiAL_lty, 
               # AL_MS_tSNE_lty,
               AL_SVM_SL_lty,
               AL_VSVM_SL_lty, AL_VSVM_SL_Un_lty, AL_VSVM_SL_vUn_lty
)

setwd(paste0(path,"GitHub/active-learning-virtual-SVM/","images/",city))

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.80 # 0.76
    ylowerBound = 0.46 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}

type = "l"

######################################## Accuracy ##########################################
file_name_acc = "20251005_PROVA_cologne_multiclass_shape_acc_AQS3VSVM_20Unl_20nR_9SizePor"
file_name_kappa = "20251005_PROVA_cologne_multiclass_shape_Kappa_AQS3VSVM_20Unl_20nR_9SizePor"


png(filename=paste0(file_name_acc,".png"),
    units="in", 
    width=20, 
    height=16, 
    pointsize=24,
    res=96)

# # ******************************************************************************************************


avgSVM           = ExCsvMSD(AccuracySVM)[1,]
avgSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un)[1,]
avgVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL)[1,]
avgVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un)[1,]
avgVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn)[1,]

avgVSVM_SL_Un_it        = ExCsvMSD(AccuracyAL_MCLU)[1,]
avgVSVM_SL_Un_itSL      = ExCsvMSD(AccuracyAL_MS)[1,]
avgVSVM_SL_Un_itTSL     = ExCsvMSD(AccuracyAL_MS_semiAL)[1,]
avgVSVM_SL_Un_itSL2     = ExCsvMSD(AccuracyAL_MS_tSNE)[1,]

avgALSVM_SL_Un_b   = ExCsvMSD(AccuracyALSVM_SL_Un)[1,]
avgALVSVM_SL       = ExCsvMSD(AccuracyALVSVM_SL)[1,]
avgALVSVM_SL_Un_b  = ExCsvMSD(AccuracyALVSVM_SL_Un)[1,]
avgALVSVM_SL_vUn_b = ExCsvMSD(AccuracyALVSVM_SL_vUn)[1,]

# *********************************************

msdSVMPlot = plot(x, avgSVM,log = "x",
                  ylim=range(c(ylowerBound,yUpperBound)),
                  pch=20, type= type,       col = SVM_col, lwd = 2,lty = SVM_lty,
                  xlab= "number of labeled samples per class",
                  ylab= "accuracy",
                  main = paste(city,"-", model_prob,"classification problem -", invariance,"invariance")
)

lines(x, avgSVM_SL_Un_b,   type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
lines(x, avgVSVM_SL,       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
lines(x, avgVSVM_SL_Un_b,  type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
lines(x, avgVSVM_SL_vUn_b, type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)

# lines(x, avgVSVM_SL_Un_it,    type=type, col=AL_MCLU_SVM_col, lwd=2, lty=AL_MCLU_SVM_lty)
lines(x, avgVSVM_SL_Un_itSL,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_lty)
# lines(x, avgVSVM_SL_Un_itSL2, type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_tSNE_lty)
# lines(x, avgVSVM_SL_Un_itTSL, type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_semiAL_lty)

lines(x, avgALSVM_SL_Un_b,   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
lines(x, avgALVSVM_SL,       type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
lines(x, avgALVSVM_SL_Un_b,  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
lines(x, avgALVSVM_SL_vUn_b, type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)


# # ******************************************************************************************************

legend("bottomright", 
       lenged_names,
       lty = lenged_lty,
       col= legend_col
) 

dev.off()

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.81 # 0.76
    ylowerBound = 0.40 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}


# ===== Accuracy +/- std dev =====
if(nrow(AccuracySVM) > 1){
  
  sdSVM           = ExCsvMSD(AccuracySVM)[2,]
  sdSVM_SL_Un_b   = ExCsvMSD(AccuracySVM_SL_Un)[2,]
  sdVSVM_SL       = ExCsvMSD(AccuracyVSVM_SL)[2,]
  sdVSVM_SL_Un_b  = ExCsvMSD(AccuracyVSVM_SL_Un)[2,]
  sdVSVM_SL_vUn_b = ExCsvMSD(AccuracyVSVM_SL_vUn)[2,]
  
  sdVSVM_SL_Un_it    = ExCsvMSD(AccuracyAL_MCLU)[2,]
  sdVSVM_SL_Un_itSL  = ExCsvMSD(AccuracyAL_MS)[2,]  
  sdVSVM_SL_Un_itTSL = ExCsvMSD(AccuracyAL_MS_semiAL)[2,]  
  sdVSVM_SL_Un_itSL2 = ExCsvMSD(AccuracyAL_MS_tSNE)[2,]
  
  sdALSVM_SL_Un_b    = ExCsvMSD(AccuracyALSVM_SL_Un)[2,]
  sdALVSVM_SL        = ExCsvMSD(AccuracyALVSVM_SL)[2,]
  sdALVSVM_SL_Un_b   = ExCsvMSD(AccuracyALVSVM_SL_Un)[2,]
  sdALVSVM_SL_vUn_b  = ExCsvMSD(AccuracyALVSVM_SL_vUn)[2,]
  
  png(filename=paste0(file_name_acc,"_sd.png"),
      units="in", width=20, height=16,
      pointsize=24, res=96)
  
  msdSVMPlot = plot(x, avgSVM, log="x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type=type, col=SVM_col, lwd=2, lty=SVM_lty,
                    xlab="number of labeled samples per class",
                    ylab="accuracy +/- std dev",
                    main=paste(city,"-", model_prob,"classification problem -", invariance,"invariance"))
  
  # Families
  lines(x, avgSVM_SL_Un_b,   type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
  lines(x, avgVSVM_SL,       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
  lines(x, avgVSVM_SL_Un_b,  type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
  lines(x, avgVSVM_SL_vUn_b, type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)
  
  # lines(x, avgVSVM_SL_Un_it,    type=type, col=AL_MCLU_SVM_col, lwd=2, lty=AL_MCLU_SVM_lty)
  lines(x, avgVSVM_SL_Un_itSL,  type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_lty)
  # lines(x, avgVSVM_SL_Un_itSL2, type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_tSNE_lty)
  # lines(x, avgVSVM_SL_Un_itTSL, type=type, col=AL_MS_col,        lwd=2, lty=AL_MS_semiAL_lty)
  
  lines(x, avgALSVM_SL_Un_b,   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
  lines(x, avgALVSVM_SL,       type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
  lines(x, avgALVSVM_SL_Un_b,  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
  lines(x, avgALVSVM_SL_vUn_b, type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)
  
  # Std dev bars (arrows)
  arrows(x, avgVSVM_SL - sdVSVM_SL,     x, avgVSVM_SL + sdVSVM_SL,     length=0.075, angle=90, code=3, col=VSVM_SL_col, lty=VSVM_SL_lty)
  arrows(x, avgVSVM_SL_vUn_b - sdVSVM_SL_vUn_b, x, avgVSVM_SL_vUn_b + sdVSVM_SL_vUn_b, length=0.075, angle=90, code=3, col=VSVM_SL_col, lty=VSVM_SL_vUn_lty)
  # arrows(x, avgVSVM_SL_Un_it - sdVSVM_SL_Un_it, x, avgVSVM_SL_Un_it + sdVSVM_SL_Un_it, length=0.075, angle=90, code=3, col=AL_MCLU_SVM_col, lty=AL_MCLU_SVM_lty)
  # arrows(x, avgVSVM_SL_Un_itTSL - sdVSVM_SL_Un_itTSL, x, avgVSVM_SL_Un_itTSL + sdVSVM_SL_Un_itTSL, length=0.075, angle=90, code=3, col=AL_MS_col, lty=AL_MS_semiAL_lty)
  
  arrows(x, avgVSVM_SL_Un_itSL - sdVSVM_SL_Un_itSL,     x, avgVSVM_SL_Un_itSL + sdVSVM_SL_Un_itSL,     length=0.075, angle=90, code=3, col=AL_MS_col, lty=AL_MS_lty)
  # arrows(x, avgVSVM_SL_Un_itSL2 - sdVSVM_SL_Un_itSL2, x, avgVSVM_SL_Un_itSL2 + sdVSVM_SL_Un_itSL2, length=0.075, angle=90, code=3, col=AL_MS_col, lty=AL_MS_tSNE_lty)
  
  arrows(x, avgALVSVM_SL - sdALVSVM_SL,     x, avgALVSVM_SL + sdALVSVM_SL,     length=0.075, angle=90, code=3, col=AL_VSVM_SL_col, lty=AL_VSVM_SL_lty)
  arrows(x, avgALVSVM_SL_vUn_b - sdALVSVM_SL_vUn_b, x, avgALVSVM_SL_vUn_b + sdALVSVM_SL_vUn_b, length=0.075, angle=90, code=3, col=AL_VSVM_SL_col, lty=AL_VSVM_SL_vUn_lty)
  
  legend("bottomright", lenged_names, lty=lenged_lty, col=legend_col) 
  dev.off()
}

if(model_prob == "multiclass"){
  
  if(city=="hagadera"){
    yUpperBound = 0.972
    ylowerBound = 0.81
  }
  
  if(city=="cologne"){
    yUpperBound = 0.73 # 0.76
    ylowerBound = 0.33 # 0.54
    
  }
}
if(model_prob == "binary"){
  
  if(city=="hagadera"){
    yUpperBound = 0.975
    ylowerBound = 0.79
    
  }
  if(city=="cologne"){
    yUpperBound = 0.935
    ylowerBound = 0.81
    
  }
}

# ===== Kappa =====
png(filename=paste0(file_name_kappa,".png"),
    units="in", width=20, height=16,
    pointsize=24, res=96)

if(nrow(KappaSVM) > 1){
  msdSVMPlot = plot(x, ExCsvMSD(KappaSVM)[1,], log="x",
                    ylim=range(c(ylowerBound,yUpperBound)),
                    pch=20, type=type, col=SVM_col, lwd=2, lty=SVM_lty,
                    xlab="number of labeled samples per class",
                    ylab="Kappa-score",
                    main=paste(city,"-", model_prob,"classification problem -", invariance,"invariance"))
  
  lines(x, ExCsvMSD(KappaSVM_SL_Un)[1,],     type=type, col=SVM_SL_col,   lwd=2, lty=SVM_SL_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL)[1,],       type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL_Un)[1,],    type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_Un_lty)
  lines(x, ExCsvMSD(KappaVSVM_SL_vUn)[1,],   type=type, col=VSVM_SL_col,  lwd=2, lty=VSVM_SL_vUn_lty)
  
  # lines(x, ExCsvMSD(KappaAL_MCLU)[1,],       type=type, col=AL_MCLU_SVM_col, lwd=2, lty=AL_MCLU_SVM_lty)
  lines(x, ExCsvMSD(KappaAL_MS)[1,],         type=type, col=AL_MS_col,       lwd=2, lty=AL_MS_lty)
  # lines(x, ExCsvMSD(KappaAL_MS_tSNE)[1,],    type=type, col=AL_MS_col,       lwd=2, lty=AL_MS_tSNE_lty)
  # lines(x, ExCsvMSD(KappaAL_MS_semiAL)[1,],  type=type, col=AL_MS_col,       lwd=2, lty=AL_MS_semiAL_lty)
  
  lines(x, ExCsvMSD(KappaALSVM_SL_Un)[1,],   type=type, col=AL_SVM_SL_col,  lwd=2, lty=AL_SVM_SL_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL)[1,],     type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL_Un)[1,],  type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_Un_lty)
  lines(x, ExCsvMSD(KappaALVSVM_SL_vUn)[1,], type=type, col=AL_VSVM_SL_col, lwd=2, lty=AL_VSVM_SL_vUn_lty)
}

legend("bottomright", lenged_names, lty=lenged_lty, col=legend_col) 
dev.off()
