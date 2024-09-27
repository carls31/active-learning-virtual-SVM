####################################################
## Erstellung von Abbildungen zum Paper Invarianz ##
####################################################
library(foreign)
library(gdalUtils)
library(stringr)
library(raster)

path = '/home/data1/Lorenzo/'
if(!dir.exists(path)){path = "D:/"}
setwd(paste0(path, "GitHub/active-learning-virtual-SVM/"))

############################# important ###########################################
# for this workflow to run, the data should be saved in a structer like this:     #
# e.g.: "./results_9/HagShapeMulti_accuracyMatrix_image_10samples.RData"          #
# the containing folder should be named results_XXX where XXX can be any number   #
###################################################################################


################
##     1.    ###         
#####################################################################################################################
# extract overall statistics for all runs and both locations
source('./thematic_maps/R.Code/extract.overall.acc_Function.R')
input <- list.files("./thematic_maps/results_1/new","Matrix.*.RData",recursive = T,full.names = T)

result <- extract.acc(input[8],percent = T)

write.csv(result,"./results/TablesResultMulti/statistics_eng.csv")# this output is just to inspect the data written
write.csv2(result,"./results/TablesResultMulti/statistics.csv") # this output is just to inspect the data written

# write out overall data (both locations)
source(paste0(path, "GitHub/active-learning-virtual-SVM/thematic_maps/R.Code/prepare.overall.statistics_Function.R"))
write.csv2(prepare.oa.stats(result,"sd"),"./results/TablesResultMulti/overall_stats_SD.csv")
write.csv2(prepare.oa.stats(result,"mean"),"./results/TablesResultMulti/overall_stats_MEAN.csv")



################
##     2.    ###  
#####################################################################################################################
# extract precision and F1 mean and sd for every Multi class at both locations
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/extract.class.F1ACC_Function.R')
input.mul <- list.files("./thematic_maps/results_1/new","Multi.*Matrix.*.RData",recursive = T,full.names = T)
result2 <- extract.accandF1(input.mul[4],percent=T)

# calculate the sd and mean
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/prepare.class.statistics_Function.R')
result3 <- prepare.class(result2,round = T)

# writing csv files to save mean and sd for each class
for(i in 1:length(result3)){
  write.csv2(result3[[i]],paste0("./results/TablesResultMulti/MUL",names(result3[i]),".csv"))
}



################
##     3.    ###  
#########################################################################################
############################# data formatting for tables ################################
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/format.output_Function.R')

# overall data loaded into R from the earlier output
SD <- read.csv2("./results/TablesResultMulti/overall_stats_SD.csv",row.names = 1)
MEAN <- read.csv2("./results/TablesResultMulti/overall_stats_MEAN.csv",row.names = 1)
write.csv2(format.output(MEAN,SD),"./results/TablesResultMulti/overall_combined.csv",quote=F)

# load mean and sd for the multi-classes cologne
col_SD_class <- read.csv2("./results/TablesResultMulti/MULColSD.csv")
col_MEAN_class <- read.csv2("./results/TablesResultMulti/MULColMEAN.csv")

# rearrange rows to fit the order in the publication:
col_SD_class <- col_SD_class[c(1,3,5,2,4,6),]
col_MEAN_class <- col_MEAN_class[c(1,3,5,2,4,6),]

# write combined output for cologne formatted to fit the publication
write.csv2(format.output(col_MEAN_class,col_SD_class),"./results/TablesResultMulti/MULCol_class_combined.csv",quote=F)

# load mean and sd for the multi-classes hagadera
hag_SD_class <- read.csv2("./results/TablesResultMulti/MULHagSD.csv")
hag_MEAN_class <- read.csv2("./results/TablesResultMulti/MULHagMEAN.csv")

# rearrange rows to fit the order in the publication:
hag_SD_class <- hag_SD_class[c(2,5,1,3,4),]
hag_MEAN_class <- hag_MEAN_class[c(2,5,1,3,4),]

# write combined output for hagadera formatted to fit the publication
write.csv2(format.output(hag_MEAN_class,hag_SD_class),"./results/TablesResultMulti/MULHag_class_combined.csv",quote=F) # this gives the ? symbol in R on windows:"\u00b1"




###############################################################################################
########## additional task: stats by clss for binary results ##############################


#############################
## 2. (for binary data)   ###  
############################# 
# calculate precision and F1 mean and sd for every Binary class at both locations
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/extract.class.F1ACC_Function.R')
input.bin <- list.files("./results/TablesResult","Binary.*Matrix.*.RData",recursive = T,full.names = T)
input.bin= input.bin[1:14]
result2 <- extract.accandF1(input.bin,percent=T)

# calculate the sd and mean
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/prepare.class.statistics_Function.R')
result3 <- prepare.class(result2,round = T)

# writing csv files to save mean and sd for each class
for(i in 1:length(result3)){
  write.csv2(result3[[i]],paste0("./results/TablesResult/BIN",names(result3[i]),".csv"))
}


#############################
## 3. (for binary data)   ###  
############################# 
source('C:/Users/tunc_oz/desktop/apply_model/thematic_maps/R.Code/format.output_Function.R')

# load mean and sd for the Binary-classes cologne
BINcol_SD_class <- read.csv2("./results/TablesResult/BINColSD.csv")
BINcol_MEAN_class <- read.csv2("./results/TablesResult/BINColMEAN.csv")
# save output to file
write.csv2(format.output(BINcol_MEAN_class,BINcol_SD_class),"./results/TablesResult/BINCol_class_combined.csv",quote=F)


# load mean and sd for the Binary-classes hagadera
BINhag_SD_class <- read.csv2("./results/TablesResult/BINHagSD.csv")
BINhag_MEAN_class <- read.csv2("./results/TablesResult/BINHagMEAN.csv")
# save output to file
write.csv2(format.output(BINhag_MEAN_class,BINhag_SD_class),"./results/TablesResult/BINHag_class_combined.csv",quote=F)





