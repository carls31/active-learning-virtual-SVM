###### Create rasters from shapefile and dbf data ######
#install.packages("gdalUtils")
install.packages("raster")
gdal_setInstallation()

valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
  
library(foreign)
library(gdalUtils)
library(stringr)
library(raster)
setwd("C:/Users/tunc_oz/desktop/apply_model/accuracy_assessment_make_maps")


#### file names for .csv to create rasters from
choice.col <- c("1ColScaleBinary",
                "3ColShapeBinary",
                "9ColScaleMulti",
                "12ColShapeMulti")

choice.hag <- c("2HagScaleBinary",
                "1HagScaleBinary",
                "3HagScaleMulti",
                "16HagShapeMulti")

# get relevant paths
########################## Function to get full paths to relevant files ##############
getfilenames <- function(select){
  for (i in 1:length(select)){
  assign(paste0("storage",i),list.files(paste0("./results_",str_extract(select[i],"[0-9]*")),
                                        paste0(sub("[0-9]*","",select[i]),".*.csv$"),full.names = T))
  }
  names <- c(storage1,storage2,storage3,storage4)
  return(names)
}

##################################

col <- getfilenames(choice.col)
hag <- getfilenames(choice.hag)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++ Cologne +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# load shapefile database
dbf <- read.dbf("./shape_files/cologne/cologne_frame_overrun.dbf")

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  


#### get necessary info and write new dbf file
############## col and hag
new.dbf <- data.frame("Referenz"=dbf[,19])

####+++++++++++++++ here we need to include col and hag in one dbf file
for(i in 1:length(col)){
  
  # append a column for every csv file to the dbf and write it out
  new.dbf <- cbind(new.dbf,name=read.csv2(col[i])[,2])
  
  #create column name
  colnames(new.dbf)[i+1]<- gsub("[a-z_]*","",gsub("_[0-9]*[a-z]*.csv$","",gsub("^.+/","",ifelse(grepl("Shape",col[i]),sub("Shape","SHape",col[i]),sub("Scale","SCale",col[i])))))
}


###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  
write.dbf(new.dbf,"./shape_files/cologne/cologne_frame_overrun.dbf",factor2char = F)

# create attribute file
y <- cbind("Value"=1:6,replicate(4,rep(255,6)),levels(new.dbf[,2]))
write.table(y,"./Referenz_col.txt",sep=",", quote=F,row.names=F)

# load reference raster
ref.raster <- raster("./raster/col_referenz.tif")

# use gdal to rasterize extremly fast

for(i in 2:ncol(new.dbf)){
  writeRaster(ref.raster,paste0("./raster/cologne/",colnames(new.dbf)[i],".tif"),overwrite=TRUE)
  gdal_rasterize(src_datasource ="./shape_files/cologne/final_cologne_frame.shp",
                 dst_filename = paste0("./raster/cologne/",colnames(new.dbf)[i],".tif"),
                 a=colnames(new.dbf)[i])
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#------------------------------------------------------------------------------------------------------------
#-------------------------- Hagadera ----------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

# load shapefile database
dbf <- read.dbf("./shape_files/hagadera/hagadera_frame_new.dbf")

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  


#### get necessary info and write new dbf file
############## col and hag
new.dbf <- data.frame("Referenz"=dbf[,1])

####+++++++++++++++ here we need to include col and hag in one dbf file
for(i in 1:length(hag)){
  
  # append a column for every csv file to the dbf and write it out
  new.dbf <- cbind(new.dbf,name=read.csv2(hag[i])[,2])
  
  #create column name
  colnames(new.dbf)[i+1]<- gsub("[a-z_]*","",gsub("_[0-9]*[a-z]*.csv$","",gsub("^.+/","",ifelse(grepl("Shape",hag[i]),sub("Shape","SHape",hag[i]),sub("Scale","SCale",hag[i])))))
}


###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  
write.dbf(new.dbf,"./shape_files/hagadera/final_hagadera_frame.dbf",factor2char = F)

# create attribute file
y <- cbind("Value"=1:5,replicate(4,rep(255,5)),levels(new.dbf[,12]))
write.table(y,"./Referenz_hag.txt",sep=",", quote=F,row.names=F)

# load reference raster
ref.raster <- raster("./raster/hag_referenz.tif")

# use gdal to rasterize extremly fast

for(i in 2:ncol(new.dbf)){
  writeRaster(ref.raster,paste0("./raster/hagadera/",colnames(new.dbf)[i],".tif"))
  gdal_rasterize(src_datasource ="./shape_files/hagadera/final_hagadera_frame.shp",
                 dst_filename = paste0("./raster/hagadera/",colnames(new.dbf)[i],".tif"),
                 a=colnames(new.dbf)[i])
}







