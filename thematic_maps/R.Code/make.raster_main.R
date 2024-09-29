library(foreign)
# library(gdalUtils)
# library(stringr)
# library(raster)
setwd("/home/data1/Lorenzo/tunc_oz/thematic_maps")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++ Cologne +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # Install and load the terra package
# install.packages("terra")
library(terra)
# 
# # Load the shapefile (vector data)
# vector_data <- vect("path_to_shapefile.shp")
# 
# # Create an empty raster template (define extent and resolution)
# raster_template <- rast(ext(vector_data), resolution = 10)
# 
# # Rasterize the vector data onto the raster template
# rasterized <- rasterize(vector_data, raster_template, field = "attribute_column")
# 
# # Save the rasterized output
# writeRaster(rasterized, "rasterized_output.tif", overwrite = TRUE)

# # Install and load the raster package
# install.packages("raster")
# library(raster)
# library(rgdal)  # Used to load vector data in combination with raster
# 
# # Load vector data (shapefile)
# vector_data <- shapefile("path_to_shapefile.shp")
# 
# # Create an empty raster template
# raster_template <- raster(extent(vector_data), resolution = 10)
# 
# # Rasterize the shapefile
# rasterized <- rasterize(vector_data, raster_template, field = "attribute_column")
# 
# # Save the rasterized output
# writeRaster(rasterized, "rasterized_output.tif", overwrite = TRUE)



# load shapefile database
# dbf <- read.dbf("./shape_files/cologne_prova/cologne_frame.dbf")
dbf <- vect("./shape_files/cologne_prova/cologne_frame.dbf")

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  


#### get necessary info and write new dbf file
############## col and hag
new.dbf <- data.frame("reference"=dbf[,19])

####+++++++++++++++ here we need to include col and hag in one dbf file

  # append a column for every csv file to the dbf and write it out
  new.dbf <- cbind(new.dbf,name=read.csv2("./shape_files/cologne_prova/20240928AL_MS+tSNE_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2])
  
  #create column name
  colnames(new.dbf)[2] <- "ALv1tcmsc"

  new.dbf$ALv1tcmsc <- as.numeric(as.factor(new.dbf$ALv1tcmsc))

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  
  write.dbf(new.dbf,"./shape_files/cologne_prova/cologne_label_prova.dbf",factor2char = F)
  # writeVector(new.dbf,"./shape_files/cologne_prova/cologne_label_prova.dbf",factor2char = F)
  new.dbf<-vect("./shape_files/cologne_prova/cologne_label_prova.dbf")
  # new.dbf<-vect("./shape_files/cologne_prova/cologne_label_prova.shp")
  
  # load reference raster
ref.raster <- rast("./raster/col_referenz.tif")

crs(new.dbf)  # Check the CRS of the SpatVector
crs(ref.raster)  # Check the CRS of the reference raster
crs(new.dbf) <- crs(ref.raster)
ext(new.dbf)
ext(ref.raster)
ext(new.dbf)<-ext(ref.raster)


# If they are different, project new.dbf to match ref.raster
# new.dbf <- project(new.dbf, crs(ref.raster))

# Rasterize the vector data onto the raster template
rasterized <- rasterize(new.dbf, ref.raster, field = "ALv1tcmsc")

# Save the rasterized output
writeRaster(rasterized, paste0("./raster/cologne/","ALv1tcmsc",".tif"), overwrite = TRUE)

plot(rasterized)


# use gdal to rasterize extremly fast

  writeRaster(ref.raster,paste0("./raster/cologne/",colnames(new.dbf)[2],".tif"),overwrite=TRUE)
  gdal_rasterize(src_datasource ="./shape_files/cologne/final_cologne_frame.shp",
                 dst_filename = paste0("./raster/cologne/",colnames(new.dbf)[2],".tif"),
                 a=colnames(new.dbf)[2])


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
write.dbf(new.dbf,"./shape_files/hagadera/final_hagadera_frame_new.dbf",factor2char = F)

# create attribute file
y <- cbind("Value"=1:5,replicate(4,rep(255,5)),levels(new.dbf[,12]))
write.table(y,"./Referenz_hag.txt",sep=",", quote=F,row.names=F)

# load reference raster
ref.raster <- raster("./raster/hag_referenz.tif")

# use gdal to rasterize extremly fast

for(i in 2:ncol(new.dbf)){
  writeRaster(ref.raster,paste0("./raster/hagadera/new/",colnames(new.dbf)[i],".tif"))
  gdal_rasterize(src_datasource ="./shape_files/hagadera/final_hagadera_frame_new.dbf",
                 dst_filename = paste0("./raster/hagadera/new/",colnames(new.dbf)[i],".tif"),
                 a=colnames(new.dbf)[i])
}







