# library(foreign)
# library(gdalUtils)
# library(stringr)
# library(raster)
# setwd("/home/data1/Lorenzo/tunc_oz/thematic_maps")
setwd("/home/data1/Lorenzo/GitHub/active-learning-virtual-SVM/thematic_maps")

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
# dbf <- vect("./shape_files/cologne_prova/cologne_frame.dbf")

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  


#### get necessary info and write new dbf file
############## col and hag
# new.dbf <- data.frame("reference"=dbf[,19])

####+++++++++++++++ here we need to include col and hag in one dbf file

  # append a column for every csv file to the dbf and write it out
  # new.dbf <- cbind(new.dbf,name=read.csv2("./shape_files/cologne_prova/20240928AL_MS+tSNE_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2])
  
  #create column name
  # colnames(new.dbf)[2] <- "ALv1tcmsc"

  # new.dbf$ALv1tcmsc <- as.numeric(as.factor(new.dbf$ALv1tcmsc))

###### be careful!!! a .dbf file cannot have attribute names longer than 10 characters.
# it will be cut after the 10th character without a warning when writing  
  # write.dbf(new.dbf,"./shape_files/cologne_prova/cologne_label_prova.dbf",factor2char = F)

  # new.dbf<-read.dbf("./shape_files/cologne_prova/cologne_label_prova.dbf")

  new.shp<-vect("./shape_files/cologne/cologne_frame.shp")
  # nrow(new.dbf)
  nrow(new.shp)
  
  model_name <- "VSVM_SLUn_col_mul_sc"
  # new.shp$ALv1tcmsc <- new.dbf$ALv1tcmsc
  ALv1tcmsc <-read.csv2("./cologne/20240928VSVM_SLUn_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
  length(ALv1tcmsc)
  new.shp$ALv1tcmsc <- as.numeric(as.factor(ALv1tcmsc))
  
  # load reference raster
ref.raster <- rast("./raster/col_referenz.tif")

crs(new.shp)  # Check the CRS of the SpatVector
crs(ref.raster)  # Check the CRS of the reference raster
# crs(new.dbf) <- crs(ref.raster)
ext(new.shp)
ext(ref.raster)
# ext(new.dbf)<-ext(ref.raster)


# If they are different, project new.dbf to match ref.raster
# new.dbf <- project(new.dbf, crs(ref.raster))

# Rasterize the vector data onto the raster template
rasterized <- rasterize(new.shp, ref.raster, field = "ALv1tcmsc")

# Save the rasterized output
writeRaster(rasterized, paste0("./raster/cologne/",model_name,".tif"), overwrite = TRUE)

info_levels <- levels(as.factor(ALv1tcmsc))
colors <- viridis(length(info_levels))

# > colors
# [1] "#440154FF" "#414487FF"
# [3] "#2A788EFF" "#22A884FF"
# [5] "#7AD151FF" "#FDE725FF"

plot(rasterized)
# Add a legend with the factor levels
legend("topright",        # Position of the legend
       legend = info_levels,  # Use the factor levels from the data
       fill = colors,     # Use the corresponding colors
       # title = "Classes",  # Title of the legend
       cex = 0.7)         # Adjust the size of the legend text

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#------------------------------------------------------------------------------------------------------------
#-------------------------- Hagadera ----------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

setwd("/home/data1/Lorenzo/GitHub/active-learning-virtual-SVM/thematic_maps")

new.shp<-vect("./shape_files/hagadera/hagaderarasteizeready.shp")
nrow(new.shp)

model_name <- "AL_MCLU+kmeans_SVM_hag_bin_sc"
model_pred <-read.csv2("./hagadera/20240928AL_MCLU+kmeans_SVM_hagadera_binary_scale_14Size_20Unl_samples.csv")[,2]
length(model_pred)
new.shp$model_pred <- as.numeric(as.factor(model_pred))

# load reference raster
ref.raster <- rast("./raster/hag_referenz.tif")

crs(new.shp)  # Check the CRS of the SpatVector
crs(ref.raster)  # Check the CRS of the reference raster

ext(new.shp)
ext(ref.raster)
# ext(new.shp)<-ext(ref.raster)
ext(ref.raster)<-ext(new.shp)
# Rasterize the vector data onto the raster template
rasterized <- rasterize(new.shp, ref.raster, field = "model_pred")

# Save the rasterized output
writeRaster(rasterized, paste0("./raster/hagadera/",model_name,".tif"), overwrite = TRUE)

plot(rasterized)


