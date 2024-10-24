setwd("D:/GitHub/active-learning-virtual-SVM/thematic_maps")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++ Cologne +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # Install and load the terra package
# install.packages("terra")
library(terra)
library(viridis)

new.shp<-vect("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/cologne/cologne_frame.shp")
nrow(new.shp)

# model_name <- "VSVM_SLUn_col_mul_sc"
# model_name <- "SVM_col_mul_sh"
# model_name <- "ALMCLUkSVM_col_mul_sc"
# model_name <- "ALMSksemiSVM_col_mul_sc"
# model_name <- "ALMSkTSVM_col_mul_sc"
# model_name <- "ALMStSVM_col_mul_sc"
# model_name <- "ALMSSLSVM_col_mul_sc"
# model_name <- "SVM_col_mul_sc"
# model_name <- "SVMSLUn_col_mul_sc"
# model_name <- "VSVMSL_col_mul_sc"
model_name <- "VSVMSLUn_col_mul_sc"

# new.shp$ALv1tcmsc <- new.dbf$ALv1tcmsc
# ALv1tcmsc <-read.csv2("./cologne/20240928VSVM_SLUn_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240926SVM_cologne_multiclass_shape_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928AL_MCLU+kmeans_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928AL_MS+tSNE_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928SVM_SLUn_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
# ALv1tcmsc <-read.csv2("./cologne/20240928VSVM_SL_cologne_multiclass_scale_48Size_20Unl_samples.csv")[,2]
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

# Rasterize the vector data onto the raster template
rasterized <- rasterize(new.shp, ref.raster, field = "ALv1tcmsc")

# Save the rasterized output
writeRaster(rasterized, paste0("./raster/cologne/",model_name,".tif"), overwrite = TRUE)

info_levels <- levels(as.factor(ALv1tcmsc))
colors <- viridis(length(info_levels))

plot(rasterized, col = colors)
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


