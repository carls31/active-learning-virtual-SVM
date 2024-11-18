setwd("D:/GitHub/active-learning-virtual-SVM/make_maps")

# # Install and load the terra package
# install.packages("terra")
library(terra)
library(viridis)
library(RColorBrewer)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++ Cologne +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

new.shp_col<-vect("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/cologne/cologne_frame.shp")
nrow(new.shp_col)

model_name_col <- c("SVM_col_mul_sc",
                    "SVMSLUn_col_mul_sc",
                    "VSVMSL_col_mul_sc",
                    "VSVMSLUn_col_mul_sc",
                    "VSVMSLvUn_col_mul_sc",
                    "ALMSkTSVM_col_mul_sc",
                    "ALMStSVM_col_mul_sc",
                    "ALMStSLSVM_col_mul_sc",
                    "ALMSksemiSVM_col_mul_sc",
                    "ALMCLUkSVM_col_mul_sc" )

preds_col <- c( "./cologne/20240926SVM_cologne_multiclass_shape_48Size_20Unl_samples.csv",
                "./cologne/20240928SVM_SLUn_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928VSVM_SL_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928VSVM_SLUn_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928VSVM_SLvUn_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928AL_MS+tSNE_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv",
                "./cologne/20240928AL_MCLU+kmeans_SVM_cologne_multiclass_scale_48Size_20Unl_samples.csv" )

# Define colors for each label based on real-life colors
colors <- c(
  "shadow" = "#696969",                # Dim gray for shadows
  "roofs" = "#8B0000",                # Dark red for roofs
  "bushes_trees" = "#228B22",         # Forest green for bushes and trees
  "facade" = "#B8860B",               # Dark goldenrod for facades
  "meadow" = "#7CFC00",               # Lawn green for meadows
  "other_impervious_surface" = "#A9A9A9"  # Dark gray for impervious surfaces like concrete
)

# load reference raster
ref.raster_col <- rast("./raster/col_referenz.tif")

for (i in seq_along(preds_col)) { 

  cat(paste0("reading csv... [",i,"/",length(preds_col),"]\n"))
  pred_col <- read.csv2(preds_col[i])[,2]
  
  cat(paste0(length(pred_col)," pixels predicted\n"))
  new.shp_col$pred_col <- as.numeric(as.factor(pred_col))

  # if (crs(new.shp_col)==crs(ref.raster_col)){
  #   cat("Geometry layer and raster layer have same CRS\n")
  # }else{
  #   crs(new.shp_col) <- crs(ref.raster_col)
  # }
  # 
  # if (ext(new.shp_col)==ext(ref.raster_col)){
  #   cat("Geometry layer and raster layer have same extent\n")
  # }else{
  #   ext(new.shp_col)<-ext(ref.raster_col)
  # }

  # Rasterize the vector data onto the raster template
  rasterized_col <- rasterize(new.shp_col, ref.raster_col, field = "pred_col")
  
  # Save the rasterized_col output
  writeRaster(rasterized_col, paste0("./raster/cologne/",model_name_col[i],".tif"), overwrite = TRUE)
  
  # info_levels_col <- levels(as.factor(pred_col))
  # colors <- viridis(length(info_levels_col))
  # Match colors to factor levels
  info_levels_col <- levels(as.factor(pred_col))
  colors <- colors[info_levels_col]
  
  # Define the file path and open a PNG device
  png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/cologne/",model_name_col[i],".png"), width = 1100, height = 1100, res = 0)
  
  # Plot the raster
  plot(rasterized_col, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
  
  # info_levels_col[1] = gsub("_", "/", info_levels_col[1])
  # info_levels_col[4] = gsub("_", " ", info_levels_col[4])
  # # Add a legend with the factor levels
  # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
  #        legend = info_levels_col,  # Use the factor levels from the data
  #        fill = colors ,    # Use the corresponding colors
  #        # title = "Classes",  # Title of the legend
  #        cex = 3
  #        )         # Adjust the size of the legend text
  
  # Close the device to save the file
  dev.off()

}

cat(paste0("printing reference raster...\n"))

# Add "unclassified" color if it is not already in colors
if (!"unclassified" %in% names(colors)) {
  colors <- c(
    colors["shadow"], 
    "unclassified" = "#FFFFFF",           # White for unclassified areas
    colors["roofs"], 
    colors[c("bushes_trees", "facade", "meadow", "other_impervious_surface")]
  )}

info_levels_col <- c("shadow", 
                     "unclassified",
                     "roofs",
                     "bushes_trees",
                     "facade",
                     "meadow",
                     "other_impervious_surface")

# Define the file path and open a PNG device
png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/cologne/ref_raster_col_mul.png"), 
    width = 2160, height = 2160, res = 0)

# Plot the raster
plot(ref.raster_col, col = colors, legend = FALSE, axes = FALSE, box = FALSE)

# info_levels_col[4] = gsub("_", "/", info_levels_col[4])
# info_levels_col[7] = gsub("_", " ", info_levels_col[7])
# # Add a legend with the factor levels
# legend("bottomleft", #inset = c(0.111, 0.008),        # Position of the legend
#        legend = info_levels_col,  # Use the factor levels from the data
#        fill = colors ,    # Use the corresponding colors
#        # title = "Classes",  # Title of the legend
#        cex = 6
# )         # Adjust the size of the legend text

# Close the device to save the file
dev.off()

rm(new.shp_col,rasterized_col,ref.raster_col,preds_col)

#-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------- Hagadera -------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

new.shp_hag<-vect("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/hagadera/hagaderarasteizeready.shp")
nrow(new.shp_hag)

model_name_hag <- c("SVM_hag_mul_sc",
                     "SVMSLUn_hag_mul_sc",
                     "VSVMSL_hag_mul_sc",
                     "VSVMSLUn_hag_mul_sc",
                     "VSVMSLvUn_hag_mul_sc",
                     "ALMSkTSVM_hag_mul_sc",
                     "ALMStSVM_hag_mul_sc",
                     "ALMStSLSVM_hag_mul_sc",
                     "ALMSksemiSVM_hag_mul_sc",
                     "ALMCLUkSVM_hag_mul_sc")

preds_hag <-c( "./hagadera/20240928SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928SVM_SLUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928VSVM_SL_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928VSVM_SLUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928VSVM_SLvUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928AL_MS+kmeans+Train_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928AL_MS+tSNE_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928AL_MS+tSNE+SL_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
               "./hagadera/20240928AL_MCLU+kmeans_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv" )

# for (i in seq_along(preds_hag)) { 
#   path_pred_hag <- preds_hag[i]
#   cat(paste0("reading csv [",i,"/",length(preds_hag),"]\n"))  # Print index and file path
# }

# Define colors for each label based on real-life colors and a YlOrBr-inspired palette
colors <- c(
  "bare_soil" = "#D9A673",        # Light brown to represent exposed soil
  "shadow" = "#8B8B8B",           # Gray for shadowed areas         
  "fence" = "#C2B280",            # Khaki for fences, representing a neutral weathered look
  "build_up_area" = "#8B4513",    # Saddle brown to represent built-up areas and structures
  "vegetation" = "#9ACD32"        # Yellow-green to represent vegetation
)

# load reference raster
ref.raster_hag <- rast("./raster/hag_referenz.tif")

for (i in seq_along(preds_hag)) { 
  
  cat(paste0("reading csv... [",i,"/",length(preds_hag),"]\n"))
  pred_hag <- read.csv2(preds_hag[i])[,2]
  
  cat(paste0(length(pred_hag)," pixels predicted\n"))
  new.shp_hag$pred_hag <- as.numeric(as.factor(pred_hag))
  
  # if (crs(new.shp_hag)==crs(ref.raster_hag)){
  #   cat("Geometry layer and raster layer have same CRS\n")
  # }else{
  #   crs(new.shp_hag) <- crs(ref.raster_hag)
  # }
  # 
  # if (ext(new.shp_hag)==ext(ref.raster_hag)){
  #   cat("Geometry layer and raster layer have same extent\n")
  # }else{
  #   ext(new.shp_hag)<-ext(ref.raster_hag)
  # }

  # Rasterize the vector data onto the raster template
  rasterized_hag <- rasterize(new.shp_hag, ref.raster_hag, field = "pred_hag")
  
  # Save the rasterized_hag output
  writeRaster(rasterized_hag, paste0("./raster/hagadera/",model_name_hag[i],".tif"), overwrite = TRUE)
  
  # info_levels_hag <- levels(as.factor(pred_hag))
  # colors <- brewer.pal(length(info_levels_hag), "YlOrBr")
  # Match colors to factor levels
  info_levels_hag <- levels(as.factor(pred_hag))
  colors <- colors[info_levels_hag]
  
  # Define the file path and open a PNG device
  png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/hagadera/",model_name_hag[i],".png"), width = 2160, height = 2160, res = 0)
  
  # Plot the raster
  plot(rasterized_hag, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
  
  # info_levels_hag[1] = gsub("_", "/", info_levels_hag[1])
  # info_levels_hag[2] <- sub("_", "-", info_levels_hag[2])
  # info_levels_hag[2] <- gsub("_", " ", info_levels_hag[2])  
  # # Add a legend with the factor levels
  # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
  #        legend = info_levels_hag,  # Use the factor levels from the data
  #        fill = colors ,    # Use the corresponding colors
  #        # title = "Classes",  # Title of the legend
  #        cex = 3
  # )         # Adjust the size of the legend text
  
  # Close the device to save the file
  dev.off()

}

cat(paste0("printing reference raster...\n"))

# # Add "unclassified" color if it is not already in colors
# if (!"unclassified" %in% names(colors)) {
#   colors <- c(
#     colors["bare_soil"], 
#     colors["vegetation"], 
#     colors[c("fence", "shadow", "build_up_area")],
#     "unclassified" = "#FFFFFF"         # White for unclassified areas]
#   )
# }

info_levels_hag <- c("bare_soil",
                     "shadow",
                     "fence",
                     "build_up_area",
                     "vegetation" #, "unclassified"
                     )

# Define the file path and open a PNG device
png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/hagadera/ref_raster_hag_mul.png"), width = 2160, height = 2160, res = 0)

# Plot the raster
plot(ref.raster_hag, col = colors, legend = FALSE, axes = FALSE, box = FALSE)

# info_levels_hag[1] = gsub("_", " ", info_levels_hag[1])
# info_levels_hag[5] <- sub("_", "-", info_levels_hag[5])
# info_levels_hag[5] <- gsub("_", " ", info_levels_hag[5])  
# # Add a legend with the factor levels
# legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
#        legend = info_levels_hag,  # Use the factor levels from the data
#        fill = colors ,    # Use the corresponding colors
#        # title = "Classes",  # Title of the legend
#        cex = 6
# )         # Adjust the size of the legend text

# Close the device to save the file
dev.off()

# rast_prova = rast("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/raster/hag_mul_landscape_final.tif")
# plot(rast_prova)
