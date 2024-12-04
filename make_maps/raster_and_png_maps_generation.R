setwd("D:/GitHub/active-learning-virtual-SVM/make_maps")

# # Install and load the terra package
# install.packages("terra")
library(terra)
library(viridis)
library(RColorBrewer)

city = "cologne"    # cologne or hagadera location
invariance = "scale"   # scale or shape invariance
model_prob = "multiclass"  # multiclass or binary problem

model_names = c(
  "SVM",
  "SVMSLUn",
  "VSVMSL",
  "VSVMSLUn",
  "VSVMSLvUn",
  "ALMSkTSVM",
  "ALMStSVM",
  "ALMStSLSVM",
  "ALMSksemiSVM",
  "ALMCLUkSVM"
)

# Define reusable function for processing a city's predictions
process_city <- function(city, model_prob, invariance, shape_file, ref_raster_path, model_names, pred_files, colors,colors_uncl, output_dir) {
  cat("processing",city,model_prob,invariance,"\n")
  
  # Load the reference raster and shapefile
  ref_raster <- rast(ref_raster_path)
  shp <- vect(shape_file)

  for (i in seq_along(pred_files)) {
    cat(paste0("Reading CSV... [", i, "/", length(pred_files), "]\n"))

    # Load predictions
    pred <- read.csv2(pred_files[i])[, 2]
    cat(paste0("found ",length(pred), " predicted pixels\n"))
    shp$pred <- as.numeric(as.factor(pred))

    # Rasterize the vector data
    rasterized <- rasterize(shp, ref_raster, field = "pred")

    # Save the rasterized output
    raster_output_path <- file.path(output_dir, "make_maps", "raster", city, paste0(model_names[i], "_", city, "_", model_prob, "_", invariance,".tif"))
    writeRaster(rasterized, raster_output_path, overwrite = TRUE)

    # Generate color levels
    info_levels <- levels(as.factor(pred))
    colors <- colors[info_levels]

    # Plot the raster
    png_output_path <- file.path(output_dir, "images", "maps", city, model_prob, invariance, paste0(model_names[i], "_", city, "_", model_prob, "_", invariance,".png"))
    png(png_output_path, width = 8020, height = 8020, res = 0)
    plot(rasterized, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
    dev.off()
    gc() 
  }
  
  cat("Printing reference raster...\n")

  # Plot reference raster
  png_output_path <- file.path(output_dir, "images", "maps", city, model_prob, invariance, paste0("ref_raster_", city, ".png"))
  png(png_output_path, width = 8020, height = 8020, res = 0)
  plot(ref_raster, col = colors_uncl, legend = FALSE, axes = FALSE, box = FALSE)

  # info_levels <- names(colors_uncl)
  # info_levels[1] = gsub("_", " ", info_levels[1])
  # info_levels[5] <- sub("_", "-", info_levels[5])
  # info_levels[5] <- gsub("_", " ", info_levels[5])
  # # Add a legend with the factor levels
  # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
  #        legend = info_levels,  # Use the factor levels from the data
  #        fill = colors_uncl ,    # Use the corresponding colors
  #        # title = "Classes",  # Title of the legend
  #        cex = 6
  # )         # Adjust the size of the legend text

  dev.off()
  
  rm(shp, rasterized, ref_raster, pred_files)  # Clean up
}


# Define city-specific parameters
output_dir <- "D:/GitHub/active-learning-virtual-SVM"

cities <- list(
  cologne = list(
    shape_file = "D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/cologne/cologne_frame.shp",
    ref_raster_path = "D:/tunc_oz/apply_model/accuracy_assessment_make_maps/raster/col_referenz.tif",

    pred_files = c(
      "./cologne/20241128SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128SVM_SLUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128VSVM_SL_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128VSVM_SLUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128VSVM_SLvUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128AL_MS+tSNE_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
      "./cologne/20241128AL_MCLU+kmeans_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv"
    ),
    colors = c(
      "shadow" = "#696969", "roofs" = "#8B0000",
      "bushes_trees" = "#228B22", "facade" = "#B8860B",
      "meadow" = "#7CFC00", "other_impervious_surface" = "#A9A9A9"
    ),
    colors_uncl = c(
      "shadow" = "#696969", "unclassified" = "#FFFFFF", "roofs" = "#8B0000",
      "bushes_trees" = "#228B22", "facade" = "#B8860B",
      "meadow" = "#7CFC00", "other_impervious_surface" = "#A9A9A9"
    )
  ),
  
  
  hagadera = list(
    shape_file = "D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/hagadera/hagaderarasteizeready.shp",
    ref_raster_path = "D:/tunc_oz/apply_model/accuracy_assessment_make_maps/raster/hag_referenz.tif",
    
    pred_files = c(
      "./hagadera/20241130SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130SVM_SLUn_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130VSVM_SL_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130VSVM_SLUn_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130VSVM_SLUn_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130AL_MS+kmeans+Train_SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130AL_MS+tSNE_SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      # "./hagadera/20241130AL_MS+tSNE+SL_SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv",
      "./hagadera/20241130AL_MCLU+kmeans_SVM_hagadera_multiclass_scale_100Size_20Unl_samples.csv"
    ),
    colors = c(
      "bare_soil" = "#D9A673", "shadow" = "#8B8B8B",
      "fence" = "#C2B280", "build_up_area" = "#8B4513",
      "vegetation" = "#9ACD32"
    ),
    colors_uncl = c(
      "bare_soil" = "#D9A673", "vegetation" = "#9ACD32",
      "shadow" = "#8B8B8B",
      "fence" = "#C2B280", 
      "build_up_area" = "#8B4513","unclassified" = "#FFFFFF"
    )
  )
)

# Process city
params <- cities[[city]]
process_city(
  city = city,
  model_prob = model_prob,
  invariance = invariance, 
  shape_file = params$shape_file,
  ref_raster_path = params$ref_raster_path,
  model_names = model_names,
  pred_files = params$pred_files,
  colors = params$colors,
  colors_uncl = params$colors_uncl,
  output_dir = output_dir
)

cat("City processed successfully!\n")

























































# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+++++++++++++++++++++++++++++++++++++++ Cologne +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# new.shp_col<-vect("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/cologne/cologne_frame.shp")
# nrow(new.shp_col)
# 
# model_name_col <- c("SVM_cologne_multiclass_scale",
#                     "SVMSLUn_cologne_multiclass_scale",
#                     "VSVMSL_cologne_multiclass_scale",
#                     "VSVMSLUn_cologne_multiclass_scale",
#                     "VSVMSLvUn_cologne_multiclass_scale",
#                     "ALMSkTSVM_cologne_multiclass_scale",
#                     "ALMStSVM_cologne_multiclass_scale",
#                     "ALMStSLSVM_cologne_multiclass_scale",
#                     "ALMSksemiSVM_cologne_multiclass_scale",
#                     "ALMCLUkSVM_cologne_multiclass_scale" )
# 
# preds_col <- c( "./cologne/20241128SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128SVM_SLUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128VSVM_SL_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128VSVM_SLUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128VSVM_SLvUn_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128AL_MS+kmeans+Train_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128AL_MS+tSNE_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128AL_MS+tSNE+SL_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128AL_MS+kmeans+semiSL_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv",
#                 "./cologne/20241128AL_MCLU+kmeans_SVM_cologne_multiclass_scale_120Size_20Unl_samples.csv" )
# 
# # Define colors for each label based on real-life colors
# colors <- c(
#   "shadow" = "#696969",                # Dim gray for shadows
#   "roofs" = "#8B0000",                # Dark red for roofs
#   "bushes_trees" = "#228B22",         # Forest green for bushes and trees
#   "facade" = "#B8860B",               # Dark goldenrod for facades
#   "meadow" = "#7CFC00",               # Lawn green for meadows
#   "other_impervious_surface" = "#A9A9A9"  # Dark gray for impervious surfaces like concrete
# )
# 
# # load reference raster
# ref.raster_col <- rast("./raster/col_referenz.tif")
# 
# for (i in seq_along(preds_col)) {
# 
#   cat(paste0("reading csv... [",i,"/",length(preds_col),"]\n"))
#   pred_col <- read.csv2(preds_col[i])[,2]
# 
#   cat(paste0(length(pred_col)," pixels predicted\n"))
#   new.shp_col$pred_col <- as.numeric(as.factor(pred_col))
# 
#   # if (crs(new.shp_col)==crs(ref.raster_col)){
#   #   cat("Geometry layer and raster layer have same CRS\n")
#   # }else{
#   #   crs(new.shp_col) <- crs(ref.raster_col)
#   # }
#   #
#   # if (ext(new.shp_col)==ext(ref.raster_col)){
#   #   cat("Geometry layer and raster layer have same extent\n")
#   # }else{
#   #   ext(new.shp_col)<-ext(ref.raster_col)
#   # }
# 
#   # Rasterize the vector data onto the raster template
#   rasterized_col <- rasterize(new.shp_col, ref.raster_col, field = "pred_col")
# 
#   # Save the rasterized_col output
#   writeRaster(rasterized_col, paste0("./make_maps/raster/cologne/",model_name_col[i],".tif"), overwrite = TRUE)
# 
#   # info_levels_col <- levels(as.factor(pred_col))
#   # colors <- viridis(length(info_levels_col))
#   # Match colors to factor levels
#   info_levels_col <- levels(as.factor(pred_col))
#   colors <- colors[info_levels_col]
# 
#   # Define the file path and open a PNG device
#   png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/cologne/",model_name_col[i],".png"), width = 1100, height = 1100, res = 0)
# 
#   # Plot the raster
#   plot(rasterized_col, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
# 
#   # info_levels_col[1] = gsub("_", "/", info_levels_col[1])
#   # info_levels_col[4] = gsub("_", " ", info_levels_col[4])
#   # # Add a legend with the factor levels
#   # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
#   #        legend = info_levels_col,  # Use the factor levels from the data
#   #        fill = colors ,    # Use the corresponding colors
#   #        # title = "Classes",  # Title of the legend
#   #        cex = 3
#   #        )         # Adjust the size of the legend text
# 
#   # Close the device to save the file
#   dev.off()
# 
# }
# 
# cat(paste0("printing reference raster...\n"))
# 
# # Add "unclassified" color if it is not already in colors
# if (!"unclassified" %in% names(colors)) {
#   colors <- c(
#     colors["shadow"],
#     "unclassified" = "#FFFFFF",           # White for unclassified areas
#     colors["roofs"],
#     colors[c("bushes_trees", "facade", "meadow", "other_impervious_surface")]
#   )}
# 
# # info_levels_col <- c("shadow",
# #                      "unclassified",
# #                      "roofs",
# #                      "bushes_trees",
# #                      "facade",
# #                      "meadow",
# #                      "other_impervious_surface")
# 
# # Define the file path and open a PNG device
# png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/cologne/ref_raster_col_mul.png"),
#     width = 2160, height = 2160, res = 0)
# 
# # Plot the raster
# plot(ref.raster_col, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
# 
# # info_levels_col[4] = gsub("_", "/", info_levels_col[4])
# # info_levels_col[7] = gsub("_", " ", info_levels_col[7])
# # # Add a legend with the factor levels
# # legend("bottomleft", #inset = c(0.111, 0.008),        # Position of the legend
# #        legend = info_levels_col,  # Use the factor levels from the data
# #        fill = colors ,    # Use the corresponding colors
# #        # title = "Classes",  # Title of the legend
# #        cex = 6
# # )         # Adjust the size of the legend text
# 
# # Close the device to save the file
# dev.off()
# 
# rm(new.shp_col,rasterized_col,ref.raster_col,preds_col)
# 
# #-------------------------------------------------------------------------------------------------------------------------
# #-------------------------------------- Hagadera -------------------------------------------------------------------------
# #-------------------------------------------------------------------------------------------------------------------------
# 
# new.shp_hag<-vect("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/shape_files/hagadera/hagaderarasteizeready.shp")
# nrow(new.shp_hag)
# 
# model_name_hag <- c("SVM_hag_mul_sc",
#                      "SVMSLUn_hag_mul_sc",
#                      "VSVMSL_hag_mul_sc",
#                      "VSVMSLUn_hag_mul_sc",
#                      "VSVMSLvUn_hag_mul_sc",
#                      "ALMSkTSVM_hag_mul_sc",
#                      "ALMStSVM_hag_mul_sc",
#                      "ALMStSLSVM_hag_mul_sc",
#                      "ALMSksemiSVM_hag_mul_sc",
#                      "ALMCLUkSVM_hag_mul_sc")
# 
# preds_hag <-c( "./hagadera/20240928SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928SVM_SLUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928VSVM_SL_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928VSVM_SLUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928VSVM_SLvUn_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928AL_MS+kmeans+Train_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928AL_MS+tSNE_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928AL_MS+tSNE+SL_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928AL_MS+kmeans+semiSL_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv " ,
#                "./hagadera/20240928AL_MCLU+kmeans_SVM_hagadera_multiclass_scale_40Size_20Unl_samples.csv" )
# 
# # for (i in seq_along(preds_hag)) { 
# #   path_pred_hag <- preds_hag[i]
# #   cat(paste0("reading csv [",i,"/",length(preds_hag),"]\n"))  # Print index and file path
# # }
# 
# # Define colors for each label based on real-life colors and a YlOrBr-inspired palette
# colors <- c(
#   "bare_soil" = "#D9A673",        # Light brown to represent exposed soil
#   "shadow" = "#8B8B8B",           # Gray for shadowed areas         
#   "fence" = "#C2B280",            # Khaki for fences, representing a neutral weathered look
#   "build_up_area" = "#8B4513",    # Saddle brown to represent built-up areas and structures
#   "vegetation" = "#9ACD32"        # Yellow-green to represent vegetation
# )
# 
# # load reference raster
# ref.raster_hag <- rast("./raster/hag_referenz.tif")
# 
# for (i in seq_along(preds_hag)) { 
#   
#   cat(paste0("reading csv... [",i,"/",length(preds_hag),"]\n"))
#   pred_hag <- read.csv2(preds_hag[i])[,2]
#   
#   cat(paste0(length(pred_hag)," pixels predicted\n"))
#   new.shp_hag$pred_hag <- as.numeric(as.factor(pred_hag))
#   
#   # if (crs(new.shp_hag)==crs(ref.raster_hag)){
#   #   cat("Geometry layer and raster layer have same CRS\n")
#   # }else{
#   #   crs(new.shp_hag) <- crs(ref.raster_hag)
#   # }
#   # 
#   # if (ext(new.shp_hag)==ext(ref.raster_hag)){
#   #   cat("Geometry layer and raster layer have same extent\n")
#   # }else{
#   #   ext(new.shp_hag)<-ext(ref.raster_hag)
#   # }
# 
#   # Rasterize the vector data onto the raster template
#   rasterized_hag <- rasterize(new.shp_hag, ref.raster_hag, field = "pred_hag")
#   
#   # Save the rasterized_hag output
#   writeRaster(rasterized_hag, paste0("./raster/hagadera/",model_name_hag[i],".tif"), overwrite = TRUE)
#   
#   # info_levels_hag <- levels(as.factor(pred_hag))
#   # colors <- brewer.pal(length(info_levels_hag), "YlOrBr")
#   # Match colors to factor levels
#   info_levels_hag <- levels(as.factor(pred_hag))
#   colors <- colors[info_levels_hag]
#   
#   # Define the file path and open a PNG device
#   png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/hagadera/",model_name_hag[i],".png"), width = 2160, height = 2160, res = 0)
#   
#   # Plot the raster
#   plot(rasterized_hag, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
#   
#   # info_levels_hag[1] = gsub("_", "/", info_levels_hag[1])
#   # info_levels_hag[2] <- sub("_", "-", info_levels_hag[2])
#   # info_levels_hag[2] <- gsub("_", " ", info_levels_hag[2])  
#   # # Add a legend with the factor levels
#   # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
#   #        legend = info_levels_hag,  # Use the factor levels from the data
#   #        fill = colors ,    # Use the corresponding colors
#   #        # title = "Classes",  # Title of the legend
#   #        cex = 3
#   # )         # Adjust the size of the legend text
#   
#   # Close the device to save the file
#   dev.off()
# 
# }
# 
# cat(paste0("printing reference raster...\n"))
# 
# # # Add "unclassified" color if it is not already in colors
# # if (!"unclassified" %in% names(colors)) {
# #   colors <- c(
# #     colors["bare_soil"], 
# #     colors["vegetation"], 
# #     colors[c("fence", "shadow", "build_up_area")],
# #     "unclassified" = "#FFFFFF"         # White for unclassified areas]
# #   )
# # }
# 
# info_levels_hag <- c("bare_soil",
#                      "shadow",
#                      "fence",
#                      "build_up_area",
#                      "vegetation" #, "unclassified"
#                      )
# 
# # Define the file path and open a PNG device
# png(paste0("D:/GitHub/active-learning-virtual-SVM/images/maps/hagadera/ref_raster_hag_mul.png"), width = 2160, height = 2160, res = 0)
# 
# # Plot the raster
# plot(ref.raster_hag, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
# 
# # info_levels_hag[1] = gsub("_", " ", info_levels_hag[1])
# # info_levels_hag[5] <- sub("_", "-", info_levels_hag[5])
# # info_levels_hag[5] <- gsub("_", " ", info_levels_hag[5])  
# # # Add a legend with the factor levels
# # legend("topright", #inset = c(0.111, 0.008),        # Position of the legend
# #        legend = info_levels_hag,  # Use the factor levels from the data
# #        fill = colors ,    # Use the corresponding colors
# #        # title = "Classes",  # Title of the legend
# #        cex = 6
# # )         # Adjust the size of the legend text
# 
# # Close the device to save the file
# dev.off()
# 
# # rast_prova = rast("D:/tunc_oz/apply_model/accuracy_assessment_make_maps/raster/hag_mul_landscape_final.tif")
# # plot(rast_prova)
