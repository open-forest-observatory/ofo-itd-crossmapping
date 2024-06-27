# THIS SCRIPT IS OBSOLETE because it has been rewritten as a series of scripts from 07_make-chms.R
# to 20_predict-trees....R.

##title: "Tree delineations using drone based point cloud data"

##########################################

## In this module we use unmanned aerial system derived point cloud data to delineate individual tree locations.

#check the required libraries are available. If not install required libraries.

list.of.packages <- c("tidyverse","lidR","terra","raster","rgdal","ForestTools","RCSF","sp","sf","stars","rgl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Load the required libraries. We use several libraries that can help with loading, reading spatial data including raster and point cloud data, 
## analyse those data, and to visualize and write the spatial data back into a desired location.

library(tidyverse)
library(lidR)
library(terra)
library(raster)
library(rgdal)
library(ForestTools)
library(RCSF)
library(sp)
library(sf)
library(stars)
library(rgl)
rgl::setupKnitr(autoprint = TRUE)

################################################################
# set working environment
wd <- "C:/Users/nayani/mydata/other_projects/Open_forest_observatory/outputs/dsm_dtm"
setwd(wd)

dtm.files = list.files(wd, pattern = "dtm-ptcloud", recursive = TRUE, full.names = FALSE)


#x = strsplit(dtm.files[1], split = "_")




for (n in 2:length(dtm.files)) {
  # Initialize directories and files
  
  x = strsplit(dtm.files[n], split = "_")
  
  site_name <- paste(x[[1]][1],x[[1]][2],sep="_")
  
  # Initialize directories and files
  data_dir_out <- file.path(wd, site_name)
  
  # Create the directory if it does not exist
  if (!file.exists( data_dir_out)) {
    dir.create(data_dir_out, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  dtm <- raster(dtm.files[n])
  dsm <- raster(paste(paste(x[[1]][1],x[[1]][2],sep="_"),"dsm-ptcloud.tif", sep="_"))
  chm <- dsm - dtm
  
  # Define the output directory
  output_dir_1 <- file.path(data_dir_out,"ITD")
  
  
  # Create the directory if it does not exist
  if (!file.exists(output_dir_1)) {
    dir.create(output_dir_1, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  
  
  #####Parameter 1: CHM resolution 
  #1. fixed resolution: approximately highest resolution the sfm can produce
  
  # Case 1 : fixed resolution
  res_chm  <- res(chm) #[1] 0.1336096 0.1336096)
  extent_chm <- extent(chm)
  # Define the new resolution
  new_res <- 0.25  # Change this value to your desired resolution
  
  # Create a template raster with the new resolution
  extent_r <- extent(chm)
  ncol_new <- ceiling((extent_r@xmax - extent_r@xmin) / new_res)
  nrow_new <- ceiling((extent_r@ymax - extent_r@ymin) / new_res)
  resampled_chm <- raster(ncol = ncol_new, nrow = nrow_new)
  
  # Set the extent of the new raster to match the original raster
  extent(resampled_chm) <- extent_chm
  
  # Set the resolution of the new raster
  res(resampled_chm) <- new_res
  
  # Resample the original raster to the new raster
  resampled_chm <- resample(chm, resampled_chm, method = "bilinear")  # Use "bilinear" or "ngb"
  
  
  
  
  #####Parameter 2: Smoothed CHM and smoothing window/function (depends on species composition, but should not heavily smooth)
  #1. moving window size (3 × 3, 5 × 5, 7 × 7)
  #2. Gaussian sigma
  
  window_size <- c(3,5,7,9)
  raster_list <- list()
  
  for (i in seq_along(window_size)) {
    
    ws = matrix(1, window_size[i], window_size[i])
    
    chm_smooth <- raster::focal(resampled_chm, w = ws, mean)
    chm_smooth[raster::getValues(chm_smooth) < 0] <- 0
    # Store the raster in the list with a dynamically generated name
    raster_name <- paste("chm", window_size[i], sep="_")
    raster_list[[raster_name]] <- chm_smooth
  }
  
  
  #####individual tree detection
  
  #####Parameter 3: Window size to select the local maxima
  #1. fixed size (test what's the best fixed size: 3 x 3, 5 x 5, 7 x 7, 9 x 9)
  
  # Define the output directory
  output_dir <- file.path(output_dir_1,"local_max")
  
  
  # Create the directory if it does not exist
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  for (i in seq_along(window_size)) {
    print(i)
    
    for (r in seq_along(raster_list)) {
      print(r)
      
      chm1 = raster_list[[i]]
      ttops <- locate_trees(chm1, lmf(window_size[r]))
      
      # Check if tree tops are detected
      if (nrow(ttops) == 0) {
        stop("No tree tops were detected.")
      }
      
      
      # Convert to an sf object
      ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
      
      # Add X and Y coordinates to the attribute table
      
      ttops_sf = ttops %>%
        mutate(X = unlist(map(ttops$geometry,1)),
               Y = unlist(map(ttops$geometry,2)))
      
      # Check if conversion is successful
      if (!inherits(ttops_sf, "sf")) {
        stop("Conversion to sf object failed.")
      }
      
      # Define the output GeoPackage name
      output_gpkg <- paste("local_max_w_smoothed_chm_", window_size[i],window_size[r], sep="_")
      
      # Define the output GeoPackage path
      path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
      
      # Check if the GeoPackage file already exists
      if (file.exists(output_gpkg)) {
        cat("GeoPackage already exists. Skipping creation.\n")
      } else {
        # Save to GeoPackage
        st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
        
        # Check if the GeoPackage is created successfully
        if (file.exists(output_gpkg)) {
          cat("Tree tops have been saved successfully to the GeoPackage.\n")
        } else {
          cat("Failed to create the GeoPackage.\n")
        }
      }
      
      
      # plot( chm1, col = height.colors(50))
      # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
    }
  }
  
  
  
  
  #2. Variable size
  #1. Based on tree height- crown radius relationship  (Popescu and Wynne (2004)): # the equation "x^2*c + x*b + a" where x is the pixel height
  
  
  f <- function(x) {x * 0.1 + 3}
  heights <- seq(0,30,5)
  ws <- f(heights)
  plot(heights, ws, type = "l", ylim = c(0,6))
  
  
  # Define the output directory
  output_dir <- file.path(output_dir_1,"linear")
  
  
  # Create the directory if it does not exist
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  
  
  for (i in seq_along(window_size)) {
    print(i)
    
    
    chm1 = raster_list[[i]]
    ttops <- locate_trees(chm1, lmf(f))
    
    
    # Check if tree tops are detected
    if (nrow(ttops) == 0) {
      stop("No tree tops were detected.")
    }
    
    
    # Convert to an sf object
    ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
    
    # Add X and Y coordinates to the attribute table
    
    ttops_sf = ttops %>%
      mutate(X = unlist(map(ttops$geometry,1)),
             Y = unlist(map(ttops$geometry,2)))
    
    # Check if conversion is successful
    if (!inherits(ttops_sf, "sf")) {
      stop("Conversion to sf object failed.")
    }
    
    # Define the output GeoPackage name
    output_gpkg <- paste("smoothed_vwf_linear_", window_size[i], sep="_")
    
    # Define the output GeoPackage path
    path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
    
    # Check if the GeoPackage file already exists
    if (file.exists(output_gpkg)) {
      cat("GeoPackage already exists. Skipping creation.\n")
    } else {
      # Save to GeoPackage
      st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
      
      # Check if the GeoPackage is created successfully
      if (file.exists(output_gpkg)) {
        cat("Tree tops have been saved successfully to the GeoPackage.\n")
      } else {
        cat("Failed to create the GeoPackage.\n")
      }
    }
    
  }
  
  
  # Define the output directory
  output_dir <- file.path(output_dir_1,"exponetial")
  
  
  # Create the directory if it does not exist
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  
  
  f <- function(x) {
    y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
    y[x < 2] <- 3
    y[x > 20] <- 5
    return(y)
  }
  
  heights <- seq(-5,50,0.5)
  ws <- f(heights)
  plot(heights, ws, type = "l",  ylim = c(0,5))
  
  
  raster_list4 <- list()
  
  for (i in seq_along(window_size)) {
    print(i)
    
    
    chm1 = raster_list[[i]]
    ttops <- locate_trees(chm1, lmf(f))
    
    # Check if tree tops are detected
    if (nrow(ttops) == 0) {
      stop("No tree tops were detected.")
    }
    
    
    # Convert to an sf object
    ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
    
    # Add X and Y coordinates to the attribute table
    
    ttops_sf = ttops %>%
      mutate(X = unlist(map(ttops$geometry,1)),
             Y = unlist(map(ttops$geometry,2)))
    
    # Check if conversion is successful
    if (!inherits(ttops_sf, "sf")) {
      stop("Conversion to sf object failed.")
    }
    
    # Define the output GeoPackage name
    output_gpkg <- paste("smoothed_vwf_exponetial_", window_size[i], sep="_")
    
    # Define the output GeoPackage path
    path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
    
    # Check if the GeoPackage file already exists
    if (file.exists(output_gpkg)) {
      cat("GeoPackage already exists. Skipping creation.\n")
    } else {
      # Save to GeoPackage
      st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
      
      # Check if the GeoPackage is created successfully
      if (file.exists(output_gpkg)) {
        cat("Tree tops have been saved successfully to the GeoPackage.\n")
      } else {
        cat("Failed to create the GeoPackage.\n")
      }
    }
    
    #raster_list4[[raster_name]] <- ttops
    
    # plot( chm1, col = height.colors(50))
    # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
    
  }
  
  
  
  # Define the output directory
  output_dir <- file.path(output_dir_1,"quadratic")
  
  
  # Create the directory if it does not exist
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Directory created successfully.\n")
  } else {
    cat("Directory already exists.\n")
  }
  
  a <- 3 # was 0.3
  b <- 0.04 #0.04
  c <- 0.002 #0
  
  #f <- function(x){x^2*c + x*b + a} # window filter function to use in next step
  
  
  f <- function(x) {
    y <- x^2*c + x*b + a
    y[x < 2] <- 3
    y[x > 23] <- 5
    return(y)
  }
  
  
  heights <- seq(0,50,0.5)
  ws <- f(heights)
  plot(heights, ws, type = "l",  ylim = c(0,max(ws)))
  
  
  #raster_list4 <- list()
  
  for (i in seq_along(window_size)) {
    print(i)
    
    
    chm1 = raster_list[[i]]
    ttops <- locate_trees(chm1, lmf(f))
    
    # Check if tree tops are detected
    if (nrow(ttops) == 0) {
      stop("No tree tops were detected.")
    }
    
    
    # Convert to an sf object
    ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
    
    
    ttops_sf = ttops %>%
      mutate(X = unlist(map(ttops$geometry,1)),
             Y = unlist(map(ttops$geometry,2)))
    
    # Check if conversion is successful
    if (!inherits(ttops_sf, "sf")) {
      stop("Conversion to sf object failed.")
    }
    
    # Define the output GeoPackage name
    output_gpkg <- paste("smoothed_vwf_quadratic_", window_size[i], sep="_")
    
    # Define the output GeoPackage path
    path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
    
    # Check if the GeoPackage file already exists
    if (file.exists(output_gpkg)) {
      cat("GeoPackage already exists. Skipping creation.\n")
    } else {
      # Save to GeoPackage
      st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
      
      # Check if the GeoPackage is created successfully
      if (file.exists(output_gpkg)) {
        cat("Tree tops have been saved successfully to the GeoPackage.\n")
      } else {
        cat("Failed to create the GeoPackage.\n")
      }
    }
    
    #raster_list4[[raster_name]] <- ttops
    
    # plot( chm1, col = height.colors(50))
    # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
    
  }
  
  
  
}




# ######
# #dsm <- "path to dsm"
# #dtm  <- "Path to dtm" 
# dsm <- raster("set14-thin22_20240326T0520_dsm-ptcloud.tif")
# dtm <- raster("set14-thin22_20240326T0520_dtm-ptcloud.tif")
# chm <- dsm - dtm
# 
# # Define the output directory
# output_dir_1 <- paste0(wd,"ITD")
# 
# 
# # Create the directory if it does not exist
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Directory created successfully.\n")
# } else {
#   cat("Directory already exists.\n")
# }
# 
# 
# 
# #####Parameter 1: CHM resolution 
#   #1. fixed resolution: approximately highest resolution the sfm can produce
# 
# # Case 1 : fixed resolution
# res_chm  <- res(chm) #[1] 0.1336096 0.1336096)
# extent_chm <- extent(chm)
# # Define the new resolution
# new_res <- 0.25  # Change this value to your desired resolution
# 
# # Create a template raster with the new resolution
# extent_r <- extent(chm)
# ncol_new <- ceiling((extent_r@xmax - extent_r@xmin) / new_res)
# nrow_new <- ceiling((extent_r@ymax - extent_r@ymin) / new_res)
# resampled_chm <- raster(ncol = ncol_new, nrow = nrow_new)
# 
# # Set the extent of the new raster to match the original raster
# extent(resampled_chm) <- extent_chm
# 
# # Set the resolution of the new raster
# res(resampled_chm) <- new_res
# 
# # Resample the original raster to the new raster
# resampled_chm <- resample(chm, resampled_chm, method = "bilinear")  # Use "bilinear" or "ngb"
# 
# 
# 
# 
# #####Parameter 2: Smoothed CHM and smoothing window/function (depends on species composition, but should not heavily smooth)
#   #1. moving window size (3 × 3, 5 × 5, 7 × 7)
#   #2. Gaussian sigma
# 
# window_size <- c(3,5,7,9)
# raster_list <- list()
# 
# for (i in seq_along(window_size)) {
#   
#   ws = matrix(1, window_size[i], window_size[i])
#     
#   chm_smooth <- raster::focal(resampled_chm, w = ws, mean)
#   chm_smooth[raster::getValues(chm_smooth) < 0] <- 0
#   # Store the raster in the list with a dynamically generated name
#   raster_name <- paste("chm", window_size[i], sep="_")
#   raster_list[[raster_name]] <- chm_smooth
# }
# 
# 
# #####individual tree detection
# 
# #####Parameter 3: Window size to select the local maxima
#   #1. fixed size (test what's the best fixed size: 3 x 3, 5 x 5, 7 x 7, 9 x 9)
# 
# # Define the output directory
# output_dir <- paste0(wd,output_dir_1,"local_max")
# 
# 
# # Create the directory if it does not exist
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Directory created successfully.\n")
# } else {
#   cat("Directory already exists.\n")
# }
# 
# for (i in seq_along(window_size)) {
#   print(i)
#   
#   for (r in seq_along(raster_list)) {
#     print(r)
#   
#     chm1 = raster_list[[i]]
#     ttops <- locate_trees(chm1, lmf(window_size[r]))
#     
#     # Check if tree tops are detected
#     if (nrow(ttops) == 0) {
#       stop("No tree tops were detected.")
#     }
#     
#     
#     # Convert to an sf object
#     ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
#     
#     # Add X and Y coordinates to the attribute table
#     
#     ttops_sf = ttops %>%
#       mutate(X = unlist(map(ttops$geometry,1)),
#              Y = unlist(map(ttops$geometry,2)))
#     
#     # Check if conversion is successful
#     if (!inherits(ttops_sf, "sf")) {
#       stop("Conversion to sf object failed.")
#     }
#     
#     # Define the output GeoPackage name
#     output_gpkg <- paste("local_max_w_smoothed_chm_", window_size[i],window_size[r], sep="_")
#     
#     # Define the output GeoPackage path
#     path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
#     
#     # Check if the GeoPackage file already exists
#     if (file.exists(output_gpkg)) {
#       cat("GeoPackage already exists. Skipping creation.\n")
#     } else {
#       # Save to GeoPackage
#       st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
#       
#       # Check if the GeoPackage is created successfully
#       if (file.exists(output_gpkg)) {
#         cat("Tree tops have been saved successfully to the GeoPackage.\n")
#       } else {
#         cat("Failed to create the GeoPackage.\n")
#       }
#     }
#     
#   
#   # plot( chm1, col = height.colors(50))
#   # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
#   }
# }
# 
# 
# 
# 
#   #2. Variable size
#     #1. Based on tree height- crown radius relationship  (Popescu and Wynne (2004)): # the equation "x^2*c + x*b + a" where x is the pixel height
# 
# 
# f <- function(x) {x * 0.1 + 3}
# heights <- seq(0,30,5)
# ws <- f(heights)
# plot(heights, ws, type = "l", ylim = c(0,6))
# 
# 
# # Define the output directory
# output_dir <- paste0(wd,output_dir_1,"linear")
# 
# 
# # Create the directory if it does not exist
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Directory created successfully.\n")
# } else {
#   cat("Directory already exists.\n")
# }
# 
# 
# 
# for (i in seq_along(window_size)) {
#   print(i)
#   
#   
#   chm1 = raster_list[[i]]
#   ttops <- locate_trees(chm1, lmf(f))
#   
#   
#   # Check if tree tops are detected
#   if (nrow(ttops) == 0) {
#     stop("No tree tops were detected.")
#   }
#   
#   
#   # Convert to an sf object
#   ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
#   
#   # Add X and Y coordinates to the attribute table
#   
#   ttops_sf = ttops %>%
#     mutate(X = unlist(map(ttops$geometry,1)),
#            Y = unlist(map(ttops$geometry,2)))
#   
#   # Check if conversion is successful
#   if (!inherits(ttops_sf, "sf")) {
#     stop("Conversion to sf object failed.")
#   }
#   
#   # Define the output GeoPackage name
#   output_gpkg <- paste("smoothed_vwf_linear_", window_size[i], sep="_")
#   
#   # Define the output GeoPackage path
#   path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
#   
#   # Check if the GeoPackage file already exists
#   if (file.exists(output_gpkg)) {
#     cat("GeoPackage already exists. Skipping creation.\n")
#   } else {
#     # Save to GeoPackage
#     st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
#     
#     # Check if the GeoPackage is created successfully
#     if (file.exists(output_gpkg)) {
#       cat("Tree tops have been saved successfully to the GeoPackage.\n")
#     } else {
#       cat("Failed to create the GeoPackage.\n")
#     }
#   }
# 
# }
# 
# 
# # Define the output directory
# output_dir <- paste0(wd,output_dir_1,"exponetial")
# 
# 
# # Create the directory if it does not exist
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Directory created successfully.\n")
# } else {
#   cat("Directory already exists.\n")
# }
# 
# 
# 
# f <- function(x) {
#   y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
#   y[x < 2] <- 3
#   y[x > 20] <- 5
#   return(y)
# }
# 
# heights <- seq(-5,50,0.5)
# ws <- f(heights)
# plot(heights, ws, type = "l",  ylim = c(0,5))
# 
# 
# raster_list4 <- list()
# 
# for (i in seq_along(window_size)) {
#   print(i)
#   
#   
#   chm1 = raster_list[[i]]
#   ttops <- locate_trees(chm1, lmf(f))
#   
#   # Check if tree tops are detected
#   if (nrow(ttops) == 0) {
#     stop("No tree tops were detected.")
#   }
#   
#   
#   # Convert to an sf object
#   ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
#   
#   # Add X and Y coordinates to the attribute table
#   
#   ttops_sf = ttops %>%
#     mutate(X = unlist(map(ttops$geometry,1)),
#            Y = unlist(map(ttops$geometry,2)))
#   
#   # Check if conversion is successful
#   if (!inherits(ttops_sf, "sf")) {
#     stop("Conversion to sf object failed.")
#   }
#   
#   # Define the output GeoPackage name
#   output_gpkg <- paste("smoothed_vwf_exponetial_", window_size[i], sep="_")
#   
#   # Define the output GeoPackage path
#   path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
#   
#   # Check if the GeoPackage file already exists
#   if (file.exists(output_gpkg)) {
#     cat("GeoPackage already exists. Skipping creation.\n")
#   } else {
#     # Save to GeoPackage
#     st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
#     
#     # Check if the GeoPackage is created successfully
#     if (file.exists(output_gpkg)) {
#       cat("Tree tops have been saved successfully to the GeoPackage.\n")
#     } else {
#       cat("Failed to create the GeoPackage.\n")
#     }
#   }
#   
#   #raster_list4[[raster_name]] <- ttops
#   
#   # plot( chm1, col = height.colors(50))
#   # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
#   
# }
# 
# 
# 
# # Define the output directory
# output_dir <- paste0(wd,output_dir_1,"quadratic")
# 
# 
# # Create the directory if it does not exist
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
#   cat("Directory created successfully.\n")
# } else {
#   cat("Directory already exists.\n")
# }
# 
# a <- 3 # was 0.3
# b <- 0.04 #0.04
# c <- 0.002 #0
# 
# #f <- function(x){x^2*c + x*b + a} # window filter function to use in next step
# 
# 
# f <- function(x) {
#   y <- x^2*c + x*b + a
#   y[x < 2] <- 3
#   y[x > 23] <- 5
#   return(y)
# }
# 
# 
# heights <- seq(0,50,0.5)
# ws <- f(heights)
# plot(heights, ws, type = "l",  ylim = c(0,max(ws)))
# 
# 
# #raster_list4 <- list()
# 
# for (i in seq_along(window_size)) {
#   print(i)
#   
#   
#   chm1 = raster_list[[i]]
#   ttops <- locate_trees(chm1, lmf(f))
#   
#   # Check if tree tops are detected
#   if (nrow(ttops) == 0) {
#     stop("No tree tops were detected.")
#   }
#   
#   
#   # Convert to an sf object
#   ttops_sf <- st_as_sf(ttops, coords = c("X", "Y"), crs = st_crs(chm1))
#   
#   
#   ttops_sf = ttops %>%
#     mutate(X = unlist(map(ttops$geometry,1)),
#            Y = unlist(map(ttops$geometry,2)))
# 
#   # Check if conversion is successful
#   if (!inherits(ttops_sf, "sf")) {
#     stop("Conversion to sf object failed.")
#   }
#   
#   # Define the output GeoPackage name
#   output_gpkg <- paste("smoothed_vwf_quadratic_", window_size[i], sep="_")
#   
#   # Define the output GeoPackage path
#   path_to_package_save = file.path(output_dir, paste0(output_gpkg,".gpkg"))
#   
#   # Check if the GeoPackage file already exists
#   if (file.exists(output_gpkg)) {
#     cat("GeoPackage already exists. Skipping creation.\n")
#   } else {
#     # Save to GeoPackage
#     st_write(ttops_sf, path_to_package_save, layer = "tree_tops", driver = "GPKG", delete_dsn = TRUE, overwrite=TRUE)
#     
#     # Check if the GeoPackage is created successfully
#     if (file.exists(output_gpkg)) {
#       cat("Tree tops have been saved successfully to the GeoPackage.\n")
#     } else {
#       cat("Failed to create the GeoPackage.\n")
#     }
#   }
#   
#   #raster_list4[[raster_name]] <- ttops
#   
#   # plot( chm1, col = height.colors(50))
#   # plot(sf::st_geometry( ttops), add = TRUE, pch = 3)
#   
# }
# 
# 
# #2. h-cr quantiles (file:///C:/Users/nayani/mydata/other_projects/Open_forest_observatory/Data/IDT_literature/remotesensing-12-02115-v2.pdf)
# #####3. linear , non-linear h-cr relationship (linear / order polynomial quantile)



###### Need parameter boundary selection to adapt wide variety of forest categories with variable site conditions

########Accuracy assessment
  #Recall

    #r = TP/(TP + FN)

  #precision
    #p = TP/(TP + FP)


  #F-score
    #F = 2 *(r * p)/(r + p)

###This needs to be tested against different site conditions


################################################################




