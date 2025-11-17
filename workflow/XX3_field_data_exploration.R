# Packages
library(terra)    # for CHM raster
library(sf)       # for tree points (GeoPackage)
library(ggplot2)  # for plotting

# ----------------- 1. Read data -----------------

# CHM raster
chm <- terra::rast("C:/Users/nayani/mydata/ofo-itd-crossmapping/chm-mesh/0026.tif")

# Individual tree points (GeoPackage)
# Replace 'trees' with your layer name if needed
trees <- sf::st_read("C:/Users/nayani/mydata/ofo-itd-crossmapping/trees/0026.gpkg")

# ----------------- 2. Align CRS -----------------

# Make sure tree points are in same CRS as CHM
trees <- sf::st_transform(trees, crs = terra::crs(chm))

# ----------------- 3. Prep data for ggplot -------

# CHM to data frame (x, y, height)
chm_df <- terra::as.data.frame(chm, xy = TRUE, na.rm = TRUE)
names(chm_df)[3] <- "chm_height"   # rename band to something clear

# Trees to data frame with coordinates
trees_coords <- sf::st_coordinates(trees)
trees_df <- cbind(sf::st_drop_geometry(trees), trees_coords)

# IMPORTANT: set this to your tree height column name
# e.g., if your column is "height_m" change below accordingly
height_col <- "height"   # change to your actual column if different
trees_df[[height_col]] <- as.numeric(trees_df[[height_col]])

# ----------------- 4. Plot: CHM + trees + labels -

ggplot() +
  # CHM background
  geom_raster(
    data = chm_df,
    aes(x = x, y = y, fill = chm_height)
  ) +
  scale_fill_viridis_c(name = "CHM height (m)", na.value = NA) +
  
  # Tree points
  geom_point(
    data = trees_df,
    aes(x = X, y = Y),
    color = "black",
    size  = 1.5
  ) +
  
  # Tree height labels
  geom_text(
    data = trees_df,
    aes(x = X, y = Y, label = round(.data[[height_col]], 1)),
    color         = "white",
    size          = 2.5,
    vjust         = -0.5,
    check_overlap = TRUE
  ) +
  
  coord_equal() +
  labs(
    x = "Easting (m)",
    y = "Northing (m)",
    title = "CHM with Individual Trees and Heights"
  ) +
  theme_minimal() +
  theme(
    # no grid
    panel.grid = element_blank(),
    
    # bold axis lines
    axis.line = element_line(colour = "black", linewidth = 0.8),
    
    # tick marks
    axis.ticks       = element_line(colour = "black", linewidth = 0.6),
    axis.ticks.length = unit(3, "pt")
  )
