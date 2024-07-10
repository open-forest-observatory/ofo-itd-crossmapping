# install.packages("neonUtilities")
# 
# install.packages("neonOS")
# 
# install.packages("terra")
# 
# install.packages("devtools")

#devtools::install_github("NEONScience/NEON-geolocation/geoNEON")

library(terra)

library(neonUtilities)

library(neonOS)

library(geoNEON)

library(sf)


options(stringsAsFactors=F)



# set working directory

# adapt directory path for your system

wd <- "C:/Users/nayani/mydata/other_projects/Open_forest_observatory/Data/"

setwd(wd)

NEON_poly = st_read(paste0(wd,"All_NEON_TOS_Plots_V10/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Polygons_V10.shp"))


veglist <- loadByProduct(dpID="DP1.10098.001", 
                         site="GRSM", 
                         package="basic", 
                         release="RELEASE-2023",
                         check.size = FALSE)


vegmap <- getLocTOS(veglist$vst_mappingandtagging, 
                    "vst_mappingandtagging")


veg <- joinTableNEON(veglist$vst_apparentindividual, 
                     vegmap, 
                     name1="vst_apparentindividual",
                     name2="vst_mappingandtagging")



veg$year = as.numeric(format(veg$date.x, "%Y"))

veg_new = veg[veg$year>= "2015",]

plot_names  = as.list(unique(veg_new$plotID))
plot_names = plot_names[-1]

#TEAK_list = data.frame()

#SOAP_list = data.frame()

#SJER_list = data.frame()

#NIWO_list = data.frame()

GRSM_list = data.frame()


stem_data_GRSM = data.frame()

for (i in 1:length(plot_names)){
  
  veg_new_plt = veg_new[veg_new$plotID==plot_names[[i]],c(3,4,32,33,46,49,76)]
  veg_new_plt = na.omit(veg_new_plt)
  
  if (length(veg_new_plt$siteID)>=1) {
    
    stem_data_GRSM = rbind(stem_data_GRSM,veg_new_plt)
    print(i)
    
    GRSM_list = rbind(GRSM_list,plot_names[[i]]) 
    colnames(GRSM_list) <- "GRSM_list"
    
    dev.new()
    
    symbols(veg_new_plt$adjEasting, 
            veg_new_plt$adjNorthing, 
            circles=veg_new_plt$stemDiameter/100/2, 
            inches=F, xlab="Easting", ylab="Northing", main=plot_names[[i]])
  }
    else{
      
      print("no heights have been recorded")
      
    }
   
  
 }
 

GRSM_select = NEON_poly[NEON_poly$plotID %in% GRSM_list$GRSM_list,]
 
RMNP_select = NEON_poly[NEON_poly$plotID %in% RMNP_list$RMNP_list,]

RMNP_select = RMNP_select[RMNP_select$subtype!="mammalGrid",]


NIWO_select = NEON_poly[NEON_poly$plotID %in% NIWO_list$NIWO_list,]

SJER_select = NEON_poly[NEON_poly$plotID %in% SJER_list$SJER_list,]

TEAK_select = NEON_poly[NEON_poly$plotID %in% TEAK_list$TEAK_plots,]

SOAP_select = NEON_poly[NEON_poly$plotID %in% SOAP_list$SOAP_list,]


st_write(GRSM_select, "GRSM_plots_with_Individual_tree_locations.shp")

st_write(SJER_select, "SJER_plots_with_Individual_tree_locations.shp")

st_write(TEAK_select, "TEAK_plots_with_Individual_tree_locations.shp")

st_write(SOAP_select, "SOAP_plots_with_Individual_tree_locations.shp")


st_write(RMNP_select, "RMNP_plots_with_Individual_tree_locations.shp")

