#load libraries ####
library(raster)
library(sf)
library(ggplot2)
library(plyr)
library(dplyr)


# water protected areas shapefiles ####
p<-list(st_read("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/waterbodies/twsg_fuer_grundwasser_uferfiltrat/twsg_fuer_grundwasser_uferfiltrat.shp"),
        st_read ("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/waterbodies/twsg_fuer_talsperren/twsg_fuer_talsperren.shp"))

cs<-sf::st_read("D:/bestma (vs-grp01.zih.tu-dresden.de) (Y)/Vivi/data_encrypted/study_area/Mulde_delimitation_and_grid/Mulde_EPSG3035.shp")

cs <- st_transform(cs, crs=3035)

for ( x in seq_along(p)) {
  
  p[[x]] <- st_transform(p[[x]], crs(cs))
  
  p[[x]] <- st_buffer(p[[x]], 1) # buffer to avoid topology exception error
  
  p[[x]] <- st_crop(p[[x]], cs)
  
  p[[x]] <- p[[x]][,1]
  
}

# water bodies shapefile
p <- list(st_read("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/waterbodies/Gewaessernetz Sachsen/Gewaessernetz Sachsen/SHAPE_1/DATEN/FLIESSGEWAESSER_SN_ARBEITSSTAND.shp"),
          st_read("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/waterbodies/Gewaessernetz Sachsen/Gewaessernetz Sachsen/SHAPE_1/DATEN/STANDGEWAESSER_SN_ARBEITSSTAND.shp"))

cs <- sf::st_read("D:/bestma (vs-grp01.zih.tu-dresden.de) (Y)/Vivi/data_encrypted/study_area/Mulde_delimitation_and_grid/Mulde_EPSG3035.shp")

cs <- st_transform(cs, crs=3035)

cs <- st_buffer(cs, 20)

for ( x in seq_along(p)) {
  
  p[[x]] <- st_transform(p[[x]], crs(cs))
  
  p[[x]] <- st_buffer(p[[x]], 1) # buffer to avoid topology exception error
  
  p[[x]] <- st_crop(p[[x]], cs)
  
  p[[x]] <- p[[x]][,1]
  
}


# field lies in a water protection areas? 
inv<-st_read("C:/Users/Vivi A.S/Documents/efa/2019/invekos_original2019/join2019.shp") %>%
  st_transform(cs, crs=3035)

head(inv)
nrow(inv) # 67336
unique(inv$AUM_INFO)

# create a column to idenfify fields with permanent grassland
inv[grep(pattern="GL", x=inv$AUM_INFO),"GL"] <- "T"
inv[is.na(inv$GL),"GL"] <- "F"
# remove permanent grassland fields ####
inv2<-filter(inv, GL == "F")
nrow(inv2) #57570

inv<-inv2 # to calculate the buffers 

inv$field_id <- row.names(inv)

inv <- st_buffer(inv, 1)

for ( x in seq_along(p)) {
  
  p[[x]] <- st_intersection(p[[x]], inv)
  
}

inv$water_prot <- F

inv$water_prot[inv$field_id %in% p[[1]]$field_id] <- T
inv$water_prot[inv$field_id %in% p[[2]]$field_id] <- T
p.field <- inv[,c("field_id", "water_prot")]
p.field <- p.field[!is.na(p.field$field_id),]
st_geometry(p.field) <- NULL
head(p.field)

# merge back with original field invekos (not the buffers)
head(inv2)
inv2$field_id <- row.names(inv2)
inv3<-merge(inv2,p.field, by="field_id")
nrow(inv3)
head(inv3)

inv<-inv3

### presence of water bodies within a buffer of 20 meters?

inv$rowname <- row.names(inv)
inv.buf <- st_buffer(inv, 20)

for ( x in seq_along(p)) {
  
  p[[x]] <- st_intersection(p[[x]], inv.buf)
  
}

inv$water_body <- F

inv$water_body[inv$rowname %in% p[[1]]$rowname] <- T
inv$water_body[inv$rowname %in% p[[2]]$rowname] <- T

p.field <- inv[,c("field_id", "water_body")]

p.field <- p.field[!is.na(p.field$field_id),]
st_geometry(p.field) <- NULL
head(p.field)

head(inv,10)
nrow(inv)


# extract erosion risk ####
# extract erosion risk KLSR - in9 
library(terra)
erosion_KLSR_proj <-terra::rast("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/idadata_erosion_saxony/DATEN/EROSION_KLSR_proj.tif")
inv$erosion_KLSR = terra::extract(x=erosion_KLSR_proj, y=vect(inv), na.rm=T, method="simple", weights=T, fun="mean", touches=T)[,2]
head(inv)

# extract WIND KV erosion risk - in9 
erosion_WIND_KV_proj <-terra::rast("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/idadata_erosion_saxony/DATEN/erosion_WIND_KV_proj.tif")
inv$erosion_WIND_KV = terra::extract(x=erosion_WIND_KV_proj, y=vect(inv), na.rm=T, method="simple", weights=T, fun="mean", touches=T)[,2]
head(inv)


# calculate field perimeter ####
library(lwgeom)
inv$perimeter<-st_perimeter(inv)
head(inv) #calculated in meters! 

#Polygon shape properties ####
#convert field area in meters 
library(units)
inv$fieldarea_ha<-set_units(st_area(inv),ha)
inv$fieldarea_m<-st_area(inv)
head(inv)

# richardson compactness index ####
inv$rich_compact<-(2*sqrt(pi*inv$fieldarea_m))/inv$perimeter 
head(inv)
inv4<-inv
nrow(inv4)
head(inv4)

# Disadvantage areas - AZL ####
#efa9
# disadvantage areas shapefile 
p <- st_read("C:/Users/Vivi A.S/Documents/efa/2019/invekos_original2019/free_invekos/FB_mulde.shp") %>%
  select(FB_AZL) # select only the column of ANC areas 
head(p)
nrow(p)

inv52<-st_join(inv4,p,largest = TRUE)
nrow(inv52)

inv5<-inv52
head(inv5)

# calculate farm centroid distances ####
#farms <- inv5 %>% group_by(ANY_NR) %>% summarise(farmarea=sum(fieldarea_ha)) %>%  st_centroid()

# calculate the centroid of each field
#inc = st_centroid(inv5)

# for each field, calculate distance from centroid of farm
#for ( i in seq_along(inc$ANY_NR)) {
#  inc$dist[i] = st_distance(inc[i,], farms[farms$ANY_NR==inc$ANY_NR[i],])
#}  #started 17:36 - 20:30
#head(inc)

#st_write(inc, "C:/Users/Vivi A.S/Documents/efa/2019/Field_dist_2019.shp") #### waiting here 

inc<-st_read("C:/Users/Vivi A.S/Documents/efa/2019/Field_dist_2019.shp")
head(inv5)
inv<-inv5
head(inc)

# join distance column ####

inc2<- inc %>%
  st_drop_geometry() %>%
  select(dist)
head(inc2)

inv$distance_m<-inc2$dist
head(inv)

###### add environmental variables  #####
inv6<-inv
library(terra)
library(raster)

# slope 
slope <-terra::rast("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/slope/Slope20_Mulde.tif")
inv6$slope = terra::extract(x=slope, y=vect(inv6), na.rm=T, method="simple", weights=T, fun="mean", touches=T)[,2]
summary(inv6) 

# elevation ####
elevation<-terra::rast("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/elevation/Mulde_dgm20.tif")
inv6$elevation = terra::extract(x=elevation, y=vect(inv6), na.rm=T, method="simple", weights=T, fun="mean", touches=T)[,2]
summary(inv6)

# aspect ####
aspect<-terra::rast("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/3.aspect/Aspect20_Mulde.tif")
inv6$aspect = terra::extract(x=aspect, y=vect(inv6), na.rm=T, method="simple", weights=T, fun="mean", touches=T)[,2]
summary(inv6)

# northness  and eastness
inv6$northness<-cos(inv6$aspect)
inv6$eastness<-sin(inv6$aspect)
head(inv6)

# soil fertility BP_AL & soil moisture OEKFEU####
soilfertility<-st_read("C:/Users/Vivi A.S/Documents/efa/environmental_MULDE/soil_fertility_and_moisture/BSA200.shp") %>%
  st_transform(cs, crs=3035) %>%
  select(BP_AL,OEKFEU)
inv7 <- st_join(inv6,soilfertility, largest = T)
nrow(inv6)
nrow(inv7)
summary(inv7)

# natura 2000! -> if the field is in a natura 2000 area???¿¿

natura<-st_read("D:/bestma (vs-grp01.zih.tu-dresden.de) (Y)/Vivi/data_encrypted/Regional_context/natura_2000/Natura2000_saxony.shp") %>%
  st_transform(cs, crs = 3035)
natura$natura<-1
natura<-select(natura,natura)
head(natura)

inv8 <- st_join(inv7,natura, largest = T)
head(inv8)
invtest<-inv8
inv8$natura[is.na(inv8$natura)] <- 0
head(inv8)

# write spatial with ALL VARIABLES per field ####
#st_write(inv8,"C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019.shp")

## add Small woody features  ####
# I successfully united the SWF and invekos in ARCgis -> name swf_in9_union - but it does not make sense b/c I do not
# know how to associate it to a farm. 

### presence of small woody features within a buffer of 50 meters? #####
# read invekos joined with SWF
SWF<-st_read("C:/Users/Vivi A.S/Documents/efa/2019/smallwoodyfeatures/SWFpolygons.shp") %>%
  filter(gridcode == 1) 
head(SWF)
nrow(SWF)

inv<-st_read("C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019.shp")
head(inv)

inv.buf <- st_buffer(inv, 50)
nrow(inv.buf)
nrow(inv)
head(inv)

SWFINTER1 <- st_intersection(SWF,inv.buf)
head(SWFINTER1)
nrow(SWFINTER1)
nrow(inv)

inv$SWF <- 0
inv$SWF[inv$rowname %in% SWFINTER1$rowname] <- 1
head(inv)
nrow(inv)

st_write(inv,"C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019_SWF.shp")



