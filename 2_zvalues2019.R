#load libraries ####
library(raster)
library(sf)
library(ggplot2)
library(plyr)
library(dplyr)


#############
# prepare dataframe for z values ####
#"C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019_SWF.shp"
#"C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019.shp"
in9<-st_read("C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/spatial2019_SWF.shp")%>% 
  st_drop_geometry()
head(in9)
in9[is.na(in9)]<-0
nrow(in9)
unique(in9$EFA_TYP)
unique(in9$EFA_TYP_)
in9<-dplyr::rename(in9, EFAlinear = EFA_TYP_, EFAfield = EFA_TYP)
head(in9)
unique(in9$EFAlinear)
unique(in9$EFAfield)

head(in9)
#organic from AES
in9[grep(pattern="OEBL", x=in9$AUM_INF),"MERKM_OKO"] <- "T"

# calculate variables at the farm level 
in9_farm<-in9
head(in9)
head(in9_farm)
unique(in9_farm$MERKM_OKO)

in9_farm$MERKM_OKO[is.na(in9_farm$MERKM_OKO)]<-"F"


in9_farm<-in9_farm %>%
  group_by(ANY_NR) %>%
  summarize( farmarea_ha = sum(fildr_h),
             areaLF = sum(fildr_h[EFAlinear > 0]),
             area_natura = sum(fildr_h[natura > 0]),
             areaANC = sum(fildr_h[FB_AZL == "J"]),
             areawaterprot = sum(fildr_h[wtr_prt == 1]),
             areawaterbody = sum(fildr_h[wtr_bdy == 1]),
             areaoforganic = sum(fildr_h[MERKM_OKO == "T"]),
             areaSWF = sum(fildr_h[SWF == 1]))
head(in9_farm)
nrow(in9_farm)

# to have the original area including the grasland fields 
f_info_anne<-read.table("C:/Users/Vivi A.S/Documents/efa/farm_specialization_2019.txt", header = T) %>%
  select(ANY_NR,area_ha)
head(f_info_anne)

in9_farm2<-merge(in9_farm,f_info_anne, by="ANY_NR") %>%
  rename(farmarea2 = area_ha)
nrow(in9_farm2)
head(in9_farm2)

in9_farm3<-in9_farm2 %>%
  group_by(ANY_NR) %>%
  summarize( 
             prop_LF = areaLF/farmarea2,
             prop_natura = area_natura/farmarea2,
             propANC = areaANC/farmarea2,
             propwaterprot = areawaterprot/farmarea2,
             propwaterbody = areawaterbody/farmarea2,
             proporganic = areaoforganic/farmarea2,
             prop_SWF = areaSWF/farmarea2,
             farm_area2 = unique(farmarea2))
head(in9_farm3)
nrow(in9_farm3)

# merge farm and field in one df
in9_models<-merge(in9,in9_farm3, by="ANY_NR")
head(in9_models)

# keep farms with more than 15 hectares
in9_models<-filter(in9_models,farm_area2 > 15)
min(in9_models$farm_area2)
filter(in9_models,proporganic >0.9)
max(in9_models$proporganic)


# check if farms with organic also have EFA 

a<-filter(in9_models,proporganic >0)
unique(a$EFAlinear)
unique(a$EFAfield)

# yes, there are farms with fields dedicated to EFA and also fields with organic farming 


# identify the farms that have EFA
EFAfarms<-in9_models %>%
  filter(EFAlinear > 0 | EFAfield > 0) %>%
  group_by(ANY_NR)
head(EFAfarms)
length(unique(EFAfarms$ANY_NR)) # 879 famrs with EFA 

EFAfarms2<-EFAfarms %>%
  summarize (ANY_NR = unique(ANY_NR))
head(EFAfarms2)
length(unique(EFAfarms2$ANY_NR))

NONEFAfarms<-in9_models %>%
  filter(EFAlinear == 0 | EFAfield == 0) %>%
  group_by(ANY_NR)
head(NONEFAfarms)
length(unique(NONEFAfarms$ANY_NR)) # 1481 famrs without EFA 


# dataframe of farms only with EFA fields ######
ONLYEFA<-left_join(EFAfarms2,in9_models)
nrow(in9_models)
nrow(ONLYEFA)
length(unique(ONLYEFA$ANY_NR)) # certo! 

write.table(ONLYEFA, "C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/ONLYEFA.csv",sep = ",", row.names = F)

# summary statistics ####

efa_DF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/ONLYEFA.csv",sep = ",", header = T)
head(efa_DF)
nrow(efa_DF) #41936
length(unique(efa_DF$ANY_NR)) #879
unique(efa_DF$EFAlinear)

# percentage of EFAs
# percentage of catch crops and fallow of the total number of EFA fields..
# proportion of productive and non productive EFa on the total number of EFA fields.....
head(efa_DF)
unique(efa_DF$EFAfield)
unique(efa_DF$EFAlinear)
EFAfields<-filter(efa_DF, efa_DF$EFAfield == c(60,52,61,62,65,53,59,66,64) | efa_DF$EFAlinear == c(58,70,71,78,72,54,77,76,73,57,74))
EFAfields2<-filter(efa_DF, efa_DF$EFAfield == 0 & efa_DF$EFAlinear == 0)
head(EFAfields)
nrow(EFAfields) #739
nrow(EFAfields2) #34360


# remove linear fields 
efa_DF2<-filter(efa_DF, efa_DF$EFAlinear == 0)
nrow(efa_DF2) #38313
length(unique(efa_DF2$ANY_NR)) #879

# field size
mean(efa_DF2$fildr_h)
sd(efa_DF2$fildr_h)

# farms info #### 
farms<-efa_DF2 %>%
  group_by(ANY_NR) %>%
  summarize(
    farmarea = sum(fildr_h),
    farmarea2 = unique(farm_area2),
    n_fields = length(farm_area2)
  ) %>%
  mutate(Field_size_farm = n_fields/farmarea)
head(farms)

mean(farms$farmarea)
sd(farms$farmarea)
median(farms$farmarea)

mean(farms$n_fields)
sd(farms$n_fields)
median(farms$n_fields)

max(farms$Field_size_farm)



#general info ####
efadf<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/ONLYEFA.csv",sep = ",", header = T)
head(efadf)


# create group and columns variable 

efadf[which(efadf$EFAfield %in% 060),"group"] <- "Productive" # nitrogen fixing 
efadf[which(efadf$EFAfield %in% 060),"name"] <- "Nitrogen fixing" # nitrogen fixing 
efadf[which(efadf$EFAfield %in% 052),"group"] <- "Productive" # catch crop
efadf[which(efadf$EFAfield %in% 052),"name"] <- "Catch crop" # catch crop
efadf[which(efadf$EFAfield %in% 061),"name"] <- "Aforestation" # aforestation
efadf[which(efadf$EFAfield %in% 061),"group"] <- "Productive" # aforestation
efadf[which(efadf$EFAfield %in% 062),"group"] <- "Non-productive" # fallow
efadf[which(efadf$EFAfield %in% 062),"name"] <- "Fallow land" # fallow
efadf[which(efadf$EFAfield %in% 065),"group"] <- "Non-productive" # annual bee pastures
efadf[which(efadf$EFAfield %in% 065),"name"] <- "Bee_pastures_annual"
efadf[which(efadf$EFAfield %in% 053),"group"] <- "Productive" # undersown
efadf[which(efadf$EFAfield %in% 053),"name"] <- "Undersown" # undersown
efadf[which(efadf$EFAfield %in% 059),"group"] <- "Productive" # short rotation
efadf[which(efadf$EFAfield %in% 059),"name"] <- "Short_rotation_plantations" # 
efadf[which(efadf$EFAfield %in% 066),"group"] <- "Non-productive" # perennial bee pastures
efadf[which(efadf$EFAfield %in% 066),"name"] <- "Bee_pastures_perenial"
efadf[which(efadf$EFAfield %in% 064),"group"] <- "Productive" # mixed_silphie
efadf[which(efadf$EFAfield %in% 064),"name"] <- "Mixed_Silphie"

efadf[which(efadf$EFAlinear %in% 058),"group"] <- "Non-productive" # buffer strips AL
efadf[which(efadf$EFAlinear %in% 057),"group"] <- "Non-productive" # buffer strips GL
efadf[which(efadf$EFAlinear %in% 054),"group"] <- "Non-productive" # stipes at the edge of the forest 
efadf[which(efadf$EFAlinear %in% 058),"name"] <- "Buffer strips (AL)" # buffer strips AL
efadf[which(efadf$EFAlinear %in% 057),"name"] <- "Buffer strips (GL)" # buffer strips GL
efadf[which(efadf$EFAlinear %in% 054),"name"] <- "Edge_strips" 
efadf[which(efadf$EFAlinear %in% 070),"group"] <- "Landsc. Features" # hedges
efadf[which(efadf$EFAlinear %in% 071),"group"] <- "Landsc. Features" # row of trees 
efadf[which(efadf$EFAlinear %in% 072),"group"] <- "Landsc. Features" # field trees
efadf[which(efadf$EFAlinear %in% 073),"group"] <- "Landsc. Features" # wetlands
efadf[which(efadf$EFAlinear %in% 074),"group"] <- "Landsc. Features" # single trees
efadf[which(efadf$EFAlinear %in% 076),"group"] <- "Landsc. Features" # natural stone walls
efadf[which(efadf$EFAlinear %in% 077),"group"] <- "Landsc. Features" # petrified surfaces
efadf[which(efadf$EFAlinear %in% 078),"group"] <- "Landsc. Features" # field borders 
efadf[which(efadf$EFAlinear %in% 070),"name"] <- "Hegdes"
efadf[which(efadf$EFAlinear %in% 071),"name"] <- "Row of trees"
efadf[which(efadf$EFAlinear %in% 072),"name"] <- "Field trees"
efadf[which(efadf$EFAlinear %in% 073),"name"] <- "Wetlands"
efadf[which(efadf$EFAlinear %in% 074),"name"] <- "Single trees"
efadf[which(efadf$EFAlinear %in% 076),"name"] <- "Natural stone wall"
efadf[which(efadf$EFAlinear %in% 077),"name"] <- "Rocks and stones"
efadf[which(efadf$EFAlinear %in% 078),"name"] <- "Field borders"

efadf$group[is.na(efadf$group)] <- "NOEFA"
efadf$name[is.na(efadf$name)] <- "NOEFA"
head(efadf)

test1<-as.data.frame(table(efadf$group))
test1$perc<-test1$Freq/sum(test1$Freq) 
sum(test1$perc)


a<-test1[-2,]
a$perc<-a$Freq/sum(a$Freq) 
sum(a$perc) 
a
# fallow and catchcrop from the total
b<-as.data.frame(table(efadf$name))
b<-b[-15,]
b$perc<-b$Freq/sum(b$Freq) 
sum(b$perc)
b
b[order(b$perc),] 


LF<-filter(efadf,group=="Landsc. Features")
a<-as.data.frame(table(LF$name))
sum(a$Freq)
a$perc<-a$Freq/2328
a
sum(a$perc)
a$order <- rank(-a$perc)
a <- a[order(a$order),]

LF<-filter(efadf,group=="Non-productive")
a<-as.data.frame(table(LF$name))
sum(a$Freq)
a$perc<-a$Freq/2808
a
sum(a$perc)
a$order <- rank(-a$perc)
a <- a[order(a$order),]
a

LF<-filter(efadf,group=="Productive")
a<-as.data.frame(table(LF$name))
sum(a$Freq)
a$perc<-a$Freq/2440
a
sum(a$perc)
a$order <- rank(-a$perc)
a <- a[order(a$order),]
a


# calculate z values ####
str(ONLYEFA)

# z values are not calculated for the variables that already depict the farm context and dummy variables


ONLYEFA$OEKFEU<-as.numeric(ONLYEFA$OEKFEU)

ONLYEFA2<-as.data.frame(ONLYEFA) %>%
  group_by(ANY_NR) %>%
  summarize(
    fieldarSD= sd(fildr_h, na.rm = T),
    fieldar2 = mean(fildr_h, na.rm = T),
    Soil_fertilitySD = sd(BP_AL, na.rm = T),
    Soil_fertility2 = mean(BP_AL, na.rm = T),
    Soil_moistureSD = sd(OEKFEU, na.rm = T),
    Soil_moisture2 = mean(OEKFEU, na.rm = T),
    Wind_erosionSD = sd(e_WIND_, na.rm = T),
    Wind_erosion2 = mean(e_WIND_, na.rm = T),
    soil_erosionSD = sd(er_KLSR, na.rm = T),
    soil_erosion2 = mean(er_KLSR, na.rm = T),
    slopeSD = sd(slope, na.rm = T),
    slope2 = mean(slope, na.rm = T),
    elevationSD = sd(elevatn, na.rm = T),
    elevation2 = mean(elevatn, na.rm = T),
    aspectSD = sd(aspect, na.rm = T),
    aspect2 = mean(aspect, na.rm = T),
    compactnessSD = sd(rch_cmp, na.rm = T),
    compactness2 = mean(rch_cmp, na.rm = T),
    distSD = sd(dstnc_m, na.rm = T),
    dist2 = mean(dstnc_m, na.rm = T),
    nothSD=sd(nrthnss,na.rm=T),
    north2=mean(nrthnss,na.rm=T),
    eastSD=sd(eastnss,na.rm=T),
    east2=sd(eastnss,na.rm=T)
  )
nrow(ONLYEFA2)
head(ONLYEFA2)

ONLYEFA3<-merge(ONLYEFA,ONLYEFA2, by = "ANY_NR")
nrow(ONLYEFA)
head(ONLYEFA3)

# Manually z value
(x - mean(x)) / sd(x)
scale(x) # list 

ONLYEFA3$z_fieldar<-(ONLYEFA3$fildr_h - ONLYEFA3$fieldar2) / ONLYEFA3$fieldarSD
ONLYEFA3$z_Soil_fertility<-(ONLYEFA3$BP_AL - ONLYEFA3$Soil_fertility2) / ONLYEFA3$Soil_fertilitySD
ONLYEFA3$z_Soil_moisture<-(ONLYEFA3$OEKFEU - ONLYEFA3$Soil_moisture2) / ONLYEFA3$Soil_moistureSD
ONLYEFA3$z_winderodion<-(ONLYEFA3$e_WIND_ - ONLYEFA3$Wind_erosion2) / ONLYEFA3$Wind_erosionSD
ONLYEFA3$z_soilerosion<-(ONLYEFA3$er_KLSR - ONLYEFA3$soil_erosion2) / ONLYEFA3$soil_erosionSD
ONLYEFA3$z_slope<-(ONLYEFA3$slope - ONLYEFA3$slope2) / ONLYEFA3$slopeSD
ONLYEFA3$z_elevation<-(ONLYEFA3$elevatn - ONLYEFA3$elevation2) / ONLYEFA3$elevationSD
ONLYEFA3$z_compactness<-(ONLYEFA3$rch_cmp - ONLYEFA3$compactness2) / ONLYEFA3$compactnessSD
ONLYEFA3$z_dist<-(ONLYEFA3$dstnc_m - ONLYEFA3$dist2) / ONLYEFA3$distSD
ONLYEFA3$z_north<-(ONLYEFA3$nrthnss - ONLYEFA3$north2) / ONLYEFA3$nothSD
ONLYEFA3$z_east<-(ONLYEFA3$eastnss - ONLYEFA3$east2) / ONLYEFA3$eastSD

head(ONLYEFA3)
efadf<-ONLYEFA3

# # replace nas with median ####
efadf$z_fieldar[is.na(efadf$z_fieldar)] <- median(efadf$z_fieldar,na.rm=T)
efadf$z_Soil_fertility[is.na(efadf$z_Soil_fertility)] <- median(efadf$z_Soil_fertility,na.rm=T)
efadf$z_Soil_moisture[is.na(efadf$z_Soil_moisture)] <- median(efadf$z_Soil_moisture,na.rm=T)
efadf$z_winderodion[is.na(efadf$z_winderodion)] <- median(efadf$z_winderodion,na.rm=T)
efadf$z_soilerosion[is.na(efadf$z_soilerosion)] <- median(efadf$z_soilerosion,na.rm=T)
efadf$z_slope[is.na(efadf$z_slope)] <- median(efadf$z_slope,na.rm=T)
efadf$z_elevation[is.na(efadf$z_elevation)] <- median(efadf$z_elevation,na.rm=T)
efadf$z_compactness[is.na(efadf$z_compactness)] <- median(efadf$z_compactness,na.rm=T)
efadf$z_dist[is.na(efadf$z_dist)] <- median(efadf$z_dist,na.rm=T)
efadf$z_north[is.na(efadf$z_north)] <- median(efadf$z_north,na.rm=T)
efadf$z_east[is.na(efadf$z_east)] <- median(efadf$z_east,na.rm=T)

head(efadf)

# name EFAs per group and type
unique(efadf$EFAfield)


efadf[which(efadf$EFAfield %in% "060"),"group"] <- "Productive" # nitrogen fixing 
efadf[which(efadf$EFAfield %in% "060"),"name"] <- "Nitrogen fixing" # nitrogen fixing 
efadf[which(efadf$EFAfield %in% "052"),"group"] <- "Productive" # catch crop
efadf[which(efadf$EFAfield %in% "052"),"name"] <- "Catch crop" # catch crop
efadf[which(efadf$EFAfield %in% "061"),"name"] <- "Aforestation" # aforestation
efadf[which(efadf$EFAfield %in% "061"),"group"] <- "Productive" # aforestation
efadf[which(efadf$EFAfield %in% "062"),"group"] <- "Non-productive" # fallow
efadf[which(efadf$EFAfield %in% "062"),"name"] <- "Fallow land" # fallow
efadf[which(efadf$EFAfield %in% "065"),"group"] <- "Non-productive" # annual bee pastures
efadf[which(efadf$EFAfield %in% "065"),"name"] <- "Bee_pastures_annual"
efadf[which(efadf$EFAfield %in% "053"),"group"] <- "Productive" # undersown
efadf[which(efadf$EFAfield %in% "053"),"name"] <- "Undersown" # undersown
efadf[which(efadf$EFAfield %in% "059"),"group"] <- "Productive" # short rotation
efadf[which(efadf$EFAfield %in% "059"),"name"] <- "Short_rotation_plantations" # 
efadf[which(efadf$EFAfield %in% "066"),"group"] <- "Non-productive" # perennial bee pastures
efadf[which(efadf$EFAfield %in% "066"),"name"] <- "Bee_pastures_perenial"
efadf[which(efadf$EFAfield %in% "064"),"group"] <- "Productive" # mixed_silphie
efadf[which(efadf$EFAfield %in% "064"),"name"] <- "Mixed_Silphie"

efadf[which(efadf$EFAlinear %in% "058"),"group"] <- "Non-productive" # buffer strips AL
efadf[which(efadf$EFAlinear %in% "057"),"group"] <- "Non-productive" # buffer strips GL
efadf[which(efadf$EFAlinear %in% "054"),"group"] <- "Non-productive" # stipes at the edge of the forest 
efadf[which(efadf$EFAlinear %in% "058"),"name"] <- "Buffer strips (AL)" # buffer strips AL
efadf[which(efadf$EFAlinear %in% "057"),"name"] <- "Buffer strips (GL)" # buffer strips GL
efadf[which(efadf$EFAlinear %in% "054"),"name"] <- "Edge_strips" 
efadf[which(efadf$EFAlinear %in% "070"),"group"] <- "Landsc. Features" # hedges
efadf[which(efadf$EFAlinear %in% "071"),"group"] <- "Landsc. Features" # row of trees 
efadf[which(efadf$EFAlinear %in% "072"),"group"] <- "Landsc. Features" # field trees
efadf[which(efadf$EFAlinear %in% "073"),"group"] <- "Landsc. Features" # wetlands
efadf[which(efadf$EFAlinear %in% "074"),"group"] <- "Landsc. Features" # single trees
efadf[which(efadf$EFAlinear %in% "076"),"group"] <- "Landsc. Features" # natural stone walls
efadf[which(efadf$EFAlinear %in% "077"),"group"] <- "Landsc. Features" # petrified surfaces
efadf[which(efadf$EFAlinear %in% "078"),"group"] <- "Landsc. Features" # field borders 
efadf[which(efadf$EFAlinear %in% "070"),"name"] <- "Hegdes"
efadf[which(efadf$EFAlinear %in% "071"),"name"] <- "Row of trees"
efadf[which(efadf$EFAlinear %in% "072"),"name"] <- "Field trees"
efadf[which(efadf$EFAlinear %in% "073"),"name"] <- "Wetlands"
efadf[which(efadf$EFAlinear %in% "074"),"name"] <- "Single trees"
efadf[which(efadf$EFAlinear %in% "076"),"name"] <- "Natural stone wall"
efadf[which(efadf$EFAlinear %in% "077"),"name"] <- "Rocks and stones"
efadf[which(efadf$EFAlinear %in% "078"),"name"] <- "Field borders"

head(efadf)
efadf$group[is.na(efadf$group)] <- "NOEFA"
efadf$name[is.na(efadf$name)] <- "NOEFA"


write.table(efadf, "C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFASWF.csv",sep = ",", row.names = F)

