# load libraries ####
library(dplyr)
library(caret)
library(nnet)


#efaDF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFA.csv", sep = ",", header= T)
#head(efaDF)
efaDF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFASWF.csv", sep = ",", header= T)
head(efaDF)
nrow(efaDF)

# make ANC a summy variable 
efaDF[which(efaDF$FB_AZL %in% "J"),"AZL"] <- 1
efaDF[which(efaDF$FB_AZL %in% "N"),"AZL"] <- 0

# select only variables of interest 
efaalldf2<-select(efaDF,ANY_NR,group,name,
                  farm_area2,prop_LF,prop_natura,propANC,propwaterprot,propwaterbody,proporganic,prop_SWF, # farm level
                  z_fieldar,AZL,SWF,wtr_prt,wtr_bdy,z_compactness,z_dist,SWF, # field level 
                  z_Soil_fertility, 
                  z_Soil_moisture,z_winderodion,
                  z_soilerosion,z_slope,
                  z_elevation,z_north,
                  z_east)
head(efaalldf2)

library("PerformanceAnalytics")
#chart.Correlation(efaalldf2[,-c(1:3,12:15)], histogram=TRUE, pch=19)

res<-cor(efaalldf2[,-c(1:3,13:16)])
library(corrplot)
dev.new()
corrplot(res, type = "upper", order = "hclust", method = "number",
         tl.col = "black", tl.srt = 45)
library("Hmisc")
res2 <- rcorr(as.matrix(efaalldf2[,-c(1:3,13:16)]))
res2


# dataframe with group as response variable ####

head(efaalldf2)
colnames(efaalldf2)
efa_group<-efaalldf2[,-c(1,3,9)]# take out water bodies
head(efa_group)
nrow(efa_group)  #41936
colnames(efa_group)
table(efa_group$group)

# setting reference level
efa_group$group<-as.factor(efa_group$group)
efa_group$group <- relevel(efa_group$group, ref = "NOEFA")
unique(efa_group$group)

#remove group = landscape features ?
#efa_group<-efa_group[!(efa_group$group == "Landsc. Features"),]
#unique(test$group)

# multinomial model by GROUP ####
head(efa_group)
multi_group_model<-multinom(group ~ ., data = efa_group, Hess = T, model =T, maxit = 1000)
summary(multi_group_model)

#mult. model farm structure 
multi_group_model_farm<-multinom(group ~ farm_area2 + prop_LF + prop_natura +
                            propANC + propwaterprot + propwaterbody + proporganic, data = efa_group, Hess = T, model =T)
summary(multi_group_model_farm)

#mult. model field structure 
multi_group_model_field<-multinom(group ~ z_fieldar + AZL + wtr_prt +
                              wtr_bdy + z_compactness + z_dist, data = efa_group, Hess = T, model =T)
summary(multi_group_model)

# mult model environmental variables 
multi_group_model_environmental<-multinom(group ~ z_Soil_fertility + z_Soil_moisture + z_winderodion +
                              z_soilerosion + z_slope + z_elevation + z_north + z_east, data = efa_group, Hess = T, model =T)
summary(multi_group_model_environmental)



# model evaluation metrics and P values ####
multi_group_model_farm<-multinom(group ~ farm_area2 + prop_LF + prop_natura +
                                   propANC + propwaterprot + propwaterbody + proporganic, data = efa_group, Hess = T, model =T)
summary(multi_group_model_farm)

multi_group_model<-multi_group_model_farm
# model evaluation metrics 
# convert coefficients to odds
exp(coef(multi_group_model))
# see predicted values 
head(round(fitted(multi_group_model), 2))
# wald z test --->> to get P values 
z_group <- summary(multi_group_model)$coefficients/summary(multi_group_model)$standard.errors
z_group
# 2-tailed z test
p_group <- (1 - pnorm(abs(z_group), 0, 1)) * 2
p_group
round(p_group,3)

# likelihood ratio test 
library(afex)
set_sum_contrasts() # use sum coding, necessary to make type III LR tests valid
library(car)
LRT_bygroup<-Anova(multi_group_model,type="III")
LRT_bygroup # LTR by group 


# multinomial model by PRODUCTIVE ####
efaDF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFASWF.csv", sep = ",", header= T)
head(efaDF)

#  create a df that contains fields from farms with productive options #####
data_set<-efaDF
head(data_set)
nrow(data_set) #41936
str(data_set) 
unique(data_set$group)

P_data_set<-data.frame()
n=0
for(farm_i in unique(data_set$ANY_NR)){ #for each farm:
  # check if there is at least one field with group = P
  if(any("Productive" %in% data_set[data_set$ANY_NR == farm_i, ]$group)){ # if yes, then extract that farm in P_data_set
    cat("farm",farm_i,"has one P\n")
    n=n+1
    P_data_set <- rbind(P_data_set, data_set[data_set$ANY_NR == farm_i,])
  }
}
head(P_data_set,20)

nrow(P_data_set) # 34737
length(unique(P_data_set$ANY_NR)) #  618 farms have at least one field with a productive option
table(P_data_set$name)

productivedf<-P_data_set
head(productivedf)

# naming all non-productive categories as non-efa
unique(productivedf$name)
name2<-productivedf$name
unique(name2)
name2[name2 %in% list("Fallow land","Buffer strips (AL)","Buffer strips (GL)","Edge_strips",
                      "Bee_pastures_perenial","Bee_pastures_annual","Hegdes","Row of trees",
                      "Field trees","Wetlands","Single trees","Natural stone wall",
                      "Rocks and stones","Field borders")]="NOEFA"
name2<-as.character(name2)
productivedf$productive<-as.factor(name2)
head(productivedf,40)
levels(productivedf$productive)
table(productivedf$productive)

# df for multinomial regression ####
productivedf[which(productivedf$FB_AZL %in% "J"),"AZL"] <- 1
productivedf[which(productivedf$FB_AZL %in% "N"),"AZL"] <- 0
head(productivedf)

productivedf<-select(productivedf,ANY_NR,productive,
                     farm_area2,prop_LF,prop_natura,propANC,propwaterprot,propwaterbody,proporganic,prop_SWF, # farm level
                     z_fieldar,AZL,wtr_prt,wtr_bdy,z_compactness,z_dist,SWF, # field level 
                     z_Soil_fertility, 
                     z_Soil_moisture,z_winderodion,
                     z_soilerosion,z_slope,
                     z_elevation,z_north,
                     z_east)
head(productivedf)

head(productivedf)
productivedf_model<-productivedf[,-c(1,8)]
str(productivedf_model)

# set reference level
productivedf_model$productive <- relevel(productivedf_model$productive, ref = "NOEFA")

# multinomial model pr####
multinom_model_productive <- multinom(productive ~ ., data = productivedf_model, Hess = T, model =T)
summary(multinom_model_productive) # stop after 100 iterations

multinom_model_productive2 <- multinom(productive ~ ., data = productivedf_model, Hess = T, model =T, maxit = 1000)
summary(multinom_model_productive2) # converged at 310 iterations

#mult. model farm structure ####
multi_group_model_farm_PROD<-multinom(productive ~ farm_area2 + prop_LF + prop_natura +
                                        propANC + propwaterprot + propwaterbody + proporganic, data = productivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_farm_PROD)


#mult. model field structure ####
multi_group_model_field_PROD<-multinom(productive ~ z_fieldar + AZL + wtr_prt +
                                         wtr_bdy + z_compactness + z_dist, data = productivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_field_PROD)

# mult model environmental variables ####
multi_group_model_environmental_PROD<-multinom(productive ~ z_Soil_fertility + z_Soil_moisture + z_winderodion +
                                                 z_soilerosion + z_slope + z_elevation + z_north + z_east, data = productivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_field_PROD)

# posthoc multinomial comparisons (not working quite well still...)
library(lsmeans)


# multinomial model by NON-PRODUCTIVE ####
efaDF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFASWF.csv", sep = ",", header= T)
head(efaDF)

#  create a df that contains fields from farms with productive options #####
data_set<-efaDF
head(data_set)
nrow(data_set) #41936
str(data_set)
unique(data_set$group)

P_data_set<-data.frame()
n=0
for(farm_i in unique(data_set$ANY_NR)){ #for each farm:
  # check if there is at least one field with group = P
  if(any("Non-productive" %in% data_set[data_set$ANY_NR == farm_i, ]$group)){ # if yes, then extract that farm in P_data_set
    cat("farm",farm_i,"has one P\n")
    n=n+1
    P_data_set <- rbind(P_data_set, data_set[data_set$ANY_NR == farm_i,])
  }
}
head(P_data_set,20)
nrow(P_data_set) # 29782
length(unique(P_data_set$ANY_NR)) #  530 farms have at least one field with a nonproductive option

nonproductivedf<-P_data_set
head(nonproductivedf)


# naming all productive categories as non-efa
unique(nonproductivedf$name)
name2<-nonproductivedf$name
unique(name2)
name2[name2 %in% list("Undersown","Short_rotation_plantations","Mixed_Silphie","Nitrogen fixing",
                      "Catch crop","Aforestation","Hegdes","Row of trees",
                      "Field trees","Wetlands","Single trees","Natural stone wall",
                      "Rocks and stones","Field borders")]="NOEFA"
name2<-as.character(name2)
nonproductivedf$NONproductive<-as.factor(name2)
head(nonproductivedf,40)
levels(nonproductivedf$NONproductive)
table(nonproductivedf$NONproductive)

# df for multinomial regression ####
nonproductivedf[which(nonproductivedf$FB_AZL %in% "J"),"AZL"] <- 1
nonproductivedf[which(nonproductivedf$FB_AZL %in% "N"),"AZL"] <- 0
head(nonproductivedf)

nonproductivedf<-dplyr::select(nonproductivedf,ANY_NR,NONproductive,
                     farm_area2,prop_LF,prop_natura,propANC,propwaterprot,propwaterbody,proporganic,prop_SWF, # farm level
                     z_fieldar,AZL,wtr_prt,wtr_bdy,z_compactness,z_dist,SWF, # field level 
                     z_Soil_fertility, 
                     z_Soil_moisture,z_winderodion,
                     z_soilerosion,z_slope,
                     z_elevation,z_north,
                     z_east)
head(nonproductivedf)

head(nonproductivedf)
nonproductivedf_model<-nonproductivedf[,-c(1,8)]
str(nonproductivedf_model)

# set reference level
nonproductivedf_model$NONproductive <- relevel(nonproductivedf_model$NONproductive, ref = "NOEFA")

# multinomial model pr####
multinom_model_NONproductive <- multinom(NONproductive ~ ., data = nonproductivedf_model, Hess = T, model =T)
summary(multinom_model_NONproductive) # stop after 100 iterations

multinom_model_NONproductive2 <- multinom(NONproductive ~ ., data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multinom_model_NONproductive2) # converged at 230iterations

#mult. model farm structure ####
multi_group_model_farm_NONPROD<-multinom(NONproductive ~ farm_area2 + prop_LF + prop_natura +
                                        propANC + propwaterprot + propwaterbody + proporganic, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_farm_NONPROD)


#mult. model field structure ####
multi_group_model_field_NONPROD<-multinom(NONproductive ~ z_fieldar + AZL + wtr_prt +
                                         wtr_bdy + z_compactness + z_dist, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_field_NONPROD)

# mult model environmental variables ####
multi_group_model_environmental_NONPROD<-multinom(NONproductive ~ z_Soil_fertility + z_Soil_moisture + z_winderodion +
                                                 z_soilerosion + z_slope + z_elevation + z_north + z_east, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_environmental_NONPROD)

# multinomial model LANDSCAPE FEATURES ####
efaDF<-read.table("C:/Users/Vivi A.S/Dropbox/TUD/1.EFA_project/scripts/2019_IALE/MASTER_ONLYEFASWF.csv", sep = ",", header= T)
head(efaDF)

#  create a df that contains fields from farms with landscape features#####
data_set<-efaDF
head(data_set)
nrow(data_set) #41936
str(data_set)
unique(data_set$group)

P_data_set<-data.frame()
n=0
for(farm_i in unique(data_set$ANY_NR)){ #for each farm:
  # check if there is at least one field with group = P
  if(any("Landsc. Features" %in% data_set[data_set$ANY_NR == farm_i, ]$group)){ # if yes, then extract that farm in P_data_set
    cat("farm",farm_i,"has one P\n")
    n=n+1
    P_data_set <- rbind(P_data_set, data_set[data_set$ANY_NR == farm_i,])
  }
}
head(P_data_set,20)
nrow(P_data_set) # 16577
length(unique(P_data_set$ANY_NR)) #  268 farms have at least one field with a nonproductive option

nonproductivedf<-P_data_set
head(nonproductivedf)


# naming all productive categories as non-efa
unique(nonproductivedf$name)
name2<-nonproductivedf$name
unique(name2)
name2[name2 %in% list("Undersown","Short_rotation_plantations","Mixed_Silphie","Nitrogen fixing",
                      "Catch crop","Aforestation","Fallow land","Buffer strips (AL)","Buffer strips (GL)","Edge_strips",
                      "Bee_pastures_perenial","Bee_pastures_annual")]="NOEFA"
name2<-as.character(name2)
nonproductivedf$NONproductive<-as.factor(name2)
head(nonproductivedf,40)
levels(nonproductivedf$NONproductive)
table(nonproductivedf$NONproductive)

# df for multinomial regression ####
nonproductivedf[which(nonproductivedf$FB_AZL %in% "J"),"AZL"] <- 1
nonproductivedf[which(nonproductivedf$FB_AZL %in% "N"),"AZL"] <- 0
head(nonproductivedf)

nonproductivedf<-dplyr::select(nonproductivedf,ANY_NR,NONproductive,
                               farm_area2,prop_LF,prop_natura,propANC,propwaterprot,propwaterbody,proporganic,prop_SWF, # farm level
                               z_fieldar,AZL,wtr_prt,wtr_bdy,z_compactness,z_dist,SWF, # field level 
                               z_Soil_fertility, 
                               z_Soil_moisture,z_winderodion,
                               z_soilerosion,z_slope,
                               z_elevation,z_north,
                               z_east)
head(nonproductivedf)

head(nonproductivedf)
nonproductivedf_model<-nonproductivedf[,-c(1,8)]
str(nonproductivedf_model)

# set reference level
nonproductivedf_model$NONproductive <- relevel(nonproductivedf_model$NONproductive, ref = "NOEFA")

# multinomial model pr####
multinom_model_LANDF <- multinom(NONproductive ~ ., data = nonproductivedf_model, Hess = T, model =T)
summary(multinom_model_LANDF) # stop after 100 iterations

multinom_model_LANDF2 <- multinom(NONproductive ~ ., data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multinom_model_LANDF2) # converged at 230iterations

#mult. model farm structure ####
multinom_model_LANDF_FARM<-multinom(NONproductive ~ farm_area2 + prop_LF + prop_natura +
                                      propANC + propwaterprot + propwaterbody + proporganic, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_farm_NONPROD)


#mult. model field structure ####
multinom_model_LANDF_FIELD<-multinom(NONproductive ~ z_fieldar + AZL + wtr_prt +
                                       wtr_bdy + z_compactness + z_dist, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_field_NONPROD)

# mult model environmental variables ####
multinom_model_LANDF_ENV<-multinom(NONproductive ~ z_Soil_fertility + z_Soil_moisture + z_winderodion +
                                     z_soilerosion + z_slope + z_elevation + z_north + z_east, data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multi_group_model_environmental_NONPROD)



