# load libraries
library(dplyr)
library(caret)
library(nnet)

# read dataframes ####
efa9<-read.table("C:/Users/Vivi A.S/Documents/efa/2019/maindatasets2019/efadfrow2019_multinomial_MODELDF_15_12_21.csv", header = T, sep = ",")
efa8<-read.table("C:/Users/Vivi A.S/Documents/efa/2018/maindatasets2018/efadfrow2018_multinomial_MODELDF_16_12_21.csv", header = T, sep = ",")

head(efa9)
nrow(efa9) #45404
length(unique(efa9$ANY_NR))

head(efa8)
nrow(efa8) #44079
length(unique(efa8$ANYNR))

efa8<-rename(efa8, ANY_NR = ANYNR )

# merge df for both years 
efa<-rbind(efa9,efa8)
nrow(efa) #89483
head(efa)

#take out the fields with landscape features
efa<-efa[!efa$group=="Landsc. Features",]
head(efa)
nrow(efa) #84995

#take out aforestation
unique(efa$name)
efa<-efa[!efa$name=="Aforestation",]
head(efa)
nrow(efa) #84939

#take out bee pastures
unique(efa$name)
efa<-efa[!efa$name=="Bee_pastures_annual",]
head(efa)
nrow(efa) #84439
efa<-efa[!efa$name=="Bee_pastures_perenial",]
head(efa)
nrow(efa) #84360
length(unique(efa$ANY_NR))

# correct for farms that are in the boundaries
efa<-filter(efa, farmarea >15)
length(unique(efa$ANY_NR)) #1709
nrow(efa)

# multinomial model per group (productive, non productive)###
# dataframe with group as response variable 
efa_group<-efa[,-c(1,3)]
head(efa_group)
nrow(efa_group)  #84995
unique(efa_group$year)

# setting reference level
efa_group$group<-as.factor(efa_group$group)
efa_group$group <- relevel(efa_group$group, ref = "NOEFA")

# multinomial model by GROUP ####
multi_group_model<-multinom(group ~ ., data = efa_group, Hess = T, model =T)
summary(multi_group_model)

#test model ####

#group model 
# partition data 
# set random seed 
set.seed(1000)
index <- createDataPartition(efa_group$group, p = .70, list = FALSE)
train <- efa_group[index,]
test <- efa_group[-index,]

# training dataset 
train$ClassPredicted <- predict(multi_group_model, newdata = train, "class")
# classification table
tab <- table(train$group, train$ClassPredicted)
tab
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2) # 89.1 accuracy

# test dataset 
test$ClassPredicted <- predict(multi_group_model, newdata = test, "class")
# classification table
tab <- table(test$group, test$ClassPredicted)
tab
round((sum(diag(tab))/sum(tab))*100,2) #89.03 accuracy


# model evaluation metrics ####
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
LRT_bygroup # LTR by group ####

# posthoc multinomial comparisons (not working quite well still...)
library(lsmeans)
#lsmeans(multi_group_model, pairwise ~ group | z_fieldar, adjust="tukey", mode = "prob")
#lsmeans(multi_group_model, pairwise ~ group | z_fieldar, adjust="tukey", mode = "latent") # get mean values


#_______________________________________________________________________________________________

# multinomial model PRODUCTIVE (Aforestation, Catchcrop, Nitrogen fixing, Undersown) ####
head(efa)
nrow(efa) # 84360

#  create a df that contains fields from farms with productive options #####
data_set<-efa
head(data_set)
nrow(data_set) # 84360
str(data_set)

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
nrow(P_data_set) # 69106
length(unique(P_data_set$ANY_NR)) #  1209 farms have at least one field with a productive option

productivedf<-P_data_set
head(productivedf)


# productive options are:
#productive<-list("Catch crop","Nitrogen fixing","Undersown","Aforestation")
# non productive 
#non-productive<-c("Fallow land","Buffer strips (AL)","Buffer strips (GL)","Edge_strips","Bee_pastures_annual","Bee_pastures_perenial")
# landscape features 
#landscapefeat<-c("Hegdes","Row of trees","Field trees","Wetlands","Single trees","Natural stone wall","Rocks and stones","Field borders")


# naming all non-productive categories as non-efa
name2<-productivedf$name
unique(name2)
name2[name2 %in% list("Fallow land","Buffer strips (AL)","Buffer strips (GL)","Edge_strips")]="NOEFA"
name2<-as.character(name2)
productivedf$productive<-as.factor(name2)
head(productivedf,40)
levels(productivedf$productive)

# df for multinomial regression ####
head(productivedf)
productivedf_model<-productivedf[,-c(1:3)]
str(productivedf_model)

# set reference level
productivedf_model$productive <- relevel(productivedf_model$productive, ref = "NOEFA")

# multinomial model pr####
multinom_model_productive <- multinom(productive ~ ., data = productivedf_model, Hess = T, model =T)
summary(multinom_model_productive)

multinom_model_productive2 <- multinom(productive ~ ., data = productivedf_model, Hess = T, model =T, maxit = 1000)
summary(multinom_model_productive2) # converged at 140 iterations



############

#test model ####

#group model 
# partition data 
# set random seed 
set.seed(1000)
index <- createDataPartition(productivedf_model$productive, p = .70, list = FALSE)
train <- productivedf_model[index,]
test <- productivedf_model[-index,]

# training dataset 
train$ClassPredicted <- predict(multinom_model_productive2, newdata = train, "class")
# classification table
tab <- table(train$productive, train$ClassPredicted)
tab
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2) # 93!! accuracy

# test dataset 
test$ClassPredicted <- predict(multinom_model_productive2, newdata = test, "class")
# classification table
tab <- table(test$productive, test$ClassPredicted)
tab
round((sum(diag(tab))/sum(tab))*100,2) #93! accuracy

# model evaluation metrics ####
# convert coefficients to odds
exp(coef(multinom_model_productive))
# see predicted values 
head(round(fitted(multinom_model_productive), 2))
# wald z test --->> to get P values 
z_prod <- summary(multinom_model_productive)$coefficients/summary(multinom_model_productive)$standard.errors
z_prod
# 2-tailed z test
p_prod <- (1 - pnorm(abs(z_prod), 0, 1)) * 2
p_prod

# likelihood ratio test 
library(afex)
set_sum_contrasts() # use sum coding, necessary to make type III LR tests valid
library(car)
LRT_productive<-Anova(multinom_model_productive2,type="III")
LRT_productive # LTR productive ####

# posthoc multinomial comparisons (not working quite well still...)
#library(lsmeans)
#lsmeans(multinom_model_productive, pairwise ~ group | z_fieldar, adjust="tukey", mode = "prob")
#lsmeans(multinom_model_productive, pairwise ~ group | z_fieldar, adjust="tukey", mode = "latent") # get mean values

#_______________________________________________________________________________________________


# multinomial model NONPRODUCTIVE ####
head(efa)
nrow(efa)#84360

#  create a df that contains fields from farms with productive options #####
data_set<-efa
head(data_set)
nrow(data_set) 
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
nrow(P_data_set) # 58010
length(unique(P_data_set$ANY_NR)) #  1056 farms have at least one field with a nonproductive option

nonproductivedf<-P_data_set
head(nonproductivedf)


# productive options are:
#productive<-list("Catch crop","Nitrogen fixing","Undersown","Aforestation")
# non productive 
#non-productive<-c("Fallow land","Buffer strips (AL)","Buffer strips (GL)","Edge_strips","Bee_pastures_annual","Bee_pastures_perenial")
# landscape features 
#landscapefeat<-c("Hegdes","Row of trees","Field trees","Wetlands","Single trees","Natural stone wall","Rocks and stones","Field borders")


# naming all non-productive categories as non-efa
name2<-nonproductivedf$name
unique(name2)
name2[name2 %in% list("Catch crop","Nitrogen fixing","Undersown")]="NOEFA"
name2<-as.character(name2)
nonproductivedf$nonproductive<-as.factor(name2)
head(nonproductivedf,40)
levels(nonproductivedf$nonproductive)

# df for multinomial regression ####
head(nonproductivedf)
nonproductivedf_model<-nonproductivedf[,-c(1:3)]
str(nonproductivedf_model)

# set reference level
nonproductivedf_model$nonproductive <- relevel(nonproductivedf_model$nonproductive, ref = "NOEFA")

# multinomial model np####
multinom_model_nonproductive <- multinom(nonproductive ~ ., data = nonproductivedf_model, Hess = T, model =T, maxit = 1000)
summary(multinom_model_nonproductive) # converged at 150 iterations 

############

#test model ####

#group model 
# partition data 
# set random seed 
set.seed(1000)
index <- createDataPartition(nonproductivedf_model$nonproductive, p = .70, list = FALSE)
train <- nonproductivedf_model[index,]
test <- nonproductivedf_model[-index,]

# training dataset 
train$ClassPredicted <- predict(multinom_model_nonproductive, newdata = train, "class")
# classification table
tab <- table(train$nonproductive, train$ClassPredicted)
tab
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2) # 91!! accuracy

# test dataset 
test$ClassPredicted <- predict(multinom_model_nonproductive, newdata = test, "class")
# classification table
tab <- table(test$nonproductive, test$ClassPredicted)
tab
round((sum(diag(tab))/sum(tab))*100,2) #91! accuracy

# model evaluation metrics ####
# convert coefficients to odds
exp(coef(multinom_model_nonproductive))
# see predicted values 
head(round(fitted(multinom_model_nonproductive), 2))
# wald z test --->> to get P values 
z_non_prod <- summary(multinom_model_nonproductive)$coefficients/summary(multinom_model_nonproductive)$standard.errors
z_non_prod
# 2-tailed z test
p_non_prod <- (1 - pnorm(abs(z_non_prod), 0, 1)) * 2
p_non_prod

# likelihood ratio test 
library(afex)
set_sum_contrasts() # use sum coding, necessary to make type III LR tests valid
library(car)
LTR_nonp<-Anova(multinom_model_nonproductive,type="III")
LTR_nonp # LTR np ####

# posthoc multinomial comparisons (not working quite well still...)
#library(lsmeans)
#lsmeans(multinom_model_nonproductive, pairwise ~ group | z_fieldar, adjust="tukey", mode = "prob")
#lsmeans(multinom_model_nonproductive, pairwise ~ group | z_fieldar, adjust="tukey", mode = "latent") # get mean values

