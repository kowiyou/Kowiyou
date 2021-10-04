# Data analysis MSc Geography Dimakatso Machetele
# By Dr Kowiyou Yessoufou, University of Johannesburg, South Africa

rm=list(ls())
library(Amelia)
library(ggplot2)
library(tidyr)
library(dplyr)
#Imputation of missing values NA
# before imputing let's see the structure of NA in the dataset

data_missing <- read.table("C:\\Users\\kowiyouy\\Desktop\\UJ\\MSc\\2019\\MSc supervision\\Geography\\2019\\data\\final dataset_29May2020\\road_accident_data.txt",header=TRUE)
a.out <- amelia(data_missing, m = 5, ts = "year") # run 5 imputations
missmap(a.out)# structure of missing values
plot(a.out, which.vars = 2:6)# diagnostic plot comparing densities of observed vs. imputed values from column 2 to 6 xcluding year since there is no missing year
plot(a.out, which.vars = 7:11)# diagnostic plot comparing densities of observed vs. imputed values from column 7 to 11

# The 5 imputations led to 5 imputed values for each NA; the final value used for the rest of the analysis is the average of the 5 imputed values for each NA.
# This led to a "complete dataset without NA that we called "final_combine_imputed_data.txt"
#Let's read this complete dataset.
#data_machetele <- read.table("C:\\Users\\kowiyouy\\OneDrive - University of Johannesburg\\Desktop\\UJ\\MSc\\2019\\MSc supervision\\Geography\\2019\\data\\data\\final dataset_29May2020\\final_combine_imputed_data.txt",header=TRUE)
data_machetele <- read.table("C:\\Users\\kowiyouy\\OneDrive - University of Johannesburg\\Desktop\\UJ\\MSc\\2019\\MSc supervision\\Geography\\2019\\paper\\Frontiers in Future transportation\\dossier\\Supplemental Data\\Table S1_final_combine_imputed_data.txt",header=TRUE)
attach(data_machetele)
names(data_machetele)

# ploting trends of number of crashes and number of casualities over years

df0 <- data_machetele %>%
  select(year, crashes_sc, casualities_sc) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df0, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

######### ploting trends of mber_fatal_crashes,number_persons_killed

df1 <- data_machetele %>%
  select(year, fatal_crashes,persons_killed) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df1, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
################

# plot relationships between significant correlates of crashes

df2 <- data_machetele %>%
  select(regist_vehicles_sc, dist_travelled_sc, population_SA_sc,crashes_sc) %>%
  gather(key = "variable", value = "value", -population_SA_sc)

ggplot(df2, aes(x = population_SA_sc, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07")) +
  theme_minimal()


# plot relationships between significant correlates of casualities
df3 <- data_machetele %>%
  select(regist_vehicles, population_SA,casualities) %>%
  gather(key = "variable", value = "value", -population_SA)

ggplot(df3, aes(x = population_SA, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07")) +
  theme_minimal()


# plot relationships between significant correlates of fatal crashes
df4 <- data_machetele %>%
  select(nber_vehicles, regist_vehicles,population_SA,dist_travelled,fatal_crashes) %>%
  gather(key = "variable", value = "value", -population_SA)

ggplot(df4, aes(x = population_SA, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07","#FF9999")) +
  theme_minimal()

# plot relationships between significant correlates of person killed

# see figure trends over years of persons killed


#### Testing road accident model in SA using Structural Equation modeling approach

library(piecewiseSEM)
library(lmerTest)
library(nlme)
library(lme4)
library(MASS)
library(car)

### Accessing the dataset to be analysed on my laptop

###create metamodels

################
#in each model, family 'poisson' is not included because response variables are scaled and therefore took negative  value
# The VIF function was used to remove collinear predicting variables from each model in the SEM
# To do so, predicting variables with VIF>5 was removed from each model and this removal start from the variable with the highest VIF such that only variables with VIF<5 are left in each model
#The resulting models were then combined to form the SEM as follows:
#-------------SEM revised by removing collinear predicting variables----------------------------
model_sem <- psem(
  glm(crashes_sc~regist_vehicles_sc+dist_travelled_sc,data=data_machetele),
  glm(casualities_sc~nber_vehicles_sc +dist_travelled_sc, data=data_machetele),
  glm(fatal_crashes_sc~nber_vehicles_sc +casualities_sc,data=data_machetele),
  glm(persons_killed_sc~nber_vehicles_sc+population_SA_sc,data=data_machetele),
  glm(nber_vehicles_sc~population_SA_sc,data=data_machetele),
  glm(regist_vehicles_sc~population_SA_sc+nber_vehicles_sc,data=data_machetele),
  glm(unregist_vehicles_sc~population_SA_sc +nber_vehicles_sc,data=data_machetele),
  glm(driver_licence_sc~population_SA_sc+regist_vehicles_sc,data=data_machetele),
  glm(dist_travelled_sc~ driver_licence_sc +regist_vehicles_sc, data=data_machetele)
)
summary(model_sem,conserve = TRUE)

# After fitting this SEM, the global goodness-of-fit is: Global goodness-of-fit:
#Fisher's C = 1718.486 with P-value = 0 and on 56 degrees of freedom, suggesting that the SEM fitted is not a suitable one to model the data at hand.
#And the R2 for each response variable are:
#Response           method R.squared
#crashes_sc           none      0.99
#casualities_sc       none      0.83
#fatal_crashes_sc     none      0.94
#persons_killed_sc    none      0.90
#nber_vehicles_sc     none      0.04
#regist_vehicles_sc   none      0.99
#unregist_vehicles_sc none      0.97
#driver_licence_sc    none      0.53
#dist_travelled_sc    none      0.85
#-----------------------

# Let's bring back missing and significant variables into the SEM 
#missing variables that have significant effects on each variable are added to each model
model_sem <- psem(
  glm(crashes_sc~regist_vehicles_sc+dist_travelled_sc+population_SA_sc+nber_vehicles_sc,data=data_machetele),
  glm(casualities_sc~nber_vehicles_sc +dist_travelled_sc+ population_SA_sc+ regist_vehicles_sc, data=data_machetele),
  glm(fatal_crashes_sc~nber_vehicles_sc +casualities_sc+ population_SA_sc+ regist_vehicles_sc+ persons_killed_sc+ crashes_sc,data=data_machetele),
  glm(persons_killed_sc~nber_vehicles_sc+population_SA_sc+ regist_vehicles_sc+ crashes_sc+ casualities_sc,data=data_machetele),
  glm(nber_vehicles_sc~population_SA_sc,data=data_machetele),
  glm(regist_vehicles_sc~population_SA_sc+nber_vehicles_sc,data=data_machetele),
  glm(unregist_vehicles_sc~population_SA_sc +nber_vehicles_sc+ crashes_sc+ regist_vehicles_sc+ fatal_crashes_sc+ persons_killed_sc+ dist_travelled_sc,data=data_machetele),
  glm(driver_licence_sc~population_SA_sc+regist_vehicles_sc+ nber_vehicles_sc+ crashes_sc+ unregist_vehicles_sc+ fatal_crashes_sc,data=data_machetele),
  glm(dist_travelled_sc~ driver_licence_sc +regist_vehicles_sc+ persons_killed_sc+ population_SA_sc+ nber_vehicles_sc, data=data_machetele)
)
summary(model_sem,conserve = TRUE)
# Fisher's C = 14.468 with P-value = 0.153 and on 10 degrees of freedom

#Response method R.squared
#crashes_sc           none      1.00
#casualities_sc       none      1.00
#fatal_crashes_sc     none      1.00
#persons_killed_sc    none      0.99
#nber_vehicles_sc     none      0.04
#regist_vehicles_sc   none      0.99
#unregist_vehicles_sc none      0.99
# driver_licence_sc   none      0.99
#dist_travelled_sc    none      0.94

#---------
model_sem <- psem(
  glm(crashes_sc~nber_vehicles_sc+regist_vehicles_sc+unregist_vehicles_sc+driver_licence_sc+population_SA_sc+dist_travelled_sc,data=data_machetele),
  glm(casualities_sc~nber_vehicles_sc+regist_vehicles_sc+unregist_vehicles_sc+driver_licence_sc+population_SA_sc+dist_travelled_sc+crashes_sc,data=data_machetele),
  glm(fatal_crashes_sc~nber_vehicles_sc+regist_vehicles_sc+unregist_vehicles_sc+driver_licence_sc+population_SA_sc+dist_travelled_sc+casualities_sc,data=data_machetele),
  glm(persons_killed_sc~nber_vehicles_sc+crashes_sc+casualities_sc+regist_vehicles_sc+unregist_vehicles_sc+driver_licence_sc+population_SA_sc+dist_travelled_sc+fatal_crashes_sc,data=data_machetele),
  glm(nber_vehicles_sc~population_SA_sc,data=data_machetele),
  glm(regist_vehicles_sc~population_SA_sc+nber_vehicles_sc,data=data_machetele),
  glm(unregist_vehicles_sc~population_SA_sc+regist_vehicles_sc+nber_vehicles_sc,data=data_machetele),
  glm(driver_licence_sc~population_SA_sc+unregist_vehicles_sc+regist_vehicles_sc+nber_vehicles_sc,data=data_machetele),
  glm(dist_travelled_sc~population_SA_sc+unregist_vehicles_sc+driver_licence_sc+nber_vehicles_sc+regist_vehicles_sc, data=data_machetele)
)
summary(model_sem,conserve = TRUE)

### Predicting the future of crashes, fatal crashes, casualties and persons killed over the next 20 years
# This will be done using the GARCH model

library(tseries)
library(quantmod)
library(rugarch)
# to first test if variance of data is non constant.
adf.test(fatal_crashes)
adf.test(persons_killed)
adf.test(data_machetele$casualities)
adf.test(data_machetele$crashes)
## fitting garch to number of persons killed in road accident
par(mfrow=c(2,2)) 
total_crashes <- data_machetele$crashes
#build the first garch model
model_total_crashes <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model="std") #armaOrder=c(1,1) means we want to use the x for one day before and Y for 1 day before to predict current condition
#lets fit this model to our time series data
total_crashes_Garch <- ugarchfit(spec=model_total_crashes,data=total_crashes) #AIC=26.958
total_crashes_Predict <- ugarchboot(total_crashes_Garch,n.ahead = 10,method = c("Partial","Full")[1])#n.ahead = 10 means we want to predict for 10 years
plot(total_crashes_Predict,which=2)

number_casualities <- data_machetele$casualities
#build the first garch model
model_casualities <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)),distribution.model="std") #armaOrder=c(1,1) means we want to use the x for one day before and Y for 1 day before to predict current condition
#lets fit this model to our time series data
total_casualities_Garch <- ugarchfit(spec=model_casualities,data=number_casualities) #AIC=21.193
total_casualities_Predict <- ugarchboot(total_casualities_Garch,n.ahead = 20,method = c("Partial","Full")[1])#n.ahead = 10 means we want to predict for 10 days
plot(total_casualities_Predict,which=2)

fatal_crashes <- data_machetele$fatal_crashes
#build the first garch model
model_fatal_crashes <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(2,2)),distribution.model="std") #armaOrder=c(1,1) means we want to use the x for one day before and Y for 1 day before to predict current condition
#lets fit this model to our time series data
fatal_crashes_Garch <- ugarchfit(spec=model_fatal_crashes,data=fatal_crashes) #AIC=15.162
fatal_crashes_Predict <- ugarchboot(fatal_crashes_Garch,n.ahead = 20,method = c("Partial","Full")[1])#n.ahead = 10 means we want to predict for 10 days
plot(fatal_crashes_Predict,which=2)

persons_killed <- data_machetele$persons_killed
model_persons_killed <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)),distribution.model="std") #armaOrder=c(1,1) means we want to use the x for one day before and Y for 1 day before to predict current condition
#fit the model
person_killed_Garch <- ugarchfit(spec=model_persons_killed,data=persons_killed) #AIC=16.449
person_killed_Predict <- ugarchboot(person_killed_Garch,n.ahead = 20,method = c("Partial","Full")[1])#n.ahead = 10 means we want to predict for 10 days
plot(person_killed_Predict,which=2,ylab="persons killed")
