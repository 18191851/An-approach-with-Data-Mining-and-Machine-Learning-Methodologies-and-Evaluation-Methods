#Multiple Regression
library(caret)
library(rpart)
library(party)
library(tree)
library(class)
library(car)
library(caTools)
library(class)
library(caret)
library(psych)

#Retreving the dataset from the location
setwd("C:/Users/DINESH GUDIVADA/Downloads/Project")
data<- read.csv("Superstoredata_KNN_LOGISTIC_Multiple.csv",stringsAsFactors = FALSE,header = TRUE, sep = ",")
data

str(data)
nrow(data)

#Training and Testing the Model
data_train <- data_new[1:7000, ]
data_test <- data_new[7001:9947,]

#Performing Linear Regression Algorithm
lm.fit <- lm(Profit ~ wtd_gmean_atomic_radius+wtd_range_atomic_radius+std_atomic_radius+wtd_std_atomic_radius+wtd_gmean_Density+mean_FusionHeat+wtd_mean_FusionHeat+gmean_FusionHeat+wtd_gmean_FusionHeat+wtd_std_FusionHeat+wtd_mean_ThermalConductivity+entropy_Valence +	wtd_entropy_Valence+range_Valence+wtd_range_Valence+std_Valence+wtd_std_Valence,data=data_new)
lm_pred <- predict(lm.fit,data_test)
vif(lm.fit)


data$Quantity <- as.numeric(data$Qunatity)
str(data_new)

summary(lm.fit)

plot(lm.fit)

cor(critical_temp, lm.fit, method="pearson")

plot(lm_pred, type = "l", lty = 1.8, col = "blue")
#----------------------------------------------------------------------------
#Evaluation Methods
rmse = RMSE(lm_pred,data_test$critical_temp)
rmse

rsquare = (cor(lm_pred,data_test$critical_temp))^2
rsquare




#lm.fit=lm(critical_temp ~ number_of_elements+mean_atomic_mass	+wtd_mean_atomic_mass+gmean_atomic_mass+wtd_gmean_atomic_mass+entropy_atomic_mass+wtd_entropy_atomic_mass+range_atomic_mass+wtd_range_atomic_mass+std_atomic_mass+wtd_std_atomic_mass+	mean_fie	+wtd_mean_fie	+gmean_fie	+wtd_gmean_fie	+entropy_fie	+wtd_entropy_fie+	range_fie	+wtd_range_fie+	std_fie	wtd_std_fie	+mean_atomic_radius+	wtd_mean_atomic_radius+	gmean_atomic_radius	+wtd_gmean_atomic_radius+	entropy_atomic_radius	+wtd_entropy_atomic_radius+	range_atomic_radius+	wtd_range_atomic_radius	+std_atomic_radius+	wtd_std_atomic_radius+	mean_Density+	wtd_mean_Density+	gmean_Density	+wtd_gmean_Density+	entropy_Density	+wtd_entropy_Density+	range_Density	+wtd_range_Density+	std_Density	+wtd_std_Density+	mean_ElectronAffinity	+wtd_mean_ElectronAffinity	+gmean_ElectronAffinity+	wtd_gmean_ElectronAffinity+	entropy_ElectronAffinity+	wtd_entropy_ElectronAffinity+	range_ElectronAffinity+	wtd_range_ElectronAffinity+	std_ElectronAffinity+	wtd_std_ElectronAffinity	+mean_FusionHeat+	wtd_mean_FusionHeat+	gmean_FusionHeat+	wtd_gmean_FusionHeat+	entropy_FusionHeat+	wtd_entropy_FusionHeat	+range_FusionHeat	+wtd_range_FusionHeat	+std_FusionHeat+	wtd_std_FusionHeat	+mean_ThermalConductivity+	wtd_mean_ThermalConductivity+	gmean_ThermalConductivity+	wtd_gmean_ThermalConductivity	+entropy_ThermalConductivity	+wtd_entropy_ThermalConductivity+	range_ThermalConductivity+	wtd_range_ThermalConductivity	+std_ThermalConductivity+	wtd_std_ThermalConductivity+	mean_Valence+	wtd_mean_Valence	+gmean_Valence	+wtd_gmean_Valence+	entropy_Valence+	wtd_entropy_Valence	+range_Valence+	wtd_range_Valence	+std_Valence+	wtd_std_Valence
 #,data=data_new )

#pairs.panels(data_new)
#car.plot(data_new)
