#Mileage<-Cars
#Scatter plot Matrix
pairs(Cars)
#correlation matrix
cor(Cars)
#Regression Model and Summary
model<-lm(MPG~HP+VOL+SP+WT,data=Cars)
summary(model)
reg_vol<-lm(MPG~VOL,data=Cars)
summary(reg_vol)
reg_wt<-lm(MPG~WT,data=Cars)
summary(reg_wt)
reg_wt_vol<-lm(MPG~WT+VOL,data=Cars)
summary(reg_wt_vol)
#Regression Model and Summary
model<-lm(MPG~.,data=Cars)
summary(model)
#Multi-colinearity
install.packages(car)
library(car)
#Variance Inflation factor
car::vif(model)
#Subset selection
library(MASS)
stepAIC(model)
#Full Model and Summary
#Regression Model and Summary
model.car<-lm(MPG~.,data=Cars)
summary(model)
#Diagnostic plots
#Residual plots,QQ-plots,std.Residuals vs fifted
plot(model)
#Residuals vs Regressor
library(car)
residualPlots(model.car)
#Added Variable Plots
avPlots(model.car)
#QQ Plots of standrized residuals
qqPlot(model.car)
#Deletion Diagnostics
influenceIndexPlot(model.car)#Index Plots of Influence
#Iteration 1
#Remove 77th Observation
Cars["HP2"]<-Cars$HP*Cars$HP
Cars["SP2"]<-Cars$SP*Cars$SP
Cars1<-Cars[-77,]
model1<-lm(MPG~.,data=Cars1)
summary(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)
#Iteration2
Cars2<-Cars[-c(77,66,81),]
model2<-lm(MPG~.,data=Cars2[,-c(5)])
summary(model2)
