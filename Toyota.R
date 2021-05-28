Toyota<-Toyota_r
#check if na values are there
colSums(is.na(Toyota_r))
Toyota<-na.omit(Toyota_r)
colnames(Toyota)
Toyota<-Toyota[,c(3,4,7,9,16,13,14,17,18)]
#Toyota<-subset(Toyota,select=c("Price","CC")
#Scatter plot Matrix
pairs(Toyota)
#correlation matrix
cor(Toyota)
#Regression Model and Summary
model.car<-lm(Price~.,data=Toyota)
summary(model.car)
summary(model)
#Multi-colinearity
install.packages(car)
library(car)
#Variance Inflation factor
car::vif(model.car)
#Subset selection
library(MASS)
stepAIC(model.car)
#Full Model and Summary
#Regression Model and Summary
model.car<-lm(Price~.,data=Toyota)
summary(model.car)
#Diagnostic plots
#Residual plots,QQ-plots,std.Residuals vs fifted
plot(model.car)
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
#Remove 79th Observation
Toyota["Age2"]<-Toyota$Age_08_04*Toyota$Age_08_04
Toyota1<-Toyota[-c(79),]
model1<-lm(Price~.,data=Toyota1)
summary(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)
#Iteration2
Toyota2<-Toyota[-c(79,219),]
model2<-lm(Price~.,data=Toyota2)
summary(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
#Iteration2
Toyota2<-Toyota[-c(79,219),]
model2<-lm(Price~.,data=Toyota2)
summary(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
#Iteration2
Toyota2<-Toyota[-c(79,219),]
model2<-lm(Price~.,data=Toyota2)
summary(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
#Iteration2
Toyota2<-Toyota[-c(79,219),]
model2<-lm(Price~.,data=Toyota2)
summary(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
#Final
Toyota3<-Toyota[-c(79,218,217,950,58,520,521,957,600),]
model3<-lm(Price~.,data=Toyota3)
summary(model3)
library(car)
model2.car<-lm(Price~.,data=Toyota)
summary(model2.car)
testdata<-data.frame(Age_08_04=20,KM=2000,Doors=3,HP=90,Gears=5,cc=2000,Quarterly_Tax=200,Weight=2500,Age2=400)
predict(model2.car,testdata)


