#Renaming the dataset

data = X1_20220622_FinalDataForSAR
data
#----Hierarchical Regression----
#Establsih first model in hierarchical regression
mod3 = lm(Neg.0_Pos.1 ~ Sky, data = data)
mod3
plot(data$Sky, data$Neg.0_Pos.1)
abline(a = 1.063, b = -1.357, col = 'red')
summary(mod3)
#Establish second model in hierarchical regression
mod4 = lm(Neg.0_Pos.1 ~ Sky+Wall+Building+Fence+Tree+Road+Sidewalk+Streetlight+Signboard+Person+Bicycle+MotorVehicle, data = data)
mod4

#Summarize models
summary(mod3)
summary(mod4)

#Compare models
anova(mod3, mod4)
##R Square change
changeMod = 0.9576 - 0.01171 #copying from r squre values
changeMod
#----Diagnositics and Assumptions----
#Visualize
plot(mod4)

data2 = data
#Diagnositics
data2$std.residuals = rstandard(mod4)
data2$Cooks.distance = cooks.distance(mod4)
data2$leverage = hatvalues(mod4)

#Outlier check
which(data2$std.residuals > 3.29 | data2$std.residuals < -3.29)#Discrepancy
which(data2$leverage > 3*mean(data2$leverage))#Leverage
which(data2$Cooks.distance > 1)#Influence

#Assumptions
data2$residuals = resid(mod4)
shapiro.test(data2$residuals)#normality of error term

library(lmtest)
bptest(mod4, studentize = FALSE)#Homoskedasticity of error

library(car)
dwt(mod4)#Independence of the error term #no error

library(car)
##Multicollinearity
# no p value. we have to look into data
vif(mod4)#Nothing larger than 10. no worries then
1/vif(mod4)#Below .2 maybe, below .1 is an issue
mean(vif(mod4))#Should not be substantially greater than 1


#----Final Model----
summary(mod4)
#Always adjusted r value. 
#model looks substantial looking in F and significant looking in p (model significance)
#All the models are significant
#adverts is the most significant as we have to look into t value


#Beta weights
library(lm.beta)
lm.beta(mod4)
#look into the significant variable


#results table (APA table output)

#b is estimated, se is standard error


###Sensitivity Analysis----

library(boot)

X <- data.frame(Sky = data$Sky,
                Wall = data$Wall,
                Building = data$Building)

# linear model : Y = X1 + X2 + X3
X

y <- data.frame(X1 = data$Neg.0_Pos.1)
y

# sensitivity analysis
library(sensitivity)
x <- src(X, y, nboot = 100)
print(x)
plot(x)

library(ggplot2)
ggplot(x)

