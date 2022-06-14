#Renaming the dataset
data = `Compiled_Part1&Part2_2`

#----Hierarchical Regression----
#Establsih first model in hierarchical regression
mod3 = lm(Very.Negative ~ sky, data = data)
mod3
plot(data$sky, data$Very.Negative)
abline(a = 0.24616, b = 0.07029, col = 'red')
summary(mod3)
#Establish second model in hierarchical regression
mod4 = lm(Very.Negative ~ wall+building+fence+tree+plant+grass+road+sidewalk+streetlight+signboard+person+bicycle+car+truck+motorcycle, data = data)
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
