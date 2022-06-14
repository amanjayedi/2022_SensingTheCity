#----Hierarchical Regression----
#Establsih first model in hierarchical regression
mod1 = lm(SolarRadiation ~ ï..T1_Depth, data = OverlandP04ForSVR)
mod1
plot(OverlandP04ForSVR$ï..T1_Depth, OverlandP04ForSVR$SolarRadiation)
abline(a = 655.5160, b = 0.4002, col = 'red')
summary(mod1)
#Establish second model in hierarchical regression
mod2 = lm(SolarRadiation ~ ï..T1_Depth+T2_Depth+T3_Depth+T1_Height_U+T2_Height_U+T3_Height_U+T1_Setback_FU+T2_Setback_FU+T3_Setback_FU+T1_Roof_Height+T2_Roof_Height+T3_Roof_Height, data = OverlandP04ForSVR)
mod2

#Summarize models
summary(mod1)
summary(mod2)

#Compare models
anova(mod1, mod2)
##R Square change
changeMod = 0.9632 - -0.001417 #copying from r squre values
changeMod
#----Diagnositics and Assumptions----
#Visualize
plot(mod2)

OverlandP04ForSVR2 = OverlandP04ForSVR
#Diagnositics
OverlandP04ForSVR2$std.residuals = rstandard(mod2)
OverlandP04ForSVR2$Cooks.distance = cooks.distance(mod2)
OverlandP04ForSVR2$leverage = hatvalues(mod2)

#Outlier check
which(OverlandP04ForSVR2$std.residuals > 3.29 | OverlandP04ForSVR2$std.residuals < -3.29)#Discrepancy
which(OverlandP04ForSVR2$leverage > 3*mean(OverlandP04ForSVR2$leverage))#Leverage
which(OverlandP04ForSVR2$Cooks.distance > 1)#Influence

#Assumptions
OverlandP04ForSVR2$residuals = resid(mod2)
shapiro.test(OverlandP04ForSVR2$residuals)#normality of error term

library(lmtest)
bptest(mod2, studentize = FALSE)#Homoskedasticity of error

library(car)
dwt(mod2)#Independence of the error term #no error

library(car)
##Multicollinearity
# no p value. we have to look into data
vif(mod2)#Nothing larger than 10. no worries then
1/vif(mod2)#Below .2 maybe, below .1 is an issue
mean(vif(mod2))#Should not be substantially greater than 1


#----Final Model----
summary(mod2)
#Always adjusted r value. 
#model looks substantial looking in F and significant looking in p (model significance)
#All the models are significant
#adverts is the most significant as we have to look into t value


#Beta weights
library(lm.beta)
lm.beta(mod2)
#look into the significant variable


#results table (APA table output)

#b is estimated, se is standard error
