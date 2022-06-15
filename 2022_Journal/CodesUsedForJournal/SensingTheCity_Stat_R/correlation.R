#work on 14 June, 2022----
#rm(list=ls())
cordata = `20220613_raw`

#---- (do not use this block) Assumptions----
##Descriptives
library(pastecs)
library(knitr)
kable(stat.desc(cordata, basic = FALSE, norm = TRUE), digits = 3)
#issues with normality
#The data is not normaly distributed
#kurt and skew are super high

##Normality
library(ggplot2)
ggplot(cordata, aes(sky)) + 
  theme(legend.position = "none") + 
  geom_histogram(aes(y=..density..), colour="slate gray", fill="white") + 
  labs(x = "sky", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(cordata$sky), sd = sd(cordata$sky)), color = "red", size = 1)


##Linearity
library(ggplot2)
plot(cordata$Neg.0._Pos.1., cordata$sky)
abline(lm(cordata$Neg.0._Pos.1. ~ cordata$sky), col="red")
qplot(cordata$Revise, cordata$Anxiety)
qplot(cordata$Exam, cordata$Anxiety)




#Plotting the data----
library(ggplot2)
library(cowplot)

A = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Sky)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)
#skyplot

B = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Wall)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)
#wallplot

C = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Building)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

D = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Fence)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

E = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Tree)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

F = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Road)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

G = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Sidewalk)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

H = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Streetlight)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

I = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Signboard)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

J = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Person)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

K = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = Bicycle)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)

L = ggplot(data = cordata, aes(x = Neg.0._Pos.1., y = MotorVehicle)) + 
  geom_point(color='slate gray') +
  geom_smooth(method = "lm", se = FALSE)



plot_grid(A, B, C, D, E, F, G, H, I, J, K, L, labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"), ncol = 4, nrow = 3)



#----Pearson Correlations----
##Establish Dataset
cordata2 = cordata[, c("Sky", "Anxiety", "Revise")] #rows, columns
cor(cordata2)

##All correlations, no p-values


##All correlations, with p-values
library(Hmisc)
examMatrix = as.matrix(cordata2)
examMatrix
rcorr(examMatrix)
##Single Correlation
cor.test(cordata$Exam, cordata$Anxiety)
###for DF, n-2

##R-Squared Values
cor(cordata2)^2

##Point-Biserial Correlation
library(dplyr)
cordata2$Gender01 = recode(cordata$Gender, "Female" = 0, "Male" = 1) 
cordata2$Gender01


#APA Tables Package
library(apaTables)
apa.cor.table(cordata2)
apa.cor.table(cordata2, filename = "cortable.doc")
#######do not report SD for categorical data

##Partial Correlations
library(ggm)
EX_AX_GN = pcor(c("Exam", "Anxiety", "Gender01"), var(cordata2))
EX_AX_GN
pcor.test(EX_AX_GN, 1, 103)


detach("package:ggm")



#----Non-parametric techniques----
##Bootstrapping for Exam and Revise
boot_cor = function(cordata2, i)cor(cordata2$Exam[i], cordata2$Revise[i], use = "complete.obs", method = "pearson")
library(boot)
bootexamRevise = boot(cordata2, boot_cor, 2000)
bootexamRevise
boot.ci(bootexamRevise)

##load liar data
##Spearmans
cor(liarData$Position, liarData$Creativity, method = "spearman")
cor.test(liarData$Position, liarData$Creativity, method = "spearman")

##Kendall's tau
cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, method = "kendall")
