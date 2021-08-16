
library(reshape2)
library(DescTools)
library(ggplot2)
library(FrF2)
library(BsMD)


#####################################
#1)






#####################################
#2 a)

data=cbind(c(1,-1,1,-1,1,-1,1,-1),c(-1,-1,1,1,-1,-1,1,1),c(-1,-1,-1,-1,1,1,1,1))
y=cbind(c(11,12,10,11,16,14,15,19))

flasherType = data[,1]
inertia = data[,2]
task = data[,3]

g = lm(y~flasherType*inertia*task) #location effect model

summary(g)


#b

#full log, not useful just for completeness
DanielPlot(g, half=F, autolab=F, main="Normal plot of location effects")

#half log, shows flash:inert:task,Flash:inertia,task are off the normal line/not grouped together
DanielPlot(g, half=T, autolab=F, main="Normal plot of location effects")

effect = round(2*g$coef[-1],3)
effect
median(abs(effect))
s0 = 1.5*median(abs(effect))
s0

2.5*s0
abs(effect)<2.5*s0
PSE = 1.5*median(abs(effect[abs(effect)<2.5*s0]))
PSE

tPSE=abs(effect)/PSE
#let alpha be 0.05, then IER of alpha is 2.3
abs(tPSE)

#shows no significance









#####################################
#3 a)











































