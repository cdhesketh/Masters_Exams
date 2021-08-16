
library(mvtnorm)
library(RVAideMemoire)
library(rstatix)
library(mnormt)
library(nFactors)

setwd('C:\\Users\\User\\Desktop\\School\\Math_537\\Test2')
data=read.table('Apple.txt', header=T)

head(data)

######################################################
#1 a)

y=cbind(data$y1,data$y2,data$y3,data$y4)
model=manova(y~as.factor(Rootstock),data=data)
lam=summary.manova(model,test="Wilks")$stats[3]
lam

######################################################
#1 b)

temp=summary.manova(model,test="Wilks")
temp$SS$Residuals

model$residuals

mqqnorm(model$residuals)
#fairly normal


box_m(y, data$Rootstock)

# With a p-value of .711 this implies that each y_i came from similar variances.


######################################################
#1 c)

n=10000
lam2=matrix(0,1,n)

data2=as.matrix(data)

ind=sample(1:48,replace=T)
final=cbind((data2[,1]),(data2[ind,2:5]))
results= final[,2:5]
y=cbind(final[,2],final[,3],final[,4],final[,5])
model=manova(y~as.factor(final[,1]),data=as.data.frame(final))
lam2[1]=summary.manova(model,test="Wilks")$stats[3]

for(i in 2:n)
{
  ind=sample(1:48,replace=T)
  final=cbind((data2[,1]),(data2[ind,2:5]))
  results=results+final[,2:5]
  y=cbind(final[,2],final[,3],final[,4],final[,5])
  model=manova(y~as.factor(final[,1]),data=as.data.frame(final))
  lam2[i]=summary.manova(model,test="Wilks")$stats[3]
}

sum(lam2<lam)/n


results=results/n

results=cbind((data2[,1]),results)
y=cbind(results[,2],results[,3],results[,4],results[,5])
model=manova(y~as.factor(results[,1]),data=as.data.frame(results))
mqqnorm(model$residuals)



######################################################
#2 a)

data2=read.table('drivPoints.txt', sep=",", header=T)

data2F=scale(data2[,6:dim(data2)[2]],scale=F)

pca = prcomp(data2F)
pca
plot(pca,type="l")

source("ggbiplot2.R")

componet=pca$rotation[,1:2]

#All principle components printed

g = ggbiplot(pca, obs.scale = 1, var.scale = 1, 
             groups = as.factor(data2[,4]), ellipse = TRUE,
             circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g

#Only the first two principle componenets printed (important ones)

g = ggbiplot(pca, obs.scale = 1, var.scale = 1, 
             groups = as.factor(data2[,4]), ellipse = TRUE,
             circle = TRUE,numComp=2)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g



head(data2)


######################################################
#2 b)

X=data2F

f1=factanal(data2F,factors=9,rotation="varimax")

f2=factanal(data2F,factors=4,rotation="varimax")

ev <- eigen(cor(X)) # get eigenvalues
ap <- parallel(subject=nrow(X),var=ncol(X),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)














