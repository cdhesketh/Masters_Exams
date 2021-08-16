
library(reshape2)
setwd("C:\\Users\\User\\Desktop\\School\\Math_531T\\Exam1")


#####################################################
#1)


















#####################################################
#2)
data<-read.table("http://www2.isye.gatech.edu/%7Ejeffwu/book/data/pulp.dat", h=T)



Y = as.matrix(data)
n = rep(nrow(Y),ncol(Y))
k=ncol(Y) 
N=sum(n)
alpha = 0.05

Yidot = apply(Y, 2, mean) # sample mean for each treatment
avgY = mean(Yidot) # grand mean

kprime <- choose(k,2)

kprime = choose(k,2)
res = sum((Y-t(Yidot%*%t(rep(1,5))))^2)/(N-k)
b=qt(1-(alpha/(2*kprime)), N-k)*sqrt(2/5*res)

t=qtukey(1-alpha,k,N-k)/sqrt(2)*sqrt(2/5*res)

stats=matrix(0,ncol(Y)-1,ncol(Y))

for(j in 1:ncol(Y)-1)
{
  for(i in j:ncol(Y))
  {
    stats[j,i]=Yidot[i]-Yidot[j]
  }
}
uppBoundB=stats
uppBoundB[1,2:4]=stats[1,2:4]+b
uppBoundB[2,3:4]=stats[2,3:4]+b
uppBoundB[3,4]=stats[3,4]+b

lowBoundB=stats
lowBoundB[1,2:4]=stats[1,2:4]-b
lowBoundB[2,3:4]=stats[2,3:4]-b
lowBoundB[3,4]=stats[3,4]-b

uppBoundT=stats
uppBoundT[1,2:4]=stats[1,2:4]+t
uppBoundT[2,3:4]=stats[2,3:4]+t
uppBoundT[3,4]=stats[3,4]+t

lowBoundT=stats
lowBoundT[1,2:4]=stats[1,2:4]-t
lowBoundT[2,3:4]=stats[2,3:4]-t
lowBoundT[3,4]=stats[3,4]-t





#####################################################
#3)







#b)
f=21.47/2.39
pf(f,3,26,lower=F)*2



#c)
yA=66.1
yB=65.75
yC=62.63
yD=63.85
k=4
Nk=26
alpha = 0.01

a=1/7
b=1/8
c=1/9
d=1/6

AB=abs(yA-yB)/(sqrt(2.39*(a+b)))
AC=abs(yA-yC)/(sqrt(2.39*(a+c)))
AD=abs(yA-yD)/(sqrt(2.39*(a+d)))
BC=abs(yB-yC)/(sqrt(2.39*(b+c)))
BD=abs(yB-yD)/(sqrt(2.39*(b+d)))
CD=abs(yC-yD)/(sqrt(2.39*(c+d)))

qtukey(1-alpha,k,Nk)/sqrt(2)

AB
AC
AD
BC
BD
CD

#D)
con=1/2*(yA+yB)-1/2*(yC+yD)
con2=c(1/2,1/2,-1/2,-1/2)
contrasts(as.factor(con2),as.factor(c(1,0,-1,0)))

AB=abs(yA-yB)/(sqrt(2.39*(a+b)))
AC=abs(yA-yC)/(sqrt(2.39*(a+c)))
AD=abs(yA-yD)/(sqrt(2.39*(a+d)))
BC=abs(yB-yC)/(sqrt(2.39*(b+c)))
BD=abs(yB-yD)/(sqrt(2.39*(b+d)))
CD=abs(yC-yD)/(sqrt(2.39*(c+d)))

SE=sqrt(2.39*(1/4*a+1/4*b+1/4*c+1/4*d))

f=(1/2*yA+1/2*yB-1/2*yC-1/2*yD)/SE

pf(f,3,26,lower=F)*2

#####################################################
#4)
data=read.csv("cement.csv")
head(data)

data.m = melt(data)

g=lm(value~variable, data = data.m)
anova(g)

Y = as.matrix(data)
n = rep(nrow(Y),ncol(Y))
k=ncol(Y) 
N=sum(n)
alpha = 0.05

Yidot = apply(Y, 2, mean) # sample mean for each treatment
avgY = mean(Yidot) # grand mean

kprime <- choose(k,2)

kprime = choose(k,2)
res = sum((Y-t(Yidot%*%t(rep(1,nrow(Y)))))^2)/(N-k)
b=qt(1-(alpha/(2*kprime)), N-k)*sqrt(2/5*res)

t=qtukey(1-alpha,k,N-k)/sqrt(2)*sqrt(2/5*res)

stats=matrix(0,ncol(Y)-1,ncol(Y))

for(j in 1:ncol(Y)-1)
{
  for(i in j:ncol(Y))
  {
    stats[j,i]=Yidot[i]-Yidot[j]
  }
}


uppBoundB=stats
uppBoundB[1,2:3]=stats[1,2:3]+b
uppBoundB[2,3]=stats[2,3]+b

lowBoundB=stats
lowBoundB[1,2:3]=stats[1,2:3]-b
lowBoundB[2,3]=stats[2,3]-b

uppBoundT=stats
uppBoundT[1,2:3]=stats[1,2:3]+t
uppBoundT[2,3]=stats[2,3]+t

lowBoundT=stats
lowBoundT[1,2:3]=stats[1,2:3]-t
lowBoundT[2,3]=stats[2,3]-t












































