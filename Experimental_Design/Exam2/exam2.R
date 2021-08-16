
library(reshape2)
library(DescTools)
library(ggplot2)

setwd("C:\\Users\\User\\Desktop\\School\\Math_531T\\Exam2")


#####################################
#1a)



#b)

alpha=.01

qtukey(1-alpha,5,8)/sqrt(2)

yA=45
yB=58
yC=46
yD=45
yE=56

AB=abs(yA-yB)/sqrt(5*2/3)
AC=abs(yA-yC)/sqrt(5*2/3)
AD=abs(yA-yD)/sqrt(5*2/3)
AE=abs(yA-yE)/sqrt(5*2/3)
BC=abs(yB-yC)/sqrt(5*2/3)
BD=abs(yB-yD)/sqrt(5*2/3)
BE=abs(yB-yE)/sqrt(5*2/3)
CD=abs(yC-yD)/sqrt(5*2/3)
CE=abs(yC-yE)/sqrt(5*2/3)
DE=abs(yD-yE)/sqrt(5*2/3)

AB
AE
BC
BD
CE
DE

#c)


#Since these are above the calculated Tukey, then we would expect the F test to fail at a level of 0.01.


#####################################
#2a)

#Paired comparison test with 6 blocks and 2 factors (A and B).

#b)

data=c(9,19,28,22,18,8,10,22,30,21,23,12)
data

qt(0.975,5)

#p-value:
t.test(data[7:12],data[1:6],paired = T,alternative="greater")

#c)
#for confidence interval:
t.test(data[7:12],data[1:6],paired = T,alternative="two.sided")


#####################################
#3)

data=read.table("https://www2.isye.gatech.edu/~jeffwu/book/data/throughput.dat", h=T)
data

ggplot(data=data,aes(x=Method,y=Throughput,fill=Method))+geom_boxplot(outlier.shape=2,outlier.colour="blue")+scale_fill_brewer(palette="Set1")

results=aov(formula = Throughput ~ as.factor(Day) + as.factor(Operator)+ as.factor(Machine)+ as.factor(Method), data = data)
summary(results)
anova(results)

analysis=PostHocTest(results,method="hsd")$'as.factor(Method)'

qtukey(.95,5,8)/sqrt(2) #2*4=8

abs(analysis[,1])/sqrt(20.4*(2/5))



























































