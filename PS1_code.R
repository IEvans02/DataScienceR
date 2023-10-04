#importing and reading the data 
igfdata = read.csv("igfdata.csv", header=T) 
library(tidyverse)
library(ggplot2)
library(dplyr)

#part(a)
#plotting igf to visualise
#creating a suitable plot for the distribution of igf
#plot(density(igfdata$igf), main="igf",na.rm=T)
hist(igfdata$igf,xlab="IGF",main="Distribution of IGF")
#part(b)
#Generating boxplots for igf for men and women to compare
boxplot(igfdata$igf ~ igfdata$sex,
        ylab="IGF",xlab="Sex" ,col = "green",
        main="Plot comparing the IGFs for males and females")

#part(c)
#Creating a scatterplot of igf against age
plot(igfdata$age, igfdata$igf,
     main="Graph to show IGF against Age",
     xlab="Age", ylab="IGF", pch=20)

#part(d)
df_pd = data.frame(x=igfdata$age,y=igfdata$igf)
model1 = lm(y~x,data=df_pd)
summary(model1)

  #part(e)
#Working out mean and standard deviation for both sexes
#mean
mean = by(igfdata$igf, igfdata$sex,mean, na.rm=T)
#sd
sd = by(igfdata$igf, igfdata$sex,sd, na.rm=T)

#part(f)
mean2 = mean[1]-mean[2]
l=lm(igf~sex,data=igfdata)
summary(l)

#part(g)
#working out igf wrt age and sex and seeing 
# if they are a factor against igf
under15 = filter(igfdata,age<=c(15))
#df_under15 = data.frame(under15)
lm.fit_g = lm(igf~age+sex,data=under15)
summary(lm.fit_g)

#part(h)
res = resid(lm.fit_g)
plot(fitted(lm.fit_g), res,
     ylab="Residuals",main="Response Y")
abline(0,0)
#we can observe fanning shape, thus can consider
#a graph with logs
lm.fit_g2=lm(log(igf)~age+sex,data=under15)
res=resid(lm.fit_g2)
plot(fitted(lm.fit_g2),(res),main="Log(Y) Residual Graph"
     ,ylab="Res")
abline(0,0)
     
#part(i)
over15 = filter(igfdata,age>c(15))
lm.fit_i=lm(igf~age+sex,data=over15)
summary(lm.fit_i)


#(j)
over = ifelse(igfdata$age>c(15),1,0)

df = data.frame(igf = igfdata$igf, 
                sex = igfdata$sex, overr=over,underr=under)
model5=lm(igf~sex+over,data=df)
summary(model5)
