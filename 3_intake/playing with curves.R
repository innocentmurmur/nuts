income.dist<-read.csv("~/Rdata/nutrients/income_histogram_by_country_code.csv")
income<-read.csv("~/Rdata/nutrients/income_deciles_by_country_code.csv")
income.counts<-read.csv("~/Rdata/nutrients/income_counts_by_country_code.csv")
  
  View(income.dist)

hist(income.counts$AFG)
plot(density(income.counts$AFG))
plot(ecdf(income.counts$AFG))
qqnorm(income.counts$AFG)
abline(0,1)
x.wei<-rweibull(n=2000,shape=2.1,scale=1.1)
x.teo<-rweibull(n=200, shape=2,scale=1)
qqplot(income.dist$AFG,x.wei)
abline(0,1)

require(MASS)

fitdistr(income$AFG,"weibull")
# shape        scale   
# 2.0378921   11.3559007 
# ( 0.4654131) ( 1.8727181)


serb<-c(3.46,4.9,6.08,7.14,8.17,9.28,10.55,12.21,14.88,23.33)
serbint<-c(1,1,2,2,2,2,3,3,3,5)

fitdistr(serb,"weibull")
#shape        scale   
#1.9533399   11.3475291 
#( 0.4549091) ( 1.9493281

qqplot(serb,rweibull(n=200, shape=1.953399,scale=11.3475291))

hist(serb)
lines(dweibull(xx,scale=11.3475291,shape=1.953399))

curve(dweibull(xx,scale=11.3475291,shape=1.953399),from=0,to=50,main="weibull distribution")
curve(dpois(x,lambda=2.5),from=0,to=10)

table(serbint)
tab.os<-table(serbint)
lambda.est<-mean(serbint))

freq.os<-vector()
for(i in 1: length(tab.os)) freq.os[i]<-tab.os[[i]]

freq.ex<-(dpois(0:max(serbint),lambda=lambda.est)*10)

h<-hist(serbint,breaks=5)
> xhist<-c(min(h$breaks), h$breaks)
> yhist<-c(0,h$density,0)
xfit<-seq(min(serbint),max(serbint),length=10)
> yfit<-dnorm(xfit,mean=mean(serbint),sd=sd(serbint))
> plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),main="normal pdf and histogram")
> lines(xfit,yfit,col="red")



fitdistr(income.counts$AFG,"Poisson")
mean(income.counts$AFG)
qqplot(income.counts$AFG,rpois(n=200, lambda=2.5))
abline(0,1)

table(income.counts$AFG)
tab.os<-table(income.counts$AFG)
lambda.est<-mean(income.counts$AFG)

freq.os<-vector()
for(i in 1: length(tab.os)) freq.os[i]<-tab.os[[i]]

freq.ex<-(dpois(0:3,lambda=lambda.est)*20)
#freq.ex<-(dpois(0:max(income.counts$AFG)),lambda=lambda.est)*20)
freq.ex

acc<-mean(abs(freq.os-trunc(freq.ex)))
acc


chisquared.AFG<-sum(((freq.os-freq.ex)^2)/freq.ex)
#but I have no idea how to get the p and the df... i forget these things. 




h<-hist(income.counts$AFG,breaks=5)
xhist<-c(min(h$breaks), h$breaks)
yhist<-c(0,h$density,0)
xfit<-seq(min(income.counts$AFG),max(income.counts$AFG),length=10)
yfit<-dpois(1:10,lambda=mean(income.counts$AFG))
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),main="normal pdf and histogram")
lines(xfit,yfit,col="red")


haiti<-c(0.66,
         1.72,
         2.67,
         3.58,
         4.57,
         5.79,
         7.55,
         10.09,
         15.7,
         47.67
         )
xx <- seq(0,80, length=300)
plot(pafg~max.percent, data=income.dist, main="AZE",xaxt='n', ylab="percent of population/10")
lines(xx, qgamma(pafg,shape=1,rate=1), col="red")

summary(fit)





##making a frequency table
factorx<-factor(cut(serb, breaks=nclass.Sturges(serb)))
xout<-as.data.frame(table(factorx))
xout<-transform(xout,cumFreq=cumsum(Freq),relative=prop.table(Freq))
xout

