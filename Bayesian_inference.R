

setwd("C:/Users/jimflower/Desktop/bas")
library(readxl)

day1<-read_excel("day1.xlsx")

shapiro.test(day1$X)
qqnorm(day1$X,main="acc")
qqline(day1$X)
shapiro.test(day1$Y)
qqnorm(day1$Y,main="acc")
qqline(day1$Y)
shapiro.test(day1$Z)
qqnorm(day1$Z,main="acc")
qqline(day1$Z)






#이항분포
# 사전데이타
a<-36 ; b<-14

#관측 데이타
n<-197 ; x<-126

# a discretization of the possible theta values
theta = seq(0, 1, length=50)
prior.theta = dbeta(theta, a, b)

# prob of data|theta(likelihood)
likhd.theta = dbinom(x, n, theta)

# joint prob of data & theta
joint.xtheta <- prior.theta*likhd.theta

# posterior of theta 
post.theta <- dbeta(theta, a+x, b+n-x) 

par(mfrow=c(2, 2)) # set up a 2x2 plotting window
plot(theta, prior.theta, type="l", sub="(a) prior: pi(theta)")
abline(v=(a-1)/(a-1+b-1), lty=2) # a vertical line at the mode

plot(theta, likhd.theta, type="l", sub="(b) likelihood: f(x|theta)")
abline(v=x/n,lty=2)

plot(theta, joint.xtheta, type="l", sub="(c) prior x likelihood:pi(theta)x f(x|theta)")
abline(v=(a+x-1)/(a+b+n-2), lty=2)

plot(theta, post.theta, type="l", sub="(d) posterior: pi(theta|x)")
abline(v=(a+x-1)/(a+b+n-2), lty=2)
par(mfrow=c(1, 1))


#사후분포
par(mfrow=c(1, 1))
plot(theta, post.theta, type="l", col="blue")
lines(theta, prior.theta, col="red", lty=2)

legend(.5, 3, legend=c(paste("beta(",a,", ",b,") prior"),
                       paste("post under beta(",a,", ",b,") prior")),
       lty=c(2, 1), col=c("red", "blue"), bty="n")



# simulation-based inference
theta = rbeta(2000, a+x, b+n-x) # generate posterior samples

hist(theta, prob=T, main="Histogram of theta")
lines(density(theta))

mean.theta = mean(theta)
abline(v=mean.theta, lty=2)

quantile(theta, c(.025, .975)) # simulation-based quantiles

qbeta(c(.025, .975), a+x, b+n-x) # theoretical quantiles

# simulation-based estimates
mean(theta) ; var(theta) 

# theoretical estimates
(a+x)/(a+b+n) ; (a+x)*(b+n-x)/((a+b+n+1)*(a+b+n)^2)





a<-36 ; b<-14
# X|theta ~ B(n, theta)
n<-197 ; x<-126
m=10; z=0:10
#pred.z=(gamma(m+1)/(gamma(z+1)*gamma(m-z+1)))*beta(a+x+z,b+n-x+m-z)/beta(a+x,b+n-x)
pred.z=choose(m,z)*beta(a+x+z,b+n-x+m-z)/beta(a+x,b+n-x)
par(mfrow=c(1,1))
plot(z,pred.z,xlab='z',ylab='probability',type='h')
title('Predictive Distribution: a=36, b=14, n=197, x=126, m=10')
(m.z<-sum(z*pred.z))
sum((z-m.z)^2*pred.z)

# mean
m*(a+x)/(a+b+n)
# variance
(m*(a+x)*(b+n-x)*(a+b+n+m))/((a+b+n)^2*(a+b+n+1))




#사후 구간
theta = seq(0, 1, length=50)
prior.theta = dbeta(theta, a, b)
likhd.theta = dbinom(x, n, theta)
joint.xtheta <- prior.theta*likhd.theta
post.theta <- dbeta(theta, a+x, b+n-x) 
par(mfrow=c(1, 1))
plot(theta, post.theta, type="l", sub="(d) posterior: pi(theta|x)")
abline(v=0.596, lty=1)
abline(v=0.714, lty=1)
abline(v=0.5929987, lty=2)
abline(v=0.7149494, lty=2)


#사후구간 수치
a=36; b=14
x=126; n=197
theta=seq(0,1,length=1001)
ftheta=dbeta(theta,a+x, n-x+b)
prob=ftheta/sum(ftheta)

HPD=HPDgrid(prob, 0.95)

HPD.grid=c( min(theta[HPD$index]), max(theta[HPD$index]) )
HPD.grid



#고전적 신뢰구간
library(binom)
#2 p.133
n1=247; x1=162

CI.exact=binom.confint(x1, n1, conf.level = 0.95, methods = c("exact"))
CI.exact=c(CI.exact$lower, CI.exact$upper)

CI.exact


#사후분위수 사후구간
HPD.approx=qbeta(c(0.025, 0.975),a+x, n-x+b)

p=x/n
CI.asympt=c(p-1.96*sqrt(p*(1-p)/n) ,p+1.96*sqrt(p*(1-p)/n) )

HPD.approx
CI.asympt


theta = seq(0, 1, length=50)
prior.theta = dbeta(theta, a, b)
likhd.theta = dbinom(x, n, theta)
joint.xtheta <- prior.theta*likhd.theta
post.theta <- dbeta(theta, a+x, b+n-x) 
plot(theta, post.theta, type="l", sub="(d) posterior: pi(theta|x)")
abline(v=0.5956068 , lty=1)
abline(v=0.7137430, lty=1)
abline(v=0.5725482 , lty=2)
abline(v=0.7066396, lty=2)







#포아송 분포---------------------------------------------------------------



##포아송


x1 = rep(c(0, 1, 2, 3, 4, 5, 6, 7), c(31, 42, 43, 36, 17, 6, 4, 2))
x2 = rep(c(0, 1, 2, 3, 4, 5, 6, 7), c(42, 45, 44, 32, 8, 7, 2, 1))

par(mfrow=c(1,2))
plot(table(x1), xlab="x1", ylab="frequency", main="City 1", type="h",xlim=c(0,7))
plot(table(x2), xlab="x2", ylab="frequency", main="City 2", type="h",xlim=c(0,7))


a =2; b = 1
n1 = length(x1); s1 = sum(x1)
n2 = length(x2); s2 = sum(x2)

postmean.theta1 = (a+s1)/(b+n1)
postmean.theta2 = (a+s2)/(b+n2)

### 두 시티와 감마분포
par(mfrow=c(1, 1))

theta <- seq(0, 6, length=100)

plot(theta, dgamma(theta, a+s1, b+n1), type="l", xlab="theta",ylab="p(theta | x)")
lines(theta, dgamma(theta, a+s2, b+n2), lty=2)
lines(theta, dgamma(theta, a, b), lty=3)

legend(3.5, 1.5, legend=c(paste("City 1"),
                          paste("City 2"), paste("Gamma(2, 1) prior")),
       lty=c(1, 2, 3), bty="n")


#제프리 분포
a = 1/2; b = 0

plot(theta, dgamma(theta, a+s1, b+n1), type="l", xlab="theta",ylab="p(theta | x)")
lines(theta, dgamma(theta, a+s2, b+n2), lty=2)
lines(theta, 1/sqrt(theta), lty=3)

legend(3.5, 1.5, legend=c(paste("City 1"),
                          paste("City 2"), paste("Jeffrey prior")),
       lty=c(1, 2, 3), bty="n")




x1=c(rep(0,31),rep(1,42),rep(2,43),rep(3,36),rep(4,17),rep(5,6),rep(6,4),rep(7,2))
x2=c(rep(0,42),rep(1,45),rep(2,44),rep(3,32),rep(4,8),rep(5,7),rep(6,2),rep(7,1))

a <-2 ; b<- 1
n1 <-length(x1); s1<- sum(x1)
n2 <-length(x2); s2<- sum(x2)
x<- seq(0:10)

par(mfrow=c(1,2))
plot(x,dnbinom(x,size=a+s1,prob=(b+n1)/(b+n1+1)), xlab="x_{n+1}",
     ylab="P(x_{n+1}|x_1,...,x_n)", type="h", main="City 1")
abline(v=2.2903,col="red",lty=2)
plot(x,dnbinom(x,size=a+s2,prob=(b+n2)/(b+n2+1)), xlab="x_{n+1}",
     ylab="P(x_{n+1}|x_1,...,x_n)",type="h", main="City 2")
abline(v=1.516,col="red",lty=2)





HPDgrid=function(prob, level=0.95){
  prob.sort=sort(prob, decreasing=T)
  M=min(which(cumsum(prob.sort)>=level))
  height=prob.sort[M]
  HPD.index=which(prob >= height)
  HPD.level=sum(prob[HPD.index])
  res=list(index=HPD.index, level=HPD.level)
  return(res)
}
HPD=HPDgrid(prob, 0.95)
HPD.grid=c( min(theta[HPD$index]), max(theta[HPD$index]) )
HPD.grid










x1=c(rep(0,31),rep(1,42),rep(2,43),rep(3,36),rep(4,17),rep(5,6),rep(6,4),rep(7,2))
x2=c(rep(0,42),rep(1,45),rep(2,44),rep(3,32),rep(4,8),rep(5,7),rep(6,2),rep(7,1))
a=2; b=1
n1=length(x1); s1=sum(x1)
n2=length(x2); s2=sum(x2)

c(n1,s1)
c(n2,s2)

########## 1. 

##Approximation method using lattice points
HPDgrid=function(prob, level=0.95){
  prob.sort=sort(prob, decreasing=T)
  M=min(which(cumsum(prob.sort)>=level))
  height=prob.sort[M]
  HPD.index=which(prob >= height)
  HPD.level=sum(prob[HPD.index])
  res=list(index=HPD.index, level=HPD.level)
  return(res)
}

theta=seq(1,4,length=1001)

ftheta=dgamma(theta,a+s1,b+n1)
prob=ftheta/sum(ftheta)
HPD1=HPDgrid(prob, 0.95)
HPD1.grid=c(min(theta[HPD1$index]),max(theta[HPD1$index]))
HPD1.grid
HPD1$level

dgamma(HPD1.grid,a+s1,b+n1)

ftheta=dgamma(theta,a+s2,b+n2)
prob=ftheta/sum(ftheta)
HPD2=HPDgrid(prob, 0.95)
HPD2.grid=c(min(theta[HPD2$index]),max(theta[HPD2$index]))
HPD2.grid
HPD2$level

dgamma(HPD2.grid,a+s2,b+n2)

##
par(mfrow=c(1,1))
plot(theta,dgamma(theta,a+s1,b+n1),type='l',xlab=expression(theta),ylab=expression(paste(pi,'(',theta,'|x)')))
lines(theta,dgamma(theta,a+s2,b+n2),col=2)
abline(v=c(HPD1.grid[1],HPD1.grid[2]),lty=2,col=1)
abline(v=c(HPD2.grid[1],HPD2.grid[2]),lty=3,col=2)
abline(v=1.845531 , lty=2, col=3)
abline(v=2.264359, lty=2,col=3)
abline(v=1.548961 , lty=2,col=3)
abline(v=1.934555, lty=2,col=3)


legend(3.0,1.5,c('city 1','city 2'),lty=2:3,col=1:2)

HPD1.grid[2]-HPD1.grid[1]
HPD2.grid[2]-HPD2.grid[1]









#Approximation CI using quantile
lc1=qgamma(0.025,a+s1,b+n1)
uc1=qgamma(0.975,a+s1,b+n1)
c(lc1,uc1)

dgamma(c(lc1,uc1),a+s1,b+n1)

lc2=qgamma(0.025,a+s2,b+n2)
uc2=qgamma(0.975,a+s2,b+n2)
c(lc2,uc2)

dgamma(c(lc2,uc2),a+s2,b+n2)

##

plot(theta,dgamma(theta,a+s1,b+n1),type='l',xlab=expression(theta),ylab=expression(paste(pi,'(',theta,'|x)')))
lines(theta,dgamma(theta,a+s2,b+n2),col=2)
abline(v=c(lc1,uc1),lty=2,col=1)
abline(v=c(lc2,uc2),lty=3,col=2)
abline(v=1.845531 , lty=2, col=3)
abline(v=2.264359, lty=2,col=3)
abline(v=1.548961 , lty=2,col=3)
abline(v=1.934555, lty=2,col=3)
legend(3.0,1.5,c('city 1','city 2'),lty=2:3,col=1:2)

uc1-lc1
uc2-lc2






lc1=qgamma(0.025,a+s1,b+n1); uc1=qgamma(0.975,a+s1,b+n1)
c(lc1,uc1) # (1.666, 2.449) => length = 0.783
dgamma(c(lc1,uc1),a+s1,b+n1)
lc2=qgamma(0.025,a+s2,b+n2); uc2=qgamma(0.975,a+s2,b+n2)
c(lc2,uc2) # (1.846, 2.704) => length = 0.858
dgamma(c(lc2,uc2),a+s2,b+n2)
##
plot(theta,dgamma(theta,a+s1,b+n1),type='l',xlab=expression(theta),
     ylab=expression(paste(pi,'(',theta,'|x)')))
lines(theta,dgamma(theta,a+s2,b+n2),col=2)
abline(v=c(lc1,uc1),col=1)
abline(v=c(lc2,uc2),col=2)
legend(3.0,1.5,c('city 1','city 2'),lty=1:1,col=1:2)
uc1-lc1; uc2-lc2






cityl1=c(2.054945-1.96*sqrt(2.066236/181) ,2.054945+1.96*sqrt(2.066236/181) )
cityl1 #1.845531 2.264359 => length = 0.418828

cityl2=c(1.741758-1.96*sqrt(1.751328/181) ,1.741758+1.96*sqrt(1.751328/181) )
cityl2 #1.548961 1.934555 => length = 0.38594




##정규분포

#K리그 선수
mu0=19384; s0=1054; n=27; xbar=18574; s=771

# posterior of theta
(w=(1+s^2/(n*s0^2))^{-1})
(mu.post=w*xbar+(1-w)*mu0)
(s.post=sqrt(w*s^2/n))
s.post^2

par(mfrow=c(1,2))
theta=seq(mu.post-3*s.post,mu.post+3*s.post,length=100)
plot(theta,dnorm(theta,mu.post,s.post),type='l',main='posterior and prior of theta')
#plot(theta,dnorm(theta,mu.post,s.post),type='l',main='posterior and prior of theta', xlim=c(-20,20))
lines(theta,dnorm(theta,mu0,s0),lty=2,col=2)
legend(5.5,0.5,c('posterior','prior'),lty=1:2,col=1:2)

# predictive for x(new)
(mu.new=mu.post)
(s.new=sqrt(s^2+s.post^2))

x.new=seq(mu.new-5*s.new,mu.new+5*s.new,length=100)
plot(x.new,dnorm(x.new,mu.new,s.new),type='l',main='predictive density of x(new)')
abline(v=18565.96,col="red",lty=2)



## Bayesian Credible Region: HPD




#posterior

mu0=19384; s0=1054; n=27; xbar=18574; s=771

# posterior of theta
(w=(1+s^2/(n*s0^2))^{-1})
(mu.post=w*xbar+(1-w)*mu0)
(s.post=sqrt(w*s^2/n))
s.post^2

########## 1. 

##Approximation method using lattice points
HPDgrid=function(prob, level=0.95){
  prob.sort=sort(prob, decreasing=T)
  M=min(which(cumsum(prob.sort)>=level))
  height=prob.sort[M]
  HPD.index=which(prob >= height)
  HPD.level=sum(prob[HPD.index])
  res=list(index=HPD.index, level=HPD.level)
  return(res)
}


theta=seq(mu.post-5*s.post,mu.post+5*s.post,length=100001)
ftheta=dnorm(theta,mu.post,s.post)
prob=ftheta/sum(ftheta)
HPD1=HPDgrid(prob, 0.95)
HPD1.grid=c(min(theta[HPD1$index]),max(theta[HPD1$index]))
HPD1.grid
HPD1$level

dnorm(HPD1.grid,mu.post,s.post)

##
par(mfrow=c(1,1))
plot(theta,dnorm(theta,mu.post,s.post),type='l',xlab=expression(theta),ylab=expression(paste(pi,'(',theta,'|x)')))
abline(v=c(HPD1.grid[1],HPD1.grid[2]),lty=2,col=2)
legend(8.0,0.5,c('HPD interval'),lty=2,col=2)

########## 2. 
theta=seq(mu.post-5*s.post,mu.post+5*s.post,length=1001)

#Approximation CI using quantile
lc=qnorm(0.025,mu.post,s.post)
uc=qnorm(0.975,mu.post,s.post)
c(lc,uc)

dnorm(c(lc,uc),mu.post,s.post)

##
par(mfrow=c(1,1))
plot(theta,dnorm(theta,mu.post,s.post),type='l',xlab=expression(theta),ylab=expression(paste(pi,'(',theta,'|x)')))

abline(v=c(HPD1.grid[1],HPD1.grid[2]),lty=2,col=4)
abline(v=c(lc,uc),lty=2,col=2)
abline(v=18301.76  , lty=2, col=3)
abline(v=18877.72, lty=2,col=3)

legend(8.0,0.4,c('quantile','lattice'),lty=c(2,4),col=c(2,4))








