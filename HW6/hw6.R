#Prob 1
library(datasets)
library(boot)
data(mtcars)
mpg<-mtcars$mpg
m<-10000

my.boot<-function(x,f,b){
  theta.hat<-f(x)
  B<-b
  theta.hat.b<-numeric(B)
  for (t in 1:B){
    i <- sample(1:length(x), replace = TRUE)
    theta.hat.b[t]<-f(x,i)
  }
  #return(R)
  theta.star<-(1/B)*sum(theta.hat.b)
  se<-sqrt((1/(B-1))*sum((theta.hat.b-theta.star)^2))
  bi<-theta.star-theta.hat
  
  ci.norm<-f(theta.hat.b)+c(-1,1)*qnorm(.975)*se
  ci.basic<-theta.star+quantile((theta.hat.b-theta.hat),c(0.025,0.975))
  ci.percentile<-quantile(theta.hat.b,c(0.025,0.975))
  result<-matrix(c(bi,se,ci.norm,ci.basic,ci.percentile),ncol=8)
  colnames(result)<-c("Bias","Std.Error","CI Norm L","CI Norm U","CI Basic L","CI Basic U","CI Perc L","CI Perc U")
  result<-as.table(result)
  return(result)
  #return(c(se,bi,ci.norm,ci.basic,ci.percentile))
}

my.median<-function(x,indices){
  return(median(x[indices]))
}

my.mean<-function(x,indices){
  return(mean(x[indices]))
}


my.boot(mpg,my.mean,m)
R.boot<-boot(mpg,statistic=function(x,i) mean(x[i]),R=m)
R.boot.ci<-boot.ci(R.boot)
t.test(mpg,mu=0)

##Could not be done for median as data is normally distributed
################################################################
#Prob 2
library(ISwR)
data("cystfibr")
attach(cystfibr)

n<-nrow(cystfibr)
cv.err.1<-cv.err.2<-cv.err.3<-numeric(n)
for (j in 1:n){
  y<-pemax[-j];a<-age[-j];s<-sex[-j];w<-weight[-j];b<-bmp[-j];f<-fev1[-j];r<-rv[-j];fr<-frc[-j];t<-tlc[-j]
  m1<-lm(y~a+s+w+b+f+r+fr+t,data=cystfibr)
  yhat1<-m1$coefficients[1]+m1$coefficients[2]*age[j]+m1$coefficients[3]*sex[j]+m1$coefficients[4]*weight[j]+m1$coefficients[5]*bmp[j]+m1$coefficients[6]*fev1[j]+m1$coefficients[7]*rv[j]+m1$coefficients[8]*frc[j]+m1$coefficients[9]*tlc[j]
  cv.err.1[j]<-pemax[j]-yhat1
  
  h<-height[-j]
  m2<-lm(y~a+s+h+w+b,data=cystfibr)
  yhat2<-m2$coefficients[1]+m2$coefficients[2]*age[j]+m2$coefficients[3]*sex[j]+m2$coefficients[4]*height[j]+m2$coefficients[5]*weight[j]+m2$coefficients[6]*bmp[j]
  cv.err.2[j]<-pemax[j]-yhat2
  
  m3<-lm(y~a+s+f+r+fr+t,data=cystfibr)
  yhat3<-m3$coefficients[1]+m3$coefficients[2]*age[j]+m3$coefficients[3]*sex[j]+m3$coefficients[4]*fev1[j]+m3$coefficients[5]*rv[j]+m3$coefficients[6]*frc[j]+m3$coefficients[7]*tlc[j]
  cv.err.3[j]<-pemax[j]-yhat3
}
print(c(mean(cv.err.1^2),mean(cv.err.2^2),mean(cv.err.3^2)))


#Model 3 is the best model as the error is the least

##################################################################
#Prob 3
library("rmutil")
n<-1000
a<-2
b<-2
x<-b/((1-runif(n))^(1/a))
hist(x,probability = TRUE,main = "Pareto Distribution")
y<-seq(0.1,100,.1)
lines(y,(2*b^a)/(y^3),col="red")
lines(y,dpareto(y,m=a,s=b),col="blue")
legend("topright",legend=c("Formula","Pareto Function"),col=c("red","blue"),lwd=c(1,1.5))

###################################################################
#Prob 4

m<-10000
beta.mc<-function(m,a,b,x){
  z<-rbeta(m,shape1 = a,shape2 = b)
  g<-(z<x)
  cdf<-mean(g)
  return(cdf)
}
est.beta<-vector()
x<-seq(0.1,0.9,length=9)
for (i in x){
  est.beta<-append(est.beta,beta.mc(m,2,2,i))
}
real.beta<-pbeta(x,2,2)
print(round(rbind(x,est.beta,real.beta),3))
rm(est.beta)

