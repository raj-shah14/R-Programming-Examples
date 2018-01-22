
##########################################################
#prob 1
library(readr)
my_two_test<-function(succ,n,type="z"){
  su1<-succ[1]
  su2<-succ[2]
  p_hat<-sum(succ)/sum(n)
  n1<-n[1]
  n2<-n[2]
  p1_hat<-su1/n1
  p2_hat<-su2/n2
  
  
  ts<-(p1_hat-p2_hat)/sqrt(p_hat*(1-p_hat)*((1/n1)+(1/n2)))
  p_val<-2*(1-pnorm(abs(ts)))
  
  if(type == "z"){
    result<-matrix(c(ts,type,p_val,p1_hat,p2_hat),ncol=5)
    colnames(result)<-c("TS","Type","P-Value","Sample Estimate","Sample Estimate")
    result<-as.table(result)
    #print(result)
    print(p_val)
  }
  if(type == "chi"){
    result<-matrix(c(ts^2,type,p_val,p1_hat,p2_hat),ncol=5)
    colnames(result)<-c("TS","Type","P-Value","Sample Estimate","Sample Estimate")
    result<-as.table(result)
    print(result)
  }
  
}

#my_two_test(succ = c(23,25),n = c(40,50))
p.set.1<-c(0.80,0.28)
p.set.2<-c(0.47,0.54)
p.set.3<-c(0.25,0.25)
for (n in c(10^2,10^4,10^6)){
  b1<-rbinom(1,n,p.set.1[1])
  b2<-rbinom(1,n,p.set.1[2])
  vec_set1<-c(b1,b2)
  my_two_test(vec_set1,c(n,n))
  
  b3<-rbinom(1,n,p.set.2[1])
  b4<-rbinom(1,n,p.set.2[2])
  vec_set2<-c(b3,b4)
  my_two_test(vec_set2,c(n,n))
  
  b5<-rbinom(1,n,p.set.3[1])
  b6<-rbinom(1,n,p.set.3[2])
  vec_set3<-c(b5,b6)
  my_two_test(vec_set3,c(n,n))
}


##########################################################
#prob 2
pepulc_heal<-c(23,18)
pepulc_total<-c(30,31)
prop.test(pepulc_heal,pepulc_total,correct = FALSE)
prop.test(pepulc_heal,pepulc_total,correct = FALSE,alternative = "l")
#null 
#h0 p1=p2 where p1 is from Drug 1 and p2 from Drug 2 (p1=23/30 p2=18/31)
#Ha p1!= p2



#Fishers test is more accurate for sample samples as it calculates exact p-val.If all of the expected values are very large, 
#Fisher's exact test becomes computationally impractical.
#For large samples it is not advisable to use.

prop.test(matrix(c(23,18,7,13),2),correct = FALSE)
fisher.test(matrix(c(23,18,7,13),2))

#-> Here we see the p-value has reduced for Fisher's test as compared to chi-squared
#-> (Ask about Significance?) Since Fisher's test is exact test, we can say from results,
# that its deviation from null hypothesis is less compared to chi squared.
# but since its greater than 0.05 we dont reject the null hypothesis

#->In fisher's test, the output confidence interval describes the odds ratio.
# CI contains 1 
##########################################################
#Prob 3
library(readxl)
LungCapData <- read_excel("C:/Users/Raj Shah/Desktop/Studies UH/MATH 6359 Stastical Computing/Homework/Answers/HW3/LungCapData.xls")
attach(LungCapData)

Lungcap.lm<-lm(LungCap~Age,data=LungCapData)
summary(Lungcap.lm)


par(mfrow=c(1,2))
plot(Lungcap.lm)

Lungcap.lm<-lm(LungCap~Age,data=LungCapData,subset=-103)
summary(Lungcap.lm)

plot(Lungcap.lm)


par(mfrow=c(1,1))
plot(x = Age,y = LungCap)
abline(lm(LungCap~Age),col="red")s


##########################################################
#Prob 4
library(readr)
library(datasets)
data(mtcars)
attach(mtcars)
pairs(mtcars, gap=0, cex.labels=0.9)

cars.lm<-lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars)
summary(cars.lm)

step(cars.lm)

summary(lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars))
summary(lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear,data=mtcars))
summary(lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am,data=mtcars))
summary(lm(mpg~cyl+disp+hp+drat+wt+qsec+vs,data=mtcars))
summary(lm(mpg~cyl+disp+hp+drat+wt+qsec,data=mtcars))
summary(lm(mpg~cyl+disp+hp+drat+wt,data=mtcars))
summary(lm(mpg~cyl+disp+hp+wt,data=mtcars))
summary(lm(mpg~cyl+hp+wt,data=mtcars))
summary(lm(mpg~cyl+wt,data=mtcars))
summary(lm(mpg~wt,data=mtcars))


