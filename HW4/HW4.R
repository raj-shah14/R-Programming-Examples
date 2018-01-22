##Prob 1

library(readxl)
LungCapData <- read_excel("C:/Users/Raj Shah/Desktop/Studies UH/MATH 6359 Stastical Computing/Homework/Answers/HW4/LungCapData.xls")
attach(LungCapData)

lm_1<-lm(LungCap~Gender,data=LungCapData)
summary(lm_1)

lm_2<-lm(LungCap~Age+I(Age^2),data=LungCapData)
summary(lm_2)

lm_3<-lm(LungCap~Age*Gender,data=LungCapData)
summary(lm_3)
#####################################################

#Prob 2
my_f_test<-function(x,z){
  ssdb=0
  ssdw=0
  xbar=mean(x)
  for (c in (1:length(z.levels))){
  xbar_i=mean(x[z==z.levels[c]])
  ssdb=round(ssdb+sum((length(x[z==z.levels[c]]))*((xbar_i-xbar)^2)),2)
  for (i in (1:length(x[z==z.levels[c]]))){
    x_ij=x[z==z.levels[c]][i]
    ssdw=round(ssdw+sum((x_ij-xbar_i)^2),2)
  }
}  
k=length(z.levels)
n=length(x)
dofb=k-1
dofw=n-k
msb=round((ssdb/dofb),2)
msw=round((ssdw/dofw),2)
f=round(msb/msw,4)
p_val=round(pf(q = f,df1=dofb,df2 = dofw,lower.tail = FALSE),4)
result<-matrix(c(dofb,dofw,ssdb,ssdw,f,p_val),ncol=6)
colnames(result)<-c("DofB","DofW","SSD_B","SSD_W","F Val","Pr(>F)")
result<-as.table(result)
print(result)
}

x<-runif(n=50,min=100,max=200)
#z.levels<-c("a","b","c")
z.levels <- letters[1:8]
z<-as.factor(sample(z.levels,size=50,replace=T))


####################################################
#Prob3

library(ISwR)
data("lung")
attach(lung)

anova(lm(volume~method))


anova(lm(volume~method+subject))




###################################################
#prob4
library(ISwR)
data("zelazo")
attach(zelazo)

zel <- unlist(zelazo,use.names=FALSE)
baby<-c(rep("Active",length(1:6)),rep("Passive",length(7:12)),rep("None",length(13:17)),rep("Ctr.8w",length(18:23)))
f_test<-factor(baby)
anova(lm(zel~f_test))
summary(lm(zel~f_test))
pairwise.t.test(zel,f_test,p.adjust.method = "bonferroni")






