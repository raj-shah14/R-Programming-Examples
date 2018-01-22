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
   return(result)
 }
 if(type == "chi"){
   result<-matrix(c(ts^2,type,p_val,p1_hat,p2_hat),ncol=5)
   colnames(result)<-c("TS","Type","P-Value","Sample Estimate","Sample Estimate")
   result<-as.table(result)
   return(result)
 }
 
}

#my_two_test(succ = c(23,25),n = c(40,50))
p1<-0.82
p2<-0.26
b1<-rbinom(82 ,size = 100,p = p1)
b2<-rbinom(26,size = 100,p = p2)
