library(readr)
normal<-function(n,mean,sd){
  dis<-rnorm(n,mean = mean,sd = sd)
  mean.diff<-abs(mean-mean(dis))
  var.diff<-abs(sd^2-var(dis))
  med.diff<-abs(qnorm(.5,mean = mean,sd = sd)-median(dis))
  quant.diff<-abs(quantile(dis,c(.99))-qnorm(.99,mean = mean,sd = sd))
  prob<-pnorm(median(dis),mean = mean,sd = sd)
  result<-matrix(c(mean.diff,var.diff,med.diff,quant.diff,prob),ncol = 5)
  colnames(result)<-c("Mean.diff","Var.diff","Med.diff","Q99.dif.99%","Prob.Val")
  result<-as.table(result)
  return(result)
}

for (n in c(10^2,10^4,10^6))
  print(normal(n,6,2))