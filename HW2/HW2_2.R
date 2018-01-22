my_test <- function(vec,mu,ci_lvl=.95){
  t<-(mean(vec)-mu)/(sd(vec)/sqrt(length(vec)))
  df<-length(vec)-1
  p_val<-2*pt(-abs(t),df = df)
  error<-qt((1-((1-ci_lvl)/2)),df =df)*sd(vec)/(sqrt(length(vec)))
  CI_low<-mean(vec)-error
  CI_high<-mean(vec)+error
  sam_est<-mean(vec)
  result<-matrix(c(t,df,CI_low,CI_high,p_val,sam_est),ncol=6)
  colnames(result)<-c("t.val","df.val","CI,low","CI.high","p.val","Sample.estimates")
  result<-as.table(result)
  return(result)
}



my_test(c(1:10),mu = 5,ci_lvl = 0.95)
