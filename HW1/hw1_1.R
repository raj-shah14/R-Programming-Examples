find_terms <- function(x,printavg=FALSE){
  n <- length(x)
  ans<-(sum(x[0:n])/n)
  if (printavg == TRUE) print(ans)
  if (n %% 2 == 0)  #Checks No. of Elements in Sequence 
  return(T)     #Prints True if Even number of Elements
  return(F)     
}
x <- c(1,2,3,4)
find_terms(x)

y <- c(6,6,6)
find_terms(y,printavg=T)

z <- c(1,5,3,7,9)
find_terms(z,printavg=T)

w<-c(7,3,4,9,2,7,5,8)
find_terms(w,printavg=T)

q<-c(9,3,2,4,8,3,4)
find_terms(q,printavg=F)