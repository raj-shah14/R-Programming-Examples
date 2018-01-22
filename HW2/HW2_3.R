library(readr)

computers <-read.csv("C:/Users/Raj Shah/Downloads/Studies UH/MATH 6359 Stastical Computing/Homework/Answers/HW2/data2.csv",header=TRUE,sep=",")
par(mfrow=c(2,2))

attach(computers)
Harddisk<-computers$hd
Price<-computers$price

hist(Harddisk,breaks = 20,main = "Hard Disk for PC",col="red",labels = TRUE)
hist(Price,breaks = 20,main = "Price for PC",col="green",labels = TRUE)

qqnorm(Harddisk,main="Q-Q plot of Hard Disk for PC")
qqnorm(Price,main="Q-Q plot of Price for PC")

par(mfrow=c(1,1))
boxplot(Harddisk,Price,ylab="Personal Computers",col=2:3,ylim=c(100,4000),names = c("Hard Disk","Price"))

t.test(Harddisk,Price)
wilcox.test(Harddisk,Price)
