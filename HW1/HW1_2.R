library(ISwR)
juul.sex.igf1 <- juul[c("sex","igf1")]
head(juul.sex.igf1) # - Both head() and summary() calls.
summary(juul.sex.igf1) # show there are missing observations (NA).
juul.sex.igf1.noNA <- na.omit(juul.sex.igf1) # - Excludes the missing observations.
head(juul.sex.igf1.noNA) # - Now NA's are gone.
summary(juul.sex.igf1.noNA) # - Yep, they are gone.
attach(juul.sex.igf1.noNA)


## for-loop - goes through values of 'sex' and 'igf1' scalar-by-scalar
#....
count_m<-0
count_f<-0
for (i in 1:length(juul.sex.igf1.noNA$igf1)){
    ifelse(juul.sex.igf1.noNA$igf1[i]>400,ifelse(juul.sex.igf1.noNA$sex[i]<1.1,count_m<-count_m + 1,count_f<-count_f + 1),1)
}
d<-data.frame(count_m,count_f)
names(d)<-c("M","F")
print(d)

#
# ## vectorized solution - operates on 'sex' and 'igf1' as vectors
# #....
 number<-juul.sex.igf1.noNA$sex[juul.sex.igf1.noNA$igf1>400]
 print(table(ifelse(number>1.1,'F','M')))
#
#
#
# # subsetting solution - via function subset() for the 'juul.sex.igf1.noNA' data frame
# # + another function that actually counts the number
 m_num<-subset(juul.sex.igf1.noNA,sex=="1" & igf1>400 | sex=="2" & igf1>400,select=c(sex))
 c_label<-table(m_num)
 names(c_label)<-c("M","F")
 print(c_label)
