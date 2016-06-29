# a) Sample Mean of our data
library(Hmisc)
hs<-c(1,2,3,4,5,6,7)
hno<-c(27,54,16,13,6,3,1)
data<-data.frame(hs,hno)
sm<-weighted.mean(data$hs,data$hno)
sm
# b)Standard Deviation of Household size
varData<-weighted.mean(data$hs^2,data$hno)-sm^2
sdData<-sqrt(varData)
sdData
# c) Standard Error
se<- sdData/sqrt(7)
se
# d) approximate probability that the absolute value of the error in a 
#survey of this form
#and size is less than 0
pnorm(0,0,0.49)
# e) Yescoz the sample mean pf household size is coming out to be 2.41 
#with standard error around 0.48 which is mostly between 2 and 3.
