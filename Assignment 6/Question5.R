# a) EX of X
library(Hmisc)
x<-c(-2,-1,12)
px<-c(0.3,0.6,0.1)
plot(x,px ,xlab="x values",ylab="Probability of x",main="PMF")
EX<-wtd.mean(x,px)
EX
# b) Variance OF X
Varx<-wtd.mean(x^2,px)-(EX^2)
Varx
# c) Expected value of ???= µ
# E[???]= 0
# d) Variance of ???
Varx_b<-Varx/3
Varx_b
# e) Given that n=100
y<-rnorm(100,0,sqrt(5.4))
# P(???>0.5) is
1-pnorm(0.5,mean(y),sd(y))
