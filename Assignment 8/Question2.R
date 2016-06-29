# a) Generating 4 samples from Normal Distribution with n=19
d <- data.frame(matrix(ncol = 4, nrow = 19))
i=4
while (i>0){
  x<-rnorm(19)
  d[,i]<-x
  i=i-1
}
d
# b) QQ Plot for each sample
plot<-function(x) { qqnorm(x)}
apply(d,2,plot )
# c) ratio of Interquantile range to standard deviation for each sample
x<-c()
ratio<-function(x) { IQR(x)/sd(x)}
x<-c(x,apply(d,2,ratio))
x
# d) After trying in R, It is quite plausible to say that x bar was drawn
# from normal distribution
# 2.
z<-c(1.1402,-1.8658,0.8520,-1.8251,0.8530,-0.0589,-1.6554,-1.7599,-1.4330,
     -1.3853,2.9794,2.4919,2.1601,2.2670,-0.5479,-0.7164,0.6462,
     -0.8365,1.1997)
tval= ((mean(z)-0)/(sd(z)/sqrt(19)))
tval
pval<- 1-pt(tval,18)
pval
ci<- mean(z)+c(-1,1)*qt(.950,18)*sd(z)/sqrt(19)
ci
# Since P value is so high , we cannot reject null hypothesis HO:?? ??? 0
# 3
z<- sort(z)
z
k<- qbinom(0.05,19,0.5)
k
# K=6 and 90% Confidence Interval of median (xk+1, xn-k) = (x7,x13)
z[7]
z[13]
