data1<- c(0.246,0.530,1.098,2.063,0.327,0.583,1.158,2.105,0.423,0.613,1.163,
         2.106,0.425,0.641,1.439,4.363,0.434,1.054,1.464,7.517)
# A) Empirical CDF
fn=ecdf(data1)
plot(fn)
# B) Mean, Variance, Median and IQR of data
mean(data1)
var(data1)
median(data1)
quantile(fn)
# IQR= 1.61375- 0.50600= 1.10775
# C) 
IQR(data1)/sqrt(var(data1))
# 0.6466837 shows that data is not drawn from normal distribution as ratio
# does not conform to that of normal distribution it can also be seen from
# plotting kernel density function or box plot.
# D)
qqnorm(data1)
#The normal probability plot does not conform to a normal distribution. 
#Only looking at this piece of information, this data does not seem to 
#be drawn from a normal distribution.
# E)
y<-log(data1)
plot(density.default(x=y))
qqnorm(y)
quantile(y)
IQR(y)
IQR(y)/sqrt(var(y))
# From looking at kernel density plot, normal plot and IQR to stdev ratio
# We can say that y is not drawn from normal distribution.
