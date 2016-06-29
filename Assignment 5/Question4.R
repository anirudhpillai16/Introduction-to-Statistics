data<- c(462,425,164,784,625,472,658,658,663,928,92,230,96,626,1277,225,150,
         320,496,157,458,933,861,174,431)
# a) Empirical CDF
fn= ecdf(data)
plot(fn)
# b) Mean and Variance
m=mean(data)
m
v=var(data)
v
# c) Median and Interquantile Range
me=median(data)
me
quantile(fn)
# d) Ratio of IQR to square root of variance
# IQR= 658-225= 433
ratio= 433/sqrt(var(data))
ratio
# e) Box Plot
boxplot(data)



