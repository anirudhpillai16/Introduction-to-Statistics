normal<- c(4.1,6.3,7.8,8.5,8.9,10.4,11.5,12.0,13.8,17.6,24.3,37.2)
diabetic<- c(11.5,12.1,16.1,17.8,24.0,28.8,33.9,40.7,51.3,56.2,61.7,69.2)
qqnorm(normal,main="Normal")
qqnorm(diabetic,main = "Diabetic")
boxplot(normal,diabetic,main="Box Plot of Normal and Diabetic")
hist(normal,prob=TRUE)
lines(density(normal),col="blue")
hist(diabetic,prob=TRUE)
lines(density(diabetic),col="blue")
# 1 )
#After seeing qqplot,boxplot and histogram we can say that samples are 
# not drawn from normal distribution.
# 2)
# (a) Natural Logarithm
lognormal<- log(normal)
logdiabetic<- log(diabetic)
hist(lognormal,prob=TRUE)
lines(density(lognormal),col="blue")
hist(logdiabetic,prob=TRUE)
lines(density(logdiabetic),col="blue")
qqnorm(lognormal,main="Natuarl Log of Normal")
qqnorm(logdiabetic,main ="Natural Log of Diabetic")
# (b) Square Root
sqrtnormal<-sqrt(normal)
sqrtdiabetic<-sqrt(diabetic)
hist(sqrtnormal,prob=TRUE)
lines(density(sqrtnormal),col="red")
hist(sqrtdiabetic,prob=TRUE)
lines(density(sqrtdiabetic),col="red")
qqnorm(sqrtnormal,main="Square root Log of Normal")
qqnorm(sqrtdiabetic,main ="Square root of Diabetic")
# I would prefer log transformation over square root transformation since
# log transformation is more symmetric to normal distribution.
# 3)
# As seen from histograms, density plots and qqplots, log transformed
# measurements appear closer to normal distribution.
# 4)
# Welch's t-test
Delta = mean(logdiabetic) - mean(lognormal)
se = sqrt(var(logdiabetic)/12 + var(lognormal)/12)
Tw = Delta/se
nu = (var(logdiabetic)/12+var(lognormal)/12)^2/((var(logdiabetic)/12)^2/11+(var(lognormal)/12)^2/11)
Pvalue = 2*(1-pt(abs(Tw),df=nu))
Pvalue

# Welch 95% confidence interval
q = qt(0.975, df=nu)
lower = Delta - q*se
upper = Delta + q*se
CI<-c(lower,upper)
CI
# Since P-value is quite low we can reject H0 in favor of Ha.