# Question 3 (10.5 Problem Set D)
r<- c(0.693,0.662,0.690,0.606,0.570,0.749,0.672,0.628,0.609,0.844,0.654,
      0.615,0.668,0.601,0.576,0.670,0.606,0.611,0.553,0.933)
z=log(r)
plot(density(r))
plot(density(z))

boxplot(r,z,horizontal = TRUE,main="Box Plots")
s=IQR(r)/sqrt(var(r))
s
t=IQR(z)/sqrt(var(z))
t
# 1)Both the ratios and the log of the ratios are very similar when tested
# for normality but log of ratios behave like normal distribution. This can
# be easily seen by looking at the density plot or boxplot. In all the 
# cases the log of the ratios is slightly better with respect to normal
# distribution i.e. more shifted to right, In General Density plot of
# ratios has two bumps where as Density plot of log of ratios has only one
# and after looking at IQR to Stdev ratio we can say that log of ratios
# is closer to what we expect to be normal deviation.
#______________________________________________________________________
# 2) I would use the log of the ratios for which an assumption of 
# normality seems more plausible. Therefore the mean we would like to test
# now is 
log(0.618034)
# For the hypothesis testing, from the point of view of the anthropologist
# would be:
# H0 : � = ???0.4812. vs. H1 : �!= ???0.4812
# One could argue that the anthropologist wants to minimize Type I error,
# i.e., that the Shoshoni civilization actually used golden rectangles 
# but the test shows otherwise. This is why in the test H0 represent
# the golden ratio.
# TO Calculate the Student's 1-sample t-test ,we need mean
m=mean(z)
m
st= sqrt(var(z))
st
tn= (m+0.4812)/(st/sqrt(20))
tn
# p = 2 ??? pt(???2.02, df = 19) = 0.05771 > 0.05 = ?? ??? fail to reject H0
y<- sort(r)
# 3) To build a confidence interval with confidence 0.90, the following 
# needs to hold: 1????? = 0.90 =???   ??/2 = 0.05
k=qbinom(0.05, 20, 0.5)
# By Experimentation
1- pbinom(k,20,0.5)
# We can construct a confidence interval of 94% which is very close to
# 95% and any other choice would be way off the value.
# The form of interval is  (sorting the values) : 
# (x(k+1), x(n-k))= (x7, x14)
y[7]
y[14]
