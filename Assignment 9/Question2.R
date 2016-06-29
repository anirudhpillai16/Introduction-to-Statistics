typea<- c(233,291,312,250,246,197,268,224,239,239,254,276,234,181,248,252,
          202,218,212,325)
typeb<- c(344,185,263,246,224,212,188,250,148,169,226,175,242,252,153,183,
          137,202,194,213)
qqnorm(typea,main = "TYPE A")
qqnorm(typeb,main = "TYPE B")
boxplot(typea,typeb,main="Box Plot of Type A and Type B")
# QQplot for both Type A and Type B suggests some values may be inconsistent
# with normal distribution specially largest in each set as seen in boxplot.
a=IQR(typea)/sqrt(var(typea))
a
b=IQR(typeb)/sqrt(var(typeb))
b
# Ratio for Type B suggest sample more close to normal distribution but
# also has large outlier hence I would not assume data was drawn from
# normal distribution although there is slight chance of being picked up
# from normal distribution.
delta<- mean(typea)- mean(typeb)
n1=length(typea)
n2=length(typeb)
va=var(typea)/n1
vb=var(typeb)/n2
se=sqrt(va+vb)
nu<- (va+vb)^2/(va^2/(n1-1)+vb^2/(n2-1))
# If we let alpha= 0.05 then
1- pt(2.5621,nu)
# 0.007405 < 0.05 = alpha -> reject H0
# b) We want 90% confidence interval for delta,
qt=qt(0.95,nu)
lower=delta-qt*se
upper=delta+qt*se
lower 
upper
