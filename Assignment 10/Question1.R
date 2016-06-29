A<- c(37.54,37.01,36.71,37.03,37.32,37.01,37.03,37.70,37.36,36.75,37.45,38.85)
B<- c(40.17,40.80,39.76,39.70,40.79,40.44,39.79,39.38)
C<- c(39.04,39.21,39.05,38.24,38.53,38.71,38.89,38.66,38.51,40.08)
ALL<-c(A,B,C)
boxplot(A,B,C,main="Box Plot of A,B and C",names=c('A','B','C'),col = 
          c("Green","Blue","Yellow"))
# Check Normality
qqnorm(A,main="QQ Plot for Sample A")
qqnorm(B,main="QQ Plot for Sample B")
qqnorm(C,main="QQ Plot for Sample C")
n1=length(A)
n2=length(B)
n3=length(C)
N=n1+n2+n3
# Check homoscedasticity
sda=sd(A)
sdb=sd(B)
sdc=sd(C)
sda
sdb
sdc
# Assumptions of Normality:-
# The distribution resembles closer to normality for sample B though 
# location of median may question it's symmetry and normal distribution. 
# We cannot conclude that data is normally distributed. Sample A and C are pretty
# much symmetric and have outliers that questions normal distribution.
# Assumption of Homoscedasticity:-
# Standard Deviation of all the samples are more or less equal
# Hence Homoscedasticity assumption is quite plausible.
meana=mean(A)
meanb=mean(B)
meanc=mean(C)
grand.mean=mean(ALL)

# Total Sum of Squares
SST = sum( (ALL-grand.mean)^2 )
total.df = N - 1

# Between sum-of-squares and mean-square
SSB = n1*(meana-grand.mean)^2 +
  n2*(meanb-grand.mean)^2 +
  n3*(meanc-grand.mean)^2
between.df = 2 
between.meansquare = SSB/2

# Within sum-of-squares and mean-square
SSW = sum( (A-meana)^2 ) +
  sum( (B-meanb)^2 ) +
  sum( (C-meanc)^2 )
# Alternative formula
SSW = (n1-1)*var(A) +
  (n2-1)*var(B) +
  (n3-1)*var(C)
within.df = N - 3
within.meansquare = SSW/within.df

# Check these are equal
SST
SSB + SSW
# The two are equal.

# Are these close?
between.meansquare
within.meansquare

# F-test
F = between.meansquare/within.meansquare
# P-value
1 - pf(F, df1=between.df, df2=within.df)
