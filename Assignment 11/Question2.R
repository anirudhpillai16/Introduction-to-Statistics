height<- c(3/4,1/4)
leaves<- c(3/4,1/4)
dominant<- c(3/4,3/4)
recessive<- c(1/4,1/4)
# a) Probability of Each Ej
E1<- 3/4*3/4
E1
E2<- 3/4*1/4
E2
E3<- 1/4*3/4
E3
E4<- 1/4*1/4
E4
# b)
observed<- c(926,288,293,104)
observed
# n=1611 given in the problem
expected<-c(E1,E2,E3,E4)*1611
expected
# LR chi-squared test
G2 = 2 * sum(observed * log(observed/expected))
G2
# Degrees of Freedom= 3
1 - pchisq(G2, df=3)
# Pearson's chi-squared
X2 = sum((observed - expected)^2 / expected)
X2
1 - pchisq(X2, df=3)
# From P-value we cannot reject our Null Hypothesis hence both
# observed and expected values are more or less similar.
