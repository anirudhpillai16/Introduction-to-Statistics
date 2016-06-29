threeft<- c(173,150)
thirtyfive<- c(125,73)
male<- c(173,125)
female<- c(150,73)
observed<- c(173,125,150,73)
N=sum(observed)
e1=(sum(male)*sum(threeft))/N
e2=(sum(male)*sum(thirtyfive))/N
e3=(sum(female)*sum(threeft))/N
e4=(sum(female)*sum(thirtyfive))/N
expected<- c(e1,e2,e3,e4)
expected
observed
# Degree of Freedom= (r-2)(c-1)= 1
# LR chi-squared test
G2 = 2 * sum(observed * log(observed/expected))
G2
1 - pchisq(G2, df=1)
# Pearson's chi-squared
X2 = sum((observed - expected)^2 / expected)
X2
1 - pchisq(X2, df=1)
# From our p-value, we reject our Null Hypothesis that sex ratio of
# Panamanian sandflies varies with height above ground.