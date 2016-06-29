count<- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
freq<- c(57,203,383,525,532,408,273,139,45,27,10,4,0,1,1)
# a) Average Observed Count 
x_bar=sum(count*freq)/sum(freq)
x_bar
# b) 
count1<- c(0,1,2,3,4,5,6,7,8,9)
freq1<- c(57,203,383,525,532,408,273,139,45,27)
# Expected Value for 0-9 values
expected1= dpois(count1,x_bar)*2608
expected1
# Expected value for count greater than 10
expected2=(1-ppois(9,x_bar))*2608
expected2
# Combining all the values
expected=c(expected1,expected2)
expected
# Observed Values
observed<- c(57,203,383,525,532,408,273,139,45,27,16)
# LR chi-squared test
G2 = 2 * sum(observed * log(observed/expected))
p=1-pchisq(G2,9)
# Pearson's chi-squared
X2 = sum((observed - expected)^2 / expected)
1 - pchisq(X2, df=9)
# Unrestricted dimension is 10 where as restricted dimension is 1
# hence degree of freedom df=9.
# If count of alpha particle scintillations follow a Poisson
# distribution then p= 0.12534 for likelihood ratio test statistics.
# Hence we cannot dismiss Null Hypothesis that data was drawn from
# Poisson istribution.
# Also using Pearson's chisquared test we get larger p value which
# fails to rejec the null hypothesis.