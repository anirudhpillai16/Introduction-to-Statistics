# 144 Races
observed<- c(29,19,18,25,17,10,15,11)
# Ho:- Horse's starting position does not affect winning
expected<-rep(144/8, 8)
# LR chi-squared test
G2 = 2 * sum(observed * log(observed/expected))
1 - pchisq(G2, df=7)
# Pearson's chi-squared
X2 = sum((observed - expected)^2 / expected)
1 - pchisq(X2, df=7)
# We can see that both results yield value less than 0.05 hence we can 
# reject our Null Hypothesis that horse's starting position does not
# affect it's chance of winning. ( I am assuming 95% Confidence
#Interval) since it's not mentioned in problem.