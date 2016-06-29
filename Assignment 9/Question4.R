earlier<-c(74,114,114,87,92,55,67,118,79,83,79,92,99,87)
now<-c(70,98,90,95,88,108,110,96,91,88,120,96,90,90)
hist(earlier,prob=TRUE)
lines(density(earlier),col="red")
hist(now,prob=TRUE)
lines(density(now),col="red")
qqnorm(earlier,main="Feature Films made in 1956")
qqnorm(now,main="Feature Films made in 1996")
t.test(now,earlier)
# P-value =0.2809 which shows that mean length of movies in 1956 is more
# than mean length of movies in 1996 with 28% probability assuming null
# hypothesis is true.
# 95% Confidence Interval is (-5.623,18.480) shows that difference in movie
# is not necessarily above 0.
# 0.01 and 0.05 are ideal significance values in hypothesis testing which
# is read as probability of null hypothesis being true.
# After conducting welch's t-test which is based on normality of samples
# datasets are normally distributed as seen from plots to conduct experiment.
