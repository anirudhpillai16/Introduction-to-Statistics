score<- c(90,81,72,58,90,81,71,58,89,81,70,57,88,80,66,56,85,79,65,56,85,79,
          63,53,84,78,62,48,82,76,62,44,82,75,61,40,82,74,59,35,33)
qqnorm(score)
qqline(score)
ratio<- IQR(score)/sqrt(var(score))
ratio
# a) Since most of the data is concentrated around identity line and ratio is
# is around 1.491, we can say that data is normally distributed.

boxplot(score,horizontal = TRUE)
plot(density(score))
median(score)
mean(score)

# b) After seeing the density plot we can say that this is bimodal with
# two peaks, Most of the data is concentrated around mean and median between
# 60 and 80

