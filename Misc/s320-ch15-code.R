# See also the html file for the regression lecture

batting = read.table("C:/Stats/batting.txt", header=TRUE)
summary(batting)
plot(batting$OB2012, batting$OB2013)
# Model 1: Predict the mean
# for everyone
mean(batting$OB2013)
SST = sum((batting$OB2013 - mean(batting$OB2013))^2)
abline(h=mean(batting$OB2013), col="blue")

# Model 2: Predict using regression
slope = cor(batting$OB2012, batting$OB2013) *
  sd(batting$OB2013) / sd(batting$OB2012)
intercept = mean(batting$OB2013) -
  slope * mean(batting$OB2012)
predictions = intercept + slope*batting$OB2012
SSE = sum((batting$OB2013 - predictions)^2)
abline(intercept, slope, col="red")
# Ratio
SSE/SST
1 - cor(batting$OB2012, batting$OB2013)^2

# Model 3: Predict 2013 OB = 2012 OB
abline(0, 1, col="orange")
sum((batting$OB2013 - batting$OB2012)^2)

# Inference for the slope
# Test hyp that slope = 0
se = sd(batting$OB2013)/sd(batting$OB2012) *
  sqrt((1-cor(batting$OB2012, batting$OB2013)^2)/
         (length(batting$OB2012)-2))
T = slope / se
# P-value
2 * (1-pt(T, length(batting$OB2012)-2))
# 95% CI
slope - qt(.975, df=length(batting$OB2012)-2)*se
slope + qt(.975, df=length(batting$OB2012)-2)*se

# F-test
r = cor(batting$OB2012, batting$OB2013)
F = (length(batting$OB2012)-2)*r^2/(1-r^2)
# P-value
1 - pf(F, 1, length(batting$OB2012)-2)

# Prediction
# Robinson Cano:
# 2012: 264 OB, 697 AB
# 2013: ? OB, 681 AB
qqnorm(batting$OB2012)
qqnorm(batting$OB2013)
residuals = batting$OB2013 - predictions
qqnorm(residuals)

# Create two new ratio variables
obp2012 = batting$OB2012 / batting$PA2012
obp2013 = batting$OB2013 / batting$PA2013
qqnorm(obp2012)
qqnorm(obp2013)
plot(obp2012, obp2013)

# Fit regression
slope = cor(obp2012,obp2013)*sd(obp2013)/
  sd(obp2012)
intercept = mean(obp2013)-slope*mean(obp2012)
# Check
lm(obp2013 ~ obp2012)

cano2012 = 264/697
# Point prediction for 2013 OBP
slope * cano2012 + intercept
# Point prediction for 2013 OB
(slope * cano2012 + intercept) * 681
# Prediction error
resOBP = obp2013 - (slope*obp2012+intercept)
SSE.OBP = sum(resOBP^2)
pred.error = sqrt(SSE.OBP/(length(obp2012)-2))
pred.error * 681

