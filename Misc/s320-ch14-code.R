### Bivariate normal data

# Trosset's bivariate normal functions
source("binorm.R")
# or just copy-paste if you don't know the path

# No association:
x = rnorm(1000)
y = rnorm(1000)
plot(x, y)
binorm.scatter(cbind(x, y))
cor(x, y)

# Strong positive
# linear association
x = rnorm(1000)
y = rnorm(1000) + x
binorm.scatter(cbind(x, y))
cor(x, y)

# Strong negative linear association
x = rnorm(1000)
y = rnorm(1000) - x
binorm.scatter(cbind(x, y))
cor(x, y)

# Weak positive linear association
x = rnorm(1000)
y = rnorm(1000) + x/4
binorm.scatter(cbind(x, y))
cor(x, y)

# Weak negative linear association
x = rnorm(1000)
y = rnorm(1000) - x/4
binorm.scatter(cbind(x, y))
cor(x, y)

# Nonlinear association: NOT bivariate normal
x = rnorm(1000)
y = rnorm(1000) + x^2
binorm.scatter(cbind(x, y))
# Ellipse does a bad job

# Real data: Checking bivariate normality
load("anorexia.RData")
# or file.choose()
# std.before: Weight before standard treatment
# std.diff: Change in weight
std.diff = std.after - std.before
# Is std.before approx. normal?
qqnorm(std.before)
# Is std.diff approx. normal?
qqnorm(std.diff)
# Is the data bivariate normal?
plot(std.before, std.diff)
binorm.scatter(cbind(std.before, std.diff))
cor(std.before, std.diff)

# What is correlation?
# Change x to standard units:
mean.before = mean(std.before)
sd.before = sd(std.before)
z.before = (std.before - mean.before) / sd.before
# Change y to standard units:
mean.diff = mean(std.diff)
sd.diff = sd(std.diff)
z.diff = (std.diff - mean.diff) / sd.diff
# Find the average of the products
n = length(std.before)
sum(z.before * z.diff) / (n - 1)
# Same as correlation

# Correlation is a measure of linear association

x = rnorm(1000)
y = rnorm(1000) + x
plot(x, y)
cor(x, y)
# Correlation does a good job
# of measuring the relationship

# Lognormal: The logs of the data come from a normal
x = rlnorm(100)
y = rlnorm(100)
plot(x, y)
cor(x, y)
# Correlation does an okay job
# of measuring the relationship

# Nonlinear data
x = rnorm(1000)
y = x^3
plot(x, y)
cor(x, y)
cor(x, y, method="kendall")
cor(x, y, method="spearman")


x = rnorm(1000)
y = x^2
plot(x, y)
cor(x, y)
# Correlation does a bad job
# of measuring the relationship
# Note: There are other kinds of correlation
# but they fail as well
cor(x, y, method="kendall")
cor(x, y, method="spearman")

# Back to anorexia data:
plot(std.before, std.diff)
# Lighter girls gain weight
# Heavier girls lose weight
# What's the explanation?
summary(std.before)
summary(std.after)
plot(std.before, std.after)
cor(std.before, std.after)
# Test null hypothesis of zero correlation
cor.test(std.before, std.after)
# The relationship between weight before
# and weight after is weak at best
# It doesn't matter much whether you were
# light or heavy before
plot(density(std.after))
# You'll mostly likely end up around
# the mean after
# This is a special case of
# "REGRESSION TO THE MEAN"
# or "THE REGRESSION EFFECT"
# or just "REGRESSION"



