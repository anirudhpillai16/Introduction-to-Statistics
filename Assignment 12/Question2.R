x<- c(-0.2,-0.9,-0.4,0.6,0.4)
y<- c(0.4,-0.3,-0.3,0.5,1.1)
# a) Pearson's Correlation Coefficient is given by
print("Pearson's Correlation Coefficient is =")
r=cor(x,y)
r
# b)
plot(x,y)
# Predict y from x
slope = r *(sd(y) / sd(x))
intercept = mean(y) - (slope * mean(x))
predictions = intercept + slope*x
SSE = sum((y - predictions)^2)
abline(intercept, slope, col="red")

# Equation of line y=intercept + slope*x
# In this case, Equation is y=0.36 + 0.80x
# c)
# Predict x from y
plot(y,x)
slope2 = r *(sd(x) / sd(y))
intercept2 = mean(x) - (slope * mean(y))
predictions2 = intercept + slope*y
SSE2 = sum((x - predictions)^2)
abline(intercept2, slope2, col="blue")

# Equation of line here is x=intercept1 + slope2* y
# x=-0.32 + 0.84y

# d) Scatter Plot
source('C:/Stats/Assignment 11/binorm.R')
plot(x,y)
abline(intercept, slope, col="red")
abline(intercept2, slope2, col="blue")
binorm.scatter(cbind(x, y))
abline(intercept, slope, col="red")
abline(intercept2, slope2, col="blue")
