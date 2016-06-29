salaries = read.table(file.choose(), header=TRUE)
Faculty = salaries$Salary[salaries$Job == "Faculty"]
Athletics = salaries$Salary[salaries$Job == "Athletics"]
# 1)
boxplot(Faculty,Athletics,main="Box Plot",horizontal = TRUE,
        names=c("Faculty","Athletics"),col=c("blue","green"))
# Median salary of Faculty is more than median salary of Athletics and there
# are more outliers in Athletics than Faculty, There is one huge outlier
# in athletics compared to faculty.
t.test(Faculty,Athletics)
# 2)
qqnorm(Faculty,main = "QQ Plot for Faculty")
qqnorm(Athletics,main = "QQ Plot for Athletics")
plot(density(Faculty))
plot(density(Athletics))
# Welch's Two-Sample t-test is based on assumption that samples are 
# normally distributed.We cannot trust p -value because  we can see 
#from QQ Plot and density plot that i)both faculty and athletics are 
#not normally distributed. ii) sample size is relatively small
# iii) There are huge outliers in both faculty and athletics (There is
# huge outlier in athletics which will effect mean to large extent)
# 3)
logfac=log(Faculty)
logath=log(Athletics)
qqnorm(logfac,main="Faculty After Transformation")
qqnorm(logath,main = "Athletics After Transformation")
var1=var(logfac)
var2=var(logath)
# We should use Welch's two sample test since data is close to normal
# distribution after taking log and variances of the samples are not 
# equal. Student's t-test could have been used if variance values were
# equal or closer hence Welch'ss two sample test seems plausible.
# 4)
m1=mean(logfac)
m2=mean(logath)
Delta = mean(logfac) - mean(logath)
se = sqrt(var(logfac)/50 + var(logath)/50)
Tw = Delta/se
nu = (var(logfac)/50+var(logath)/50)^2/((var(logfac)/50)^2/49+(var(logath)/50)^2/49)
Pvalue = 2*(1-pt(abs(Tw),df=nu))
Pvalue
# Welch 95% confidence interval
q = qt(0.975, df=nu)
lower = Delta - q*se
lower
upper = Delta + q*se
upper
# Since P-value is less than 0.05, we can reject our null hypothesis.
# 5)
summary(Faculty)
summary(Athletics)
par(mfrow=c(1,2))
hist(Faculty,prob=TRUE)
lines(density(Faculty),col="blue")
hist(Athletics,prob=TRUE)
lines(density(Athletics),col="red")
# We can see that Mean and median for Faculty is more than Athletics and
# There is huge outlier in Athletics as seen in summary. From Histogram
# we can say that faculty and athletics don't have same distribution.