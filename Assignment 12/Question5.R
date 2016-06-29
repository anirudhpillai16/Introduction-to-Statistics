ext=read.table('C:/Stats/Assignment 12/examanxiety.txt',header = TRUE)
exam<-ext$Exam
anxiety<-ext$Anxiety
b = cor(exam, anxiety) * sd(exam) / sd(anxiety)
a = mean(exam) - b * mean(anxiety)
plot(anxiety, exam,main="Plot of Exam Vs Anxiety",xlab="Anxiety",ylab="Exam")
abline(a, b, col="red")

# Equation of line is y=a+bx
# Exam=111-0.73* Anxiety

# b)
fit<-lm(Exam~Anxiety,data = ext)
par(mfrow=c(2,2),mar=c(4,4,2,1))
# Plot to check for Homodeskascity, Normality of errors, Independence
# of errors
plot(fit)
# From Plot, we conculde that out of all assumptions only assumption
# about normality of error seems true.

# c)
# Suppose X and Y are bivariate normal.Then given a specific value
# of X, Y follows normal distribution.
# cor(anxiety,exam)=-0.439 correlation is negative hence we should 
# not use bivariate normal and instead fit an ellipse in scatter plot
# ellipse won't fit density but will be cross like shape.
