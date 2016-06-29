sister<- c(69,64,65,63,65,62,65,64,66,59,62)
brother<- c(71,68,66,67,70,71,70,73,72,65,66)
all<- c(sister,brother)
mb=mean(brother)
ms=mean(sister)
mall=mean(all)
n=length(sister)
sx=sum((sister-ms)^2)/(n-1)
sy=sum((brother-mb)^2)/(n-1)
r=cor(sister,brother)
r2=r^2
# a) The sample coefficient of determination, the proportion of
# variables "explained" by simple linear regression is square of 
# correlation coefficent
r2
SST = sy*(n-1)
SSR = r2 *SST
SSE=(1-r2)*SST
df1=1
df2=9
msr=r2*SST
mse=SSE/df2
f=(n-2)*r2/(1-r2)
1-pf(f,df1,df2)
# b) Ho:beta>0 
# Since p-value>0.05, we fail to reject Null Hypothesis hence we
# cannot say that knowing sister's height helps one predict her
# brother's height.
# c)
syy=sqrt(sy)
sxx=sqrt(sx)
beta1=r*(syy/sxx)
gt=qt(0.95,df2)
se=((1-r2)*sy)/((n-2)*sx)
print("Confidence Interval is=")
lower=beta1- (gt*sqrt(se))
upper=beta1+ (gt*sqrt(se))
lower
upper
# d) Given L=0.1

