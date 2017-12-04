filter = read.csv("http://www.math.mcgill.ca/yyang/regression/data/Filter.csv", header=TRUE)
y = filter$noise
x1 = filter$carsize
x2 = filter$type
fit1 = lm(y~1)
fit2 = lm(y~x1)
fit3 = lm(y~x2)
fit4 = lm(y~x1+x2)
fit5 = lm(y~x1+x2+x1:x2)
Model = c("1", "1+X1", "1+X2", "1+X1+X2", "1+X1+x2+X1:X2")
SSRes = c(anova(fit1)$Sum[1], anova(fit2)$Sum[2], anova(fit3)$Sum[2], anova(fit4)$Sum[3], anova(fit5)$Sum[4])
p = c(1, 3, 2, 4, 6)
table = data.frame(Model, SSRes, p)

n=36
f_stat = ((SSRes[2]-SSRes[5])/(p[5]-p[2]))/(SSRes[5]/(n-p[5]))
1-pf(f_stat, p[5]-p[2], n-p[5])
