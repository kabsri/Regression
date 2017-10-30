salary = read.csv("http://www.math.mcgill.ca/yyang/regression/data/salary.csv", header=TRUE)
x1 = salary$SPENDING/1000
y = salary$SALARY
fit.Salary = lm(y~x1)
summary(fit.Salary)

n = length(x1)
A = matrix (c(n, sum(x1), sum(x1), sum(x1^2)), 2, 2)
alpha = c(sum(y), sum(x1*y))
beta = solve(A) %*% alpha
beta

plot(x1, y, xlab="Spending per pupil ($1000)", ylab = "Salary ($)", pch=18)
title("Annual Teacher Salary vs Per Pupil Spending")
abline(beta, col=4)

X = matrix( c(rep(1, n), x1), n, 2)
estimatedY = X %*% beta
e = y-estimatedY
SSres = sum(e^2)
RSE = sqrt(SSres/(n-2))
RSE

ese_beta0_table = beta[1]/summary(fit.Salary)$coefficients[, 3][1]
ese_beta0_table

ESE = (RSE^2)*solve( t(X) %*% X)
ese_beta0_data = sqrt(ESE[1,1])
ese_beta0_data

SSreg = sum((estimatedY - mean(y))^2)
R_2 = SSreg/(SSres+SSreg)
R_2

Sxy = sum(y*(x1-mean(x1)))
Sxx = sum((x1-mean(x1))^2)
beta[2]*Sxy
SSreg

f = SSreg/(SSres/(n-2))
f

H = X %*% solve(t(X) %*% X) %*% t(X)
H1 = (1/n) * matrix(1, n, n)
I = diag(n)
sum(diag(I-H1))
sum(diag(H-H1))

plot(x1, e, xlab ="Spending per pupil ($1000)", ylab="Residuals", pch=18)
title("Residual Plot")
abline(0, 0, lty=2)

t(diag(I)) %*% e
t(X) %*% e
t(estimatedY) %*% e

xnew = matrix(c(1, 4.8), 1, 2)
xnew %*% beta

predictionError = RSE*sqrt((xnew %*% solve(t(X) %*% X) %*% t(xnew)))
predictionError