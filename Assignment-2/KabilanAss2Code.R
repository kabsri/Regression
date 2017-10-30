#Question 1
salary = read.csv("http://www.math.mcgill.ca/yyang/regression/data/salary.csv", header=TRUE)
x1 = salary$SPENDING/1000
y = salary$SALARY
fit.Salary = lm(y~x1)
summary(fit.Salary)

#Estimating beta
n = length(x1)
A = matrix (c(n, sum(x1), sum(x1), sum(x1^2)), 2, 2)
alpha = c(sum(y), sum(x1*y))
beta = solve(A) %*% alpha
beta

#Plotting the line of best fit we calculated
plot(x1, y, xlab="Spending per pupil ($1000)", ylab = "Salary ($)", pch=18)
title("Annual Teacher Salary vs Per Pupil Spending")
abline(beta, col=4)

#Calculating resi
X = matrix( c(rep(1, n), x1), n, 2)
estimatedY = X %*% beta
e = y-estimatedY
SSres = sum(e^2)
RSE = sqrt(SSres/(n-2))
RSE

#Calculating estimate standard error two different ways
#By values in the table
ese_beta0_table = beta[1]/summary(fit.Salary)$coefficients[, 3][1]
ese_beta0_table

#From the data directly
ESE = (RSE^2)*solve( t(X) %*% X)
ese_beta0_data = sqrt(ESE[1,1])
ese_beta0_data

#Calculating R^2
SSreg = sum((estimatedY - mean(y))^2)
R_2 = SSreg/(SSres+SSreg)
R_2

#Showing that SSReg = beta_1 * Sxy
Sxy = sum(y*(x1-mean(x1)))
Sxx = sum((x1-mean(x1))^2)
beta[2]*Sxy
SSreg

#Calculating F-statistic
f = SSreg/(SSres/(n-2))
f

#Calculating the trace of the matrices
H = X %*% solve(t(X) %*% X) %*% t(X)
H1 = (1/n) * matrix(1, n, n)
I = diag(n)
sum(diag(I-H1))
sum(diag(H-H1))

#Plotting the residuals
plot(x1, e, xlab ="Spending per pupil ($1000)", ylab="Residuals", pch=18)
title("Residual Plot")
abline(0, 0, lty=2)

#Showing the orthogonality of e, y_hat, and 1
t(diag(I)) %*% e
t(X) %*% e
t(estimatedY) %*% e

#Predicting at a new value
xnew = matrix(c(1, 4.8), 1, 2)
xnew %*% beta

#Calculating the prediction error
predictionError = RSE*sqrt((xnew %*% solve(t(X) %*% X) %*% t(xnew)))
predictionError

######################
##Quesiton 2 Code
#######################
y0 = scan("http://www.math.mcgill.ca/yyang/regression/data/US-GDP.txt")
y = 100*log(y0[-1]/y0[-278])
x1 = c(1:277)
x1_begin = c(1:(132))
x1_end = c(133:277)
y_begin = y[1:132]
y_end = y[133:277]
fit.Begin = lm(y_begin~x1_begin)
fit.End = lm(y_end~x1_end)

#Plotting the given data
plot(x1, y, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth")

#Plotting only the first half with its line of best fit
plot(x1_begin, y_begin, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth (first half)")
abline(coef(fit.Begin), col=3)

#Plotting only the second half with lines of best fit for both halves superimposed to help with comparison
plot(x1_end, y_end, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth (second half)")
abline(coef(fit.Begin), col=3)
abline(coef(fit.End), col=2)

#Getting confidence interval for the second line of best fit 
abline(c(confint(fit.End)[1,2], confint(fit.End)[2,2]), col=2, lty=2)
abline(c(confint(fit.End)[1,1], confint(fit.End)[2,1]), col=2, lty=2)