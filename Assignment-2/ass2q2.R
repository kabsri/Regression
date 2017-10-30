y0 = scan("http://www.math.mcgill.ca/yyang/regression/data/US-GDP.txt")
y = 100*log(y0[-1]/y0[-278])
x1 = c(1:277)
x1_begin = c(1:(132))
x1_end = c(133:277)
y_begin = y[1:132]
y_end = y[133:277]
fit.Begin = lm(y_begin~x1_begin)
fit.End = lm(y_end~x1_end)

plot(x1, y, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth")

plot(x1_begin, y_begin, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth (first half)")
abline(coef(fit.Begin), col=3)

plot(x1_end, y_end, xlab="Time (Quarter Years)", ylab="GDP Growth (%)", pch=18)
title("US GDP Growth (second half)")
abline(coef(fit.Begin), col=3)
abline(coef(fit.End), col=2)

abline(c(confint(fit.End)[1,2], confint(fit.End)[2,2]), col=2, lty=2)
abline(c(confint(fit.End)[1,1], confint(fit.End)[2,1]), col=2, lty=2)