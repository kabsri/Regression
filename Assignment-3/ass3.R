data = read.csv("http://www.math.mcgill.ca/yyang/regression/data/cigs.csv" , header=TRUE)
y = data$CO
x1 = data$TAR
x2 = data$NICOTINE
x3 = data$WEIGHT

X = matrix(c(x1, x2, x3), length(x1), 3)
fit.Cigs = lm(y~X)
summary(fit.Cigs)

plot(fit.Cigs)
