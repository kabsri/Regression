file1 = "http://www.math.mcgill.ca/yyang/regression/data/a1-1.txt"
data1 = read.table(file1, header = TRUE)
model1 = lm(data1$y ~ data1$x)
coef(model1)
plot(data1$x, data1$y, pch=18, xlab="X", ylab="Y")
title("Dataset 1")
abline(coef(model1))
plot(data1$x, residuals(model1), pch=18, xlab="X", ylab="Residuals")
title("Dataset 1 Residuals")

file2 = "http://www.math.mcgill.ca/yyang/regression/data/a1-2.txt"
data2 = read.table(file2, header = TRUE)
model2 = lm(data2$y ~ data2$x)
coef(model2)
plot(data2$x, data2$y, pch=18, xlab="X", ylab="Y")
title("Dataset 2")
abline(coef(model2))
plot(data2$x, residuals(model2), pch=18, xlab="X", ylab="Residuals")
title("Dataset 2 Residuals")

file3 = "http://www.math.mcgill.ca/yyang/regression/data/a1-3.txt"
data3 = read.table(file3, header = TRUE)
model3 = lm(data3$y ~ data3$x)
coef(model3)
plot(data3$x, data3$y, pch=18, xlab="X", ylab="Y")
title("Dataset 3")
abline(coef(model3))
plot(data3$x, residuals(model3), pch=18, xlab="X", ylab="Residuals")
title("Dataset 3 Residuals")
