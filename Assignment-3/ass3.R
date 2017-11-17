#Reading the data
data = read.csv("http://www.math.mcgill.ca/yyang/regression/data/cigs.csv" , header=TRUE)
y = data$CO
x1 = data$TAR
x2 = data$NICOTINE
x3 = data$WEIGHT

#Getting SSRes(beta0, beta1, beta2, beta3)
full_model = lm(y~x1+x2+x3)
SS_res_full = anova(full_model)[4,2]
SS_res_full

#Getting SSRes(beta0, beta1, beta2)
reduced_model = lm(y~x1+x2)
SS_res_reduced = anova(reduced_model)[3,2]
SS_res_reduced

#Getting F-statistic for x1,x2,x3 model vs x1,x2 model
n = length(x1)
p = 4
r=1
F_stat  = ((SS_res_reduced - SS_res_full)/r)/(SS_res_full/(n-p))
F_stat

#Getting decomposition for adding x3, then x2, then x1
table_1 = anova(lm(y~x3+x2+x1))
SSR3_0 = table_1[1,2]
SSR2_03 = table_1[2,2]
SSR1_032 = table_1[3,2]
decomp_1 = c(SSR3_0, SSR2_03, SSR1_032, SSR3_0+SSR2_03+SSR1_032)
decomp_1

#Getting decomposition for adding x1, then x2
table_2 = anova(reduced_model)
SSR1_0 = table_2[1,2]
SSR2_01 = table_2[2,2]
decomp_2 = c(SSR1_0, SSR2_01, SSR1_0+SSR2_01)
decomp_2

#Getting F-statistic for x1, x2 model vs x1 model
reduced_model_2 = lm(y~x1)
SS_res_red2 = anova(reduced_model_2)[2,2]
p=3
r=1
F_stat2 = ((SS_res_red2 - SS_res_reduced)/r)/(SS_res_reduced/(n-p))
F_stat2

#Getting F-statistic for x1,x2 model vs mean model
F_stat3 = summary(reduced_model)$fstatistic[1]
F_stat3
