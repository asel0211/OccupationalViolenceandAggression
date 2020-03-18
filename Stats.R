library(broom)

# Import Dataset
Viol <- read_csv("Tidy Data.csv")
View(Viol)

# Multivariate logistic regression
fit <- glm(formula=`Violence` ~ `Time`+`Role`+`Location`, data=Viol, family=binomial)
summary(fit)
tidy(fit, exponentiate = TRUE, conf.int= TRUE) # Broom package - tidy() tool
anova(fit, test="Chisq")

# Univariate logistic regression
fit <- glm(formula=`Violence` ~ `Role`, data=Viol, family=binomial)
summary(fit)
tidy(fit, exponentiate = TRUE, conf.int= TRUE)

fit <- glm(formula=`Violence` ~ `Time`, data=Viol, family=binomial)
summary(fit)

fit <- glm(formula=`Violence` ~ `Location`, data=Viol, family=binomial)
summary(fit)

fit <- glm(formula=`Violence` ~ `Hospital`, data=Viol, family=binomial)
summary(fit)

# Chi squared test for day of the week
x=c(13,17,8,13,19,14,8) # Yes to violence, Mon-Sun
y=c(32,34,16,18,17,5,21) # No to violence, Mon-Sun
chisq.test(x, y)

# Chi squared and Fisher's Exact test for Odds Ratios, CIs, and p values
# MON
x <- matrix(data = c(13,32,79,111), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# TUE
x <- matrix(data = c(17,34,75,109), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# WED
x <- matrix(data = c(8,16,84,127), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# THUR
x <- matrix(data = c(13,18,79,125), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# FRI
x <- matrix(data = c(19,17,73,126), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# SAT
x <- matrix(data = c(14,5,78,138), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)

# SUN
x <- matrix(data = c(8,21,84,122), nrow=2, ncol=2)
chisq.test(x)
odds.ratio(x, level=0.95)