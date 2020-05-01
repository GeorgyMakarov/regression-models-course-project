# exploratory data analysis
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
summary(mtcars$mpg)

# t.test
at <- mtcars[mtcars$am == 0,]
mt <- mtcars[mtcars$am == 1,]
t.test(at$mpg, mt$mpg)$p.value
t.test(at$mpg, mt$mpg)$conf

# regression model mpg ~ am
fit1 <- lm(mpg ~ am, data = mtcars)
summary(fit1)$coeff
summary(fit1)$r.squared

# choosing top predictors
fit_full <- lm(mpg ~ ., data = mtcars)
step_fit <- step(fit_full, trace = 0)
summary(step_fit)$coeff

# multivariate regression
fit4 <- lm(mpg ~ am + cyl + hp + wt, data = mtcars)
summary(fit4)$r.squared

# compare two models
anova(fit1, fit4)

# Figure 1
par(mfrow = c(2, 2))
hist(mtcars$mpg, main = "Histogram of mpg", col = "skyblue", xlab = "MPG")
barplot(table(mtcars$cyl), main = "Histogram of number of cylinders",
        xlab = "Number of cylinders", ylab = "Frequency", col = "skyblue")
barplot(table(mtcars$am), main = "Histogram of transmission type", 
        col = "skyblue", xlab = "Transmission type", ylab = "Frequency")
barplot(table(mtcars$gear), main = "Histogram of gears", 
        col = "skyblue", xlab = "Number of gears", ylab = "Frequency")

# Figure 2
par(mfrow = c(1, 1))
boxplot(mpg ~ am, data = mtcars, xlab = "Transmission type", col = "skyblue")

# Figure 3
par(mfrow = c(2, 2))
plot(fit4)
