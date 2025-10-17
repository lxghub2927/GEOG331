# script to see examples of linear regression
# in R using built in IRIS data
# Luke Wang 10/07/25 -> Instructor: MML

rm(list=ls())

#subsetting the virginica species
flower <- iris[iris$Species == "virginica",]


#make a scatter plot to look at sepal length
plot(flower$Sepal.Length, flower$Petal.Length, pch=19,
     xlab = "Sepal Length", ylab="Petal Length",
     main = "Iris Virginica")

#fit a regression model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

#plot residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch=19,
     xlab="Sepal Length", ylab="Residuals", main = "Iris Virginica Sepal Length & Residuals")
abline(h=0)

#check normality of residuals
hist(summary(fit)$residuals, col="red",
     main = "Residual Distribution", xlab = "Residuals")

#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals, pch=19)
qqline(summary(fit)$residuals, pch=19)

#user Shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)
