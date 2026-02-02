#Start code

#Load necessary libraries for regression diagnostics
library(lmtest)

#Statistical tests and models must be based on random data (replace with your data, if needed)
#Create a random data of 500 samples for x (independent) and y (dependent) variables
set.seed(123) #Fixed random data for reproducibility
n <- 500
x <- rnorm(n)
y <- 2 * x + rnorm(n)
data <- data.frame(x, y)

#Summary of two variables inside the data: It will privide five number summary plus mean
summary(data)
#Outlier test: Need to be careful as it can influence the linear model
boxplot(data$y)
#Histogram of dependent variable: Normally distributed? (Bell shaped!)
hist(data$y)
#Shapiro-Wilk test of normality of dependent variable: Normally distributed (p-value>0.05)
print(shapiro.test(data$y))

#Gauss-Markov assumptions and diagnostics of linear regression model
print("Linearity in parameters:Regression ANOVA is significant linearly in parameters")
print("No perfect multicollinearity:Only one independent variable so no multicollinearity")
print("Exogeneity:Residuals are uncorrelated with independent variable, will check in residual diagnostic")
print("Homoscedasticity:Will check in residual diagnostic")
print("No autocorrelation:Will check in residual diagnostic")
print("Normality of errors:Will check in residual diagnostic")

#Fit a linear model
model <- lm(y ~ x, data = data)
#Summary of the model
summary(model)

#Interpretations:
print("R squared value (explained variance) is 0.8 approximately, which suggests a good model fit")
print("F-statistic is significant, p-value is less than 0.05 so y and x and linear and move forward")
print("Regression coefficient is significant but regression constant is not significant")
print("The model is good fit but not a BLUE model")
print("BLUE - Best Linear Unbiased Estimator")

#Residual Analysis 
#Get the residuals
residuals <- model$residuals
#Exogenity check: residuals are uncorrelated with independent vairable
cor_test <- cor.test(data$x, residuals)
cor_test
#Correlation is not significant so exogenity is not a problem.

#Plot the lowess of residuals (Linearity)
plot(data$x, residuals, main = "Residuals vs Fitted", xlab =
"x", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 2)
lines(lowess(data$x, residuals), col = "blue", lwd = 2)
#This plot clearly shows that the lowess line (blue) is near to the reference line (red), so linearity is suggested!
#Saved plot
plot(model, which = 1)
#Confirmatory
print(mean(residuals))
print("Mean of residuals ~ zero, so linearity of redisuals confirmed")

#Plot of residuals for independence (Time series plot)
acf(residuals, main = "ACF of Residuals")
#Mostly cut-off so independence is suggested!
#Durbin-Watson test for confirmation of independence:lmtest package
dw_test <- dwtest(model)
dw_test
#P-value is not significant so independence is confirmed!

#Plot of residuals histogram (Normality)
hist(residuals, main = "Histogram of Residuals", xlab =
       "Residuals", breaks = 20, col = "lightblue")
#Saved plot
plot(model, which = 2)
#QQ plot
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)
#This QQ plot suggest normality of residuals
#Shapiro-Wilk test for confirming normality
shapiro_test <- shapiro.test(residuals)
shapiro_test
#Since p-value is not significant, normality is confirmed!

#Plot of residuals vs fitted values (Homoscedasticity)
std_fitted_values <- scale(model$fitted.values)
std_residuals <- scale(residuals)
plot(std_fitted_values, std_residuals, main = "Standardized Residuals vs Fitted Values",
     xlab = "Standardized Fitted Values", ylab = "Standardized Residuals")
#The scatterplot of standarding residuals and standarding fitted valuea looks random so homoscedasticity confirmed!#Saved plot
plot(model, which = 3)
#The scatterplot of standarding residuals and fitted valuea looks random so homoscedasticity confirmed!
#Breusch-Pagan test for homoscedasticity:lmtest package
bp_test <- bptest(model)
bp_test
#Since p-value is not siginifcant, homoscedasticity or equality of variance of residuals is confirmed!
#All the four assumptions of residuals i.e LINE test is confirmed!

#We can also assess the outliers of residuals
cooks_distance <- cooks.distance(model)
cooks.distance(model)[which.max(cooks.distance(model))]
plot(cooks.distance(model),type="b",pch=18,col="blue")
N = 500
k = 2
cutoff = 4/ (N-k-1)
abline(h=cutoff,lty=2)
#Saved model plot
plot(model, which=4)

#Saved Leverage plot
plot(model, which=5)

#Since model fit and residuals analysis are fine, we can use the model for predictions
#Predictions with confidence intervals
new_data <- data.frame(x = c(-2, 0, 2))
predictions <- predict(model, newdata = new_data, interval = "confidence")
predictions

#End of code