
?cars

plot(cars$speed, cars$dist)

lm.output <- lm(formula = dist ~ speed, data = cars)

summary(lm.output)

lm.output$residuals
lm.output$fitted.values

plot(lm.output$residuals, cars$speed)

par(mfrow=c(2,2))

plot(lm.output)

