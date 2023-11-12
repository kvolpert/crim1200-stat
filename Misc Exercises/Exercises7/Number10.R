

# setwd
setwd("~/Documents/GitHub/crim1200-stat/Exercises7")

# loading packages
library(ggplot2)

# Plotting using pnorm
sample <- rnorm(1000, mean = 100, sd = 15)

# Graphing
ggplot(data.frame(sample), aes(x=sample)) +
  geom_histogram()

# Loading data
cars <- cars

# Viewing the attributes of the data
?cars

# Running a regression
lm.output <- lm(cars$dist ~ cars$speed)

# Looking at it
summary(lm.output)

# Graphing the variables
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = lm)

# Looking at the residuals
lm.output$residuals
lm.output$fitted.values

# Making a fraph of the residuals + x
plot(lm.output$residuals, cars$speed)


