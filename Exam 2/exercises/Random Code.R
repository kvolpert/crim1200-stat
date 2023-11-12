
# Setting working directory
setwd("~/Documents/GitHub/crim1200-stat/Exam 2/exercises")

# Loading data
data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/Exam 2/data/initech.csv")

# Loading packages
library(ggplot2)

# Looking at the plot
ggplot(data, aes(x = years, y = salary)) +
  geom_point()

# Running a linear regression
reg <- lm(data$salary ~ data$years)
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

# Taking a log of salary to transform the diagnostics
reg.log <- lm(log(data$salary) ~ data$years)
par(mfrow=c(2,2))
plot(reg.log)
par(mfrow=c(1,1))

# Looking at the summary of both regressions
summary(reg)
summary(reg.log)
