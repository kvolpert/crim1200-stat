
# Quiz 2


# Normal model

# 1. The patient recovery time from a particular surgical procedure is normally 
# distributed with a mean of 10 days and a standard deviation of 2 days.

# 1.a. What is the z-score for a patient who takes 12 days to recover?

# The mean is 10, the value is 12, and the standard deviation is 2
# Since 12 is 2 units from the mean, it is one standard deviation away, making
# the z-score 1

(12-10)/2

# 1.b. What percentage of patients take 20 days or longer to recover?

# You can use 1 - pnorm or add the argument lower.tail = FALSE to find the percentile
# to the right of the value

pnorm(20, mean = 10, sd = 2, lower.tail = FALSE) * 100

# 1.c. What percentage of patients take between 1 and 5 days to recover?

(pnorm(5, mean = 10, sd = 2) - pnorm(1, mean = 10, sd = 2)) * 100

# 1.d. How many days does it take for the 20th percentile of the distribution to recover?

qnorm(0.2, mean = 10, sd = 2)


# Linear regression 

# 2. We are going to study the relationship between number of murders per city 
# and number of robberies, only for cities with population > 1,000,000 using the 
# Uniform Crime Reports from 2017, which are crimes reported to the police, and 
# then compiled by the FBI.

# 2.a. What are some ethical concerns you might have with the data?

# consent

# 2.b. Load the data.

# Loading the data
data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/ExercisesforExam/data/ucr2017_big_cities.csv")

# Loading packages
library(ggplot2)

# 2.c. Draw a scatterplot of actual_robbery_total vs actual_murder. What does the 
# relationship look like?

ggplot(data, aes(x=actual_robbery_total, y = actual_murder)) +
  geom_point() +
  ggtitle("Title") +
  xlab("x axis") +
  ylab("y axis")

# 2.d. Fit a simple linear regression where you regress actual_robbery_total onto 
# actual_murder. 

# Running a regression
regression <- lm(data$actual_robbery_total ~ data$actual_murder)
summary(regression)

# The variable that is named (variable 2) is the one increasing by 1
# In this case, 1 more murder is associated with 22.78 more robberies

# Before interpreting the coefficients, look at the diagnostic plots. Are the 
# assumptions for fitting the linear model satisfied? 

# Plotting the regression
par(mfrow=c(2,2))
plot(regression)
par(mfrow=c(1,1))

# A. Homoscedasticity: 

# Since the scale-location plot is not flat, the relationship is heteroscedastic

# B. Independence between observations:

# There is clumping and a pattern in the residuals vs fitted plot, which suggests
# that there isn't independence between observations

# C. Normality of errors: 

# Since the value are almost entirely on the straight dashed line, this is mostly normal
# 3, 10, and the other value are a bit concerning though

# D. Linear relationship between x and y:

# The graph is somewhat linear, though the points do start to vary on either side 
# of the line of best fit
# Residuals vs fitted should be a straight line, so it's not linear


# 2.e. What are the outliers? Why do you think they are outliers?

# 7 is an outlier because it is outside of cook's distance in the residuals vs 
# leverage plot

# 11 is also concerning

# 3 is an outlier but isn't influential

data[3,]
data[7,]
data[11,]

# These cities have the largest populations, which is why they are outliers

# 2.f. Look at the regression plot on the scatterplot.
ggplot(data, aes(x=actual_robbery_total, y = actual_murder)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=FALSE) +
  ggtitle("Title") +
  xlab("x axis") +
  ylab("y axis")

# 2.g. Regardless of how the diagnostics look, interpret the slope coefficient. 
# What can you conclude about the relationship between murder and robberies? 
# (Ignore p-values or any measure of uncertainty associated with the coefficient 
# since we haven't covered that in class.)

summary(regression)$coefficients[2, 1]

# As murders increase by 1, robberies increase by 22.78 on average
regression <- lm(data$actual_robbery_total ~ data$actual_murder)


###############################################################################


# Exercises 5.3 and 5.4 

# 9)

# b) 18.6 to 31
24.8 - 6.2
24.8 + 6.2

# c) 
pnorm(31, mean = 24.8, sd = 6.2, lower.tail = FALSE) * 100

# d)
(pnorm(37.2, mean = 24.8, sd = 6.2) - 
    pnorm(31, mean = 24.8, sd = 6.2)) * 100

# e)
qnorm(0.025, mean = 24.8, sd = 6.2)

# 10)

# b) 70 to 130 (-2:+2)
100 - (15*2)
100 + (15*2)

# c) 
pnorm(115, mean = 100, sd = 15, lower.tail = FALSE) * 100

# d)
(pnorm(85, mean = 100, sd = 15) - pnorm(70, mean = 100, sd = 15)) * 100

# e)
pnorm(130, mean = 100, sd = 15, lower.tail = FALSE) * 100

# 11)
# He is 1.88 standard deviations below the mean

# 12)

# a)
# You are 2.2 sd above the mean

# b)
(100-68)/2

# 13) 

# a) 
pnorm(1250, mean = 1152, sd = 84, lower.tail = FALSE) * 100
      
# b)
pnorm(1200, mean = 1152, sd = 84) * 100

# c)
(pnorm(1100, mean = 1152, sd = 84) - pnorm(1000, mean = 1152, sd = 84)) * 100

# 14) 

# a) 
pnorm(80, mean = 100, sd = 15, lower.tail = FALSE) * 100

# b)
pnorm(90, mean = 100, sd = 15) * 100

# c)
(pnorm(132, mean = 100, sd = 15) - pnorm(112, mean = 100, sd = 15)) * 100

# 15) 

# a) 
pnorm(31, mean = 20.9, sd = 5.3, lower.tail = FALSE) * 100

# b)
pnorm(18, mean = 20.9, sd = 5.3) * 100

# c)
(pnorm(31, mean = 20.9, sd = 5.3) - pnorm(18, mean = 20.9, sd = 5.3)) * 100

# 16)

# a)
qnorm(0.99, mean = 72641, sd = 85000)

# b)
# not confident

# c)
# household income is exponential
# one standard deviation below is already in the negatives
# mean much greater than the median (not. anormal distribution)





