

# Setting the working directory
setwd("~/Documents/GitHub/crim1200-stat/Exam 2/exercises")

# Loading packages
library(tidyverse)
library(ggplot2)

# 1. Data

# a. Load the data and preview it
data <- read_csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/Exam 2/data/dat.nsduh.small.1.csv")

# NOTE: I am adding the names of the categories for the categorical variables so 
# they show up in the plots
data$irsex <- data$irsex %>% recode_factor("1" = "Male","2" = "Female")

data$speakengl <- data$speakengl %>% recode_factor("1" = "v.well", 
                                                 "2" = "well",
                                                 "3" = "n.well",
                                                 "4" = "not.at.all")

data$sexatract <- data$sexatract %>% recode_factor("1" = "only.opp", 
                                                 "2" = "mostly.opp",
                                                 "3" = "equally",
                                                 "4" = "mostly.same",
                                                 "5" = "not.sure") # ignore warning


# b. What are the variables in the data? read the codebook
names(data)

# c. How many individuals' responses were included?
nrow(data)

# 2. EDA single variable: Do EDA, visual and quantitative, and write down in words 
# what are some surprising features of each variable.

# a. age2

# quantitative: 
table(data$age2)

# visual:
ggplot(data, aes(x=factor(age2))) + 
  geom_bar() 


# b. sexatract

# quantitative: 
table(data$sexatract) # oh there's a 99 in there, which should be an NA. Let's make a new variable w 99 as NA
data$sexatract.noNAs <- ifelse(data$sexatract==99, NA, data$sexatract)

# add labels to this too:
data$sexatract.noNAs <- data$sexatract.noNAs %>% recode_factor("1" = "only.opp", 
                                                 "2" = "mostly.opp",
                                                 "3" = "equally",
                                                 "4" = "mostly.same",
                                                 "5" = "not.sure") # ignore warning



# visual:
ggplot(data, aes(x=factor(sexatract.noNAs))) + 
  geom_bar()


# c. speakengl

# quantitative
table(data$speakengl)

# visual
ggplot(data, aes(x=factor(speakengl))) + 
  geom_bar() 


# d. irsex
# quantitative
table(data$irsex)

# visual
ggplot(data, aes(x=factor(irsex))) + 
  geom_bar()




# 3. EDA two variables: Describe the relationship between the two variables visually and quantitatively.

# a. mjage and cigage

# quantitative
cor(data$mjage, data$cigage)

# visual
plot(data$mjage, data$cigage) # in base R or

ggplot(data, aes(x=mjage, y=cigage)) + 
  geom_point()



# b. sexatract and speakengl

# quantitative
table(data$sexatract, data$speakengl)

# visual
ggplot(data, aes(x=factor(sexatract.noNAs), fill=factor(speakengl))) + 
  geom_bar() # option 1
ggplot(data, aes(x=factor(sexatract.noNAs))) + 
  geom_bar() + 
  facet_wrap(~speakengl) + 
  theme(axis.text.x=element_text(angle=90))  # option 2


# notice that I used the variable where 99 was renamed as NA to be clear that 99 is not a new category


# c. iralcage and irsex

# visual
boxplot(data$iralcage~data$irsex, ylim=c(0,25)) # in base R or

ggplot(data, aes(y=iralcage, x=factor(irsex))) + 
  geom_boxplot()

# this adds labels
ggplot(data, aes(y=iralcage, x=factor(irsex))) + 
  geom_boxplot() +
scale_x_discrete(breaks = c(1, 2), labels = c("Male", "Female")) + 
  ylab("Count") + xlab("Gender")

# quantitative
data %>%
  group_by(irsex) %>%
  summarize(num.obs = n(),
            mean = mean(iralcage),
            sd = sd(iralcage),
            median = median(iralcage),
            IQR = IQR(iralcage),
            max = max(iralcage),
            min = min(iralcage)
            )


# 4. EDA two variables for subgroups: How does the relationship between sexatract and spekengl change for women vs men?

# first, here is the relationship between sexatract and speakengl, from earlier:
ggplot(data, aes(x=factor(sexatract.noNAs))) + 
  geom_bar() + 
  facet_wrap(~speakengl) 

# now we can take the same thing and make the facet_wrap include irsex as well:
ggplot(data, aes(x=factor(sexatract.noNAs))) + geom_bar() + 
  facet_wrap(speakengl~irsex) + 
  theme(axis.text.x=element_text(angle=90))




# 5. Regressions: Fit linear models for the following relationships.

# a. Are the age of first use of alcohol and the age of first use of marijuana 
# related? Is this relationship statistically significant? For both of these, what are the confidence intervals for the relevant 
# parameter? What is the proper way to interpret these?

# name the null and alternative hypotheses:
# H0: there is no relationship between iralcage and mjage
# HA: there is a (positive?) relationship between iralcage and mjage

# fit a linear model
reg <- lm(iralcage~mjage, data)

# look at diagnostics
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))
plot(data$mjage, data$iralcage)

# They don't look great. 

# Let's interpret the slope coefficient without transforming first.
summary(reg)

# the interpretation would be, an additional year of mjage is associated with 0.3 
# additional years of iralcage, and this is statistically significant at the 0.05 
# level. So in other words, people who try marijuana later also tend to try alcohol 
# later.

# Let's try transforming the vars.
reg_transformed <- lm(iralcage~log(mjage), data)
par(mfrow=c(2,2))
plot(reg_transformed)
par(mfrow=c(1,1))
plot(log(data$mjage), data$iralcage)
# much better

summary(reg_transformed)
# since we transformed x, the correct interpretation is that β represents the 
# percentage change in Y for a one percent change in ln(X). and this is statistically 
# significant at the .05 level.

# b. For both of these, what are the confidence intervals for the relevant 
# parameter? What is the proper way to interpret these?

confint(reg)

# round it to 2 decimal points:
round(confint(reg), 2)

# The 95% confidence interval for the slope coefficient is [0.20, 0.40]. This 
# does not cover zero, so we can reject the null hypothesis that iralcage and 
# mjage are unrelated. If we had collected a new sample and calculated confidence 
# intervals 100 times, 95 of those confidence intervals would cover the true value.






# ChatGPT extra exercises

# Are the age of first use of alcohol and the age of first use of marijuana 
# related? Is this relationship statistically significant? 

# Running a regression
reg <- lm(iralcage ~ cigage, data = data)

# Looking at the diagnostic plots
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

# not homeoscedastic
# relatively normal??
# not linear

# transform the y with log
reg_transform_y <- lm(log(iralcage) ~ cigage, data = data)

# look at new diagnostic plots
par(mfrow=c(2,2))
plot(reg_transform_y)
par(mfrow=c(1,1))
plot(data$cigage, log(data$iralcage))

# relatively homeoscedastic
# less normal?
# relatively linear

# transform the x with log
reg_transform_x <- lm(iralcage ~ log(cigage), data = data)

# look at new diagnostic plots
par(mfrow=c(2,2))
plot(reg_transform_x)
par(mfrow=c(1,1))
plot(log(data$cigage), data$iralcage)

# relatively homeoscedastic
# less normal?
# relatively linear

# loading a package
library(MASS)
install.packages("forecast")
library(forecast)

# box-cox transformation
result <- boxcox(reg_transform_y)
plot(result)

# For both of these, what are the confidence intervals for the relevant parameter? 
# What is the proper way to interpret these?

round(confint(reg),2)

# The 95% confidence interval for the slope coefficient is 0.07-0.26. This means
# that we can reject the null hypothesis that iralcage and cigage are unrelated.
# If we had done this 100 times with a new sample and re-calculated the coefficient
# intervals, 95 of these confidence intervals would cover the true value. 

# visual
ggplot(data, aes(x=irsex, y=mjage)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Title") +
  xlab("x label") +
  ylab("y label")

grouped_data <- aggregate(mjage ~ irsex, data = data, 
                          FUN = function(x) {
                            num.obs <- length(x)
                            mean <- round(mean(x), 0)
                            sd <- round(sd(x), 0)
                            se <- round(sd(x) / sqrt(num.obs), 0)
                            c(num.obs, mean, sd, se)
                          })

# t-test
data_ttest <- t.test(data$mjage ~ data$irsex)
data_ttest









