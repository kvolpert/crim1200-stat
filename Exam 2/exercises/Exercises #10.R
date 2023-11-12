
# Loading packages
library(tidyverse)
library(ggplot2)
library(reshape2)

# Setting the working directory
setwd("~/Documents/GitHub/crim1200-stat/Exam 2/exercises")

# 1. Data

# a. Load the data and preview it

data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/Exam 2/data/dat.nsduh.small.1.csv")

# b. What are the variables in the data? read the codebook

# variables: 

# mjage: how old were you the first time you used marijuana or hashish? 
# cigage: how old were you when you first started smoking cigs daily?
# iralcage: how old were you when you first drank alc?
# age2: second age variable
# sexatract: sexual orientation 
# speakengl: how well do you speak english?
# irsex: sex

# c. How many individuals' responses were included?

nrow(data)

# 171 responses were included


# 2. EDA single variable: Do EDA, visual and quantitative, and write down in words 
# what are some surprising features of each variable.

# a. age2

ggplot(data, aes(x = age2)) +
  geom_bar()

# yes outliers (below 5)
# mode looks to be 15
# concentrated around higher values

table(data$age2)
prop.table(table(data$age2)) * 100

# 13 values
# higher percentage fall around higher values
# 15 is the mode

# b. sexatract

data_sexatract <- data[data$sexatract < 90,]

# data_sexatract <- ifelse(data$sexatract >= 90, NA, data$sexatract)

ggplot(data_sexatract, aes(x = sexatract)) +
  geom_bar()

# yes outliers (missing data)
# concentrated around lower values
# mode is 1
# big difference between count of 1 and count of 2

table(data$sexatract)
prop.table(table(data$sexatract)) * 100

# 80% of values = 1
# way more fall around lower values
# mode is 1

# c. speakengl

ggplot(data, aes(x = speakengl)) +
  geom_bar()

# almost all answered very well (1 is the mode)
# no one answered not at all 
# very few answered 2 and 3 
# no outliers

table(data$speakengl)
prop.table(table(data$speakengl)) * 100

# 94% answered very well (mode)
# only ~6% answered well and not well combined
# no one answered not at all

# d. irsex

ggplot(data, aes(x = irsex)) +
  geom_bar()

# pretty even, but 1 is the mode
# no outliers

table(data$irsex)
prop.table(table(data$irsex)) * 100

# 53% answered male
# 47% answered female

# 3. EDA two variables: Describe the relationship between the two variables 
# visually and quantitatively.

# a. mjage and cigage

ggplot(data, aes(x=mjage, y = cigage)) +
  geom_point()

# as marijuana age increases, cigage increases slightly
# somewhat linear
# variance changes throughout
# appear to be some outliers around 14 and 21

cor(data$mjage, data$cigage, method = "pearson")

# the correlation is 0.25, which is a negligible positive correlation

# b. sexatract and speakengl

# contingency table
table(data$sexatract, data$speakengl)

# grouped barplot?
ggplot(data, aes(x = speakengl, fill = factor(sexatract))) +
  geom_bar(position = "dodge") +
  labs(x = "Speak English", y = "Count", title = "Grouped Barplot") +
  scale_fill_manual(values = c("Attracted" = "blue", "Not Attracted" = "red")) +
  theme_minimal()

# do I need to title this or do anything differently??

cor(data$sexatract, data$speakengl, method = "pearson")

# Is this what I'm supposed to do??

# how to visually represent two categorical variables
# how to quantitatively represent two categorical variables

# c. iralcage and irsex

ggplot(data, aes(x = irsex, y = iralcage)) +
  geom_boxplot() +
  labs(x = "Sex (irsex)", y = "Age (iralcage)", title = "Side-by-Side Boxplots by Sex") +
  facet_grid(. ~ irsex)

# visual:

# quantitative:

summary(data$iralcage[data$irsex == 1])
summary(data$iralcage[data$irsex == 2])

# should I do a t-test?

# 4. EDA two variables for subgroups: How does the relationship between sexatract 
# and speakengl change for women vs men?

men <- data[data$irsex == 1,]
women <- data[data$irsex == 2,]

# contingency table
table(men$sexatract, men$speakengl)

# grouped barplot?
ggplot(men, aes(x = speakengl, fill = factor(sexatract))) +
  geom_bar(position = "dodge") +
  labs(x = "Speak English", y = "Count", title = "Grouped Barplot") +
  scale_fill_manual(values = c("Attracted" = "blue", "Not Attracted" = "red")) +
  theme_minimal()

# contingency table
table(women$sexatract, men$speakengl)

# grouped barplot?
ggplot(women, aes(x = speakengl, fill = factor(sexatract))) +
  geom_bar(position = "dodge") +
  labs(x = "Speak English", y = "Count", title = "Grouped Barplot") +
  scale_fill_manual(values = c("Attracted" = "blue", "Not Attracted" = "red")) +
  theme_minimal()

# 5. Regressions: Fit linear models for the following relationships.

# a. Are men more likely to use marijuana earlier than women? Is this relationship 
# statistically significant? What does that mean?

# wdym by fit linear models
reg_mar <- lm(data$irasex ~ data$mjage)

# t-test
t_test_result <- t.test(data$mjage ~ data$irsex)
t_test_result

# b. Are the age of first use of alcohol and the age of first use of marijuana 
# related? Is this relationship statistically significant?

# c. For both of these, what are the confidence intervals for the relevant parameter? 
# What is the proper way to interpret these?




