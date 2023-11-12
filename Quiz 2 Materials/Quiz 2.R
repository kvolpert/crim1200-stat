# Quiz 2

# Setting working directory
setwd("~/Documents/GitHub/crim1200-stat/Quiz 2 Materials")

# Loading data
data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/Quiz 2 Materials/data/dat.nsduh.small.1.1.csv")

# Loading packages
library(ggplot2)

# q1

pnorm(732, mean = 527, sd = 112, lower.tail = FALSE) * 100


# q2 

qnorm(0.99, mean = 527, sd = 112)


# q3 

qnorm(0.99, mean = 527, sd = 112)

# q4

(pnorm(732, mean = 527, sd = 112) - pnorm(700, mean = 527, sd = 112)) * 100
pnorm(732, mean = 527, sd = 112) * 100

# q5

# Making a histogram for marijuana age
ggplot(data, aes(x=mjage)) +
  geom_histogram() + 
  ggtitle("Distribution of Ages of First Marijuana Use") +
  xlab("Age (years)")

# Making a histogram for cigarette age
ggplot(data, aes(x=cigage)) +
  geom_histogram() + 
  ggtitle("Distribution of Ages When Daily Cigarette Use Began") +
  xlab("Age (years)")

# Making a scatterplot comparing the two
ggplot(data, aes(x = mjage, y = cigage)) +
  geom_point() + 
  ggtitle("Age that Daily Cigarette Use Began by Age of First Marijuana Use") +
  xlab("Age of First Marijuana Use") +
  ylab("Age When Daily Cigarette Use Began")

# q7 & q8

# Running a regression
regression <- lm(data$mjage ~ data$cigage)
par(mfrow=c(2,2))
plot(regression)
par(mfrow=c(1,1))

# Checking the outliers
data[1,]

# q9
summary(regression)





