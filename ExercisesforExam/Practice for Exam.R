
# Setting working directory
setwd("/Users/katievolpert/Documents/GitHub/crim1200-stat/ExercisesforExam")

# Loading data
data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/data/income.data.csv")

# Loading packages
library(ggplot2)
library(ggthemes)

# X is an identifier
# Income and happiness are quantitative
# Education and gender are categorical

# DESCRIBE QUANTITATIVELY AND VISUALLY

 # Income (Quantitative variable)
  summary(data$income) 
  sd(data$income) 
  var(data$income) 
  IQR(data$income)

  # Histogram
  ggplot(data = data, aes(x = income)) +
    geom_histogram(position = "identity") +
    ggtitle("Title") +
    xlab("Income") +
    ylab("Count") +
    theme_economist()

  # Education (Categorical variable)
  summary(data$education)
  table(data$education)
  prop.table(table(data$education))

  # Barplot
  ggplot(data = data, aes(x = education)) +
    geom_bar(stat = "count") +
    ggtitle("Title") +
    xlab("Education Level") +
    ylab("Count")

# RELATIONSHIP BETWEEN TWO VARIABLES
  
ggplot(data = data, aes(x = education, y = happiness)) +
  geom_bar(stat = "identity")  +
  ggtitle("Title") +
  xlab("Education Level") +
  ylab("Happiness (units)")

ggplot(data = data, aes(x = income, y = happiness)) +
  geom_point(stat = "identity")  +
  ggtitle("Title") +
  xlab("Income") +
  ylab("Happiness")

# Two quantitative
cor(data$income, data$happiness, method = "pearson")

# Quantitative + categorical
data$binary <- 0
data$binary[data$education == "College"] <- 0
data$binary[data$education == "High school"] <- 1
cor(data$binary, data$happiness, method = "pearson")

# Contingency table of two variables
table(data$gender, data$education)

# Prop contingency table of two variables
prop.table(table(data$gender, data$education))




# ___________________________________________________________________________ #





# Investigating the quantitative distribution of the data
summary(data$income)
summary(data$happiness)
table(data$education)
prop.table(table(data$education))
table(data$gender)
prop.table(table(data$gender))

# Creating a histogram for income
ggplot(data, aes(x = income)) +
  geom_histogram()

# Creating a histogram for happiness
ggplot(data, aes(x = happiness)) +
  geom_histogram()



# Creating a barplot for gender and happiness
ggplot(data, aes(x = gender, y = happiness)) +
  geom_bar(stat="identity")



# Creating a barplot for education and happiness
ggplot(data, aes(x = education, y = happiness)) +
  geom_bar(stat="identity")



# Creating a barplot for income and happiness
ggplot(data, aes(x = income, y = happiness)) +
  geom_point() +
  geom_smooth(method = "lm")

# Calculating the correlation between income and happiness
cor(data$income, data$happiness, method = "pearson")
