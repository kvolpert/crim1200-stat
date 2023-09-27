
# Setting working directory
setwd("/Users/katievolpert/Documents/GitHub/crim1200-stat/ExercisesforExam")

# Loading data
data <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/data/income.data.csv")

# Loading packages
library(ggplot2)

# X is an identifier
# Income and happiness are quantitative
# Education and gender are categorical

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
