
# Setting working directory
setwd("/Users/katievolpert/Desktop/Code/CRIM 1200") 

# Install packages
# install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")
library(tidyverse)
library(ggplot2)
library(reshape2)

# Loading data
car <- mtcars

# Viewing data
head(car)

# Making a frequency table
table(car$gear)

# Making a relative frequency table
table(car$gear) / length(car$gear)

# Making a contingency table
table <- table(car$gear, car$vs)
colnames(table) <- c("V-shaped engine", "Straight engine")
rownames(table) <- c("3 gears", "4 gears", "5 gears")

# Reshaping the table into a dataframe
tab1 <- melt(table)

# Naming the columns
colnames(tab1) <- c("Gear", "Engine", "Value")

# Making a barplot
ggplot(tab1, aes(x = Engine, y = Value, fill = Gear)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Barplot comparing gears by type of engine") +
  xlab("Type of Engine") + 
  ylab("Value")







