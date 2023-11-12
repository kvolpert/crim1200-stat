
# Loading packages
library(rio)

# Setting my working directory
setwd("~/Documents/GitHub/crim1200-stat/Final project ideas")

# Loading the gun laws data
# https://www.icpsr.umich.edu/web/NACJD/studies/37363/versions/V1 
gun_laws <- import("/Users/katievolpert/Documents/GitHub/crim1200-stat/Final project ideas/gunlaws.rda")

# Loading the crime data
# https://corgis-edu.github.io/corgis/csv/state_crime/ 
crime <- read.csv("/Users/katievolpert/Documents/GitHub/crim1200-stat/Final project ideas/state_crime.csv")
