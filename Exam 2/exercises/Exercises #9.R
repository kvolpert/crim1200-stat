
# setting working directory
setwd("~/Documents/GitHub/crim1200-stat/Exam 2/exercises")

# loading packages and data
library(tidyr)
library(dplyr)
birthwt <- as_tibble(MASS::birthwt)

# renaming and recoding the variables
birthwt <- birthwt %>%
  rename(birthwt.below.2500 = low, 
         mother.age = age,
         mother.weight = lwt,
         mother.smokes = smoke,
         previous.prem.labor = ptl,
         hypertension = ht,
         uterine.irr = ui,
         physician.visits = ftv,
         birthwt.grams = bwt)
birthwt <- birthwt %>%
  mutate(race = recode_factor(race, `1` = "white", `2` = "Black", `3` = "other")) %>%
  mutate_at(c("mother.smokes", "hypertension", "uterine.irr", "birthwt.below.2500"),
            ~ recode_factor(.x, `0` = "no", `1` = "yes"))

# EDA
result <- aggregate(birthwt.grams ~ race, data = birthwt, 
                    FUN = function(x) {
                      mean.bwt <- round(mean(x), 0)
                      se.bwt <- round(sd(x) / sqrt(length(x)), 0)
                      c(mean.bwt = mean.bwt, se.bwt = se.bwt)
                    })
result

# subsetting the data
birthwt.white <- birthwt[ which(birthwt$race=='white'), ]
birthwt.Black <- birthwt[ which(birthwt$race=='Black'), ]
birthwt.other <- birthwt[ which(birthwt$race=='other'), ]

# creating a normal qq plot
qqnorm(birthwt.white$birthwt.grams, main="Normal Q-Q plot for white")
qqline(birthwt.white$birthwt.grams) # adding a line of best fit
qqnorm(birthwt.Black$birthwt.grams, main="Normal Q-Q plot for Black")
qqline(birthwt.Black$birthwt.grams)
qqnorm(birthwt.other$birthwt.grams, main="Normal Q-Q plot for other")
qqline(birthwt.other$birthwt.grams)

# Running a regression
reg.output <- lm(birthwt.grams ~ race, data = birthwt)
summary(reg.output)

# Anova
anova.output <- aov(reg.output)
summary(anova.output)

# A way to do it without the regression (has the same output)
anova.output1 <- aov(birthwt.grams ~ race, data = birthwt)
summary(anova.output1)

# honest significant difference test
tukey.test <- TukeyHSD(x=anova.output, 'race', conf.level=0.95)
plot(tukey.test , las=1 , col="brown")
