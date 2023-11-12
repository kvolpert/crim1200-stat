
# Loading packages
library(tidyverse)
library(ggplot2)

# Loading data from MASS into a tibble
birthwt <- as_tibble(MASS::birthwt)

# getting data ready

# rename variables
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

# and change factor level names
birthwt <- birthwt %>%
  mutate(race = recode_factor(race, `1` = "white", `2` = "black", `3` = "other")) %>%
  mutate_at(c("mother.smokes", "hypertension", "uterine.irr", "birthwt.below.2500"),
            ~ recode_factor(.x, `0` = "no", `1` = "yes"))

# now create boxplot showing how birthwt.grams varies between smoking status groups
ggplot(birthwt, aes(x=mother.smokes, y=birthwt.grams)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Title") +
  xlab("x label") +
  ylab("y label")

# make a summary table including standard errors
grouped_data <- aggregate(birthwt.grams ~ mother.smokes, data = birthwt, 
                          FUN = function(x) {
                            num.obs <- length(x)
                            mean.birthwt <- round(mean(x), 0)
                            sd.birthwt <- round(sd(x), 0)
                            se.birthwt <- round(sd(x) / sqrt(num.obs), 0)
                            c(num.obs, mean.birthwt, sd.birthwt, se.birthwt)
                          })
grouped_data


# run t-test
birthwt.t.test <- t.test(birthwt.grams ~ mother.smokes, data = birthwt)

# see results of t-test
birthwt.t.test

# see only p-value
birthwt.t.test$p.value

# see only group means
birthwt.t.test$estimate


