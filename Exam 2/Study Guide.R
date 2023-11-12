
# setting working directory
setwd("~/Documents/GitHub/crim1200-stat/Exam 2")


              ############# HYPOTHESIS TESTING #############

# regressions
reg <- lm(y~x, data = data)
summary(reg)
# Interpretation: the expected change in y for a one-unit change in x
# Example: "For a higher protein (x), by one gram, fat (y) is higher by one gram, on average".

# visuals for T-test 

# boxplot
ggplot(data, aes(x=categorical_x, y=quantiative_y)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Title") +
  xlab("x label") +
  ylab("y label")

# summary table
grouped_data <- aggregate(quantiative_y ~ categorical_x, data = data, 
                          FUN = function(x) {
                            num.obs <- length(x)
                            mean.birthwt <- round(mean(x), 0)
                            sd.birthwt <- round(sd(x), 0)
                            se.birthwt <- round(sd(x) / sqrt(num.obs), 0)
                            c(num.obs, mean, sd, se)
                          })
grouped_data

# T-test
data_ttest <- t.test(data$x, data$y)
data_ttest
# see only p-value
data_ttest$p.value
# see only group means
data_ttest$estimate

# Is there a relationship between two variables? Steps:

  # identify the null and alt hypothesis: 
    # null: there is no relationship between x and y.
    # there is a (positive?) relationship between x and y.

  # fit a linear model:
  reg <- lm(y ~ x, data)
  
  # look at the diagnostic plots
  par(mfrow=c(2,2))
  plot(reg)
  par(mfrow=c(1,1))

  # interpret the slope before transforming if the diagnostics are bad
  summary(reg)
  
  # transform the regression
  reg_transform <- lm(y~log(x), data)
  par(mfrow=c(2,2))
  plot(reg_transform)
  par(mfrow=c(1,1))
  summary(reg_transform)
  
  # since we transformed x, the correct interpretation is that β represents the 
  # percentage change in Y for a one percent change in ln(X). and this is statistically 
  # significant at the .05 level.

              ############# DATA TRANSFORMATIONS #############

# Running a normal linear regression
reg <- lm(data$y ~ data$x)
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))
summary(reg)

# Log(y) transformation
reg.log <- lm(log(data$y) ~ data$x)
par(mfrow=c(2,2))
plot(reg.log)
par(mfrow=c(1,1))
summary(reg.log)

# Box-cox transformation
data_log = lm(log(y) ~ x, data = data)
boxcox(data_log)






                    ############# ANOVA #############

# ANOVA test
anova_output <- aov(y ~ x, data = data)
summary(anova_output)

# honest significant difference test
tukey.test <- TukeyHSD(x=anova.output, 'race', conf.level=0.95)
plot(tukey.test , las=1 , col="brown")

                ############# CONFIDENCE #############

# Confidence Interval
reg <- lm(y ~ x, data)
confint(reg)

# round it to 2 places
round(confint(reg), 2)


             ############# REVIEW OF EDA #############

# one quantitative variable

  # quant: measures of central tendency/summary
  summary(data$x) # mean, max, min, 1st and 3rd quartiles
  sd(data$x) # standard deviation
  names(sort(table(data$x), decreasing = TRUE)[1]) #mode
  IQR(data$x) # IQR
  
  # features: mean, median, range, sd, mode
  # NOTE: if the data is skewed, use median and IQR; otherwise, use mean and sd
  
  # visual: histogram (could also use a boxplot)
  ggplot(data, aes(x=x)) +
    geom_histogram() +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")
  
  # features: modes, symmetry, outliers, skew, shape

# one categorical variable
  
  # quant: count
  table(x)
  prop.table(table(x))
  
  # features: mode
  
  # visual: barplot
  ggplot(data, aes(x=factor(x))) + 
    geom_bar() +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")
  
  # features: mode, outliers, patterns
  
# two quantitative
  
  # quantitative: correlation
  cor(x, y, method = "pearson") 
  
  # correlation strength guide:
    # 0.9-1.00: Very strong
    # 0.7-0.9: High correlation
    # 0.5-0.7: Moderate correlation
    # 0.3-0.5: Low correlation
    # 0.0-0.3: Negligible correlation
  
  # visual: scatterplot
  
  ggplot(data, aes(x=x, y=y)) + 
    geom_point() +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")
  
  # features: slope (direction and strength), linearity, variance, outliers
  
# two categorical
  
  # quantitative: contingency table
  table(data$x, data$y)
  
  # features: mode combos?
  
  # visual: side by side barplots
  ggplot(data, aes(x=factor(x))) + 
    geom_bar() + 
    facet_wrap(~y) + 
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")
  
  # features: comparison of shape, skew, outliers, symmetry, modes
  
# one quantitative, one categorical
  
  # quantitative: summary table with each variable
  data %>%
    group_by(x) %>%
    summarize(num.obs = n(),
              mean = mean(y),
              sd = sd(y),
              median = median(y),
              IQR = IQR(y),
              max = max(y),
              min = min(y)
    )
  
  # features: comparison of mean, median, iqr, max, min, sd
  
  # visual: side by side boxplots
  ggplot(data, aes(y=y, x=factor(x))) + 
    geom_boxplot() +
    scale_x_discrete(breaks = c(1, 2), labels = c("Label 1", "Label 2")) + 
    ylab("y axis") + xlab("x axis") +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")
  
  # features: comparison of measures of central tendency
  
# example of EDA for two variables for subgroups

  ggplot(data, aes(x=factor(x))) + geom_bar() + 
    facet_wrap(y ~ subgroup_of_y) + 
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")  
  
  # example with variables 
  
  ggplot(data, aes(x=factor(sexatract.noNAs))) + geom_bar() + 
    facet_wrap(speakengl~irsex) + 
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Title") +
    xlab("x label") +
    ylab("y label")


    ############# REVIEW OF LINEAR REGRESSION ASSUMPTIONS #############
  
# Homoscedasticity: the data points are consistently far away from the line of best fit
  # How to check: use the scale-location plot (it should be flat if it's homoscedastic)
  
# Normality: the data follows a normal distribution (i.e., For any fixed value of x, 
# y is normally distributed)
  # How to check: use a normal Q-Q plot (the residuals should be lined on the 
  # straight dashed line)
  
# Linearity: the line of best fit through the data points is a straight line 
# (rather than a curve or some sort of grouping factor)
  # How to check: look at the residuals vs. fitted plot (should be flat if it's linear),
  # also could just use a scatter plot (the preferred method)
  
# Independence between observations

  
             ############# REVIEW OF NORMAL MODELS #############
  
# finds the percentile (aka percentage to the left)
pnorm(value, mean = mean, sd = sd)
  
# finds the percentage to the right
pnorm(value, mean = mean, sd = sd, lower.tail = FALSE)

# finds the value that is at a given percentile
qnorm(percentile, mean = mean, sd = sd)

           ############# REVIEW OF ETHICAL CONCERNS #############
  

# Checklist:
  
  # Have we listed how this technology can be attacked or abused? [SECURITY]
  # Have we tested our training data to ensure it is fair and representative? [FAIRNESS]
  # Have we studied and understood possible sources of bias in our data? [FAIRNESS]
  # Does our team reflect a diversity of opinions, backgrounds, and kinds of thought? [FAIRNESS] 
  # What kind of user consent do we need to collect to use the data? [PRIVACY/TRANSPARENCY] 
  # Do we have a mechanism for gathering consent from users? [TRANSPARENCY]
  # Have we explained clearly what users are consenting to? [TRANSPARENCY]
  # Do we have a mechanism for redress if people are harmed by the results? [TRANSPARENCY]
  # Can we shut down this software in production if it is behaving badly?
  # Have we tested for fairness with respect to different user groups? [FAIRNESS]
  # Have we tested for disparate error rates among different user groups? [FAIRNESS]
  # Do we test and monitor for model drift to ensure our software remains fair over time? [FAIRNESS] 
  # Do we have a plan to protect and secure user data? [SECURITY]
  










