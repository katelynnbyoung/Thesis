# Installing and loading packages
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
library("tidyverse")
library("dplyr")
library("tidyr")
library("readxl")
library("ggplot2")
library(DescTools)

# Loading data
Data <- read_xlsx("D:/UP/Thesis/Thesis/Thesis.xlsx")
print(Data, n=22)


# Finding out more about each column
SOC_df <- Data$`SOC in g/100g` %>%
  summarise(average = mean(SOC),
            median = median(SOC),
            mode = Mode(SOC),
            minimum = min(SOC),
            maximum = max(SOC))

TN_df <- Data %>%
  summarise(average = mean(TN),
            median = median(TN),
            mode = Mode(TN),
            minimum = min(TN),
            maximum = max(TN))

BD_df <- Data %>%
  summarise(average = mean(BD),
            median = median(BD),
            mode = Mode(BD),
            minimum = min(BD),
            maximum = max(BD))


# Data Visualization

pairs(Data[2:4])





# Tukey's Test

# Organize the data into a data frame
SOC_Tukey <- data.frame(group = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "N", "N"),
                   values = SOC)

TN_Tukey <- data.frame(group = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "N", "N"),
                       values = TN)

BD_Tukey <- data.frame(group = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "ANR", "N", "N"),
                       values = BD)

# Fit one-way ANOVA model

SOC_model <- aov(values~group, data=SOC_Tukey)
summary(SOC_model)
# The p-value is not statistically significant. Therefore Tukey's test will not be used.

TN_model <- aov(values~group, data=TN_Tukey)
summary(TN_model)
# The p-value is not statistically significant. Therefore Tukey's test will not be used.

BD_model <- aov(values~group, data=BD_Tukey)
summary(BD_model)
# The p-value is statistically significant. Therefore Tukey's test will be used.



# Tukey Test
TukeyHSD(BD_model, conf.level=.95) 
# The p-value is statistically significant. Therefore there is a statistically 
# significant difference between the mean bulk density of each management strategy.

# Plot Confidence Levels of the Tukey Test
plot(TukeyHSD(BD_model, conf.level=.95), las = 2)



# Pearson's Correlation Test

# Testing TN and SOC
cor.test(TN, SOC, method = "pearson")

# The p-value is 0.0003695, which shows that the correlation is statistically significant.
# The correlation coefficient is 0.6910264, which shows a positive correlation. 

# Testing SOC and BD
cor.test(BD, SOC, method = "pearson")
# The p-value is not statistically significant at 0.171.

# Testing TN and BD
cor.test(BD, TN, method = "pearson")
# The p-value is 0.217, so it is not statistically significant.