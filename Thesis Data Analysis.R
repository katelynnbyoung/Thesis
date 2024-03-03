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

# Transforming the data to numeric class
Data <- transform(Data, SOC = as.numeric(SOC), 
          TN = as.numeric(TN),
          BD = as.numeric(BD),
          Plot = as.character(Plot))

# Separating the columns
SOC <- Data[2]
SOC <- as.numeric(unlist(SOC))

TN <- Data[3]
TN <- as.numeric(unlist(TN))

BD <- Data[4]
BD <- as.numeric(unlist(BD))

# Finding out more about each column
SOC_df <- SOC %>%
  summarise(average = mean(SOC),
            median = median(SOC),
            mode =  Mode(SOC),
            minimum = min(SOC),
            maximum = max(SOC))

TN_df <- TN %>%
  summarise(average = mean(TN),
            median = median(TN),
            mode = Mode(TN),
            minimum = min(TN),
            maximum = max(TN))

BD_df <- BD %>%
  summarise(average = mean(BD),
            median = median(BD),
            mode = Mode(BD),
            minimum = min(BD),
            maximum = max(BD))


# Data Visualization

pairs(Data[2:4])


# Data Correlation

cor.test(TN, SOC)

# The correlation coefficient is 0.6910264, which shows a positive correlation. 
# The p-value is 0.0003695, which shows that the correlation is statistically significant.

