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
transform(Data, SOC = as.numeric(SOC), 
          TN = as.numeric(TN),
          BD = as.numeric(BD))

# Separating the columns
SOC <- Data[2]

TN <- Data[3]

BD <- Data[4]


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

