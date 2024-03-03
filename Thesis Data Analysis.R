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


Data <- read_xlsx("D:/UP/Thesis/Thesis/Thesis.xlsx")

print(Data, n=22)

SOC <- Data[2]

TN <- Data[3]

SOC_df <- SOC %>%
  summarise(ave_ratio = mean(ratio))
  