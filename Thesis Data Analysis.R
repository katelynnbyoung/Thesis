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
SOC_df <- Data %>%
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

# Order Plot numbers for bar charts
Data_bar <- Data
Data_bar$Plot <- factor(Data_bar$Plot,
                                   levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"))

# Bar chart for SOC
SOC_plot <- ggplot(data = Data_bar) +
  geom_col(mapping = aes(x = Plot, y = SOC))+
  ylab("SOC in g/100g of soil")+
  xlab("Plot Number")+
  ggtitle("Soil Organic Carbon (SOC) per Plot")

# Bar chart for TN
TN_plot <- ggplot(data = Data_bar) +
  geom_col(mapping = aes(x = Plot, y = TN))+
  ylab("TN in g/100g of soil")+
  xlab("Plot Number")+
  ggtitle("Total Nitrogen (TN) per Plot")

# Bar chart for BD
BD_plot <- ggplot(data = Data_bar) +
  geom_col(mapping = aes(x = Plot, y = BD))+
  ylab("BD in g/cm3")+
  xlab("Plot Number")+
  ggtitle("Bulk Density (BD) per Plot")



# Independent Samples t-Test

# Create vectors for each parameter

SOC_N <- c(0.675, 1.668, 1.207, 1.406, 1.110, 1.440, 2.010, 2.245, 1.776, 1.869, 1.374, 2.307, 2.408)
SOC_ANR <- c(1.341, 1.730, 1.984, 1.952, 1.488, 1.731, 1.691, 1.574, 1.497)

TN_N <- c(0.130, 0.274, 0.365, 0.128, 0.161, 0.365, 0.295, 0.366, 0.346, 0.362, 0.187, 0.567, 0.459)
TN_ANR <- c(0.211, 0.262, 0.540, 0.437, 0.271, 0.429, 0.574, 0.185, 0.240)

BD_N <- c(1.656, 1.529, 1.682, 1.537, 1.618, 1.453, 1.433, 1.558, 1.212, 1.100, 1.198, 1.488, 1.213)
BD_ANR <- c(0.926, 1.249, 1.043, 1.251, 1.156, 1.332, 1.297, 1.583, 1.385)

# t-Test

t.test(SOC_N, SOC_ANR, var.equal=TRUE)
# The p-value is 0.9487, so the null hypothesis remains true 
# (the two population means are equal)

t.test(TN_N, TN_ANR, var.equal=TRUE)
# The p-value is 0.4914, so the null hypothesis remains true 
# (the two population means are equal)

t.test(BD_N, BD_ANR, var.equal=TRUE)
# The p-value is 0.03433, which is lower than 0.05, so we 
# reject the null hypothesis (the two population means are not equal)




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