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
Tolabing <- read_xlsx("D:/UP/Thesis/Thesis/Tolabing.xlsx")
print(Tolabing, n = 22)

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
pairs(Tolabing[2:4])

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



# Comparison Bar Charts for ANR versus non-ANR measurements

# Create data frame for SOC
SOC_data <- data.frame(
  name = c("ANR", "Rainforestation"),
  value = c(1.6653, 1.6535),
  sd = c(0.0713, 0.1426)
)

# Create comparison bar chart for SOC
ggplot(SOC_data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(x = "Management Strategy", y = "SOC (g/100g)") +
  theme_bw() +
  theme(axis.text=element_text(size=11))

# Create data frame for TN
TN_data <- data.frame(
  name = c("ANR", "Rainforestation"),
  value = c(0.3499, 0.3081),
  sd = c(0.0490, 0.0363)
)

# Create comparison bar chart for TN
ggplot(TN_data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(x = "Management Strategy", y = "TN (g/100g)") +
  theme_bw() +
  theme(axis.text=element_text(size=11))

# Create data frame for BD
BD_data <- data.frame(
  name = c("ANR", "Rainforestation"),
  value = c(1.2468, 1.4365),
  sd = c(0.0640, 0.0536)
)

# Create comparison bar chart for BD
ggplot(BD_data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(x = "Management Strategy", y = "BD (g/cm3)") +
  theme_bw() +
  theme(axis.text=element_text(size=11))

# Create data frame for SCS
SCS_data <- data.frame(
  name = c("ANR", "Rainforestation"),
  value = c(41.5471, 46.6606),
  sd = c(2.5854, 3.8510)
)

# Create comparison bar chart for SCS
ggplot(SCS_data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  labs(x = "Management Strategy", y = "SCS (tons/hectare)") +
  theme_bw() +
  theme(axis.text=element_text(size=11))





# Create stacked barplot graph for total carbon stock of ACF

# Create data frame
SCS_stacked_df <- data.frame(team = c(" "),
                Type= c("Soil Carbon Stock", "Tree Carbon Stock"),
                 points =c(2628.0848, 419.649303))

# Create graph
ggplot(SCS_stacked_df, aes(fill=Type, y=points, x = team)) + 
  geom_bar(position='stack', stat='identity') + 
  labs(x = "Total Carbon Stock of ACF", y = "Carbon Stock (tonnes)")










# Independent Samples t-Test



# Create vectors for each parameter

SOC_N <- c(0.675, 1.668, 1.207, 1.406, 1.110, 1.440, 2.010, 2.245, 1.776, 1.869, 1.374, 2.307, 2.408)
SOC_ANR <- c(1.341, 1.730, 1.984, 1.952, 1.488, 1.731, 1.691, 1.574, 1.497)

TN_N <- c(0.130, 0.274, 0.365, 0.128, 0.161, 0.365, 0.295, 0.366, 0.346, 0.362, 0.187, 0.567, 0.459)
TN_ANR <- c(0.211, 0.262, 0.540, 0.437, 0.271, 0.429, 0.574, 0.185, 0.240)

BD_N <- c(1.656, 1.529, 1.682, 1.537, 1.618, 1.453, 1.433, 1.558, 1.212, 1.100, 1.198, 1.488, 1.213)
BD_ANR <- c(0.926, 1.249, 1.043, 1.251, 1.156, 1.332, 1.297, 1.583, 1.385)

SCS_N <- c(22.3574, 51.0026, 40.5916, 43.2078, 35.9121, 41.8347, 57.6155, 69.9528, 43.0414, 41.1038, 32.9211, 68.6453, 58.4017)
SCS_ANR <- c(24.8326, 43.2201, 41.3744, 48.8262, 34.3961, 46.1213, 43.8602, 49.8340, 41.4591)





# Creates QQ-plots for each parameter


# Create the standardize function
standardize <- function(x){return((x-mean(x))/sd(x))}



# Create the QQ-plots for SOC
qqnorm(standardize(SOC_N), main = "QQ Plot of SOC for Rainforestation")
abline(0,1)

qqnorm(standardize(SOC_ANR), main = "QQ Plot of SOC for ANR")
abline(0,1)

# The points fall close to the line, therefore we can assume normality.



# Create the QQ-plots for TN
qqnorm(standardize(TN_N), main = "QQ Plot of TN for Rainforestation")
abline(0,1)

qqnorm(standardize(TN_ANR), main = "QQ Plot of TN for ANR")
abline(0,1)

# The points fall close to the line, therefore we can assume normality.



# Create the QQ-plots for BD
qqnorm(standardize(BD_N), main = "QQ Plot of BD for Rainforestation")
abline(0,1)

qqnorm(standardize(BD_ANR), main = "QQ Plot of BD for ANR")
abline(0,1)

# The points fall close to the line, therefore we can assume normality.



# t-Test

t.test(SOC_N, SOC_ANR, var.equal=TRUE)
# The p-value is 0.9487, so the null hypothesis remains true 
# (the two population means are equal)

t.test(TN_N, TN_ANR, var.equal=TRUE)
# The p-value is 0.4914, so the null hypothesis remains true 
# (the two population means are equal)

t.test(BD_N, BD_ANR, var.equal=TRUE)
# The p-value is 0.03433, which is lower than 0.05, so we 
# reject the null hypothesis (the two population means are not equal).
# Thus, the average BD for non-ANR plots is significantly higher.

t.test(SCS_N, SCS_ANR, var.equal=TRUE)
# The p-value is 0.3304, so the null hypothesis remains true
# (the two population means are equal)



# Pearson's Correlation Test

# Creating vectors
SOC <- as.numeric(unlist(Data[2]))
TN <- as.numeric(unlist(Data[3]))
BD <- as.numeric(unlist(Data[4]))
SCS <- as.numeric(unlist(Data[5]))

# Testing TN and SOC
cor.test(TN, SOC, method = "pearson")
# The p-value is 0.0003695, which shows that the correlation is statistically significant.
# The correlation coefficient is 0.6910264, which shows a positive correlation. 

# Testing SOC and BD
cor.test(BD, SOC, method = "pearson")
# The p-value is not statistically significant at 0.171.
# The correlation coefficient is -0.3026293, which shows a negative correlation.

# Testing TN and BD
cor.test(BD, TN, method = "pearson")
# The p-value is 0.217, so it is not statistically significant.
# The correlation coefficient is -0.2741305, which shows a negative correlation.

# Testing SOC and SCS
cor.test(SOC, SCS, method = "pearson")
# The p-value is 1.056e-06, which shows that the correlation is statistically significant.
# The correlation coefficient is 0.8391895, which shows a positive correlation.

# Testing SCS and BD
cor.test(BD, SCS, method = "pearson")
# The p-value is 0.2659, so it is not statistically significant.
# The correlation coefficient is 0.2479479, which shows a positive correlation.

# Testing SCS and TN
cor.test(TN, SCS, method = "pearson")
# The p-value is 0.01408, which shows that the correlation is statistically significant.
# The correlation coefficient is 0.5154371, which shows a positive correlation.



# Testing SCS, Abundance, and Diversity
SCS <- as.numeric(unlist(Tolabing[2]))
Abundance <- as.numeric(unlist(Tolabing[3]))
Diversity <- as.numeric(unlist(Tolabing[4]))

# Testing SCS and Abundance
cor.test(SCS, Abundance, method = "pearson")
# The p-value is 0.602, so it is not statistically significant.
# The correlation coefficient is 0.1176737, which shows a positive correlation.

# Testing SCS and Diversity
cor.test(SCS, Diversity, method = "pearson")
# The p-value is 0.9785, so it is not statistically significant.
# The correlation coefficient is -0.006090195, which shows a negative correlation.

# Testing Abundance and Diversity
cor.test(Abundance, Diversity, method = "pearson")
# The p-value is 3.135e-05, so it is statistically significant.
# The correlation coefficient is 0.7668752 , which shows a positive correlation.
