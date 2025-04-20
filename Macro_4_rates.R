setwd("C:/Users/stuar/OneDrive - University of Cincinnati/Time Series/R Studio")
install.packages("stringr")
library(stringr)
library(dplyr)
install.packages("lubridate")
library(lubridate)
library(ggplot2)

data <- read.csv("Macro Hw4.csv")


data <- data %>%
  mutate(Unemployment_Rate = (Unemployment / (Employment + Unemployment)) * 100) %>%
  mutate(Employment_Rate = (Employment / (Employment + Unemployment))* 100) %>%
  mutate(Non_Participation_Rate = (Not.in.the.Work.Force /(Employment + Unemployment + Not.in.the.Work.Force))* 100)

data <- data %>%
  mutate( new_date = ymd(paste(Year, str_remove(Period, "M"), "01", sep = "-")))

# Assuming your data frame is named 'data' and has columns 'new_date' and 'Unemployment_Rate'

plot(data$new_date, data$Unemployment_Rate, 
     type = "l", col = "red", 
     main = "Unemmployment Rate 2015-2025", 
     xlab = "Time (monthly)", ylab = "Rate")

# Define the shading region
start_shade <- ymd("2020-02-01")
end_shade <- ymd("2022-01-01")

# Find the x-axis limits for the shaded region
x_shade_left <- min(data$new_date[data$new_date >= start_shade])
x_shade_right <- max(data$new_date[data$new_date <= end_shade])

# Shading
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$Unemployment_Rate), 
     ytop = max(data$Unemployment_Rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA) # No border

# Replot the line on top of the shading
lines(data$new_date, data$Unemployment_Rate, col = "red")

#Add a legend.
legend("topright", legend = "Unemployment Rate", col = "red", lty = 1)

#plot 2 Employment

plot(data$new_date, data$Employment_Rate, 
     type = "l", col = "blue", 
     main = "Employment Rate 2015-2025", 
     xlab = "Time (months)", ylab = "Rate")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$Employment_Rate), 
     ytop = max(data$Employment_Rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$Employment_Rate, col = "blue")

#Add a legend.
legend("bottomright", legend = "Employment Rate", col = "blue", lty = 1)

# Plot 3 non-participation rate

plot(data$new_date, data$Non_Participation_Rate, 
     type = "l", col = "purple", 
     main = "Non-Participation Rate 2015-2025", 
     xlab = "Time (months", ylab = "Rate")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$Non_Participation_Rate), 
     ytop = max(data$Non_Participation_Rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$Non_Participation_Rate, col = "purple")

#Add a legend.
legend("topright", legend = "Non-Participation Rate", col = "purple", lty = 1)

