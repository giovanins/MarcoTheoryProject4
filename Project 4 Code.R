install.packages("stringr")
library(stringr)
library(dplyr)
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
     main = "Unemployment Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Unemployment Rate (percentage)")

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
     xlab = "Monthly Data", ylab = "Employment Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$Employment_Rate), 
     ytop = max(data$Employment_Rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$Employment_Rate, col = "blue")

#Add a legend.
legend("topright", legend = "Employment Rate", col = "blue", lty = 1)

# Plot 3 participation rate

plot(data$new_date, data$Non_Participation_Rate, 
     type = "l", col = "purple", 
     main = "Non-Participation Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Non-Participation Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$Non_Participation_Rate), 
     ytop = max(data$Non_Participation_Rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$Non_Participation_Rate, col = "purple")

#Add a legend.
legend("topright", legend = "Non-Participation Rate", col = "purple", lty = 1)






data <- data %>%
  mutate(
    employed_lag = lag(Employment),
    unemployed_lag = lag(Unemployment),
    notinlabor_lag = lag(Not.in.the.Work.Force),
    Labor_Force = Unemployment + Employment,
    laborforce_lag = lag(Labor_Force)
  )

data <- data %>%
  mutate(emp_to_uemp =  ifelse(employed_lag > 0,
                               Unemployment - unemployed_lag, NA), 
         unemp_to_emp = ifelse(employed_lag > 0,
                               Employment - employed_lag, NA),
         nonpar_to_par = ifelse(notinlabor_lag > 0,
                                Not.in.the.Work.Force-notinlabor_lag, NA),
         nonpar_to_uemp = ifelse(notinlabor_lag > 0,
                                 Unemployment - unemployed_lag, NA),
         nonpar_to_emp = ifelse(notinlabor_lag > 0,
                                Employment - employed_lag, NA),
         unempr_to_nonpar = ifelse(unemployed_lag > 0,
                                   Not.in.the.Work.Force-notinlabor_lag, NA),
         empr_to_nonpar = ifelse(employed_lag > 0,
                                   Not.in.the.Work.Force-notinlabor_lag, NA)
  
  )

data <- data %>%
  mutate(emp_to_unemp_rate = ((emp_to_uemp / employed_lag) * 100)) %>%
  mutate(unemp_to_emp_rate = ((unemp_to_emp / unemployed_lag) * 100)) %>%
  mutate(nonpar_to_par_rate = ((nonpar_to_par / notinlabor_lag) * 100)) %>%
  mutate(nonpar_to_uemp_rate = ((nonpar_to_uemp / notinlabor_lag) * 100)) %>%
  mutate(nonpar_to_emp_rate = ((nonpar_to_emp / notinlabor_lag) * 100)) %>%
  mutate(unempr_to_nonpar_rate = ((unempr_to_nonpar / unemployed_lag) * 100)) %>%
mutate(empr_to_nonpar_rate = ((unempr_to_nonpar / employed_lag) * 100))
  
#Plot 4 Employemnt to Unemployment Rate

plot(data$new_date, data$emp_to_unemp_rate, 
     type = "l", col = "purple", 
     main = "Employemnt to Unemployment Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Employemnt to Unemployment Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$emp_to_unemp_rate), 
     ytop = max(data$emp_to_unemp_rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$emp_to_unemp_rate, col = "purple")

#Add a legend.
legend("topright", legend = "Employemnt to Unemployment Rate", col = "purple", lty = 1)

# Plot 5 Unemployment to Employment 

plot(data$new_date, data$unemp_to_emp_rate, 
     type = "l", col = "purple", 
     main = "Unemployment to Employment Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Unemployment to Employment Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$unemp_to_emp_rate), 
     ytop = max(data$unemp_to_emp_rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$unemp_to_emp_rate, col = "purple")
legend("bottomright", legend = "Unemployment to Employment Rate", col = "purple", lty = 1)


# Plot 6 Unemployment to Non-Participation Rate

plot(data$new_date, data$unempr_to_nonpar_rate, 
     type = "l", col = "purple", 
     main = "Unemployment to Non-Participation Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Unemployment to Non-Participation Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$unempr_to_nonpar_rate), 
     ytop = max(data$unempr_to_nonpar_rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$unempr_to_nonpar_rate, col = "purple")
legend("topright", legend = "Unemployment to Non-Participation Rate", col = "purple", lty = 1)


# Plot 7 Non-Participation To Employment Rate

plot(data$new_date, data$nonpar_to_emp_rate, 
     type = "l", col = "purple", 
     main = "Non-Participation To Employment Rate 2015-2025", 
     xlab = "Monthly Data", ylab = "Non-Participation To Employment Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$nonpar_to_emp_rate), 
     ytop = max(data$nonpar_to_emp_rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$nonpar_to_emp_rate, col = "purple")
legend("bottomright", legend = "Non-Participation To Employment Rate", col = "purple", lty = 1)


# Plot 8 Employment To Non Participation Rate

plot(data$new_date, data$empr_to_nonpar_rate, 
     type = "l", col = "purple", 
     main = "Employment To Non Participation 2015-2025", 
     xlab = "Monthly Data", ylab = "Employment To Non Participation Rate (percentage)")
rect(xleft = x_shade_left, 
     xright = x_shade_right, 
     ybottom = min(data$empr_to_nonpar_rate), 
     ytop = max(data$empr_to_nonpar_rate), 
     col = rgb(0.8, 0.8, 0.8, 0.4), # Light gray with transparency
     border = NA)
lines(data$new_date, data$empr_to_nonpar_rate, col = "purple")
legend("topright", legend = "Employment To Non Participation Rate", col = "purple", lty = 1)


