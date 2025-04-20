# Marco Theory Project 4

## Requirements
Running this data requires the use of R Studio and Matlab. 

Make sure to have Dynare installed in Matlab in order to run code. 

The following packages need to be installed on RStudio before running the script:
 - stringr
 - dplyr
 - lubridate
 - ggplot2
 - ipumsr
 - data.table


## Part 1
### Simulating DSGE Model

For this section, you need to input the Macro Hw4 Matlab Code.txt file into Matlab in order to properly run the code. 

 1. Define your list of endogenous variables using var, your list of exogenous variables using varexo, and your list of parameters given by the report.
 2. Define your model using the equilibrium equations given.
 3. Create a steady state using the fuction steady.
 4. Define a shock category, using variable z as the shock, periods as the number of periods, and values as the value of the shock.
 5. Run the shock for a 100 periods and print output using the perfect foresight solver function
 6. Graph the data on a line plot

## Part 2
### Labor Market Flows and the COVID-19 Pandemic

#### Finding Rates

For this part use the Marcro_4_rates.R file to run data.

 1. Define data set using Macro Hw4.csv file.
 2. Create rates for Unemployment rate, Employment Rate, and Non-Participation Rate through calculation within data set.
 3. Convert the date column into a readable format for data plots.
 4. Plot the data for each rate, highlighting the period of the COVID-19 Crisis.

#### Finding Transition Rates

For this part use the Marcro_4_transitions.R file to run data. Additionally, you need to have [the follwing dataset]([https://link-url-here.org](https://mailuc-my.sharepoint.com/:u:/g/personal/olleysd_mail_uc_edu/EZlL-NHFCYhBlKAf3S1QbkQBqW_B0G3qehaR_d837olxyQ)) in your folder as well.

1. Define data set using cps_00001.xml file.
2. Redefine the labor status in the new data set.
3. Make the panel data set by coverting data.table, creating lags of previous status, filtering and couuting the transitions, and calulculating transition rates.
4. Create large plot with all transition rates.
5. Create individual plots for each transition rate.





