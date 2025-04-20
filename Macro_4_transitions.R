# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

setwd("C:/Users/stuar/OneDrive - University of Cincinnati/Time Series/R Studio")



if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)



#Employment Status Categories
data <- data %>%
  mutate(
    category = case_when(
      EMPSTAT %in% 01:12 ~ "Employed",
      EMPSTAT %in% 20:22 ~ "Unemployed",
      EMPSTAT %in% 30:36 ~ "NILF",
      TRUE ~ "NIU"  # optional: for unrecognized values
    )
  )

# Make Panel Data
library(data.table)

# Convert to data.table
panel_dt <- as.data.table(data)
setkey(panel_dt, CPSIDP, YEAR, MONTH)

# Lag EMPSTAT to track previous status
panel_dt[, EMPSTAT_lag := shift(EMPSTAT), by = CPSIDP]

panel_dt <- panel_dt[AGE >= 16]


panel_dt[, EMPSTAT_cat := fifelse(EMPSTAT %in% 01:12, "Employed",
                                  fifelse(EMPSTAT %in% 20:22, "Unemployed",
                                          fifelse(EMPSTAT %in% 30:36, "NILF", NA_character_)))]

panel_dt[, EMPSTAT_lag_cat := fifelse(EMPSTAT_lag %in% 01:12, "Employed",
                                      fifelse(EMPSTAT_lag %in% 20:22, "Unemployed",
                                              fifelse(EMPSTAT_lag %in% 30:36, "NILF", NA_character_)))]

panel_dt[, transition := paste0(EMPSTAT_lag_cat, "_to_", EMPSTAT_cat)]

# Filter to valid transitions
valid_transitions <- panel_dt[!is.na(transition) & !is.na(EMPSTAT_lag_cat)]

# Count transitions and origin group sizes
transition_rates <- valid_transitions[,
                                      .(count = .N),
                                      by = .(EMPSTAT_lag_cat, transition)
]

# Calculate rates: count / total in starting category
transition_rates[, total_origin := sum(count), by = EMPSTAT_lag_cat]
transition_rates[, rate := count / total_origin]

transition_rates <- valid_transitions[,
                                      .(count = .N),
                                      by = .(YEAR, MONTH, EMPSTAT_lag_cat, transition)
]

transition_rates[, total_origin := sum(count), by = .(YEAR, MONTH, EMPSTAT_lag_cat)]
transition_rates[, rate := count / total_origin]


# Create transition rate plots
library(ggplot2)
library(lubridate)

###create dates variable
transition_rates[, date := make_date(YEAR, MONTH, 1)]


  
# Create one plot per transition
unique_transitions <- unique(transition_rates$transition)

plots <- lapply(unique_transitions, function(tr) {
  ggplot(transition_rates[transition == tr], aes(x = date, y = rate)) +
    geom_line(color = "purple", size = 1) +
    labs(
      title = paste("Transition Rate:", tr),
      x = "Date",
      y = "Rate"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal()
})

####employment to unemployment
ggplot(transition_rates[transition == "Employed_to_Unemployed"], aes(x = date, y = rate)) +   ####transition
    geom_line(color = "purple", size = 1) +
    labs(
      title = paste("Employment to Unemployment"),     ####title =
      x = "Time (months)",
      y = "Rate"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal()
# Define shading window
start_shade <- as.Date("2020, 02")
end_shade <- as.Date("2021, 12")


#######Fancy Graphs with Shading

#######Employed to Unemployed
library(ggplot2)

# Define shading period
start_shade <- as.Date("2020-02-01")
end_shade <- as.Date("2021-12-31")

# Plot
ggplot(transition_rates[transition == "Employed_to_Unemployed"], 
       aes(x = date, y = rate, color = "EMUN")) +  # <-- label updated here
  
  annotate("rect", xmin = start_shade, xmax = end_shade, 
           ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2) +
  
  geom_line(size = 1) +
  
  labs(
    title = "Employment to Unemployment",
    x = "Time (months)",
    y = "Rate",
    color = NULL  # <-- removes legend title
  ) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("EMUN" = "purple")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # Legend formatting
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
  )



########Unemployment to Employment
# Plot
ggplot(transition_rates[transition == "Unemployed_to_Employed"], 
       aes(x = date, y = rate, color = "UNEM")) +
  
  annotate("rect", xmin = start_shade, xmax = end_shade, 
           ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2) +
  
  geom_line(size = 1) +
  
  labs(
    title = "Unemployment to Employment",
    x = "Time (months)",
    y = "Rate",
    color = NULL
  ) +
  
  #y-axis scale
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.03, 0.07, by = 0.01)  # Adjust as needed based on your data range
  ) +
  
  scale_color_manual(values = c("UNEM" = "purple")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
  )








########Non-participation to Employment
ggplot(transition_rates[transition == "NILF_to_Employed"], 
       aes(x = date, y = rate, color = "NPEM")) +
  
  annotate("rect", xmin = start_shade, xmax = end_shade, 
           ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2) +
  
  geom_line(size = 1) +
  
  labs(
    title = "Non-participation to Employment",
    x = "Time (months)",
    y = "Rate",
    color = NULL
  ) +
  
  # y-axis scale
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.03, 0.07, by = 0.01)  # Adjust as needed based on your data range
  ) +
  
  scale_color_manual(values = c("NPEM" = "purple")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
  )








########Unemployed to Non-participation
ggplot(transition_rates[transition == "Unemployed_to_NILF"], 
       aes(x = date, y = rate, color = "UNNP")) +
  
  annotate("rect", xmin = start_shade, xmax = end_shade, 
           ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.2) +
  
  geom_line(size = 1) +
  
  labs(
    title = "Unemployment to Non-participation",
    x = "Time (months)",
    y = "Rate",
    color = NULL
  ) +
  
  # y-axis scale 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.15, 0.40, by = 0.05)  # Adjust upper limit as needed
  ) +
  
  scale_color_manual(values = c("UNNP" = "purple")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
  )

