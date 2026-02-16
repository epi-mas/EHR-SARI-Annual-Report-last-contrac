library(ggplot2)
library(dplyr)
library(tidyr)

# Define country-specific details
country <- "LU"
country_name <- "Luxembourg"

df <- LU_rates  # Using LU_rates dataframe

# Define age variables to be plotted
age_vars <- c("saricase0_4", "saricase05_14", "saricase15_29", "saricase30_64", "saricase65+")

# Ensure only available columns are used
age_vars <- intersect(age_vars, colnames(df))

# Check if required variables exist
if (length(age_vars) == 0) {
  stop("No valid age group variables found in Luxembourg dataset")
}

# Convert columns to numeric
for (var in age_vars) {
  df[[var]] <- as.numeric(df[[var]])
}

# Reshape data for plotting
df_long <- df %>%
  select(week, all_of(age_vars)) %>%
  pivot_longer(cols = all_of(age_vars), names_to = "age_group", values_to = "cases") %>%
  mutate(age_group = factor(case_when(
    grepl("0_4", age_group) ~ "0-4 years",
    grepl("5_14", age_group) ~ "05-14 years",  # Fixed label
    grepl("15_29", age_group) ~ "15-29 years",
    grepl("30_64", age_group) ~ "30-64 years",
    grepl("65+", age_group) ~ "65+ years",
    TRUE ~ age_group
  ), levels = c("0-4 years", "05-14 years", "15-29 years", "30-64 years", "65+ years")))

# Define color mapping for age groups
age_colors <- c(
  "0-4 years" = "#E41A1C",    # Red
  "05-14 years" = "#377EB8",  # Blue
  "15-29 years" = "#4DAF4A",  # Green
  "30-64 years" = "#984EA3",  # Purple
  "65+ years" = "#FF7F00"     # Orange
)

# Create plot
plot_sari <- ggplot(df_long, aes(x = week, y = cases, color = age_group, group = age_group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = age_colors, drop = FALSE) +  # Ensures all groups appear
  scale_y_continuous(name = "SARI Cases", limits = c(0, max(df_long$cases, na.rm = TRUE) * 1.1)) + # Dynamic Y-axis
  labs(title = paste("SARI Cases in", country_name), x = "Epidemiological Week", color = "Age Group") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = unique(df$week)[seq(1, length(unique(df$week)), by = 2)])

# Save plot
file_path <- paste0("FIGURES/SARI_Age_Plot_", country, "_", Sys.Date(), ".png")
ggsave(file_path, plot_sari, width = 10, height = 6, dpi = 800, bg = "white")
print(paste("Plot saved:", file_path))
