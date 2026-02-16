library(ggplot2)
library(dplyr)
library(tidyr)

# Function to reduce x-axis labels for better readability
every_nth <- function(x, n) {
  x[seq(1, length(x), by = n)]
}

# Define color mapping for age groups
age_colors <- c(
  "0-4 years" = "#E41A1C",    # Red
  "05-14 years" = "#377EB8",  # Blue
  "15-29 years" = "#4DAF4A",  # Green
  "30-64 years" = "#984EA3",  # Purple
  "65-79 years" = "#FF7F00",  # Orange
  "80+ years" = "#A65628"     # Brown
)

# Load hospitalization data for Denmark
df <- DK_rates
age_vars <- c("saricaserate0_4", "saricaserate05_14", "saricaserate15_29", "saricaserate30_64", "saricaserate65_79", "saricaserate_80+")

df_long <- df %>%
  select(week, all_of(age_vars)) %>%
  pivot_longer(cols = all_of(age_vars), names_to = "age_group", values_to = "rate") %>%
  mutate(age_group = factor(case_when(
    grepl("0_4", age_group) ~ "0-4 years",
    grepl("5_14", age_group) ~ "05-14 years",  # Fixed label
    grepl("15_29", age_group) ~ "15-29 years",
    grepl("30_64", age_group) ~ "30-64 years",
    grepl("65_79", age_group) ~ "65-79 years",
    grepl("80\\+", age_group) ~ "80+ years",
    TRUE ~ age_group
  ), levels = c("0-4 years", "05-14 years", "15-29 years", "30-64 years", "65-79 years", "80+ years")))

# Ensure all age groups appear in the dataset
df_long <- df_long %>%
  complete(week, age_group, fill = list(rate = NA))

# Create hospitalization plot
plot_sari <- ggplot(df_long, aes(x = week, y = rate, color = age_group, group = age_group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = age_colors, drop = FALSE) +  # Ensures all groups appear
  scale_y_continuous(name = "SARI Admissions per 100,000", limits = c(0, max(df_long$rate, na.rm = TRUE) * 1.1)) +
  labs(title = "SARI Admissions per Age Group in Denmark", x = "Epidemiological Week", color = "Age Group") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = every_nth(df$week, 2))

# Save hospitalization plot
ggsave("FIGURES/SARI_Age_Plot_DK.png", plot_sari, width = 10, height = 6, dpi = 800, bg = "white")

# Process positivity data for Denmark
positivity_dfs <- list(
  "SARS-CoV-2" = `DK_Positivity_SARS-CoV-2`,
  "Influenza" = `DK_Positivity_Influenza`,
  "RSV" = `DK_Positivity_RSV`
)

for (virus in names(positivity_dfs)) {
  df_pos <- positivity_dfs[[virus]]
  
  # Define variable prefix dynamically
  prefix <- case_when(
    virus == "SARS-CoV-2" ~ "sc2_pos",
    virus == "Influenza" ~ "flu_pos",
    virus == "RSV" ~ "rsv_pos"
  )
  
  pos_vars <- intersect(c(
    paste0(prefix, "_0_4"), paste0(prefix, "_5_14"),
    paste0(prefix, "_15_29"), paste0(prefix, "_30_64"),
    paste0(prefix, "_65_79"), paste0(prefix, "_80+")
  ), colnames(df_pos))
  
  if (length(pos_vars) == 0) {
    print(paste("Skipping", virus, "- No valid positivity data"))
    next
  }
  
  df_long <- df_pos %>%
    select(week, all_of(pos_vars)) %>%
    pivot_longer(cols = all_of(pos_vars), names_to = "age_group", values_to = "positivity") %>%
    mutate(age_group = factor(case_when(
      age_group == paste0(prefix, "_0_4") ~ "0-4 years",
      age_group == paste0(prefix, "_5_14") ~ "05-14 years",  # Fixed label
      age_group == paste0(prefix, "_15_29") ~ "15-29 years",
      age_group == paste0(prefix, "_30_64") ~ "30-64 years",
      age_group == paste0(prefix, "_65_79") ~ "65-79 years",
      age_group == paste0(prefix, "_80+") ~ "80+ years",
      TRUE ~ age_group
    ), levels = c("0-4 years", "05-14 years", "15-29 years", "30-64 years", "65-79 years", "80+ years")))
  
  # Ensure all age groups appear in the dataset
  df_long <- df_long %>%
    complete(week, age_group, fill = list(positivity = NA))
  
  # Create positivity plot
  plot_pos <- ggplot(df_long, aes(x = week, y = positivity, color = age_group, group = age_group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = age_colors, drop = FALSE) +  # Ensures all groups appear
    scale_y_continuous(limits = c(0, 100), name = "Test Positivity (%)") +
    labs(title = paste(virus, "Positivity by Age Group in Denmark"), x = "Epidemiological Week", color = "Age Group") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(breaks = every_nth(df_pos$week, 2))
  
  # Save positivity plot
  file_path <- paste0("FIGURES/", gsub(" ", "_", virus), "_Age_Plot_DK.png")
  ggsave(file_path, plot_pos, width = 10, height = 6, dpi = 800, bg = "white")
}


