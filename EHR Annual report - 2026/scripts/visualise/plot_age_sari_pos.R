library(ggplot2)
library(dplyr)
library(tidyr)

# Function to reduce x-axis labels for better readability
every_nth <- function(x, n) {
  x[seq(1, length(x), by = n)]
}

# Country name mapping
country_names <- c(
  "DE" = "Germany", "LU" = "Luxembourg", "MT" = "Malta",
  "EE" = "Estonia", "IS" = "Iceland",
  "AU" = "Austria", "BE" = "Belgium"
)

# Define color mapping for age groups
age_colors <- c(
  "0-4 years" = "#E41A1C",    # Red
  "05-14 years" = "#377EB8",  # Blue
  "15-29 years" = "#4DAF4A",  # Green
  "30-64 years" = "#984EA3",  # Purple
  "65+ years" = "#FF7F00"     # Orange
)

# List of country codes
country_codes <- names(country_names)

for (country in country_codes) {
  
  # Check for available hospitalization dataframe
  df_rates_name <- paste0(country, "_rates")
  df_counts_name <- paste0(country, "_counts")
  
  if (exists(df_rates_name, envir = .GlobalEnv)) {
    df <- get(df_rates_name, envir = .GlobalEnv)
    age_vars <- intersect(c("saricaserate0_4", "saricaserate05_14", "saricaserate15_29", "saricaserate30_64", "saricaserate65+"), colnames(df))
    y_label <- "SARI Admissions per 100,000"
    title_text <- paste("SARI Admissions per 100,000 in", country_names[[country]])
  } else if (exists(df_counts_name, envir = .GlobalEnv)) {
    df <- get(df_counts_name, envir = .GlobalEnv)
    age_vars <- intersect(c("saricase0_4", "saricase05_14", "saricase15_29", "saricase30_64", "saricase65+"), colnames(df))
    y_label <- "SARI Cases"
    title_text <- paste("SARI Cases in", country_names[[country]])
  } else {
    print(paste("Skipping", country, "- No hospitalization data available"))
    next
  }
  
  if (length(age_vars) > 0) {
    df <- df %>% mutate(across(all_of(age_vars), as.numeric))
    df_long <- df %>%
      select(week, all_of(age_vars)) %>%
      pivot_longer(cols = all_of(age_vars), names_to = "age_group", values_to = "rate") %>%
      mutate(age_group = factor(case_when(
        grepl("0_4", age_group) ~ "0-4 years",
        grepl("5_14", age_group) ~ "05-14 years",  # Updated label for legend
        grepl("15_29", age_group) ~ "15-29 years",
        grepl("30_64", age_group) ~ "30-64 years",
        grepl("65+", age_group) ~ "65+ years",
        TRUE ~ age_group
      ), levels = c("0-4 years", "05-14 years", "15-29 years", "30-64 years", "65+ years")))
    
    # Ensure all age groups appear
    df_long <- df_long %>%
      complete(week, age_group, fill = list(rate = NA))
    
    plot_sari <- ggplot(df_long, aes(x = week, y = rate, color = age_group, group = age_group)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = age_colors, drop = FALSE) +  # Ensures all groups appear
      scale_y_continuous(name = y_label, limits = c(0, max(df_long$rate, na.rm = TRUE) * 1.1)) +
      labs(title = title_text, x = "Epidemiological Week", color = "Age Group") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = every_nth(df$week, 2))
    
    file_path <- paste0("FIGURES/SARI_Age_Plot_", country, "_", Sys.Date(), ".png")
    ggsave(file_path, plot_sari, width = 10, height = 6, dpi = 800, bg = "white")
    print(paste("Plot saved:", file_path))
  }
  
  # Identify available positivity data
  positivity_dfs <- list(
    "sc2_pos" = paste0(country, "_Positivity_SARS-CoV-2"),
    "flu_pos" = paste0(country, "_Positivity_Influenza"),
    "rsv_pos" = paste0(country, "_Positivity_RSV")
  )
  
  for (virus in names(positivity_dfs)) {
    df_name <- positivity_dfs[[virus]]
    if (exists(df_name, envir = .GlobalEnv)) {
      df_pos <- get(df_name, envir = .GlobalEnv)
      pos_vars <- intersect(c(paste0(virus, "_0_4"), paste0(virus, "_5_14"), paste0(virus, "_15_29"), paste0(virus, "_30_64"), paste0(virus, "_65+")), colnames(df_pos))
      if (length(pos_vars) > 0) {
        df_pos <- df_pos %>% mutate(across(all_of(pos_vars), as.numeric))
        df_long <- df_pos %>%
          select(week, all_of(pos_vars)) %>%
          pivot_longer(cols = all_of(pos_vars), names_to = "age_group", values_to = "positivity") %>%
          mutate(age_group = factor(case_when(
            grepl("0_4", age_group) ~ "0-4 years",
            grepl("5_14", age_group) ~ "05-14 years",  # Updated label for legend
            grepl("15_29", age_group) ~ "15-29 years",
            grepl("30_64", age_group) ~ "30-64 years",
            grepl("65+", age_group) ~ "65+ years",
            TRUE ~ age_group
          ), levels = c("0-4 years", "05-14 years", "15-29 years", "30-64 years", "65+ years")))
        
        # Ensure all age groups appear
        df_long <- df_long %>%
          complete(week, age_group, fill = list(positivity = NA))
        
        plot_pos <- ggplot(df_long, aes(x = week, y = positivity, color = age_group, group = age_group)) +
          geom_line(linewidth = 1.2) +
          scale_color_manual(values = age_colors, drop = FALSE) +  # Ensures all groups appear
          scale_y_continuous(limits = c(0, 100), name = "Test Positivity (%)") +
          labs(title = paste(virus, "Positivity by Age Group in", country_names[[country]]), x = "Epidemiological Week", color = "Age Group") +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_x_discrete(breaks = every_nth(df_pos$week, 2))
        
        file_path <- paste0("FIGURES/", virus, "_Age_Plot_", country, "_", Sys.Date(), ".png")
        ggsave(file_path, plot_pos, width = 10, height = 6, dpi = 800, bg = "white")
        print(paste("Plot saved:", file_path))
      }
    }
  }
}



