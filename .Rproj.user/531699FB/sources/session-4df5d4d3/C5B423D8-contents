
#Description of SARI rates by age group (not available for LU)

# Load required libraries
library(dplyr)
library(tidyr)

# Function to summarize SARI rates by age group
summarize_sari_rates <- function(df, country_name) {
  
  # Define standard variables for most countries
  sari_vars_standard <- c("saricaserate0_4", "saricaserate05_14", "saricaserate15_29", 
                          "saricaserate30_64", "saricaserate65+")
  
  # Define variables for Denmark (DK)
  sari_vars_DK <- c("saricaserate0_4", "saricaserate05_14", "saricaserate15_29", 
                    "saricaserate30_64", "saricaserate65_79", "saricaserate_80+")
  
  # Choose the correct set of variables
  sari_vars <- if (country_name == "Denmark") sari_vars_DK else sari_vars_standard
  
  # Ensure only existing variables are used
  sari_vars <- intersect(sari_vars, colnames(df))
  
  if (length(sari_vars) == 0) {
    message(paste("No valid SARI rate variables found for", country_name))
    return(NULL)
  }
  
  # Reshape data to long format
  df_long <- df %>%
    select(all_of(sari_vars)) %>%
    pivot_longer(cols = everything(), names_to = "age_group", values_to = "sari_rate") %>%
    mutate(age_group = recode(age_group,
                              "saricaserate0_4" = "0-4 years",
                              "saricaserate05_14" = "05-14 years",
                              "saricaserate15_29" = "15-29 years",
                              "saricaserate30_64" = "30-64 years",
                              "saricaserate65+" = "65+ years",
                              "saricaserate65_79" = "65-79 years",  # Denmark-specific
                              "saricaserate_80+" = "80+ years")) %>%  # Denmark-specific
    drop_na(sari_rate) # Remove NA values
  
  # Compute summary statistics with one decimal place
  summary_stats <- df_long %>%
    group_by(age_group) %>%
    summarise(
      mean_rate = round(mean(sari_rate, na.rm = TRUE), 1),
      median_rate = round(median(sari_rate, na.rm = TRUE), 1),
      min_rate = round(min(sari_rate, na.rm = TRUE), 1),
      max_rate = round(max(sari_rate, na.rm = TRUE), 1),
      Q1 = round(quantile(sari_rate, 0.25, na.rm = TRUE), 1),
      Q3 = round(quantile(sari_rate, 0.75, na.rm = TRUE), 1)
    ) %>%
    arrange(desc(mean_rate))
  
  # Print results
  print(paste("Summary for", country_name))
  print(summary_stats)
  
  return(summary_stats)
}

# List of countries for analysis
countries_to_analyze <- c("AU", "BE", "DK", "EE", "DE", "IS", "MT")

# Ensure the "summary" directory exists
output_dir <- "summary"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Create a list to store results for each country
summary_results <- list()

# Loop through the countries and apply function
for (country in countries_to_analyze) {
  # Retrieve the correct dataframe (e.g., AU_rates, BE_rates, etc.)
  df_name <- paste0(country, "_rates")
  
  if (exists(df_name, envir = .GlobalEnv)) {
    df <- get(df_name, envir = .GlobalEnv)
    country_full_name <- recode(country,
                                "AU" = "Austria",
                                "BE" = "Belgium",
                                "DK" = "Denmark",
                                "EE" = "Estonia",
                                "DE" = "Germany",
                                "IS" = "Iceland",
                                "MT" = "Malta")
    
    # Apply function
    summary_results[[country]] <- summarize_sari_rates(df, country_full_name)
    
    # Save output to CSV in "summary" folder
    write.csv(summary_results[[country]], 
              file.path(output_dir, paste0("SARI_Rates_Summary_", country, ".csv")), 
              row.names = FALSE)
  } else {
    message(paste("Skipping", country, "- No data available"))
  }
}


# LU - rates not provided by age group, number of cases instead

# Calculate the sum of SARI cases for each age group

# Summarize total cases and proportions by age group
age_summary_LU <- LU_rates %>%
  summarise(
    total_cases = sum(saricase_total, na.rm = TRUE),
    total_0_4 = sum(saricase0_4, na.rm = TRUE),
    total_05_14 = sum(saricase05_14, na.rm = TRUE),
    total_15_29 = sum(saricase15_29, na.rm = TRUE),
    total_30_64 = sum(saricase30_64, na.rm = TRUE),
    total_65_plus = sum(`saricase65+`, na.rm = TRUE)
  ) %>%
  mutate(
    prop_0_4 = total_0_4 / total_cases * 100,
    prop_05_14 = total_05_14 / total_cases * 100,
    prop_15_29 = total_15_29 / total_cases * 100,
    prop_30_64 = total_30_64 / total_cases * 100,
    prop_65_plus = total_65_plus / total_cases * 100
  )

# Print the proportions
print(age_summary_LU)


# Save output to CSV in "summary" folder
write.csv(age_summary_LU, 
          file.path(output_dir, paste0("SARI_counts_Summary_LU", ".csv")), 
          row.names = FALSE)






