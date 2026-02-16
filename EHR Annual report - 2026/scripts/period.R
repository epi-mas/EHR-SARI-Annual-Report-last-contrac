# DEFINE STUDY PERIOD -----------------------------------------------------

## Study period for Rates datasets

for (code in country_codes) {
  obj_name <- paste0("Rate_", code)
  
  if (exists(obj_name)) {
    assign(
      obj_name,
      get(obj_name) %>%
        dplyr::filter(week >= week_from & week <= week_to),
      envir = .GlobalEnv
    )
  }
}

## Study period for SC2 datasets
for (code in country_codes) {
  obj_name <- paste0("SC2pos_", code)
  
  if (exists(obj_name)) {
    assign(
      obj_name,
      get(obj_name) %>%
        dplyr::filter(week >= week_from & week <= week_to),
      envir = .GlobalEnv
    )
  }
}

## Study period for FLU datasets
for (code in country_codes) {
  obj_name <- paste0("FLUpos_", code)
  
  if (exists(obj_name)) {
    assign(
      obj_name,
      get(obj_name) %>%
        dplyr::filter(week >= week_from & week <= week_to),
      envir = .GlobalEnv
    )
  }
}

## Study period for RSV datasets
for (code in country_codes) {
  obj_name <- paste0("RSVpos_", code)
  
  if (exists(obj_name)) {
    assign(
      obj_name,
      get(obj_name) %>%
        dplyr::filter(week >= week_from & week <= week_to),
      envir = .GlobalEnv
    )
  }
}



# # Get all data frames in the environment
# all_dfs <- ls()
# 
# # Function to safely check range of week column
# check_week_range <- function(df_name) {
#   df <- get(df_name)
#   if ("week" %in% names(df)) {
#     range_result <- range(df$week)
#     paste(df_name, ":", range_result[1], "to", range_result[2])
#   }
# }
# 
# # Filter for relevant dataframes (counts, rates, and Positivity)
# relevant_dfs <- all_dfs[grep("(counts|rates|Positivity)", all_dfs)]
# 
# # Check ranges for all relevant dataframes
# week_ranges <- sapply(relevant_dfs, check_week_range)
# 
# # Remove NULL results and print
# week_ranges <- week_ranges[!sapply(week_ranges, is.null)]
# print(week_ranges)
# 
# # Get all data frames that start with any of the country codes
# matching_dfs <- ls(pattern = paste0("^(", paste(country_codes, collapse = "|"), ")"))
# 
# # Define week filtering function
# filter_week_range <- function(df_name) {
#   df <- get(df_name)  # Retrieve dataframe
#   
#   
# # Note: according to data availability keep from 2025-W01 to 2025-W52 in all datasets
#   ## Check if 'week' column exists
#   if ("week" %in% colnames(df)) {
#     # Convert 'week' column to character if not already
#     df <- df %>%
#       filter(week >= week_from & week <= week_to)
#     
#     ## Reassign the filtered dataframe back to the global environment
#     assign(df_name, df, envir = .GlobalEnv)
#   }
# }
# 
# # Apply the filter function to all matching dataframes
# lapply(matching_dfs, filter_week_range)
# 
# # Print filtered data frames
# print(paste("Filtered datasets according to period:", paste(matching_dfs, collapse = ", ")))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
