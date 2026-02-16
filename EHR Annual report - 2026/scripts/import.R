
# IMPORT FUNCTION FOR EXCEL SHEET SARI_per_10x5pop" -----------------------------------
## 
read_sari_rates <- function(file_path) {
  
  if (!"SARI_per_10x5pop" %in% excel_sheets(file_path)) {
    message("Sheet 'SARI_per_10x5pop' not found in: ", file_path)
    return(NULL)
  }
  
  df <- read_excel(file_path, sheet = "SARI_per_10x5pop")
  
  df %>%
    select(
      country,
      week,
      saricaserate0_4,
      saricaserate05_14,
      saricaserate15_29,
      saricaserate30_64,
      saricaserate65plus,
      saricaserate_total
    )
}


for (code in country_codes) {
  
  file_path <- file.path(source_path, paste0("Data_", code, ".xlsx"))
  
  if (!file.exists(file_path)) {
    message("File not found: ", file_path)
    next
  }
  
  obj <- read_sari_rates(file_path)
  
  if (is.null(obj)) next
  
  assign(paste0("Rate_", code), obj, envir = .GlobalEnv)
}


Rate_list <- setNames(
  purrr::map(country_codes, ~ read_sari_rates(file.path(source_path, paste0("Data_", .x, ".xlsx")))),
  paste0("Rate_", country_codes)
) %>% purrr::compact()



# Normalize names, and select only columns that exist
read_optional_sheet <- function(file_path, sheet_name, keep_cols) {
  if (!file.exists(file_path)) return(NULL)
  
  sheets <- tryCatch(excel_sheets(file_path), error = function(e) character())
  if (!(sheet_name %in% sheets)) return(NULL)
  
  df <- read_excel(file_path, sheet = sheet_name)
  
  # normalize column names across files (handle "-" vs "_", extra spaces, etc.)
  names(df) <- names(df) |>
    str_trim() |>
    str_replace_all("-", "_") |>
    str_replace_all("\\s+", "_")
  
  # keep only columns that are present
  df <- df %>% select(any_of(keep_cols))
  
  # if after selecting we don't even have country+week, drop it
  if (!all(c("country", "week") %in% names(df))) return(NULL)
  
  df
}


# CREATE A NAMED LIST --------------------------------------------------

Rate_list <- setNames(
  purrr::map(country_codes, ~ read_sari_rates(file.path(source_path, paste0("Data_", .x, ".xlsx")))),
  paste0("Rate_", country_codes)
) %>% purrr::compact()


# --- Positivity SARS-CoV-2 sheet import (optional columns) -----------------

pos_sheet <- "Positivity SARS-CoV-2"

pos_cols <- c(
  "country", "week",
  "sc2_pos_0_4",
  "sc2_pos_5_14",
  "sc2_pos_15_29",
  "sc2_pos_30_64",
  "sc2_pos_65plus",
  "sc2_pos_65_79",
  "sc2_pos_80plus",
  "sc2_pos_total"
)

# Import for each country file; skip if sheet/cols missing
SC2pos_list <- purrr::map(country_codes, function(code) {
  file_path <- file.path(source_path, paste0("Data_", code, ".xlsx"))
  read_optional_sheet(file_path, pos_sheet, pos_cols)
})

names(SC2pos_list) <- paste0("SC2pos_", country_codes)
SC2pos_list <- purrr::compact(SC2pos_list)

# (Optional) also create objects in the GlobalEnv: SC2pos_DK, SC2pos_AU, ...
purrr::iwalk(SC2pos_list, ~ assign(.y, .x, envir = .GlobalEnv))


# --- Positivity FLU sheet import (optional columns) -----------------

pos_sheet <- "Positivity Influenza"

pos_cols <- c(
  "country", "week",
  "flu_pos_0_4",
  "flu_pos_5_14",
  "flu_pos_15_29",
  "flu_pos_30_64",
  "flu_pos_65plus",
  "flu_pos_65_79",
  "flu_pos_80plus",
  "flu_pos_total"
)

# Import for each country file; skip if sheet/cols missing
FLUpos_list <- purrr::map(country_codes, function(code) {
  file_path <- file.path(source_path, paste0("Data_", code, ".xlsx"))
  read_optional_sheet(file_path, pos_sheet, pos_cols)
})

names(FLUpos_list) <- paste0("FLUpos_", country_codes)
FLUpos_list <- purrr::compact(FLUpos_list)

# (Optional) also create objects in the GlobalEnv: FLUpos_DK, FLUpos_AU, ...
purrr::iwalk(FLUpos_list, ~ assign(.y, .x, envir = .GlobalEnv))




# --- Positivity RSV sheet import (optional columns) -----------------

pos_sheet <- "Positivity RSV"

pos_cols <- c(
  "country", "week",
  "rsv_pos_0_4",
  "rsv_pos_5_14",
  "rsv_pos_15_29",
  "rsv_pos_30_64",
  "rsv_pos_65plus",
  "rsv_pos_65_79",
  "rsv_pos_80plus",
  "rsv_pos_total"
)

# Import for each country file; skip if sheet/cols missing
RSVpos_list <- purrr::map(country_codes, function(code) {
  file_path <- file.path(source_path, paste0("Data_", code, ".xlsx"))
  read_optional_sheet(file_path, pos_sheet, pos_cols)
})

names(RSVpos_list) <- paste0("RSVpos_", country_codes)
RSVpos_list <- purrr::compact(RSVpos_list)

# (Optional) also create objects in the GlobalEnv: RSVpos_DK, RSVpos_AU, ...
purrr::iwalk(RSVpos_list, ~ assign(.y, .x, envir = .GlobalEnv))



