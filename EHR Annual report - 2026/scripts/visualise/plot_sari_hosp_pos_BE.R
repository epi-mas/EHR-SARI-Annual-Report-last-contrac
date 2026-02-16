
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to reduce x-axis labels for better readability
every_nth <- function(x, n) {
  x[seq(1, length(x), by = n)]
}

# Define country name
country <- "BE"
country_name <- "Belgium"

df <- BE_counts  # Belgium only has _counts dataframe

y_label <- "SARI Hospitalisations"
hospitalization_var <- "saricase_total"

# Ensure the hospitalization column exists
if (!(hospitalization_var %in% colnames(df))) {
  stop("Hospitalization variable missing in Belgium dataset")
}

# Identify available positivity data
positivity_dfs <- list(
  "sc2_pos_total" = "BE_Positivity_SARS-CoV-2",
  "flu_pos_total" = "BE_Positivity_Influenza",
  "rsv_pos_total" = "BE_Positivity_RSV"
)

positivity_data <- list()

for (var in names(positivity_dfs)) {
  df_name <- positivity_dfs[[var]]
  if (exists(df_name, where = .GlobalEnv)) {
    df_pos <- get(df_name, envir = .GlobalEnv)
    if (var %in% colnames(df_pos)) {
      df_pos <- df_pos %>% mutate(across(all_of(var), as.numeric, .names = "{.col}")) # Convert to numeric
      positivity_data[[var]] <- df_pos[, c("week", var)]
    }
  }
}

# Merge positivity data
if (length(positivity_data) > 0) {
  positivity_df <- Reduce(function(x, y) full_join(x, y, by = "week"), positivity_data)
  df <- left_join(df, positivity_df, by = "week")
}

# Set plot title
plot_title <- paste("SARI Hospitalisations and Test Positivity in", country_name)

# Create the base plot
plot <- ggplot(df, aes(x = week)) +
  geom_bar(aes(y = .data[[hospitalization_var]], fill = "SARI Cases"), 
           stat = "identity", alpha = 0.7, fill = "grey") +
  scale_y_continuous(
    name = y_label,
    sec.axis = sec_axis(~ . * 100 / max(df[[hospitalization_var]], na.rm = TRUE), 
                        name = "Test Positivity (%)", labels = function(x) round(x))
  ) +
  labs(
    title = plot_title,
    x = "Epidemiological Week",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  ) +
  scale_x_discrete(breaks = every_nth(df$week, 2))

# Add positivity rates if available
if (length(positivity_data) > 0) {
  df_long <- df %>%
    select(week, all_of(names(positivity_data))) %>%
    pivot_longer(cols = names(positivity_data), names_to = "positivity_type", values_to = "positivity_value")
  
  positivity_legend_labels <- c(
    "sc2_pos_total" = "SARS-CoV-2 positivity",
    "flu_pos_total" = "Influenza virus positivity",
    "rsv_pos_total" = "RSV positivity"
  )
  
  df_long$positivity_type <- factor(df_long$positivity_type, 
                                    levels = names(positivity_legend_labels),
                                    labels = positivity_legend_labels[names(positivity_legend_labels) %in% df_long$positivity_type])
  
  plot <- plot +
    geom_line(data = df_long, aes(x = week, y = positivity_value * max(df[[hospitalization_var]], na.rm = TRUE) / 100, 
                                  color = positivity_type, group = positivity_type), linewidth = 1) +
    scale_color_manual(
      name = NULL,
      values = c("SARS-CoV-2 positivity" = "#E41A1C", 
                 "Influenza virus positivity" = "#377EB8", 
                 "RSV positivity" = "#E69F00")
    )
}

# Save plot
file_path <- paste0("FIGURES/SARI_plot_", country, "_", Sys.Date(), ".png")
ggsave(file_path, plot, width = 10, height = 6, dpi = 800, bg = "white")
print(paste("Plot saved:", file_path))