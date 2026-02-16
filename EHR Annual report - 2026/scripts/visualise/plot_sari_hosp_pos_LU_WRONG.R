
# LU

library(ggplot2)
library(dplyr)
library(tidyr)

# Define colors
sari_color <- "grey"  # Color for SARI Cases
sars_cov2_color <- "#E41A1C"  # Red for SARS-CoV-2 positivity

# Load hospitalization data
df <- LU_rates

# Define the hospitalization variable
hospitalization_var <- "saricaserate_total"  # Use the correct column name
y_label <- "SARI Admission Rate"

# Load positivity data
if (exists("LU_Positivity_SARS-CoV-2", envir = .GlobalEnv)) {
  df_pos <- get("LU_Positivity_SARS-CoV-2", envir = .GlobalEnv)
  
  # Ensure numeric values
  df_pos <- df_pos %>%
    mutate(across("sc2_pos_total", as.numeric)) %>%
    select(week, sc2_pos_total)
  
  # Merge with hospitalization data
  df <- left_join(df, df_pos, by = "week")
}

# Set plot title
plot_title <- "SARI Hospitalisations and SARS-CoV-2 Positivity in Luxembourg"

# Create the base plot
plot <- ggplot(df, aes(x = week)) +
  geom_bar(aes(y = .data[[hospitalization_var]], fill = "SARI Cases"),
           stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("SARI Cases" = sari_color)) +
  scale_y_continuous(
    name = y_label,
    sec.axis = sec_axis(~ . * 100 / max(df[[hospitalization_var]], na.rm = TRUE),
                        name = "Test Positivity (%)", labels = function(x) round(x))
  ) +
  labs(
    title = plot_title,
    x = "Epidemiological Week",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

# Add SARS-CoV-2 positivity if available
if ("sc2_pos_total" %in% colnames(df)) {
  plot <- plot +
    geom_line(aes(x = week, y = sc2_pos_total * max(df[[hospitalization_var]], na.rm = TRUE) / 100,
                  color = "SARS-CoV-2 positivity", group = 1), linewidth = 1) +
    scale_color_manual(values = c("SARS-CoV-2 positivity" = sars_cov2_color))
}

# Ensure the "FIGURES" directory exists
output_dir <- "FIGURES"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save plot
file_path <- file.path(output_dir, paste0("SARI_plot_LU_", Sys.Date(), ".png"))
ggsave(file_path, plot, width = 10, height = 6, dpi = 800, bg = "white")

print(paste("Plot saved:", file_path))
