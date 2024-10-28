# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Load your datasets
beta_data <- read.csv("~/Downloads/beta_dataset2/beta_import_export_data/beta_tree_events_provinces1.csv")
delta_data <- read.csv("~/Downloads/delta_dataset2/delta_import_export_data/delta_tree_events_provinces1.csv")

# Define South African provinces for filtering
sa_provinces <- c("SouthAfrica - Gauteng", "SouthAfrica - FreeState", "SouthAfrica - WesternCape", 
                  "SouthAfrica - KwaZulu-Natal", "SouthAfrica - EasternCape", "SouthAfrica - Limpopo", 
                  "SouthAfrica - Mpumalanga", "SouthAfrica - NorthWest", "SouthAfrica - NorthernCape")

# Process data function
process_data <- function(data, variant) {
  filtered_data <- data %>%
    filter(Origin %in% sa_provinces & Destination %in% sa_provinces) %>%
    mutate(Year = floor(EventTime),
           MonthFraction = EventTime - Year,
           Month = round(MonthFraction * 12),
           Month = ifelse(Month == 0, 1, Month),  # Handle edge case for January
           Date = as.Date(paste(Year, Month, "01", sep = "-"), "%Y-%m-%d"),
           Variant = variant) %>%
    filter(Date >= as.Date("2020-06-01")) %>%
    group_by(Date, Origin, Destination, Variant) %>%
    summarise(Total_Count = n(), .groups = 'drop')
  
  return(filtered_data)
}

# Process both datasets
beta_events <- process_data(beta_data, "Beta")
delta_events <- process_data(delta_data, "Delta (AY.45)")

# Combine datasets
combined_events <- bind_rows(beta_events, delta_events)
combined_events <- combined_events %>%
  mutate(Month = as.Date(paste0(Date, "-01"), format = "%b %Y-%d"))

combined_events <- combined_events %>%
  mutate(Month = floor_date(Date, "month"))

# Clean province names by removing the prefix "SouthAfrica - "
combined_events <- combined_events %>%
  mutate(Origin = gsub("SouthAfrica - ", "", Origin),
         Destination = gsub("SouthAfrica - ", "", Destination))

# Calculate frequency of events for sorting
origin_freq <- combined_events %>%
  group_by(Origin) %>%
  summarise(Total_Origin_Count = sum(Total_Count), .groups = 'drop') %>%
  arrange(desc(Total_Origin_Count))

destination_freq <- combined_events %>%
  group_by(Destination) %>%
  summarise(Total_Destination_Count = sum(Total_Count), .groups = 'drop') %>%
  arrange(desc(Total_Destination_Count))

# Convert Origin and Destination to factors ordered by frequency
combined_events <- combined_events %>%
  mutate(
    Origin = factor(Origin, levels = origin_freq$Origin),
    Destination = factor(Destination, levels = destination_freq$Destination)
  )

combined_events$Month <- factor(format(combined_events$Month, "%b %Y"), levels = unique(format(combined_events$Month, "%b %Y")))


# Create the heatmap with origins on the y-axis and destinations on the x-axis
prov_heatmap <- ggplot(combined_events, aes(x = Destination, y = Origin, fill = Total_Count)) +
  geom_tile(color = "black") +  # Add black borders around each tile
  scale_fill_gradient(
    trans = "log",
    low = "lightblue",
    high = "darkblue",
    name = "Count",
    breaks = c(1, 15, 30, 60, 100, 200),
    labels = c("1", "15", "30", "60", "100", "200")
  ) +
  labs(
    title = "Frequency of SARS-CoV-2 Events in South Africa by Province",
    x = "Destination Province",
    y = "Origin Province"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 5.5),  # Adjust font size and angle for x-axis
    axis.text.y = element_text(size = 6),                         # Adjust font size for y-axis
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Black border around the panel
    strip.background = element_blank(),
    strip.text = element_text(size = 10)                          # Size of the strip text
  ) +
  facet_grid(
    Variant ~ Month,  # Create separate rows for each variant
    scales = "free_y",  # Keeps the y-axis consistent across all plots
    switch = "y"        # Move the facet strip to the left y-axis (to have the dates above)
  ) +
  theme(
    strip.placement = "outside",  # Ensures the facet strips are outside the plot area
    strip.text.x = element_text(angle = 0, size = 7)  # Horizontal text for dates
  )


# Display the heatmap
print(prov_heatmap)

