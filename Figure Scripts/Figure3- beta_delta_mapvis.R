# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(RColorBrewer)
library(readxl)
library(rworldmap)
library(cowplot)
library(ggtree)
library(rio)
library(zoo)
library(sp)
library(dplyr)
library(data.table)
library(ggthemes)
library(paletteer)
library(ggsci)
library(tidyr) 
library(paletteer) 
################################################################################
#Delta province map
# Read the CSV data
Beta_introductions_provinces <- read.csv("/Users/talhahloonat/Downloads/delta_dataset2/delta_import_export_data/delta_tree_events_provinces1.csv")  
#Beta_introductions_provinces <- read.csv("/Users/talhahloonat/Downloads/beta_dataset2/beta_import_export_data/beta_tree_events_provinces1.csv")


# Function to convert decimal year to date
decimal2Date <- function(decimal_year) {   
  year <- floor(decimal_year)   
  remainder <- decimal_year - year   
  start_of_year <- as.Date(paste0(year, "-01-01"))   
  days_in_year <- ifelse(leap_year(year), 366, 365)   
  date <- start_of_year + round(remainder * days_in_year)   
  return(date) 
}

# Define province coordinates
province_coords <- data.frame(   
  province = c("SouthAfrica - Gauteng", "SouthAfrica - FreeState", "SouthAfrica - WesternCape",                
               "SouthAfrica - KwaZulu-Natal", "SouthAfrica - EasternCape", "SouthAfrica - Limpopo",                
               "SouthAfrica - Mpumalanga", "SouthAfrica - NorthWest", "SouthAfrica - NorthernCape"),   
  lat = c(-26.270760, -28.454110, -33.924870, -29.858680, -33.046378, -23.401294,           
          -25.565588, -26.663861, -29.728531),   
  long = c(28.112268, 26.796784, 18.424055, 31.021840, 27.854586, 29.417932,            
           30.527219, 25.938503, 20.749399) 
)  

# Convert event times to dates
Beta_introductions_provinces$date <- decimal2Date(Beta_introductions_provinces$EventTime)  

# Get lat/long for origins and destinations
province2lat <- function(province) {   
  if (province %in% province_coords$province) {     
    return(province_coords$lat[province_coords$province == province])   
  } else {     
    return(NA)   
  } 
}  

province2long <- function(province) {   
  if (province %in% province_coords$province) {     
    return(province_coords$long[province_coords$province == province])   
  } else {     
    return(NA)   
  } 
}  

# Apply lat/long functions
Beta_introductions_provinces$origin_lat <- sapply(Beta_introductions_provinces$Origin, province2lat) 
Beta_introductions_provinces$origin_long <- sapply(Beta_introductions_provinces$Origin, province2long) 
Beta_introductions_provinces$destination_lat <- sapply(Beta_introductions_provinces$Destination, province2lat) 
Beta_introductions_provinces$destination_long <- sapply(Beta_introductions_provinces$Destination, province2long)  

# Filter out non-South African origins/destinations
Beta_introductions_filtered <- Beta_introductions_provinces %>%   
  filter(!is.na(origin_lat) & !is.na(destination_lat))  

# Count transitions
transition_counts <- Beta_introductions_filtered %>%   
  mutate(transition_type = ifelse(Origin %in% province_coords$province, "Import", "Export")) %>%   
  group_by(Origin, Destination, origin_lat, origin_long, destination_lat, destination_long, date, transition_type) %>%   
  summarise(count = n(), .groups = 'drop')  

# Generate a pair key
generate_pair_key <- function(Origin, Destination) {   
  pair <- paste(sort(c(Origin, Destination)), collapse = "_")   
  return(pair) 
}  

transition_counts <- transition_counts %>%   
  mutate(province_pair = mapply(generate_pair_key, Origin, Destination))  

# Total transitions per province
total_transitions_per_province <- Beta_introductions_filtered %>%   
  pivot_longer(cols = c(Origin, Destination), names_to = "type", values_to = "province") %>%   
  group_by(province) %>%   
  summarise(total_transitions = n(), .groups = 'drop') %>%   
  left_join(province_coords, by = c("province" = "province")) %>%   
  mutate(size = total_transitions)  

# Set colors for specific provinces and grey for others
highlight_provinces <- c("SouthAfrica - Gauteng", "SouthAfrica - WesternCape",                           
                         "SouthAfrica - KwaZulu-Natal", "SouthAfrica - EasternCape")  

# Create a mapping of provinces to colors
color_mapping <- data.frame(   
  province = province_coords$province,   
  color = ifelse(province_coords$province %in% highlight_provinces,                   
                 paletteer_d("ggthemes::Classic_Cyclic")[1:length(highlight_provinces)],                   
                 "grey")  # Set grey for other provinces 
) 
# Create a mapping of provinces to colors
#color_mapping <- data.frame(
 # province = province_coords$province,
  #color = ifelse(province_coords$province %in% highlight_provinces, 
   #              paletteer_d("ggthemes::Jewel_Bright")[1:length(highlight_provinces)], 
    #             "grey")  # Set grey for other provinces
#)

# Merge color mapping with transition counts
transition_counts <- transition_counts %>%   
  left_join(color_mapping, by = c("Origin" = "province"))  

# Ensure transitions for non-highlighted provinces are grey
transition_counts <- transition_counts %>%   
  mutate(color = ifelse(Origin %in% highlight_provinces, color, "grey")) 

# Create a new column for the legend
transition_counts <- transition_counts %>%
  mutate(legend_label = ifelse(Origin %in% highlight_provinces, Origin, "Other provinces"))

# Get South Africa provinces for mapping
south_africa_provinces <- ne_states(country = "South Africa", returnclass = "sf")  

# Create the map with transitions
beta_map_all <- ggplot() +    
  theme_minimal() +    
  geom_sf(data = south_africa_provinces, fill = 'grey99', color = 'black') +  # Province borders   
  geom_curve(data = subset(transition_counts, !(origin_long == destination_long & origin_lat == destination_lat)),  # Filter identical points              
             aes(x = as.double(origin_long),                  
                 y = as.double(origin_lat),                  
                 xend = as.double(destination_long),                  
                 yend = as.double(destination_lat),                  
                 color = legend_label,                  
                 linewidth = count),              
             alpha = 0.7) +    
  geom_point(data = total_transitions_per_province,              
             aes(x = long, y = lat, size = size),              
             color = 'black', shape = 21, fill = "white") +    
  scale_color_manual(name = "Provinces:", 
                     values = c(setNames(color_mapping$color[color_mapping$province %in% highlight_provinces], 
                                         color_mapping$province[color_mapping$province %in% highlight_provinces]), 
                                "Other provinces" = "grey"),
                     breaks = c(highlight_provinces, "Other provinces"),
                     labels = c(highlight_provinces, "Other provinces")) +
  scale_size_continuous(range = c(1, 15), name = 'Total transitions', guide = "legend") +    
  coord_sf(xlim = c(16, 33), ylim = c(-35, -22)) +    
  geom_text(data = province_coords,             
            aes(x = long, y = lat, label = gsub("SouthAfrica - ", "", province)),             
            size = 4, hjust = 1.2, vjust = 1.2) +    
  ggtitle("Beta Transmission Events Between South African Provinces") +    
  xlab("Longitude") +    
  ylab("Latitude") +    
  theme(legend.position = "right")  

# Display the map
print(beta_map_all)