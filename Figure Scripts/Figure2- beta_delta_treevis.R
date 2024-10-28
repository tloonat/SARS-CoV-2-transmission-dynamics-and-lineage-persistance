# Load necessary libraries
library(ggtree)
library(readxl)
library(ape)
library(ggplot2)
library(stringr)
library(treeio)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(ggtree)
library(readxl)
library(ape)
library(ggplot2)
library(stringr)
library(treeio)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(readr)

# Load datasets
tree_file <- "/Users/talhahloonat/Downloads/delta_dataset2/delta_import_export_data/delta_annotated_tree_provinces.nexus"
metadata_file <- "/Users/talhahloonat/Downloads/delta_dataset2/attempt2/treetime2/delta_metadata1.tsv"
tree <- read.nexus(tree_file)
metadata <- read_tsv(metadata_file)

# Standardize datasets for downstream use
tree$tip.label <- gsub("[\"']", "", tree$tip.label)
metadata$Date <- as.Date(metadata$Date, format = "%Y-%m-%d")

cat("Top rows of metadata:\n")
print(head(metadata))
cat("\nTop tree tip labels:\n")
print(head(tree$tip.label))

# Define special regions and their specific colors
regions <- c("EasternCape", "Gauteng", "FreeState", "KwaZulu-Natal", "Limpopo", "Mpumalanga",
             "NorthWest", "NorthernCape", "WesternCape")

# Assign a unique color to each special region
special_colors <- c(
  "EasternCape" = "orange",
  "FreeState" = "yellow",
  "Gauteng" = "red",
  "KwaZulu-Natal" = "pink",
  "Limpopo" = "magenta",
  "Mpumalanga" = "brown",
  "NorthWest" = "purple",
  "NorthernCape" = "darkblue",
  "WesternCape" = "lightblue"
)

# Count the number of sequences from each region
region_counts <- metadata %>%
  filter(Region %in% regions) %>%
  group_by(Region) %>%
  summarise(Count = n())

# Create new labels with region names and their corresponding sequence counts
region_labels <- paste(region_counts$Region, " (n = ",region_counts$Count, ")", sep = "")

# Create a base color mapping where regions not in South Africa are dark grey
region_colors <- c(special_colors, setNames(rep("grey", length(unique(metadata$Region)) - length(regions)),
                                            setdiff(unique(metadata$Region), regions)))

# Tree plot code with black outlines and larger points for special regions
p <- ggtree(tree, mrsd = as.Date("2022-07-04"), color = '#5E6871', size = 0.15) %<+% metadata +
  # Set points with black outline and interior colors for regions
  geom_tippoint(aes(fill = Region), color = "black", shape = 21, size = 1) +  # Small default points
  geom_tippoint(aes(subset = (Region %in% regions), fill = Region), color = "black", shape = 21, size = 2.5) + # Larger points for special regions
  theme_tree2() +
  # Custom color scale for the regions with only provinces in the legend
  scale_fill_manual(values = special_colors, 
                    breaks = region_counts$Region,
                    labels = region_labels) +  # Use the new labels with counts
  # Customize the appearance of axes
  theme(axis.line.y = element_line(), axis.text.y = element_text()) +
  coord_flip() +
  scale_x_reverse() +
  # Add axis labels and title
  labs(
    x = "Time",
    y = "Number of Sequences",
    title = "Phylogenetic Reconstruction of SARS-CoV-2 Delta (AY.45) Variant Spread by Region"
  ) +
  # Customize title and axis text appearance
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 6, angle = 90, hjust = 1)
  ) +
  # Include the legend for province colors with sequence counts
  guides(size = "none", fill = guide_legend(title = "Provinces:", override.aes = list(size = 3)))

# Print the plot
print(p)


######################################################################################
######################################################################################
# Load datasets
tree_file <- "/Users/talhahloonat/Downloads/beta_dataset2/beta_import_export_data/beta_annotated_tree_provinces.nexus"
metadata_file <- "/Users/talhahloonat/Downloads/beta_dataset2/treetime2/beta_metadata.tsv"
tree <- read.nexus(tree_file)
metadata <- read_tsv(metadata_file)

# Standardize datasets for downstream use
tree$tip.label <- gsub("[\"']", "", tree$tip.label)
metadata$Date <- as.Date(metadata$Date, format = "%Y-%m-%d")

cat("Top rows of metadata:\n")
print(head(metadata))
cat("\nTop tree tip labels:\n")
print(head(tree$tip.label))

# Define special regions and their specific colors
regions <- c("EasternCape", "Gauteng", "FreeState", "KwaZulu-Natal", "Limpopo", "Mpumalanga",
             "NorthWest", "NorthernCape", "WesternCape")

# Assign a unique color to each special region
special_colors <- c(
  "EasternCape" = "orange",
  "FreeState" = "yellow",
  "Gauteng" = "red",
  "KwaZulu-Natal" = "pink",
  "Limpopo" = "magenta",
  "Mpumalanga" = "brown",
  "NorthWest" = "purple",
  "NorthernCape" = "darkblue",
  "WesternCape" = "lightblue"
)

# Count the number of sequences from each region
region_counts <- metadata %>%
  filter(Region %in% regions) %>%
  group_by(Region) %>%
  summarise(Count = n())

# Create new labels with region names and their corresponding sequence counts
region_labels <- paste(region_counts$Region, " (n = ",region_counts$Count, ")", sep = "")

# Create a base color mapping where regions not in South Africa are dark grey
region_colors <- c(special_colors, setNames(rep("grey", length(unique(metadata$Region)) - length(regions)),
                                            setdiff(unique(metadata$Region), regions)))

# Tree plot code with black outlines and larger points for special regions
p <- ggtree(tree, mrsd = as.Date("2022-03-15"), color = '#5E6871', size = 0.15) %<+% metadata +
  # Set points with black outline and interior colors for regions
  geom_tippoint(aes(fill = Region), color = "black", shape = 21, size = 1) +  # Small default points
  geom_tippoint(aes(subset = (Region %in% regions), fill = Region), color = "black", shape = 21, size = 2.5) + # Larger points for special regions
  theme_tree2() +
  # Custom color scale for the regions with only provinces in the legend
  scale_fill_manual(values = special_colors, 
                    breaks = region_counts$Region,
                    labels = region_labels) +  # Use the new labels with counts
  # Customize the appearance of axes
  theme(axis.line.y = element_line(), axis.text.y = element_text()) +
  coord_flip() +
  scale_x_reverse() +
  # Add axis labels and title
  labs(
    x = "Time",
    y = "Number of Sequences",
    title = "Phylogenetic Reconstruction of SARS-CoV-2 Beta Variant Spread by Region"
  ) +
  # Customize title and axis text appearance
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 6, angle = 90, hjust = 1)
  ) +
  # Include the legend for province colors with sequence counts
  guides(size = "none", fill = guide_legend(title = "Provinces:", override.aes = list(size = 3)))

# Print the plot
print(p)

