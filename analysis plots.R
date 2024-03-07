library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggridges)
library(vegan)
library(readr)
bob_ross <- read_csv("data/bob_ross_paintings.csv")
view(bob_ross)
abundant_colors <- bob_ross %>%
  group_by(season) %>%
  summarize(mean(num_colors))
abundant_colors
bob_ross_longer <- bob_ross %>%
  pivot_longer(c("Black_Gesso", "Bright_Red", "Burnt_Umber", "Cadmium_Yellow", "Dark_Sienna", "Indian_Red", "Indian_Yellow", "Liquid_Black", "Liquid_Clear", "Midnight_Black", "Phthalo_Blue", "Phthalo_Green", "Prussian_Blue", "Sap_Green", "Titanium_White", "Van_Dyke_Brown", "Yellow_Ochre", "Alizarin_Crimson"), names_to = "Color", values_to = "Used") %>%
  filter(Used == 1) 

#there has to be a better way to do this 
hex_codes <- data.frame(color = c("Alizarin_Crimson", "Black_Gesso", "Bright_Red", "Burnt_Umber", "Cadmium_Yellow", "Dark_Sienna", "Indian_Yellow", "Liquid_Black", "Liquid_Clear", "Midnight_Black", "Phthalo_Blue", "Phthalo_Green", "Prussian_Blue", "Sap_Green", "Titanium_White", "Van_Dyke_Brown", "Yellow_Ochre"), 
                        hex = c('#4E1500', '#000000', '#DB0000', '#8A3324', '#FFEC00', '#5F2E1F', '#FFB800', '#000000', '#FFFFFF', '#000000', '#0C0040', '#102E3C', '#021E44', '#0A3410', '#FFFFFF', '#221B15', '#C79B00'), 
                        hex_adjusted = c('#4E1500', '#828181', '#DB0000', '#8A3324', '#FFEC00', '#5F2E1F', '#FFB800', '#C9C8C8', '#D8F8FD', '#000000', '#3115AD', '#2281AD', '#064AA6', '#187525', '#FFFFFF', '#624428', '#C79B00')
)

hex_assign = c("Alizarin_Crimson" = '#4E1500', "Black_Gesso" = '#828181', "Bright_Red" = '#DB0000', "Burnt_Umber" = '#8A3324', "Cadmium_Yellow" = '#FFEC00', "Dark_Sienna" = '#5F2E1F', "Indian_Yellow" = '#FFB800', "Liquid_Black" = '#C9C8C8', "Liquid_Clear" = '#D8F8FD', "Midnight_Black" = '#000000', "Phthalo_Blue" = '#3115AD', "Phthalo_Green" = '#2281AD', "Prussian_Blue" = '#064AA6', "Sap_Green" = '#187525', "Titanium_White" = '#FFFFFF', "Van_Dyke_Brown" = '#624428', "Yellow_Ochre" = '#C79B00')

color_density <- ggplot(data = bob_ross_longer, aes(x = season, y = Color, fill = Color)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = hex_assign) +
  theme_classic() +
  theme(legend.position = "none")
color_density
ggsave(plot = color_density, filename = paste("color density 1"), device = "jpg")

color_density2 <- ggplot(data = bob_ross_longer, aes(x = season, fill = Color)) +
  geom_density(position= 'fill') +
  scale_fill_manual(values = hex_assign) +
  theme_classic() +
  theme()#legend.position = "none")
color_density2

tidybob <- bob_ross %>%
  select(c(painting_index, painting_title, season, Black_Gesso, Bright_Red, Burnt_Umber, Cadmium_Yellow, Dark_Sienna, Indian_Red, Indian_Yellow, Liquid_Black, Liquid_Clear, Midnight_Black, Phthalo_Blue, Phthalo_Green, Prussian_Blue, Sap_Green, Titanium_White, Van_Dyke_Brown, Yellow_Ochre, Alizarin_Crimson)) %>%
  drop_na() %>%
  pivot_longer(c("Black_Gesso", "Bright_Red", "Burnt_Umber", "Cadmium_Yellow", "Dark_Sienna", "Indian_Red", "Indian_Yellow", "Liquid_Black", "Liquid_Clear", "Midnight_Black", "Phthalo_Blue", "Phthalo_Green", "Prussian_Blue", "Sap_Green", "Titanium_White", "Van_Dyke_Brown", "Yellow_Ochre", "Alizarin_Crimson"), names_to = "color", values_to = "count")
#simpson diversity index
for (season_number in 1:31) {
  # Filter data for each season
  temp_data <- tidybob %>%
    filter(season == season_number) %>%
    group_by(color) %>%
    filter(count > 0) %>%
    summarize(number = n())
  
  # Calculate proportion
  temp_data <- temp_data %>%
    mutate(proportion = number/sum(number))
  
  #Calculate simpson diversity
  simpsondiversity <- diversity(temp_data$number, index = "simpson", equalize.groups = FALSE,
                                MARGIN = 1, base = exp(1))
}

#shannon diversity index
for (season_number in 1:31) {
  # Filter data for each season
  temp_data2 <- tidybob %>%
    filter(season == season_number) %>%
    group_by(color) %>%
    filter(count > 0) %>%
    summarize(number = n())
  
  # Calculate proportion
  temp_data2 <- temp_data2 %>%
    mutate(proportion = number/sum(number))
  
  #Calculate shannon diversity
  shannondiversity <- diversity(temp_data2$number, index = "shannon", equalize.groups = FALSE,
                                MARGIN = 1, base = exp(1))
}

seasonno <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
simpsond <- c(0.8780718, 0.9190012, 0.9171143, 0.9195266, 0.917458, 0.9154412, 0.9187046, 0.9170342, 0.9180612, 0.9208364, 0.918457, 0.9237076, 0.9206671, 0.9240631, 0.9222083, 0.9203609, 0.9223511, 0.9223934, 0.9235672, 0.9208107, 0.927097, 0.9236437, 0.9233942, 0.9257509, 0.9232818, 0.9262543, 0.9280345, 0.9204545, 0.9241835, 0.9253637, 0.9246181)
shannond <- c(2.131576, 2.530038, 2.54985, 2.566073, 2.565492, 2.504565, 2.549228, 2.535804, 2.562325, 2.578084, 2.550194, 2.627699, 2.572248, 2.612864, 2.595449, 2.566761, 2.602236, 2.612732, 2.620094, 2.584851, 2.653685, 2.622077, 2.61255, 2.647941, 2.611549, 2.642795, 2.689755, 2.591084, 2.623845, 2.633915, 2.647335)
diversity_results <- data.frame(seasonno, simpsond, shannond)


diversity_results %>%
  ggplot(aes(x = seasonno, y = simpsond)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Season", y = "Simpson diversity index", title = "Species diversity of colors in Bob Ross paintings over time") +
  geom_line()

diversity_results %>%
  ggplot(aes(x = seasonno, y = shannond)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Season", y = "Shannon diversity index", title = "Species diversity of colors in Bob Ross paintings over time") +
  geom_line()
