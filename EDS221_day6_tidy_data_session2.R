# --- Section 1. Filter --- #

library(palmerpenguins)
library(tidyverse)
library(lterdatasampler)


# Look for an exact match: ==
penguins_biscoe <- penguins %>%  filter(island == "Biscoe")

penguins_2007 <- penguins %>% filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie" & island == "Togersen")
# Alternative: penguins %>% filter(species == "Adelie", island == "Togersen")

gentoos_2008 <- penguins %>% filter(species == "Gentoo" & year == 2008)

# Observations where the island == DREAM OR year == 2009

penguins_dream_2009 <- penguins %>%  filter(island == "Dream" | year == 2009)

# relationship between a water temperature vs a crab size
pie_crab_plot <- ggplot (data = pie_crab, aes(x = water_temp, y = size)) +
  geom_point(color = "red")+
  theme_minimal()

# Keep observations for sites NIB, ZI, DB, JC
# Use %in% operator
pie_crab_sites <- pie_crab %>% filter(site %in% c("NIB", "ZI", "DB", "JC"))

pie_crab_sites_plot <- ggplot (data = pie_crab_sites, aes(x = water_temp, y = size, color = site)) +
  geom_point()+
  theme_minimal()

pie_crab_sites_plot

# Alternative: create a vector to store the filter values
selected_sites <- c("CC", "BB", "PIE")
pie_crab_sites_2 <- pie_crab %>% filter(site %in% selected_sites)


# Create a subset using %in% operator that inclues sites PIE, ZI, NIB, BB, CC
selected_sites_3 <- c("PIE", "ZI", "NIB", "BB", "CC")
pie_crab_sites_3 <- pie_crab %>% filter(site %in% selected_sites_3)
pie_crab_sites_3

# Excluding filter statements
# != (asks is this NOT equal to value ABC)
exclude_zi <- pie_crab %>% filter(site != "ZI")

# exclude multiple values
exclude_multiple <- pie_crab %>% filter(!site %in% c("NIB", "BB", "CC"))

# Create a subset from pie_crab that only includes observations from NIB, CC, ZI, for crabs with carapace > 13
exclude_size_13 <- pie_crab %>% filter(site %in% c("NIB", "CC", "ZI") & size >13)

# --- Selecting columns --- #


