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

# Select individual columns by name, separate them a comma
crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

# Select a range of columns using :
crabs_subset_2 <- pie_crab %>% select(site:air_temp)

# Select a range and an individual column
crabs_subset_3 <- pie_crab %>% select(date:water_temp, name)

pie_crab %>% select(name, water_temp, size)

# --- Mutate! ---#

# Use dplyr:mutate() to add or update a column, while keeping all existing columns

pie_crab_updated <- pie_crab %>%
  mutate(size_cm = size/10)

# What happens if I use mutate function to add a new column containing the mean of the size column

pie_crab_mean_updated <- pie_crab %>%
  mutate(mean = mean(pie_crab$size))


# replace a column
crabs_df_broken <-pie_crab %>%
  mutate(name = "Teddy is awesome")


# group_by function + summarize
# Group_by site and then find mean by site

mean_size_by_site <- pie_crab %>%
  group_by(site) %>%
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))

# group_by + mutate

pie_crab_group_mutate <- pie_crab %>%
  group_by(site) %>%
  mutate(mean_size = mean(size, na.rm = TRUE))

# group_by multiple columns + summarize

penguins %>%
  group_by(species,island, year) %>%
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE))

# Use dyplr::case_when() to write if-else statements more easily
crabs_bin <- pie_crab %>%
  mutate(size_binned = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))

sites_binned <-pie_crab %>%
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    # catch all statement
    .default = "High"
  ))
