###Data cleaning example

library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
setwd("~/Teaching/data_intro_ind_study/Lab 8")


# read the csv
tick_raw <- read_csv("anonymized_output.csv", show_col_types = FALSE)

# look at first few rows
head(tick_raw)

# look at structure
glimpse(tick_raw)

#We can already see lots of NAs


# original column names
names(tick_raw)
#names aren't too bad, not consistent but at least there aren't any spaces, the next line of code will standardize them
# clean them (lowercase, underscores, no spaces)
tick_clean_names <- tick_raw %>%
  clean_names()

# names after cleaning
names(tick_clean_names)




# count how many NA values per column
tick_clean_names %>%
  summarise(across(everything(), ~sum(is.na(.))))

#lots of NAs in the results area, need to explore more

#let's look for spelling issues

# results column
unique(tick_clean_names$results)

#Okay so we have plenty of inconsistencies here, with NAs and Not Tested and No tick potentially meaning the same thing. 
# pathogen names
unique(tick_clean_names$test_for)
#Not testable, NA, Unknown, NONE, and UNKNOWN would be the big red flags here
# tick species
unique(tick_clean_names$species_name)
#Some full species names, some with taxonomic authority designations, generally needs some help
# states (often messy)
unique(tick_clean_names$state)



# example: results column before cleaning
unique(tick_clean_names$results)

# clean results column
tick1 <- tick_clean_names %>%
  mutate(
    results = str_squish(results),  # remove extra spaces
    results = str_to_lower(results), # lowercase everything
    results = case_when(
      results %in% c("positive", "pos", "postive", "postivie") ~ "Positive",
      results %in% c("negative", "neg") ~ "Negative",
      results %in% c("no tick", "not tested", "is", "completed") ~ "Not Tested",
      TRUE ~ results
    )
  )

# results after cleaning
unique(tick1$results)



# before
unique(tick1$species_name)

# clean spacing + a few recodes
tick2 <- tick1 %>%
  mutate(
    species_name = str_squish(species_name),
    species_name = case_when(
      species_name %in% c("Ixodes Species", "Ixodes sp.", "I. species") ~ "Ixodes sp.",
      species_name %in% c("A. Species", "Amblyomma sp.", "Amblyomma Species") ~ "Amblyomma sp.",
      TRUE ~ species_name),
    species_name = recode(
      species_name,
      "Dermacentor andersoni Stiles" = "D. andersoni",
      "I. cookei Packard" = "I. cookei",
      "Ixodes cookei Packard" = "I. cookei",
      "Hyalomma lusitanicum" = "H. lusitanicum",
      "Rhipicephalus sanguineus (Latreille)" = "R. sanguineus",
      "D. albipictus (Packard)" = "D. albipictus",
      "Dermacentor occidentalis Marx" = "D. occidentalis",
      "Ixodes ricinus (Linnaeus)" = "I. ricinus",
      "Ixodes muris" = "I. muris",
      "I. ricinus (Linnaeus)" = "I. ricinus"
    )
  )

# after
unique(tick2$species_name)



# how many duplicate rows?
sum(duplicated(tick2))

# look at repeated sample IDs
tick2 %>%
  count(chppm_sample_no, sort = TRUE) %>%
  filter(n > 1)



# before parsing
head(tick3$start_date)

# convert to date
tick3 <- tick3 %>%
  mutate(start_date = ymd(start_date))

# after parsing
head(tick3$start_date)

summary(tick3$start_date)


nrow(tick3)
tick_clean <- tick3 %>%
  filter(
    !is.na(species_name),
    !species_name %in% c("Not a tick", "No tick in vial", "Not Identifiable", "Amblyomma sp.", "Ixodes sp."),
    results %in% c("Positive", "Negative")
  )

# how many rows left?
nrow(tick_clean)
#~3,500 rows removed because ticks not identified/identifiable, not tested for some reason


# counts by species
tick_clean %>%
  count(species_name, sort = TRUE)

# counts by pathogen
tick_clean %>%
  count(test_for, sort = TRUE)

# counts by state
tick_clean %>%
  count(state, sort = TRUE)



write_csv(tick_clean, "tick_testing_clean.csv")


# first: count positives per species x pathogen
pos_counts <- tick_clean %>%
  filter(results == "Positive") %>%
  distinct(chppm_sample_no, species_name, test_for) %>%
  count(species_name, test_for, name = "n_positive")

# second: count total ticks tested per species x pathogen
total_counts <- tick_clean %>%
  distinct(chppm_sample_no, species_name, test_for) %>%
  count(species_name, test_for, name = "n_tested")

# join + calculate prevalence
prev_species_pathogen <- total_counts %>%
  left_join(pos_counts, by = c("species_name", "test_for")) %>%
  mutate(
    n_positive = replace_na(n_positive, 0),
    prevalence = n_positive / n_tested
  ) %>%
  arrange(species_name, desc(prevalence))

# look at results
print(prev_species_pathogen)


focal_species <- c("A. americanum", "I. scapularis", "D. variabilis")

prev_species_pathogen %>%
  filter(species_name %in% focal_species)

tick_clean <- tick_clean %>%
  mutate(
    year = year(start_date)
  )

# positives
pos_time <- tick_clean %>%
  filter(species_name %in% focal_species) %>%
  filter(results == "Positive") %>%
  distinct(chppm_sample_no, species_name, test_for, year) %>%
  count(species_name, test_for, year, name = "n_positive")

# totals
total_time <- tick_clean %>%
  filter(species_name %in% focal_species) %>%
  distinct(chppm_sample_no, species_name, test_for, year) %>%
  count(species_name, test_for, year, name = "n_tested")

# combine
prev_time <- total_time %>%
  left_join(pos_time, by = c("species_name", "test_for", "year")) %>%
  mutate(
    n_positive = replace_na(n_positive, 0),
    prevalence = n_positive / n_tested
  ) %>%
  arrange(species_name, test_for, year)

prev_time

library(ggplot2)

ggplot(prev_time, aes(x = year, y = prevalence)) +
  geom_line() +
  facet_grid(species_name ~ test_for) +
  labs(
    title = "Pathogen prevalence through time",
    y = "Prevalence",
    x = "Year"
  )
