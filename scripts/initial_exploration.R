# Week 1: Initial Data Exploration ====
# Author: Olivia Jezior
# Date: 30/01/26


# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skim)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   
# - What's being measured?
#   the strength of doses, at sites a, b, and c
# - How many observations?
#   205
# - Anything surprising?
#   all the labels are messy
# - Any obvious problems?
# labels, orders, spellings, missing data

mosquito_egg_raw_step1 <- mosquito_egg_raw |> 
  mutate(treatment = case_when(
    treatment == "MEDIUM_DOSE" ~ "Medium_dose",
    treatment == "high_dose" ~ "High_dose",
    treatment == "low_dose" ~ "Low_dose",
    treatment == "control" ~ "Control",
    treatment == "CONTROL" ~ "Control",
    treatment == "LOW_DOSE" ~ "Low_dose",
    treatment == "HIGH_DOSE" ~ "High_dose",
    treatment == "medium_dose" ~ "Medium_dose",
    .default = as.character(treatment)
    
  )
  )
  # check that there is exepected amount of variables.
 # |> distinct(treatment)
  
mosquito_egg_raw_step2 <- mosquito_egg_raw_step1 |> 
  mutate(site = case_when(
    site == "Site B" ~ "Site_B",
    site == "site_a" ~ "Site_A",
    site == "Site-C" ~ "Site_C",
    site == "site_c" ~ "Site_C",
    site == "site_b" ~ "Site_B",
    site == "Site-A" ~ "Site_A",
    site == "Site C" ~ "Site_C",
    site == "Site A" ~ "Site_A",
    site == "Site-B" ~ "Site_B",
    .default = as.character(site)
    
  )
  )
# |> distinct(site)

view(mosquito_egg_raw_step2)


## missing data

library(tidyverse)
mosquito_egg_raw_step2 |> 
  group_by(treatment) |> 
  summarise(mean = mean(eggs_laid))

summary(mosquito_egg_raw_step2)

mosquito_egg_raw_step2 |> 
  group_by(treatment) |> 
  summarise(mean = mean(eggs_hatched))


mosquito_egg_raw_step3 <- mosquito_egg_raw_step2 |> 
  drop_na()

view(mosquito_egg_raw_step3)

