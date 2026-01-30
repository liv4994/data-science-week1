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