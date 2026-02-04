penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))

library(tidyverse)
str_trim(" Adelie Penguin (Pygoscelis adeliae) ")
str_trim("  Adelie Penguin (Pygoscelis adeliae)  ", side = "left")
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")
str_c("Adelie", "Penguin", sep = "_")

penguins_clean_names |>  
  distinct(sex)

# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )
# use mutate and if_else
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(sex = if_else(
    sex == "MALE", "Male", "Female"
  )
  )

# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = stringr::word(species, 1)
  ) |> 
  mutate(sex = stringr::str_to_title(sex))

penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()"
  ) 

str_detect("Genus specificus", "Genus")
# 3 possible names in species column
penguins_clean_names |> distinct(species)

penguins_clean_names |>
  filter(str_detect(species, "papua")) |>
  select(species)

# remove match for Genus (followed by a whitespace)
str_remove("Genus specificus", pattern = "Genus ")

penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

# CLEANING DUPLICATES

library(tidyverse)
# check for whole duplicate 
# rows in the data
penguins_clean_names |> 
  filter(duplicated(across(everything())))
sum() 

penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))

#count unique entries
penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )

#MISSING DATA

library(tidyverse)
penguins_clean_names |> 
  group_by(species) |> 
  summarise(mean = mean(body_mass_g))

summary(penguins_clean_names)

penguins_clean_names |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15_n_o_oo, delta_13_c_o_oo,comments,
         everything()) # reorder columns

#drop NA on everything 
penguins_clean_names |> 
  drop_na()

#checking dates
library(lubridate)
library(tidyverse)

date("2017-10-11T14:02:00")
dmy("11 October 2020")
mdy("10/11/2020")

df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)

df |> 
  mutate(
    date = as_date(date)
  )

df |> 
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

year("2017-11-28T14:02:00")
month("2017-11-28T14:02:00")
day("2017-11-28T14:02:00")

library(janitor)

excel_numeric_to_date(42370)

#calculations with dates
penguins_clean_names |> 
  summarise(min_date=min(date_egg),
            max_date=max(date_egg))
penguins_clean_names <- penguins_clean_names |> 
  mutate(year = lubridate::year(date_egg))

# return records after 2008
plants |>
  filter(date_egg >= ymd("2008-01-01"))
