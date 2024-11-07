# Load  libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(janitor)
library(stringr)

# Load Data
data = read_excel("Project/IRENA_Stats_extract_2024 H2.xlsx", sheet = 5)

# View Data
head(data)

# Clean the data
# Change names 
data = data |>
  clean_names()

head(data)

# Function for string cleaning 
clean_string <- function(x) {
  x |>
    str_to_lower() |>                      
    str_replace_all("[[:punct:]]", "") |>  
    str_replace_all("\\b(the|a|and|of|in|for)\\b", "") |>  
    str_trim() |>                          
    str_squish()                      
}
cleaned_data = data |>
  mutate(across(where(is.character), clean_string))|>
  distinct()

head(cleaned_data)

# Filter years 2014-2023 and select relevant columns
cleaned_data = cleaned_data |>
  filter(year >= 2014 & year <= 2023 & producer_type != "all types") |>
  select(region, country, year, re_or_non_re, electricity_installed_capacity_mw)
head(cleaned_data)

# Split re_or_non_re to re and nre and remove previous column
cleaned_data = cleaned_data |>
  mutate(
    re = if_else(re_or_non_re == "total renewable", "Yes", "No"),
    nre = if_else(re_or_non_re == "total nonrenewable", "Yes", "No"),
  )|>
  select(-re_or_non_re)
head(cleaned_data)

# Sum 'electricity_installed_capacity_mw' based on 're_or_non_re'  
# creating separate columns for non-renewable and renewable
capacity_data = cleaned_data |>
  group_by(country, year)|>
  summarize(total_capacity = sum(electricity_installed_capacity_mw, na.rm = TRUE))
head(capacity_data)

re_data = cleaned_data |>
  group_by(country, year)|>
  filter(re == "Yes") |>
  summarize(total_re_capacity = sum(electricity_installed_capacity_mw, na.rm = TRUE))
head(re_data)

nre_data = cleaned_data |>
  group_by(country, year)|>
  filter(nre == "Yes") |>
  summarize(total_nre_capacity = sum(electricity_installed_capacity_mw, na.rm = TRUE))
head(re_data)

# Merge the data
merged_data = cleaned_data |>
  full_join(re_data, by = c("country", "year"))|>
  full_join(nre_data, by = c("country", "year"))|>
  full_join(capacity_data, by = c("country", "year"))|>
  select(-re)|>
  select(-nre)|>
  select(-electricity_installed_capacity_mw)
head(merged_data) 


# Create percentage change in renewable energy column
percentage_data = merged_data |>
  mutate(re_capacity_percent = total_re_capacity/total_capacity * 100)|>
  mutate(re_percent_change = re_capacity_percent - lag(re_capacity_percent))
head(percentage_data)

# Delete 2014 because needed 2013 for lagged data, so does not have RE change
percentage_data = percentage_data |>
  filter(year != 2014)
head(percentage_data)

# Add a primary key called id_number
percentage_data <- percentage_data |>
  mutate(id_number = row_number()) |>
  relocate(id_number, .before = country)

head(percentage_data)

# Write the cleaned data to a new CSV
write_csv(percentage_data, "Project/renewable_energy_data.csv")
