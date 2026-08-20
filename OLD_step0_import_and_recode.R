### Created by: SKH ###
### Last updated: 260820 ###

# Setup #

# Set wd if necessary

# Unhash to install required packages
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("conflicted")
# install.packages("lubridate")

# Load libs
library(dplyr)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(lubridate)


# Load data
df_full <- readxl::read_excel("Furesø_Kommune_2019_26.xlsx", sheet = "Fuld data")


# Recoding variables for downstream purposes, creation of new variables
df_new <-
  df_full |> 
  mutate(birth_year = year(`Birth Date`), # Mutate necessary vars to filter post 2019
         post_2019 = if_else(birth_year >= 2019, "post", "pre")) |> # Extract year only
  mutate(
    ansatte_numeric = as.numeric( 
      na_if(as.character(ansatte), "Missing")) # Make ansatte workable in R, numeric for cutoffs
    )


# Filling the positive_year with summarize to enable yearly calculations across each yearly observation
df_filled <-
  df_new |> 
  mutate(positive_year = year(positive_date)) |> # Need positive var in year format
  group_by(`P-number`) |> # Group by p-number to fill p-number wise
  fill(positive_year, .direction = "downup") |>  # This gives all observations of the p-number the same positive value year
  ungroup() |> 
  mutate(time_since_positive = Year-positive_year) # |>  # Calc the difference between positive var and current year

# Dynamic version (yearly)

# DST vAR has to created after the fill as the filled time_since_positive is used
df_recode_done <-  
  df_filled |> 
  mutate( # NB! This mutate creates the variable for being dst active - conditions are changed here 
    dst_aktiv = if_else(
      ansatte_numeric >= 0.5 & time_since_positive %in% 0:1,
      1,
      0,
      missing = 0 # Needed to handle the character "Missing" which is import legacy
    )
  ) |> 
  mutate(classification = case_when( # NB! This is where the classification variable is created
    dst_aktiv == 1 ~ "dst aktiv",
    ansatte_numeric >= 0.5 ~ "iris aktiv", # This is the same as the 'Økonomisk Aktiv' variable
    ansatte_numeric < 0.5 | is.na(ansatte_numeric) ~ "inaktiv"
  )
  )


## NB FILTERS APPLIED HERE ##
df_filtered <- 
  df_recode_done |> 
  #  filter(positive_value == "new_company") |>  # Tweak the definition of new company here
  filter(post_2019 == "post") 




## THESE ARE SANITY CHECKS ##
# Check
df_filtered |>
  filter(classification == "dst aktiv") |> 
  select('P-number', Year, positive_year, time_since_positive, dst_aktiv, birth_year, ansatte, ansatte_numeric)

# Check the unique amount of p-numbers fulfilling the dst condition
df_filtered |>
  filter(classification == "dst aktiv") |> 
  summarise(
    n_unique_pnumbers = n_distinct(`P-number`, na.rm = TRUE)
  )



# Static version (period based)



