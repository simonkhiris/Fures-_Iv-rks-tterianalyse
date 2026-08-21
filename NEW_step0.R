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

# Define start year of period and kommune here before you run script! #
# Start year
start_year <- 2019
kommune_name <- "FURESØ"

# Load data
df_full <- readxl::read_excel("Furesø_Kommune_2019_26.xlsx", sheet = "Fuld data")

# Recoding variables for downstream purposes, creation of new variables
df_new <-
  df_full |> 
  mutate(birth_year = year(`Birth Date`)) |>  # Mutate necessary vars to filter post 2019
  mutate(
    ansatte_numeric = as.numeric( 
      na_if(as.character(ansatte), "Missing")) # Make ansatte workable in R, numeric for cutoffs
  )


##### ONE PERIOD VERSION #####
# Filling the positive_year with summarize to enable yearly calculations across each yearly observation
df_filled <-
  df_new |> 
  mutate(positive_year = year(positive_date)) |> # Need positive var in year format
  group_by(`P-number`) |> # Group by p-number to fill p-number wise
  fill(positive_year, .direction = "downup") |>  # This gives all observations of the p-number the same positive value year
  ungroup() |> 
  mutate(time_since_positive = Year-positive_year) |>  # Calc the difference between positive var and current year
  mutate(
    time_since_birth = Year - birth_year # Calculate a version of time since birth if that is what we want to use
  )

# Calculate the classifying variable
df_classified <- 
  df_filled |>
  group_by(`P-number`) |>
  mutate(
    # Har på noget tidspunkt over 0,5 ansatte
    iris_aktiv = as.integer(
      any(ansatte_numeric >= 0.5, na.rm = TRUE)
    ),
    
    # Aktiv i år 0 eller 1 efter positive_year
    dst_aktiv_positive = as.integer(
      any(
        ansatte_numeric >= 0.5 &
          time_since_positive %in% c(0, 1),
        na.rm = TRUE
      )
    ),
    
    # Aktiv i år 0 eller 1 efter birth_year
    dst_aktiv_birth = as.integer(
      any(
        ansatte_numeric >= 0.5 &
          time_since_birth %in% c(0, 1),
        na.rm = TRUE
      )
    ),
    
    # Aldrig over 0,5 ansatte
    inaktiv = as.integer(ansatte_numeric < 0.5)
  ) |>
  ungroup()


# P-numre sensecheck
df_sand <-
  df_classified |> 
  filter()

pnumre_uden_positive_year <- df_classified |>
  group_by(`P-number`) |>
  filter(all(is.na(positive_year))) |>
  ungroup()

pnumre_uden_positive_date <- df_classified |>
  filter(!is.na(`P-number`)) |>
  group_by(`P-number`) |>
  filter(all(is.na(positive_date))) |>
  ungroup()

## TODO: Slet

## NB FILTERS APPLIED HERE ## TODO: Move this 'till end
# TODO: Make dynamic
# df_filtered <- 
#   df_classified |> 
  # filter(birth_year >= start_year) |>
  # filter(`Kommune Name` == kommune_name)
# 
# ## NB FILTERS APPLIED HERE 
# df_filtered_sand <-
#   df_classified |>
#   filter(positive_year >= start_year) |>
#   filter(`Kommune Name` == kommune_name)

#### SANDBOX ENDS ####

# Export
# Write RDS to export for viz step
df_classified |> 
  write_rds("df_classified.rds")

#### ONE PERIOD VERSION END ####

### Filter exercises to get positive ###



