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

# Reader function due to diverging data types within columns
convert_positive_date <- function(x) {
  x <- trimws(as.character(x))
  
  resultat <- as.Date(
    rep(NA_real_, length(x)),
    origin = "1970-01-01"
  )
  
  # Excel-datonumre som 40312
  er_excelnummer <- grepl(
    "^\\d+(\\.\\d+)?$",
    x
  )
  
  resultat[er_excelnummer] <- as.Date(
    as.numeric(x[er_excelnummer]),
    origin = "1899-12-30"
  )
  
  # Tekstdatoer med eller uden klokkeslæt
  er_tekstdato <- grepl(
    "^\\d{4}-\\d{2}-\\d{2}",
    x
  )
  
  resultat[er_tekstdato] <- as.Date(
    substr(x[er_tekstdato], 1, 10),
    format = "%Y-%m-%d"
  )
  
  resultat
}


# Excel file path
filsti <- 
  "Furesø_Kommune_2019_26.xlsx"

# Define colnames
kolonnenavne <- 
  readxl::read_excel(
  filsti,
  sheet = "Fuld data",
  n_max = 0
) |>
  names()

kolonnetyper <- rep("guess", length(kolonnenavne)) # Col types

kolonnetyper[kolonnenavne == "positive_date"] <- "text" # Col types

# Load df
df_raw <- readxl::read_excel(
  filsti,
  sheet = "Fuld data",
  col_types = kolonnetyper,
  guess_max = 100000
)

# Use convertion function
df_full <- 
  df_raw |>
  mutate(
    positive_date_raw = positive_date,
    positive_date = convert_positive_date(positive_date),
    positive_year = year(positive_date)
  )


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
    
    # Aktiv i år 0 eller 1 efter birth_year # TODO: OUT
    dst_aktiv_birth = as.integer(
      any(
        ansatte_numeric >= 0.5 &
          time_since_birth %in% c(0, 1),
        na.rm = TRUE
      )
    ),
    
    # Aldrig over 0,5 ansatte
    inaktiv = case_when(
      all(is.na(ansatte_numeric)) ~ 1L,
      any(ansatte_numeric >= 0.5, na.rm = TRUE) ~ 0L,
      TRUE ~ 1L
    )
    ) |>
  ungroup()

#### SANDBOX ENDS ####

# Export
# Write RDS to export for viz step
df_classified |> 
  write_rds("df_classified.rds")

#### ONE PERIOD VERSION END ####

### CVR numbers with more than one p-number analysis ###


# All CVR numbers with more than one p-number
pnumre_pr_cvr <- df_classified |>
  filter(
    !is.na(`CVR Number`),
    !is.na(`P-number`)
  ) |>
  group_by(`CVR Number`) |>
  summarise(
    antal_p_numre = n_distinct(`P-number`),
    
    antal_company_names = n_distinct(
      `Company Name`,
      na.rm = TRUE
    ),
    
    company_names = paste(
      sort(unique(`Company Name`[!is.na(`Company Name`)])),
      collapse = " | "
    ),
    
    .groups = "drop"
  ) |>
  arrange(desc(antal_p_numre))

pnumre_pr_cvr |> View()

# CVR numbers with more than one p-number within the iværksætter conditions #


pnumre_pr_cvr_year <- df_classified |>
  filter(
    dst_aktiv_birth == 1,
    !is.na(`CVR Number`),
    !is.na(`P-number`),
    !is.na(Year),
    birth_year %in% 2019:2026
  ) |>
  group_by(
    `CVR Number`,
    Year
  ) |>
  summarise(
    antal_p_numre = n_distinct(`P-number`),
    
    antal_company_names = n_distinct(
      `Company Name`,
      na.rm = TRUE
    ),
    
    company_names = paste(
      sort(unique(`Company Name`[!is.na(`Company Name`)])),
      collapse = " | "
    ),
    
    .groups = "drop"
  ) |>
  arrange(
    Year,
    desc(antal_p_numre)
  ) |> 
  filter(antal_p_numre != 1)

CVR_numbers_with_more_than_one_pnumber <-
  pnumre_pr_cvr_year |> 
  filter(antal_p_numre != 1) |> 
  pull(`CVR Number`) |> 
  unique()

df_multiple_p <-
  df_classified |> 
  filter(`CVR Number` %in% CVR_numbers_with_more_than_one_pnumber) |> 
  select(`P-number`, `CVR Number`, Year, `Company Name`, positive_value)


pnumre_pr_cvr |> View()

