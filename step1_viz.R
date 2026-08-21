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
# install.packages("ggplot2")

# Load libs
library(dplyr)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(lubridate)
library(ggplot2)

# load data 

df_classified <-
  read_rds("df_classified.rds")


# Tabel på CVR-niveau, opdelt efter positive_value DST
table_dst_positive_cvr <- 
  df_classified |>
  filter(
    positive_value == "new_company",
    dst_aktiv_positive == 1,
  ) |>
  mutate(
    positive_value = as.factor(positive_value)
  ) |>
  distinct(
    positive_year,
    `CVR Number`,
    positive_value
  ) |>
  count(
    positive_year,
    positive_value,
    name = "antal_cvr_numre"
  ) |>
  arrange(
    positive_value,
    positive_year
  )

table_dst_positive_cvr

# Tabel på CVR-niveau, opdelt efter positive_value IRIS aktiv
table_iris_positive_cvr <- 
  df_classified |>
  filter(
    positive_value == "new_company",
    iris_aktiv == 1,
  ) |>
  mutate(
    positive_value = as.factor(positive_value)
  ) |>
  distinct(
    positive_year,
    `CVR Number`,
    positive_value
  ) |>
  count(
    positive_year,
    positive_value,
    name = "antal_cvr_numre"
  ) |>
  arrange(
    positive_value,
    positive_year
  )

table_iris_positive_cvr




# Inaktiv table #

# Status for hvert CVR-nummer baseret på alle tilhørende P-numre
cvr_aktiv_status <- df_classified |>
  filter(positive_value == "new_company") |> 
  filter(
  ) |>
  group_by(`CVR Number`) |>
  summarise(
    antal_p_numre = n_distinct(`P-number`),
    har_aktivt_pnummer = any(
      iris_aktiv == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cvr_uden_aktivt_pnummer <- cvr_aktiv_status |>
  filter(!har_aktivt_pnummer)

nrow(cvr_uden_aktivt_pnummer)

# Table for CVR's that do not have an active p-number
table_cvr_uden_aktivt_pnummer <-
  df_classified |>
  filter(
    positive_value == "new_company"
  ) |>
  distinct(
    positive_year,
    `CVR Number`
  ) |>
  inner_join(
    cvr_aktiv_status,
    by = "CVR Number"
  ) |>
  filter(!har_aktivt_pnummer) |>
  count(
    positive_year,
    name = "antal_cvr_uden_aktivt_pnummer"
  ) |>
  arrange(positive_year)

table_cvr_uden_aktivt_pnummer

# Inaktive CVR-numre, der er enkeltmandsvirksomheder
table_inaktive_enkeltmandsvirksomheder <-
  df_classified |>
  filter(
    positive_value == "new_company",
    businesstypeCVR == "Enkeltmandsvirksomhed"
  ) |>
  distinct(
    positive_year,
    `CVR Number`
  ) |>
  inner_join(
    cvr_aktiv_status,
    by = "CVR Number"
  ) |>
  filter(!har_aktivt_pnummer) |>
  count(
    positive_year,
    name = "antal_inaktive_enkeltmandsvirksomheder"
  ) |>
  arrange(positive_year)

table_inaktive_enkeltmandsvirksomheder



## NB This is only sanity check
#### DST TABLE P-NUMBER LEVEL ####
# Tabel på p-nummer-niveau, opdelt efter positive_value DST
table_dst_positive_p_number <- 
  df_classified |>
  filter(
    positive_value == "new_company",
    dst_aktiv_positive == 1,
  ) |>
  mutate(
    positive_value = as.factor(positive_value)
  ) |>
  distinct(
    positive_year,
    `P-number`,
    positive_value
  ) |>
  count(
    positive_year,
    positive_value,
    name = "antal_p_numre"
  ) |>
  arrange(
    positive_value,
    positive_year
  )

table_dst_positive_p_number



# Tabel på p-nummer-niveau, opdelt efter positive_value IRIS
table_iris_positive_p_number <- 
  df_classified |>
  filter(
    positive_value == "new_company",
    iris_aktiv == 1,
  ) |>
  mutate(
    positive_value = as.factor(positive_value)
  ) |>
  distinct(
    positive_year,
    `P-number`,
    positive_value
  ) |>
  count(
    positive_year,
    positive_value,
    name = "antal_p_numre"
  ) |>
  arrange(
    positive_value,
    positive_year
  )

table_iris_positive_p_number


#### CVR BIRTH ####

# Define start year of period and kommune here before you run script! #
# Start year
# start_year <- 2019
# kommune_name <- "FURESØ"

# Tables

# #### DST AKTIVE BIRTH ####
# 
# table_dst_year_cvr <- df_classified |>
#   filter(positive_value == "new_company") |> 
#   filter(`Kommune Name` == kommune_name) |> 
#   filter(
#     dst_aktiv_birth == 1, # Birth variable
#     !is.na(`CVR Number`)
#   ) |>
#   distinct(birth_year, `CVR Number`) |>
#   count(
#     birth_year,
#     name = "antal_cvr_numre"
#   )
# 
# table_dst_year_cvr
# 
# # LINE CHART 
# plot_dst_year_cvr <- 
#   ggplot(
#   table_dst_year_cvr,
#   aes(
#     x = birth_year,
#     y = antal_cvr_numre
#   )
# ) +
#   geom_line(
#     linewidth = 1.2,
#     colour = "#0072B2"
#   ) +
#   geom_point(
#     size = 3,
#     colour = "#0072B2"
#   ) +
#   geom_text(
#     aes(label = antal_cvr_numre),
#     vjust = -0.8
#   ) +
#   scale_x_continuous(
#     breaks = 2019:2026
#   ) +
#   scale_y_continuous(
#     limits = c(0, NA),
#     expand = expansion(mult = c(0, 0.1))
#   ) +
#   labs(
#     title = "Udvikling i antal DST-aktive virksomheder, 2019-2024",
#     subtitle = "Unikke CVR-numre opgjort efter etableringsår",
#     x = NULL,
#     y = "Antal CVR-numre",
#     caption = paste(
#       str_wrap("DST-aktiv: Mindst 0,5 ansatte i etableringsåret eller året efter. Bemærk at tal for 2025 og 2026 ikke er endelige", width = 55)
#     )
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold")
#   )
# 
# ggsave("plot_dst_year_cvr.png",
#        plot_dst_year_cvr)
# 
# #### IRIS AKTIVE BIRTH ####
# table_iris_year_cvr <- 
#   df_classified |>
#   filter(positive_value == "new_company") |>
#   filter(`Kommune Name` == kommune_name) |> 
#   filter(
#     iris_aktiv == 1,
#     !is.na(`CVR Number`),
#     birth_year %in% 2019:2026
#   ) |>
#   distinct(birth_year, `CVR Number`) |>
#   count(
#     birth_year,
#     name = "antal_cvr_numre"
#   )
# 
# table_iris_year_cvr
# 
# # LINE CHART
# plot_iris_year_cvr <-
# ggplot(
#   table_iris_year_cvr,
#   aes(
#     x = birth_year,
#     y = antal_cvr_numre
#   )
# ) +
#   geom_line(
#     linewidth = 1.2,
#     colour = "#009E73"
#   ) +
#   geom_point(
#     size = 3,
#     colour = "#009E73"
#   ) +
#   geom_text(
#     aes(label = antal_cvr_numre),
#     vjust = -0.8
#   ) +
#   scale_x_continuous(
#     breaks = 2019:2026
#   ) +
#   scale_y_continuous(
#     limits = c(0, NA),
#     expand = expansion(mult = c(0, 0.1))
#   ) +
#   labs(
#     title = "Udvikling i antal IRIS-aktive virksomheder, 2019-2026",
#     subtitle = "Unikke CVR-numre opgjort efter etableringsår",
#     x = NULL,
#     y = "Antal CVR-numre",
#     caption = str_wrap(
#       paste(
#         "IRIS-aktiv: Har mindst 0,5 ansatte i mindst ét observeret år i perioden"
#       ),
#       width = 55
#     )
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold")
#   )
# 
# ggsave("plot_iris_year_cvr.png",
#        plot_iris_year_cvr)
# #### CVR BIRTH ENDS ####
