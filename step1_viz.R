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

df_viz <-
  read_rds("df_classified.rds")

# Tables

#### DST AKTIVE ####

# split the tables with CVR
table_dst_year_cvr <- df_viz |>
  filter(
    dst_aktiv_birth == 1,
    !is.na(`CVR Number`),
    birth_year %in% 2019:2024
  ) |>
  distinct(birth_year, `CVR Number`) |>
  count(
    birth_year,
    name = "antal_cvr_numre"
  )

table_dst_year_cvr

# LINE CHART 
ggplot(
  table_dst_year_cvr,
  aes(
    x = birth_year,
    y = antal_cvr_numre
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "#0072B2"
  ) +
  geom_point(
    size = 3,
    colour = "#0072B2"
  ) +
  geom_text(
    aes(label = antal_cvr_numre),
    vjust = -0.8
  ) +
  scale_x_continuous(
    breaks = 2019:2024
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Udvikling i antal DST-aktive virksomheder, 2019-2024",
    subtitle = "Unikke CVR-numre opgjort efter etableringsår",
    x = NULL,
    y = "Antal CVR-numre",
    caption = paste(
      "DST-aktiv: Mindst 0,5 ansatte i etableringsåret",
      "eller året efter."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


#### IRIS AKTIVE ####
# Tabel: unikke IRIS-aktive CVR-numre efter etableringsår
table_iris_year_cvr <- df_viz |>
  filter(
    iris_aktiv == 1,
    !is.na(`CVR Number`),
    birth_year %in% 2019:2024
  ) |>
  distinct(birth_year, `CVR Number`) |>
  count(
    birth_year,
    name = "antal_cvr_numre"
  )

table_iris_year_cvr

# Line chart: IRIS-aktive
ggplot(
  table_iris_year_cvr,
  aes(
    x = birth_year,
    y = antal_cvr_numre
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "#009E73"
  ) +
  geom_point(
    size = 3,
    colour = "#009E73"
  ) +
  geom_text(
    aes(label = antal_cvr_numre),
    vjust = -0.8
  ) +
  scale_x_continuous(
    breaks = 2019:2024
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Udvikling i antal IRIS-aktive virksomheder, 2019-2024",
    subtitle = "Unikke CVR-numre opgjort efter etableringsår",
    x = NULL,
    y = "Antal CVR-numre",
    caption = paste(
      "IRIS-aktiv: Har mindst 0,5 ansatte",
      "i mindst ét observeret år."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#### INAKTIVE ####
# Tabel: unikke inaktive CVR-numre efter etableringsår
table_inaktiv_year_cvr <- df_viz |>
  filter(
    inaktiv == 1,
    !is.na(`CVR Number`),
    birth_year %in% 2019:2024
  ) |>
  distinct(birth_year, `CVR Number`) |>
  count(
    birth_year,
    name = "antal_cvr_numre"
  )

table_inaktiv_year_cvr

# Line chart: inaktive
ggplot(
  table_inaktiv_year_cvr,
  aes(
    x = birth_year,
    y = antal_cvr_numre
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "#D55E00"
  ) +
  geom_point(
    size = 3,
    colour = "#D55E00"
  ) +
  geom_text(
    aes(label = antal_cvr_numre),
    vjust = -0.8
  ) +
  scale_x_continuous(
    breaks = 2019:2024
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Udvikling i antal inaktive virksomheder, 2019-2024",
    subtitle = "Unikke CVR-numre opgjort efter etableringsår",
    x = NULL,
    y = "Antal CVR-numre",
    caption = paste(
      "Inaktiv: Har aldrig mindst 0,5 ansatte",
      "i de observerede data."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
