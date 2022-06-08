library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)

# census_api_key("YOUR KEY GOES HERE")

# Variables ---------------------------------------------------------------

# What variables are available for the 2020 Decennial Census?
dc2020_vars <- load_variables(2020, "pl", cache = TRUE)
dc2020_vars

# Variables are grouped into tables
dc2020_vars |>
  transmute(table = str_sub(name, 1, 2), concept) |>
  count(table, concept)


# 2020 Decennial Census ---------------------------------------------------

# total population for Nebraska counties
ne_pop_2020 <- get_decennial(
  geography = "county",
  variables = "P2_001N",
  year = 2020,
  state = "Nebraska",
)

ne_pop_2020

ne_pop_2020 |>
  slice_max(value, n = 5)

ne_pop_2020 |>
  slice_min(value, n = 5)

# visualization
ne_pop_2020 |>
  ggplot(aes(value, NAME)) +
  geom_col()

# better visualization
ne_pop_2020 |>
  mutate(NAME = str_remove(NAME, " County, Nebraska")) |>
  ggplot(aes(value, reorder(NAME, value))) +
  geom_col() +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(title = "Population of Nebraska counties",
       subtitle = "2020 Decennial US Census",
       x = "Population",
       y = "") +
  theme_light()


# Retrieving multiple variables -------------------------------------------

variables <- c("P2_001N", "P2_005N", "P2_002N", "P2_006N", "P2_007N",
               "P2_008N", "P2_009N", "P2_010N", "P2_011N")

ne_race_ethnicity_2020 <- get_decennial(
  geography = "county",
  variables = variables,
  year = 2020,
  state = "NE", # <=
  # output = "wide"
)

ne_race_ethnicity_2020

# Named list
variables <- c(
  # Total = "P2_001N",
  White = "P2_005N",
  Hispanic = "P2_002N",
  Black = "P2_006N",
  Native = "P2_007N",
  Asian = "P2_008N",
  HIPI = "P2_009N",
  Other = "P2_010N",
  TwoOrMore = "P2_011N")

ne_race_ethnicity_2020 <- get_decennial(
  geography = "county",
  variables = variables,
  year = 2020,
  summary_var = "P2_001N", # <=
  state = "NE",
  # output = "wide"
)

ne_race_ethnicity_2020

# Population by race / ethnicity
ne_race_ethnicity_2020 |>
  group_by(variable) |>
  summarize(n = sum(value)) |>
  arrange(desc(n))

# Percent Hispanic
ne_hispanic_percent <- ne_race_ethnicity_2020 |>
  filter(variable == "Hispanic") |>
  mutate(percent = 100 * (value / summary_value)) |>
  select(NAME, percent)

ne_hispanic_percent

# Lowest 5
ne_hispanic_percent |>
  slice_min(percent, n = 5)

# Highest 5
ne_hispanic_percent |>
  slice_max(percent, n = 5)


# Choropleth maps ---------------------------------------------------------

ne_race_ethnicity_2020 <- get_decennial(
  geography = "county",
  variables = variables,
  year = 2020,
  state = "NE",
  summary_var = "P2_001N",
  geometry = TRUE # <=
)

ne_race_ethnicity_2020

ne_hispanic_percent <- ne_race_ethnicity_2020 |>
  filter(variable == "Hispanic") |>
  mutate(percent = 100 * (value / summary_value)) |>
  select(NAME, percent)

ne_hispanic_percent

# exploratory map
plot(ne_hispanic_percent["percent"])

# using ggplot2 geom_sf
ne_hispanic_percent |>
  ggplot(aes(geometry = geometry, fill = percent)) +
  geom_sf()

# coordinate reference system
# library(crsuggest)
# suggest_crs(ne_hispanic_percent)
# suggest_top_crs(ne_hispanic_percent)

# more polished map
library(sf) # for st_transform function

ne_hispanic_percent |>
  st_transform(crs = 32104) |>
  ggplot(aes(geometry = geometry, fill = percent)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Nebraska Hispanic or Latino Population",
       subtitle = "2020 Decennial US Census",
       fill = "Percent of total\ncounty population")


# 2016-2020 American Community Survey -------------------------------------

acs5_2020_vars <- load_variables(2020, "acs5", cache = TRUE)
acs5_2020_vars

# Median household income

ma_income_2020 <- get_acs(
  geography = "county",
  variables = "B19013_001",
  year = 2020,
  state = "MA",
  # moe_level = 99,
  # output = "wide"
)

# Note columns
ma_income_2020

# ma_pop_2020 <- get_decennial(
#   geography = "county",
#   variables = "P1_001N",
#   year = 2020,
#   state = "MA",
# )
# ma_pop_2020

# Show uncertainty with error bars
ma_income_2020 |>
  mutate(NAME = str_remove(NAME, " County, Massachusetts")) |>
  ggplot(aes(estimate, reorder(NAME, estimate))) +
  geom_point(color = "red", size = 2.5) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  scale_x_continuous(labels = scales::label_dollar()) +
  theme_light() +
  labs(
    title = "Median Household Income for Massachusetts Counties",
    subtitle = "with 90% compatibility intervals",
    x = "",
    y = "",
    caption = "Data source: 2016-2020 ACS & tidycensus R package"
  )
