# Day 05 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2023"
day <- "05"
test <- T

# Read input data
input_data <- read_data(year, day)
test_data <- read_data(year, day, TRUE)

# Set data variable to use either test or real data
data <- if(test == TRUE) {
  test_data
} else {
  input_data
}

# Part 1 ------------------------------------------------------------------
# Identify groups
data_grouped <- 
  data |> 
  mutate(
    group = if_else(
      str_detect(input, ":"),
      input,
      lag(input)
    )
  )

# Separate pieces
seeds <- 
  data |> 
  filter(str_detect(input, "seeds:")) |> 
  separate_longer_delim(
    input, " "
  ) |> 
  filter(str_detect(input, "\\d+")) |> 
  mutate(seeds = as.numeric(input)) |> 
  select(!input)

seed_to_soil <- 
  data |> 
  filter(str_detect(input, "seed-to-soil"))
