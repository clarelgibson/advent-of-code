# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
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
# Separate the rules from the updates
rules <- 
  data |> 
  filter(str_detect(input, fixed("|"))) |> 
  separate_wider_delim(
    input,
    delim = "|",
    names = c("first", "second")
  )

updates <- 
  data |> 
  filter(str_detect(input, fixed(","))) |> 
  rowid_to_column("update_id") |> 
  separate_longer_delim(
    input,
    delim = ","
  ) |> 
  mutate(
    update_position = row_number(),
    .by = update_id,
    .after = update_id
  ) |> 
  rename(page_number = input)
