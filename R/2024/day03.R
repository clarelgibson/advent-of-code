# Day 03 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "03"
test <- F

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
mul <- 
  data |> 
  mutate(
    mul_instructions = str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)")
  ) |> 
  unnest_longer(mul_instructions) |> 
  mutate(
    x = as.numeric(stri_extract_first(mul_instructions, regex = "\\d{1,3}")),
    y = as.numeric(stri_extract_last(mul_instructions, regex = "\\d{1,3}")),
    prod = x * y
  )

# Get the solution
solution_03a <- 
  sum(mul$prod)

# Part 2 ------------------------------------------------------------------
mul_enhanced <- 
  data |> 
  separate_longer_delim(
    input,
    delim = regex("(?=don't\\(\\)|do\\(\\))")
  ) |> 
  # filter out the disabled rows
  filter(!str_starts(input, "don't()")) |> 
  mutate(
    mul_instructions = str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)")
  ) |> 
  unnest_longer(mul_instructions) |> 
  mutate(
    x = as.numeric(stri_extract_first(mul_instructions, regex = "\\d{1,3}")),
    y = as.numeric(stri_extract_last(mul_instructions, regex = "\\d{1,3}")),
    prod = x * y
  )

# Get the solution
solution_03b <- 
  sum(mul_enhanced$prod)
