# Day 01 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "01"
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
separated_data <- 
  data |> 
  separate_wider_delim(
    input,
    delim = "   ",
    names = c("first_list", "second_list")
  ) |> 
  mutate_all(as.numeric)

first_list <- 
  separated_data |> 
  arrange(first_list) |> 
  pull(first_list)

second_list <- 
  separated_data |> 
  arrange(second_list) |> 
  pull(second_list)

pairs <- 
  tibble(
    first_list = first_list,
    second_list = second_list
  ) |> 
  mutate(
    distance = abs(second_list - first_list)
  )

solution_01a <- 
  pairs |> 
  summarise(total = sum(distance)) |> 
  pull(total)

# Part 2 ------------------------------------------------------------------
similarity <- 
  pairs |> 
  select(first_list) |> 
  inner_join(
    select(
      pairs,
      second_list
    ),
    by = c("first_list" = "second_list"),
    relationship = "many-to-many"
  ) |> 
  count(first_list) |> 
  mutate(
    similarity_score = first_list * n
  )

solution_01b <- 
  similarity |> 
  summarise(total = sum(similarity_score)) |> 
  pull(total)