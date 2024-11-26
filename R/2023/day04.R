# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2023"
day <- "04"
test <- T

# Read input data
input_04 <- read_data(year, day)
test_04 <- read_data(year, day, TRUE)

# Set data variable to use either test or real data
data_04 <- if(test == TRUE) {
  test_04
} else {
  input_04
}

# Part 1 ------------------------------------------------------------------
df <- 
  data_04 |> 
  separate_wider_delim(
    cols = input,
    delim = ":",
    names = c("card", "values")
  ) |> 
  mutate(
    card = as.numeric(str_extract(card, "\\d+"))
  ) |> 
  separate_wider_delim(
    cols = values,
    delim = "|",
    names = c("winning","yours")
  ) |> 
  pivot_longer(
    cols = !card,
    names_to = "number_type",
    values_to = "number"
  ) |> 
  separate_longer_delim(
    cols = number,
    delim = " "
  ) |> 
  mutate(number = as.numeric(number)) |> 
  filter(!is.na(number))

winning <- 
  df |> 
  filter(number_type == "winning") |> 
  select(card, number)

yours <- 
  df |> 
  filter(number_type == "yours") |> 
  select(card, number)

matches <- 
  winning |> 
  inner_join(yours) |> 
  summarise(
    matches = n(),
    .by = card
  ) |> 
  mutate(
    score = 2^(matches - 1)
  )

# Find the solution
solution_04a <- 
  matches |> 
  summarise(total_score = sum(score)) |> 
  pull(total_score)

# Part 2 ------------------------------------------------------------------
highest_card <- max(df$card)

copies <- 
  matches |> 
  select(
    r1_card = card,
    r1_matches = matches
  ) |> 
  rowwise() |> 
  mutate(
    r2_card = list(seq(r1_card + 1, min(r1_card + r1_matches, highest_card)))
  ) |> 
  unnest(r2_card) |> 
  ungroup() |> 
  left_join(
    select(
      matches,
      r2_card = card,
      r2_matches = matches
    )
  ) |> 
  mutate(r2_matches = replace_na(r2_matches, 0)) |> 
  rowwise() |> 
  mutate(
    r3_card = list(seq(r2_card + 1, min(r2_card + r2_matches, highest_card)))
  ) |> 
  unnest(r3_card)
