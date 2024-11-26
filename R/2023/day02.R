# Day 02 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2023"
day <- "02"
test <- F

# Read input data
input_02 <- read_data(year, day)
test_02 <- read_data(year, day, TRUE)

# Set data variable to use either test or real data
data_02 <- if(test == TRUE) {
  test_02
} else {
  input_02
}

# Part 1 ------------------------------------------------------------------
# What is the max number of draws?
max_draw_delimiters <- 
  data_02 |> 
  mutate(draw_delimiters = str_count(input, ";")) |> 
  summarise(max_draw_delimiters = max(draw_delimiters, na.rm = T)) |>
  pull()

max_draws <-  
  max_draw_delimiters + 1

# Define the max allowed of each colour
max_allowed <- 
  tibble(
    colour = c("Red", "Green", "Blue"),
    max_allowed = c(12, 13, 14)
  )

# Build a df to find the solution
df_02a <- 
  data_02 |> 
  # Separate the game IDs from the details
  separate_wider_delim(
    input,
    delim = ":",
    names = c("game_id", "game_details")
  ) |> 
  mutate(
    game_id = as.numeric(str_extract(game_id, "\\d+"))
  ) |> 
  # Separate the game details by draw
  separate_wider_delim(
    game_details,
    delim = ";",
    names = paste0("Draw ", 1:max_draws),
    too_few = "align_start",
    too_many = "error"
  ) |> 
  pivot_longer(
    starts_with("Draw"),
    names_to = "draw_id",
    values_to = "draw_details",
    values_drop_na = T
  ) |> 
  mutate(
    draw_id = as.numeric(str_extract(draw_id, "\\d+"))
  ) |> 
  # Separate the draw details by colour
  separate_wider_delim(
    draw_details,
    delim = ",",
    names = paste0("Colour ", 1:3),
    too_few = "align_start",
    too_many = "error"
  ) |> 
  pivot_longer(
    starts_with("Colour"),
    names_to = "colour",
    values_to = "count",
    values_drop_na = T
  ) |> 
  mutate(
    colour = case_when(
      str_detect(count, "red") ~ "Red",
      str_detect(count, "blue") ~ "Blue",
      str_detect(count, "green") ~ "Green",
      TRUE ~ "Colour not detected"
    ),
    count = as.numeric(str_extract(count, "\\d+"))
  ) |> 
  left_join(max_allowed) |> 
  # did the draw fail according to the max allowed rule?
  mutate(
    fail_flag = case_when(
      count > max_allowed ~ 1,
      TRUE ~ 0
    )
  )

# Find the solution
solution_02a <- 
  df_02a |> 
  group_by(game_id) |> 
  summarise(sum_fails = sum(fail_flag)) |> 
  ungroup() |> 
  # sum the passing games
  filter(sum_fails == 0) |> 
  summarise(sum_of_ids = sum(game_id)) |> 
  pull()

# Part 2 ------------------------------------------------------------------
df_02b <- 
  df_02a |> 
  select(
    game_id,
    draw_id,
    colour,
    count
  ) |> 
  summarise(
    game_count = max(count),
    .by = c(game_id, colour)
  )

# Find the solution
solution_02b <- 
  df_02b |> 
  summarise(
    game_power = prod(game_count),
    .by = game_id
  ) |> 
  summarise(
    total_power = sum(game_power)
  ) |> 
  pull()
