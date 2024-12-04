# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "04"
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
# Put input into tidy grid
row_length <- length(data$input)[1]
widths <- c(rep(1, row_length))
names(widths) <- c(paste0("x", 1:row_length))

grid <- 
  data |> 
  rowid_to_column("row") |> 
  separate_longer_position(input, 1) |> 
  mutate(
    col = row_number(),
    .by = row
  ) |> 
  select(
    row,
    col,
    value = input
  )

# Make a dataframe of adjacent references for each primary reference
adjacent_references <- 
  grid |> 
  select(!value) |> 
  mutate(
    row_top_left = row - 1,
    col_top_left = col - 1,
    row_top_centre = row - 1,
    col_top_centre = col,
    row_top_right = row - 1,
    col_top_right = col + 1,
    row_mid_left = row,
    col_mid_left = col - 1,
    row_mid_right = row,
    col_mid_right = col + 1,
    row_bot_left = row + 1,
    col_bot_left = col - 1,
    row_bot_centre = row + 1,
    col_bot_centre = col,
    row_bot_right = row + 1,
    col_bot_right = col + 1
  )
