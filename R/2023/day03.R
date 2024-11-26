# Day 03 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2023"
day <- "03"
test <- F

# Read input data
input_03 <- read_data(year, day)
test_03 <- read_data(year, day, TRUE)

# Set data variable to use either test or real data
data_03 <- if(test == TRUE) {
  test_03
} else {
  input_03
}

# Part 1 ------------------------------------------------------------------
# Get the locations of the part numbers within each line
part_locations <- 
  data_03 |> 
  rowid_to_column("y") |> 
  mutate(
    x_start = str_locate_all(input, "\\d+") |> 
      map(~ .x[,1]),
    x_end = str_locate_all(input, "\\d+") |> 
      map(~ .x[,2])
  ) |> 
  unnest(cols = c(x_start, x_end)) |> 
  mutate(
    part_number = as.numeric(str_sub(input, x_start, x_end))
  ) |> 
  rowid_to_column("part_id") |> 
  rowwise() |> 
  mutate(x = list(seq(x_start, x_end))) |> 
  ungroup() |> 
  select(!c(x_start, x_end)) |> 
  unnest(cols = x) |> 
  select(
    input,
    x,
    y,
    part_id,
    part_number
  )

# Map the input to a grid with coordinates
row_length <- length(data_03$input)[1]
widths <- c(rep(1, row_length))
names(widths) <- c(paste0("x", 1:row_length))

grid <- 
  data_03 |> 
  separate_wider_position(
    input,
    widths = widths
  ) |> 
  rowid_to_column("y")

# Make the grid long
grid_long <- 
  grid |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "x",
    values_to = "char"
  ) |> 
  mutate(
    x = as.integer(str_extract(x, "\\d+"))
  ) |> 
  relocate(x, .before = y)

# Make a dataframe of adjacent coordinates for each primary coordinate
adjacent_coords <- 
  grid_long |> 
  select(x, y) |> 
  mutate(
    x.adj1 = x - 1,
    y.adj1 = y - 1,
    x.adj2 = x,
    y.adj2 = y -1,
    x.adj3 = x + 1,
    y.adj3 = y - 1,
    x.adj4 = x - 1,
    y.adj4 = y,
    x.adj5 = x + 1,
    y.adj5 = y,
    x.adj6 = x - 1,
    y.adj6 = y + 1,
    x.adj7 = x,
    y.adj7 = y + 1,
    x.adj8 = x + 1,
    y.adj8 = y + 1
  )

adjacent_x <- 
  adjacent_coords |> 
  select(x, y, starts_with("x.")) |> 
  pivot_longer(
    !c(x, y),
    names_to = "group",
    names_prefix = "x.",
    values_to = "adj_x"
  )

adjacent_y <- 
  adjacent_coords |> 
  select(x, y, starts_with("y.")) |> 
  pivot_longer(
    !c(x, y),
    names_to = "group",
    names_prefix = "y.",
    values_to = "adj_y"
  )

adjacent <- 
  adjacent_x |> 
  left_join(adjacent_y) |> 
  select(!group) |> 
  filter(
    between(adj_x, 1, row_length),
    between(adj_y, 1, row_length)
  ) |> 
  left_join(grid_long) |> 
  left_join(
    select(
      grid_long,
      x,
      y,
      adj_char = char
    ),
    by = c("adj_x" = "x", "adj_y" = "y")
  ) |> 
  left_join(part_locations) |> 
  filter(
    str_detect(adj_char, "[0-9\\.]", negate = T),
    !is.na(part_number)
  )

# Find the solutions
solution_03a <- 
  adjacent |> 
  select(part_id, part_number) |> 
  distinct() |> 
  summarise(sum_of_parts = sum(part_number)) |> 
  pull()
