# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "04"
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
    letter_1 = input
  )

# Get map of adjacents
adjacents <- 
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1AUVJZbQ4VwI1ov7q8kv0AW3CL2oEQ7KTZC0j-4Eq9PU/edit?gid=0#gid=0",
    sheet = "Map of Adjacents"
  )

# Build the wordsearcher
wordsearch <- 
  grid |> 
  cross_join(adjacents) |> 
  # Get the next adjacent letter (letter_2) in each direction
  mutate(
    r2 = row + modr,
    c2 = col + modc
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_2 = letter_1
    ),
    by = c("r2" = "row", "c2" = "col")
  ) |> 
  # Get letter_3
  mutate(
    r3 = r2 + modr,
    c3 = c2 + modc
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_3 = letter_1
    ),
    by = c("r3" = "row", "c3" = "col")
  ) |> 
  # Get letter_4
  mutate(
    r4 = r3 + modr,
    c4 = c3 + modc
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_4 = letter_1
    ),
    by = c("r4" = "row", "c4" = "col")
  ) |> 
  # Put the 4 letters together and check if it spells "XMAS"
  mutate(
    concat = paste0(letter_1, letter_2, letter_3, letter_4),
    is_xmas = if_else(concat == "XMAS", 1, 0)
  )
  
# Get the solution
solution_04a <- 
  wordsearch |> 
  summarise(count_of_xmas = sum(is_xmas)) |> 
  pull()

# Part 2 ------------------------------------------------------------------
valid_solutions <- c("AMMSS", "ASSMM", "AMSMS", "ASMSM")

wordsearch_2 <- 
  grid |> 
  mutate(
    pos2 = "TL",
    pos3 = "TR",
    pos4 = "BL",
    pos5 = "BR"
  ) |> 
  # Get letter 2
  inner_join(
    select(
      adjacents,
      position,
      modr2 = modr,
      modc2 = modc
    ),
    by = c("pos2" = "position")
  ) |> 
  mutate(
    r2 = row + modr2,
    c2 = col + modc2
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_2 = letter_1
    ),
    by = c("r2" = "row", "c2" = "col")
  ) |> 
  # Get letter 3
  inner_join(
    select(
      adjacents,
      position,
      modr3 = modr,
      modc3 = modc
    ),
    by = c("pos3" = "position")
  ) |> 
  mutate(
    r3 = row + modr3,
    c3 = col + modc3
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_3 = letter_1
    ),
    by = c("r3" = "row", "c3" = "col")
  ) |> 
  # Get letter 4
  inner_join(
    select(
      adjacents,
      position,
      modr4 = modr,
      modc4 = modc
    ),
    by = c("pos4" = "position")
  ) |> 
  mutate(
    r4 = row + modr4,
    c4 = col + modc4
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_4 = letter_1
    ),
    by = c("r4" = "row", "c4" = "col")
  ) |> 
  # Get letter 5
  inner_join(
    select(
      adjacents,
      position,
      modr5 = modr,
      modc5 = modc
    ),
    by = c("pos5" = "position")
  ) |> 
  mutate(
    r5 = row + modr5,
    c5 = col + modc5
  ) |> 
  inner_join(
    select(
      grid,
      row,
      col,
      letter_5 = letter_1
    ),
    by = c("r5" = "row", "c5" = "col")
  ) |> 
  # Put the 5 letters together and check if it is valid solution
  mutate(
    concat = paste0(letter_1, letter_2, letter_3, letter_4, letter_5),
    is_x_mas = if_else(concat %in% valid_solutions, 1, 0)
  )

# Get the solution
solution_04b <- 
  wordsearch_2 |> 
  summarise(
    count = sum(is_x_mas)
  ) |> 
  pull()