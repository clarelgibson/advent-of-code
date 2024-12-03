# Day 02 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "02"
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
# Determine the max number of levels per report (row)
max_items <- 
  data |> 
  mutate(
    count_of_delims = str_count(input, " ")
  ) |> 
  summarise(max_delims = max(count_of_delims, na.rm = TRUE)) |> 
  mutate(max_items = max_delims + 1) |> 
  pull(max_items)

# Pivot the report data
reports <- 
  data |> 
  separate_wider_delim(
    input,
    delim = " ",
    names = paste0("level_", 1:max_items),
    too_few = "align_start",
    too_many = "error"
  ) |> 
  rowid_to_column("report_id") |> 
  pivot_longer(
    !report_id,
    names_to = "level_id",
    names_prefix = "level_",
    values_to = "value"
  ) |> 
  mutate(across(where(is.character), as.numeric))

# Test each report
tests <- test_reports(reports)

# Put the safe report IDs into a variable
safe <- 
  tests |> 
  filter(is_safe == 1) |> 
  pull(report_id)

# Get the solution
solution_02a <-
  sum(tests$is_safe)

# Part 2 ------------------------------------------------------------------
# Remove level 1
dampened_test_01 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 1) |> 
  test_reports()

dampened_test_01_safe <- 
  dampened_test_01 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_01_safe)

# Remove level 2
dampened_test_02 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 2) |> 
  test_reports()

dampened_test_02_safe <- 
  dampened_test_02 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_02_safe)

# Remove level 3
dampened_test_03 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 3) |> 
  test_reports()

dampened_test_03_safe <- 
  dampened_test_03 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_03_safe)

# Remove level 4
dampened_test_04 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 4) |> 
  test_reports()

dampened_test_04_safe <- 
  dampened_test_04 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_04_safe)

# Remove level 5
dampened_test_05 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 5) |> 
  test_reports()

dampened_test_05_safe <- 
  dampened_test_05 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_05_safe)

# Remove level 6
dampened_test_06 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 6) |> 
  test_reports()

dampened_test_06_safe <- 
  dampened_test_06 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_06_safe)

# Remove level 7
dampened_test_07 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 7) |> 
  test_reports()

dampened_test_07_safe <- 
  dampened_test_07 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_07_safe)

# Remove level 8
dampened_test_08 <- 
  reports |> 
  filter(!report_id %in% safe) |> 
  filter(!level_id == 8) |> 
  test_reports()

dampened_test_08_safe <- 
  dampened_test_08 |> 
  filter(is_safe == 1) |> 
  pull(report_id)

safe <- 
  safe |> 
  append(dampened_test_08_safe)

# Get the solution
solution_02b <- 
  length(safe)
