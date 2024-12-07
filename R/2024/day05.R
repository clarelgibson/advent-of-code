# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2024"
day <- "05"
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
# Separate the rules from the updates
rules <- 
  data |> 
  filter(str_detect(input, fixed("|"))) |> 
  separate_wider_delim(
    input,
    delim = "|",
    names = c("before", "after")
  ) |> 
  mutate(across(c(before, after), as.numeric)) |> 
  rowid_to_column("rule_id")

updates <- 
  data |> 
  filter(str_detect(input, fixed(","))) |> 
  rowid_to_column("update_id") |> 
  separate_longer_delim(
    input,
    delim = ","
  ) |>
  mutate(
    page_id = row_number(),
    .by = update_id,
    .after = update_id
  ) |> 
  rename(page_number = input) |> 
  mutate(page_number = as.numeric(page_number)) |> 
  mutate(
    is_middle = if_else(
      page_id == (max(page_id) + 1) / 2,
      TRUE,
      FALSE
    ),
    .by = update_id
  )

# Summarise the middle value of each update
update_middles <- 
  updates |> 
  filter(is_middle) |> 
  select(
    update_id,
    middle_page_number = page_number
  )
    
# Apply the rules to each update
tests <- 
  rules |> 
  cross_join(
    select(
      updates,
      update_id
    )
  ) |> 
  distinct() |> 
  left_join(
    select(
      updates,
      update_id,
      before = page_number,
      before_page_id = page_id
    )
  ) |> 
  left_join(
    select(
      updates,
      update_id,
      after = page_number,
      after_page_id = page_id
    )
  ) |> 
  mutate(
    rule_applies = if_else(
      !is.na(before_page_id) & !is.na(after_page_id),
      TRUE,
      FALSE
    ),
    rule_pass = if_else(
      rule_applies == TRUE & after_page_id < before_page_id,
      FALSE,
      TRUE
    )
  )

test_results <- 
  tests |> 
  group_by(update_id) |> 
  summarise(
    count_of_rules = n_distinct(rule_id),
    count_of_passes = sum(rule_pass)
  ) |> 
  mutate(
    result = if_else(
      count_of_passes == count_of_rules,
      1,
      0
    )
  ) |> 
  left_join(update_middles)

solution_05a <- 
  test_results |> 
  filter(result == 1) |> 
  summarise(
    total = sum(middle_page_number)
  ) |> 
  pull(total)
