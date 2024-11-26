# Day 01 ------------------------------------------------------------------
source(here::here("R/utils.R"))
year <- "2023"
day <- "01"

data_01 <- read_data(year, day)
data_01_test <- tibble(input = readLines(here("R/2023/data/day01_test.txt")))

# Part 1 ------------------------------------------------------------------
solution_01a <- data_01 |> 
  mutate(
    first = stri_extract(input, regex = "\\d", mode = "first"),
    last = stri_extract(input, regex = "\\d", mode = "last"),
    calibration_value = as.numeric(paste0(first,last))
  ) |> 
  summarise(total_calibration = sum(calibration_value)) |> 
  pull(total_calibration)

# Part 2 ------------------------------------------------------------------
solution_01b <- data_01 |> 
  mutate(
    input_adjusted = str_replace_all(
      input,
      c(
        one = "on1e",
        two = "tw2o",
        three = "thre3e",
        four = "fou4r",
        five = "fiv5e",
        six = "si6x",
        seven = "seve7n",
        eight = "eigh8t",
        nine = "nin9e"
      )
    ),
    first = stri_extract(input_adjusted, regex = "\\d", mode = "first"),
    last = stri_extract(input_adjusted, regex = "\\d", mode = "last"),
    calibration_value = as.numeric(paste0(first,last))
  ) |> 
  summarise(total_calibration = sum(calibration_value)) |> 
  pull(total_calibration)
