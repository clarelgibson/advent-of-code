# Day 04 ------------------------------------------------------------------
source(here::here("R/utils.R"))

# Set variables
year <- "2023"
day <- "04"
test <- F

# Read input data
input_04 <- read_data_vec(year, day)
test_04 <- read_data_vec(year, day, TRUE)

# Set data variable to use either test or real data
data_04 <- if(test == TRUE) {
  test_04
} else {
  input_04
}

# Part 1 ------------------------------------------------------------------
winning <- stringr::str_extract(data_04, "\\|\\s*(\\d*\\s?)*") |>
  stringr::str_remove_all("(\\|\\s*|\\s*$)") |>
  stringr::str_split("\\s+") |>
  lapply(as.integer)

cards <- stringr::str_extract(data_04, ":\\s*(\\d*\\s)*") |>
  stringr::str_remove_all("(^\\:\\s*|\\s$)") |>
  stringr::str_split("\\s+") |>
  lapply(as.integer)

# Part 1: Matching numbers double points
result <- 0L
for (i in seq_along(cards)) {
  points <- sum(cards[[i]] %in% winning[[i]])
  if (points > 0) result <- result + 2 ^(points - 1)
}

solution_04a <- result

# Part 2: Append the pool of cards for every match with the next n cards where
# n is the number of matches
n_cards <- rep(1, length(data_04))
for (i in seq_along(n_cards)) {
  if (i == 0) next
  n <- n_cards[[i]]
  wins <- sum(winning[[i]] %in% cards[[i]])
  n_cards[seq_len(wins) + i] <- n_cards[seq_len(wins) + i] + n
}

solution_04b <- sum(n_cards)
