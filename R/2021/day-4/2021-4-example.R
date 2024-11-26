# Title:       Day 4: Giant Squid (Example)
# Project:     Advent of Code
# Date:        2021-12-26
# Author:      Clare Gibson

# SUMMARY ############################################################
# This script contains the code needed to solve the example Day 4
# puzzles in the 2021 Advent of Code challenge.

# PACKAGES ###########################################################
library(tidyverse)      # general data wrangling
library(data.table)     # for reading in data

# SOURCE QUESTION ####################################################
# https://adventofcode.com/2021/day/4

# READ DATA ##########################################################
# Read Day 4 example bingo calls
ex_calls <- 
  read_lines("2021/day-4/2021-4-example-data.txt",
             n_max=1) %>% 
  str_split(., ",") %>% 
  unlist() %>% 
  as.integer()

# Read Day 4 example bingo cards
ex_cards <- 
  read.table("2021/day-4/2021-4-example-data.txt",
             header = F,
             col.names = c("a", "b", "c", "d", "e"),
             skip = 2) %>% 
  # add a column to indicate different bingo cards
  mutate(card = factor(rep(1:(nrow(.)/5), each = 5)))

# PART 1 #############################################################
# Which card will be first to win?
# Mark the cards
ex_marks <- ex_cards %>% 
  # add a column to indicate different rows
  group_by(card) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  # add columns to indicate draw positions for each number
  mutate(pos_a = match(a, ex_calls),
         pos_b = match(b, ex_calls),
         pos_c = match(c, ex_calls),
         pos_d = match(d, ex_calls),
         pos_e = match(e, ex_calls)) %>% 
  # rearrange the columns
  select(card, row, everything())

# Pivot the card number data
ex_marks_piv1 <- ex_marks %>% 
  # remove the card numbers
  select(!starts_with("pos_")) %>% 
  # pivot the data longer
  pivot_longer(!c(card, row),
               names_to = "column",
               values_to = "number") %>% 
  # arrange by card number then column number
  arrange(card, row, column) 

# Pivot the draw position data
ex_marks_piv2 <- ex_marks %>% 
  # remove the card numbers
  select(!c("a", "b", "c", "d", "e")) %>% 
  # pivot the data longer
  pivot_longer(!c(card, row),
               names_to = "column",
               values_to = "draw_pos",
               names_prefix = "pos_") %>% 
  # arrange by card number then column number
  arrange(card, row, column) 

# Join the pivoted data
ex_marks <- ex_marks_piv1 %>% 
  left_join(ex_marks_piv2)

# Determine the winning card
ex_results <- ex_marks %>% 
  # work out when column will be completed (max draw_pos)
  group_by(card, column) %>% 
  mutate(col_complete = max(draw_pos)) %>% 
  ungroup() %>% 
  # work out when row will be completed (max draw_pos)
  group_by(card, row) %>% 
  mutate(row_complete = max(draw_pos)) %>% 
  ungroup() %>% 
  # first row and column completed by card
  group_by(card) %>% 
  summarise(first_column_completed = min(row_complete),
            first_row_completed = min(col_complete)) %>% 
  ungroup() %>% 
  # when will bingo be called
  mutate(bingo = pmin(first_column_completed,
                      first_row_completed))

# Store the number of the winning card as a variable
ex_winning_card <- ex_results %>% 
  slice_min(bingo) %>%   # winning card has the smallest bingo
  pull(card) %>% 
  as.integer()

# Store the position of the last number called as a variable
ex_winning_pos <- ex_results %>% 
  slice_min(bingo) %>% 
  pull(bingo)

# Store the last number called as a variable
ex_winning_number <- ex_marks %>% 
  filter(card == ex_winning_card,
         draw_pos == ex_winning_pos) %>% 
  pull(number)

# Store the sum of unmarked numbers as a variable
ex_winning_unmarked <- ex_marks %>% 
  # filter to the winning card
  filter(card == ex_winning_card) %>% 
  # filter to unmarked numbers only
  filter(draw_pos > ex_winning_pos) %>% 
  # sum the unmarked numbers
  summarise(unmarked_sum = sum(number)) %>% 
  pull(unmarked_sum)

# Calculate the score of the winning card
# Score is sum of uncalled numbers * last called number
ex_winning_score <- ex_winning_unmarked * ex_winning_number

# PART 2 #############################################################
# Which card will be last to win?
# Store the number of the last card as a variable
ex_last_card <- ex_results %>% 
  slice_max(bingo) %>%   # last card has largest bingo value
  pull(card) %>% 
  as.integer()

# Store the position of the last number called as a variable
ex_last_pos <- ex_results %>% 
  slice_max(bingo) %>% 
  pull(bingo)

# Store the last number called as a variable
ex_last_number <- ex_marks %>% 
  filter(card == ex_last_card,
         draw_pos == ex_last_pos) %>% 
  pull(number)

# Store the sum of unmarked numbers as a variable
ex_last_unmarked <- ex_marks %>% 
  # filter to the winning card
  filter(card == ex_last_card) %>% 
  # filter to unmarked numbers only
  filter(draw_pos > ex_last_pos) %>% 
  # sum the unmarked numbers
  summarise(unmarked_sum = sum(number)) %>% 
  pull(unmarked_sum)

# Calculate the score of the winning card
# Score is sum of uncalled numbers * last called number
ex_last_score <- ex_last_unmarked * ex_last_number