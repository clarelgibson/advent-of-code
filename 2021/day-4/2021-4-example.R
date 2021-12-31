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
d04_calls_ex <- 
  read_lines("2021/day-4/2021-4-example-data.txt",
             n_max=1) %>% 
  str_split(., ",") %>% 
  unlist() %>% 
  as.integer()

# Read Day 4 example bingo cards
d04_cards_ex <- 
  read.table("2021/day-4/2021-4-example-data.txt",
             header = F,
             col.names = c("a", "b", "c", "d", "e"),
             skip = 2) %>% 
  # add a column to indicate different bingo cards
  mutate(card = factor(rep(1:(nrow(.)/5), each = 5)))

# PART 1 #############################################################
# Which card will be first to win?
df_4_ex <- d04_cards_ex %>% 
  # add a column to indicate different rows
  group_by(card) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  # add columns to indicate draw positions for each number
  mutate(pos_a = match(a, d04_calls_ex),
         pos_b = match(b, d04_calls_ex),
         pos_c = match(c, d04_calls_ex),
         pos_d = match(d, d04_calls_ex),
         pos_e = match(e, d04_calls_ex)) %>% 
  # remove the card numbers
  select(!c("a", "b", "c", "d", "e")) %>% 
  # pivot the data longer
  pivot_longer(!c(card, row),
               names_to = "column",
               values_to = "draw_pos",
               names_prefix = "pos_") %>% 
  # arrange by card number then column number
  arrange(card, row, column) %>% 
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
