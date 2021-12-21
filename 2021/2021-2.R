# Title:       Day 2: Dive
# Project:     Advent of Code
# Date:        2021-12-21
# Author:      Clare Gibson

# SUMMARY ############################################################
# This script contains the code needed to solve the Day 2 puzzles in
# the 2021 Advent of Code challenge.

# PACKAGES ###########################################################
library(tidyverse)      # general data wrangling

# SOURCE QUESTION ####################################################
# https://adventofcode.com/2021/day/2

# READ DATA ##########################################################
# Read Day 1 input into a character vector
day_2_input <- readLines("2021/data-in/2021-2-data.txt") %>%
  as_tibble()

# PART 1 #############################################################
df_2_1 <- day_2_input %>% 
  # split string and numeric into separate columns
  separate(col = value,
           into = c("direction", "amount"),
           sep = " ",
           convert = TRUE) %>% 
  # add column to denote if instruction refers to depth (D) or 
  # horizontal position (H)
  mutate(plane = if_else(direction == "forward",
                         "H",
                         "D")) %>% 
  # convert "up" amounts to negative
  mutate(amount_c = if_else(direction == "up",
                            as.integer(0 - amount),
                            amount)) %>% 
  # calculate the total amounts for each plane
  group_by(plane) %>% 
  summarise(total_amount = sum(amount_c)) %>% 
  ungroup()

# calculate final answer as product of D and H
answer_2_1 <- prod(df_2_1$total_amount)
