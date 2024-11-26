# Title:       Day 1: Sonar Sweep
# Project:     Advent of Code
# Date:        2021-12-21
# Author:      Clare Gibson

# SUMMARY ############################################################
# This script contains the code needed to solve the Day 1 puzzles in
# the 2021 Advent of Code challenge.

# PACKAGES ###########################################################
library(tidyverse)      # general data wrangling
library(RcppRoll)       # for calculating rolling sums

# SOURCE QUESTION ####################################################
# https://adventofcode.com/2021/day/1

# READ DATA ##########################################################
# Read Day 1 input into a tibble
d01 <- readLines("2021/day-1/2021-1-data.txt") %>% 
  as.numeric() %>% 
  as_tibble()

# PART 1 #############################################################
# Count how many values larger than previous
df_1_1 <- d01 %>% 
  # add a column to return the prior value for each row
  mutate(lag = lag(value)) %>% 
  # add a column to determine if lag is greater than value
  mutate(answer = value > lag)

# Calculate the sum of TRUE values in answer
answer_1_1 <- paste("There are",
                    sum(df_1_1$answer, na.rm=TRUE),
                    "values greater than the previous.")

# Print the answer
answer_1_1

# PART 2 #############################################################
# Count how many sliding windows larger than previous
df_1_2 <- d01 %>% 
  # add a column to calculate the rolling sum
  mutate(roll_sum = roll_sum(value,
                             n = 3,
                             align = "left",
                             fill = NA)) %>% 
  # add a column to return the prior roll_sum for each row
  mutate(lag = lag(roll_sum)) %>% 
  # add a column to determine if lag is greater than value
  mutate(answer = roll_sum > lag)

# Calculate the sum of TRUE values in answer
answer_1_2 <- paste("There are",
                    sum(df_1_2$answer, na.rm=TRUE),
                    "values greater than the previous.")

# Print the answer
answer_1_2