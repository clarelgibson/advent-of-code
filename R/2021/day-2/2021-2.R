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
# Read Day 2 input into a tibble
d02 <- read.table("2021/day-2/2021-2-data.txt",
                  col.names = c("command","x"))

# PART 1 #############################################################
# Instructions =======================================================
# forward X increases the horizontal position by X units.
# down X increases the depth by X units.
# up X decreases the depth by X units.

# Build a df to calculate answer
df_2_1 <- d02 %>% 
  # add column to denote if instruction refers to depth (D) or 
  # horizontal position (H)
  mutate(plane = if_else(command == "forward",
                         "H",
                         "D")) %>% 
  # convert "up" amounts to negative
  mutate(x = if_else(command == "up",
                     as.integer(0 - x),
                     x)) %>% 
  # calculate the total amounts for each plane
  group_by(plane) %>% 
  summarise(total_amount = sum(x)) %>% 
  ungroup()

# calculate final answer as product of D and H
answer_2_1 <- prod(df_2_1$total_amount)

# PART 2 #############################################################
# Instructions =======================================================
# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   1. It increases your horizontal position by X units.
#   2. It increases your depth by your aim multiplied by X.

# Let's try this with a for loop
# Initialize values for aim, horizontal position and depth
aim <- 0
hor <- 0
dep <- 0

# For each command adjust aim, hor and dep
for(i in 1:nrow(d02)){
  if(d02$command[i] == "down"){
    aim <- aim + d02$x[i]
  }
  else if(d02$command[i] == "up"){
    aim <- aim - d02$x[i]
  }
  else if(d02$command[i] == "forward"){
    hor <- hor + d02$x[i]
    dep <- dep + (aim * d02$x[i])
  }
}

answer_2_2 <- hor*dep