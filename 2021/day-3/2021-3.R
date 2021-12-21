# Title:       Day 3: Binary Diagnostic
# Project:     Advent of Code
# Date:        2021-12-21
# Author:      Clare Gibson

# SUMMARY ############################################################
# This script contains the code needed to solve the Day 3
# puzzles in the 2021 Advent of Code challenge.

# PACKAGES ###########################################################
library(tidyverse)      # general data wrangling

# SOURCE QUESTION ####################################################
# https://adventofcode.com/2021/day/3

# READ DATA ##########################################################
# Read Day 3 input into a tibble
d03 <- readLines("2021/day-3/2021-3-data.txt") %>% 
  as_tibble()

# PART 1 #############################################################

# Build a df to calculate answer
df_3_1 <- d03 %>% 
  # split value into separate columns for each bit
  separate(col = value,
           into = c("x1", "x2", "x3", "x4", "x5", "x6",
                    "x7", "x8", "x9", "x10", "x11", "x12"),
           sep = c(1,2,3,4,5,6,7,8,9,10,11,12)) %>% 
  
  # convert character to numeric
  mutate_if(is.character, as.numeric)

# Gamma Rate =========================================================
# Define a function to compute a given bit of the gamma rate
gamma_func <- function(x){
  if(sum(df_3_1[x]) > nrow(df_3_1[x])/2){
    return(1)
  } else {
    return(0)
  }
}

# Calculate each bit of the gamma rate using the function
g1 <- gamma_func("x1")
g2 <- gamma_func("x2")
g3 <- gamma_func("x3")
g4 <- gamma_func("x4")
g5 <- gamma_func("x5")
g6 <- gamma_func("x6")
g7 <- gamma_func("x7")
g8 <- gamma_func("x8")
g9 <- gamma_func("x9")
g10 <- gamma_func("x10")
g11 <- gamma_func("x11")
g12 <- gamma_func("x12")

# Concatenate the gamma bits into a binary number
gamma_bin <- 
  paste0(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12)

# Use the strtoi function to convert to decimal
gamma_dec <- strtoi(gamma_bin, base = 2)

# Epsilon Rate =======================================================
# Invert the gamma bits to find the epsilon bits
epsilon_bin <- gamma_bin %>% 
  str_replace_all("0", "x") %>% 
  str_replace_all("1", "0") %>% 
  str_replace_all("x", "1")
  
# Use the strtoi function to convert to decimal
epsilon_dec <- strtoi(epsilon_bin, base = 2)

# calculate final answer as product of gamma_dec and epsilon_dec
answer_3_1 <- gamma_dec * epsilon_dec

# PART 2 #############################################################
