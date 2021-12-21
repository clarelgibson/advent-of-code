# Title:       Day 3: Binary Diagnostic (Example)
# Project:     Advent of Code
# Date:        2021-12-21
# Author:      Clare Gibson

# SUMMARY ############################################################
# This script contains the code needed to solve the example Day 3
# puzzles in the 2021 Advent of Code challenge.

# PACKAGES ###########################################################
library(tidyverse)      # general data wrangling

# SOURCE QUESTION ####################################################
# https://adventofcode.com/2021/day/3

# READ DATA ##########################################################
# Read Day 3 input into a tibble
d03_ex <- readLines("2021/day-3/2021-3-example-data.txt") %>% 
  as_tibble()

# PART 1 #############################################################

# Build a df to calculate answer
df_3_1_ex <- d03_ex %>% 
  # split value into separate columns for each bit
  separate(col = value,
           into = c("x1", "x2", "x3", "x4", "x5"),
           sep = c(1,2,3,4,5)) %>% 
  
  # convert character to numeric
  mutate_if(is.character, as.numeric)

# Gamma Rate =========================================================
# Define a function to compute a given bit of the gamma rate
gamma_func <- function(x){
  if(sum(df_3_1_ex[x]) > nrow(df_3_1_ex[x])/2){
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

# Concatenate the gamma bits into a binary number
gamma_bin <- 
  paste0(g1,g2,g3,g4,g5)

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
answer_3_1_ex <- gamma_dec * epsilon_dec

# PART 2 #############################################################
# Oxygen Generator Rating ============================================
# Filters each column in turn to return only the rows containing the
# most common value. Stop when only 1 row remains.
oxy_ex <- df_3_1_ex %>% 
  filter(if(nrow(.) > 1) 
    x1 == as.integer(names(which.max(table(x1)))) 
    else TRUE) %>% 
  filter(if(nrow(.) > 1) 
    x2 == as.integer(names(which.max(table(x2))))  
    else TRUE) %>% 
  filter(if(nrow(.) > 1) x3 == g3 else TRUE) %>% 
  filter(if(nrow(.) > 1) x4 == g4 else TRUE) %>% 
  filter(if(nrow(.) > 1) x5 == g5 else TRUE)
