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

# Build a df to calculate answer
df03_ex <- d03_ex %>% 
  # split value into separate columns for each bit
  separate(col = value,
           into = c("x1", "x2", "x3", "x4", "x5"),
           sep = c(1:5)) %>% 
  # convert character to numeric
  mutate_if(is.character, as.numeric)

# PART 1 #############################################################
# Gamma Rate =========================================================
mcb <- function(df, x){
  # mcb return the Most Common Bit (either 0 or 1) from a given range
  # of bits. If 0 and 1 are equally common mcb returns 1.
  if(sum(df[x]) >= nrow(df[x])/2){
    return(1)
  } else {
    return(0)
  }
}

# Calculate each bit of the gamma rate using the mcb function
g1_ex <- mcb(df03_ex, "x1")
g2_ex <- mcb(df03_ex, "x2")
g3_ex <- mcb(df03_ex, "x3")
g4_ex <- mcb(df03_ex, "x4")
g5_ex <- mcb(df03_ex, "x5")

# Concatenate the gamma bits into a binary number
gamma_bin_ex <- paste0(g1_ex,
                       g2_ex,
                       g3_ex,
                       g4_ex,
                       g5_ex)

# Use the strtoi function to convert to decimal
gamma_dec_ex <- strtoi(gamma_bin_ex, base = 2)

# Epsilon Rate =======================================================
lcb <- function(df, x){
  # lcb return the Least Common Bit (either 0 or 1) from a given
  # range of bits. If 0 and 1 are equally common lcb returns 0.
  if(sum(df[x]) >= nrow(df[x])/2){
    return(0)
  } else {
    return(1)
  }
}

# Calculate each bit of the epsilon rate using the lcb function
e1_ex <- lcb(df03_ex, "x1")
e2_ex <- lcb(df03_ex, "x2")
e3_ex <- lcb(df03_ex, "x3")
e4_ex <- lcb(df03_ex, "x4")
e5_ex <- lcb(df03_ex, "x5")

# Concatenate the epsilon bits into a binary number
epsilon_bin_ex <- paste0(e1_ex,
                         e2_ex,
                         e3_ex,
                         e4_ex,
                         e5_ex)
  
# Use the strtoi function to convert to decimal
epsilon_dec_ex <- strtoi(epsilon_bin_ex, base = 2)

# calculate final answer as product of gamma_dec and epsilon_dec
answer_3_1_ex <- gamma_dec_ex * epsilon_dec_ex

# PART 2 #############################################################
# Oxygen Generator Rating ============================================
# Filters each column in turn to return only the rows containing the
# most common value. Stop when only 1 row remains.

# Initialise a df for oxygen generator rating
oxy_ex <- df03_ex

# Iteratively filter until only one row
# X1
oxy_ex <- oxy_ex %>% 
  filter(if(nrow(.) > 1) 
    x1 == mcb(oxy_ex, "x1")
    else TRUE)
# X2
oxy_ex <- oxy_ex %>% 
  filter(if(nrow(.) > 1) 
    x2 == mcb(oxy_ex, "x2")  
    else TRUE)
# X3
oxy_ex <- oxy_ex %>% 
  filter(if(nrow(.) > 1)
    x3 == mcb(oxy_ex, "x3")
    else TRUE)
# X4
oxy_ex <- oxy_ex %>% 
  filter(if(nrow(.) > 1)
    x4 == mcb(oxy_ex, "x4")
    else TRUE)
# X5
oxy_ex <- oxy_ex %>% 
  filter(if(nrow(.) > 1)
    x5 == mcb(oxy_ex, "x5")
    else TRUE)

# Concatenate the found row into a single string
oxy_bin_ex <- oxy_ex %>% 
  unite(binary, everything(), sep="")

# Use the strtoi function to convert to decimal
oxy_dec_ex <- strtoi(oxy_bin_ex, base = 2)

# CO2 Scrubber Rating ===============================================
# Filters each column in turn to return only the rows containing the
# least common value. Stop when only 1 row remains.

# Initialise a df for oxygen generator rating
co2_ex <- df03_ex

# Iteratively filter until only one row
# X1
co2_ex <- co2_ex %>% 
  filter(if(nrow(.) > 1) 
    x1 == lcb(co2_ex, "x1")
    else TRUE)
# X2
co2_ex <- co2_ex %>% 
  filter(if(nrow(.) > 1) 
    x2 == lcb(co2_ex, "x2")  
    else TRUE)
# X3
co2_ex <- co2_ex %>% 
  filter(if(nrow(.) > 1)
    x3 == lcb(co2_ex, "x3")
    else TRUE)
# X4
co2_ex <- co2_ex %>% 
  filter(if(nrow(.) > 1)
    x4 == lcb(co2_ex, "x4")
    else TRUE)
# X5
co2_ex <- co2_ex %>% 
  filter(if(nrow(.) > 1)
    x5 == lcb(co2_ex, "x5")
    else TRUE)

# Concatenate the found row into a single string
co2_bin_ex <- co2_ex %>% 
  unite(binary, everything(), sep="")

# Use the strtoi function to convert to decimal
co2_dec_ex <- strtoi(co2_bin_ex, base = 2)

# calculate final answer as product of oxy_dec and co2_dec
answer_3_2_ex <- oxy_dec_ex * co2_dec_ex