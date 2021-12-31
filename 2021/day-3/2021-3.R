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

# Build a df to calculate answer
df03 <- d03 %>% 
  # split value into separate columns for each bit
  separate(col = value,
           into = paste0("x",1:12),
           sep = c(1:12)) %>% 
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

# Calculate and concatenate the gamma bits into a binary number
gamma_bin <- paste0(mcb(df03, "x1"),
                    mcb(df03, "x2"),
                    mcb(df03, "x3"),
                    mcb(df03, "x4"),
                    mcb(df03, "x5"),
                    mcb(df03, "x6"),
                    mcb(df03, "x7"),
                    mcb(df03, "x8"),
                    mcb(df03, "x9"),
                    mcb(df03, "x10"),
                    mcb(df03, "x11"),
                    mcb(df03, "x12"))

# Use the strtoi function to convert to decimal
gamma_dec <- strtoi(gamma_bin, base = 2)

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

# Calculate and concatenate the epsilon bits into a binary number
epsilon_bin <- paste0(lcb(df03, "x1"),
                      lcb(df03, "x2"),
                      lcb(df03, "x3"),
                      lcb(df03, "x4"),
                      lcb(df03, "x5"),
                      lcb(df03, "x6"),
                      lcb(df03, "x7"),
                      lcb(df03, "x8"),
                      lcb(df03, "x9"),
                      lcb(df03, "x10"),
                      lcb(df03, "x11"),
                      lcb(df03, "x12"))
  
# Use the strtoi function to convert to decimal
epsilon_dec <- strtoi(epsilon_bin, base = 2)

# calculate final answer as product of gamma_dec and epsilon_dec
answer_3_1 <- gamma_dec * epsilon_dec

# PART 2 #############################################################
# Oxygen Generator Rating ============================================
# Filters each column in turn to return only the rows containing the
# most common value. Stop when only 1 row remains.

# Initialise a df for oxygen generator rating
oxy <- df03

# Iteratively filter until only one row
# X1
oxy <- oxy %>% 
  filter(if(nrow(.) > 1) 
    x1 == mcb(oxy, "x1")
    else TRUE)
# X2
oxy <- oxy %>% 
  filter(if(nrow(.) > 1) 
    x2 == mcb(oxy, "x2")  
    else TRUE)
# X3
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x3 == mcb(oxy, "x3")
    else TRUE)
# X4
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x4 == mcb(oxy, "x4")
    else TRUE)
# X5
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x5 == mcb(oxy, "x5")
    else TRUE)
# X6
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x6 == mcb(oxy, "x6")
    else TRUE)
# X7
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x7 == mcb(oxy, "x7")
    else TRUE)
# X8
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x8 == mcb(oxy, "x8")
    else TRUE)
# X9
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x9 == mcb(oxy, "x9")
    else TRUE)
# X10
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x10 == mcb(oxy, "x10")
    else TRUE)
# X11
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x11 == mcb(oxy, "x11")
    else TRUE)
# X12
oxy <- oxy %>% 
  filter(if(nrow(.) > 1)
    x12 == mcb(oxy, "x12")
    else TRUE)

# Concatenate the found row into a single string
oxy_bin <- oxy %>% 
  unite(binary, everything(), sep="")

# Use the strtoi function to convert to decimal
oxy_dec <- strtoi(oxy_bin, base = 2)

# CO2 Scrubber Rating ===============================================
# Filters each column in turn to return only the rows containing the
# least common value. Stop when only 1 row remains.

# Initialise a df for oxygen generator rating
co2 <- df03

# Iteratively filter until only one row
# X1
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x1 == lcb(co2, "x1")
    else TRUE)
# X2
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x2 == lcb(co2, "x2")  
    else TRUE)
# X3
co2 <- co2 %>% 
  filter(if(nrow(.) > 1)
    x3 == lcb(co2, "x3")
    else TRUE)
# X4
co2 <- co2 %>% 
  filter(if(nrow(.) > 1)
    x4 == lcb(co2, "x4")
    else TRUE)
# X5
co2 <- co2 %>% 
  filter(if(nrow(.) > 1)
    x5 == lcb(co2, "x5")
    else TRUE)
# X6
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x6 == lcb(co2, "x6")
    else TRUE)
# X7
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x7 == lcb(co2, "x7")
    else TRUE)
# X8
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x8 == lcb(co2, "x8")
    else TRUE)
# X9
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x9 == lcb(co2, "x9")
    else TRUE)
# X10
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x10 == lcb(co2, "x10")
    else TRUE)
# X11
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x11 == lcb(co2, "x11")
    else TRUE)
# X12
co2 <- co2 %>% 
  filter(if(nrow(.) > 1) 
    x12 == lcb(co2, "x12")
    else TRUE)

# Concatenate the found row into a single string
co2_bin <- co2 %>% 
  unite(binary, everything(), sep="")

# Use the strtoi function to convert to decimal
co2_dec <- strtoi(co2_bin, base = 2)

# calculate final answer as product of oxy_dec and co2_dec
answer_3_2 <- oxy_dec * co2_dec