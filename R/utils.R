# Utilities ---------------------------------------------------------------
# Packages and functions to help with Advent of Code

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(stringi)
library(janitor)

# Functions ---------------------------------------------------------------
read_data <- function(year, day, test=FALSE){
  # Takes a 4-digit string representing the challenge year and a 2-digit string 
  # representing the challenge day and returns a tibble containing the required
  # data for that day
  path <- if(test == FALSE) {
    paste0("R/", year, "/data/day", day, ".txt")
  } else {
    paste0("R/", year, "/data/day", day, "_test.txt") 
  }
  
  df <- tibble(input = readLines(here(path)))
  
  return(df)
}

read_data_vec <- function(year, day, test=FALSE){
  # Takes a 4-digit string representing the challenge year and a 2-digit string 
  # representing the challenge day and returns a vector containing the required
  # data for that day
  path <- if(test == FALSE) {
    paste0("R/", year, "/data/day", day, ".txt")
  } else {
    paste0("R/", year, "/data/day", day, "_test.txt") 
  }
  
  input <- readLines(here(path))
  
  return(input)
}
