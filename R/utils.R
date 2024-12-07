# Utilities ---------------------------------------------------------------
# Packages and functions to help with Advent of Code

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(stringi)
library(janitor)
library(googlesheets4)

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

test_reports <- function(reports){
  # Takes a dataframe of reports and returns a dataframe indicating if each
  # report is safe
  tests <- 
    reports |> 
    mutate(
      lag = value - lag(value),
      .by = report_id
    ) |> 
    mutate(
      lag_type = case_when(
        is.na(lag) ~ NA,
        lag < 0 ~ "dec",
        lag == 0 ~ "none",
        lag > 0 ~ "inc",
        TRUE ~ "error"
      )
    ) |> 
    group_by(report_id) |> 
    summarise(
      n_types = n_distinct(lag_type, na.rm = TRUE),
      min_lag = min(abs(lag), na.rm = TRUE),
      max_lag = max(abs(lag), na.rm = TRUE),
      is_safe = case_when(
        n_types == 1 & min_lag >= 1 & max_lag <=3 ~ 1,
        TRUE ~ 0
      )
    ) |> 
    ungroup()
  
  return(tests)
}