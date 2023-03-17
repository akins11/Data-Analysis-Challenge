library(tidyverse)
library(stringi)
library(janitor)
library(lubridate)

raw_data <- read_csv("data/fifa21 raw data v2.csv")

# custom functions ------------------------------------------------------------>

convert_to_cm <- function(str) {
  
  map(str, function(s) {
    if (stringr::str_detect(s, "'")) {
      feet_inches <- stringr::str_split(s, "'")[[1]]
      
      feet_inches[2] <- stringr::str_remove(feet_inches[2], "\"") #|> as.vector()
      
      (as.numeric(feet_inches[1]) * 12 + as.numeric(feet_inches[2])) * 2.54
      
    } else {
      s
    }
  }) |>
    unlist()
  
}

convert_to_kg <- function(str) {
  
  map(str, function(s) {
    if (stringr::str_detect(s, "lbs")) {
      
      lbs <- stringr::str_remove(s, "lbs") |> as.numeric()
      
      round(lbs * 0.45359237, 2)
      
    } else {
      s
    }
  }) |>
    unlist()
}



# Data cleaning

fifa_21 <- edit_raw |> 

  janitor::clean_names() |>
  
  mutate(club = str_remove_all(club, "\\n") |> stri_trans_general("Latin-ASCII"))  |>
  
  separate(col = contract, 
           into = c("start_year", "end_year"), 
           sep = " ~ ",
           extra = "merge",
           remove = FALSE) |>
  mutate(contract_type = case_when(str_detect(start_year, "Loan") ~ "Loan",
                              start_year == "Free" ~ "Free",
                              TRUE ~ "Permanent"),
    
        end_year = case_when(str_detect(start_year, "Loan") ~ str_extract(start_year, "\\d{4}"),
                             TRUE ~ end_year),
    
        start_year = case_when(contract == "Free" ~ NA_character_, 
                               str_detect(start_year, "Loan") ~ NA_character_,
                               TRUE ~ start_year)
  ) |> 
  mutate(across(c(start_year, end_year), as.integer)) |>
  
  mutate(joined = mdy(joined),
         loan_date_end = mdy(loan_date_end)) |>
  rename(loan_end_date = loan_date_end, joined_national_team = joined)
  
  mutate(across(c(value, wage, release_clause), function(col) {
    case_when(
      str_detect(col, "M") ~ as.double(str_replace_all(col, "(€|M)", "")) * 1000000,
      str_detect(col, "K") ~ as.double(str_replace_all(col, "(€|K)", "")) * 1000,
      TRUE ~ as.double(str_replace(col, "€", ""))
    )
  })) |>
  
  mutate(height = convert_to_cm(height) |> str_remove("cm") |> as.double(),
         weight = convert_to_kg(weight) |> str_remove("kg") |> as.double()) |>
  rename(height_cm = height, weight_kg = weight) |>
  
  mutate(across(c(long_name, name), \(n) stri_trans_general(n, "Latin-ASCII"))) |>
  
  mutate(nationality = stri_trans_general(nationality, "Latin-ASCII"),
         nationality = str_replace(nationality, "&", "and"),
         nationality = if_else(nationality == "Korea DPR", "Korea DPRK", nationality)) |>
  
  mutate(across(c(w_f, sm, ir), \(s) str_extract(s, "[:digit:]") |> as.integer())) |>
  rename(weak_foot = w_f, skill_move = sm, injury_rating = ir) |>
  
  mutate(hit_num = str_replace(hits, "K", "") |> as.double(),
         hits = case_when(str_detect(hits, "K") ~ as.character(hit_num * 1000),
                          TRUE ~ hits) |> as.double()) |>
  select(-hit_num)



# position table --------------------------------------------------------------|
player_position <-  fifa_21 |> 
  select(id) |>
  bind_cols(
    fifa_21$positions |>
      str_split(" ", simplify = TRUE) |>
      as_tibble(.name_repair = "universal")
  ) |>
  pivot_longer(cols = `...2`:`...4`,    
               names_to = "num_position", 
               values_to = "position") |>
  filter(position != "") |>
  mutate(position = str_remove(position, ",")) |>
  select(-num_position)



# Save Data -------------------------------------------------------------------|
write_csv(fifa_21, "data/fifa21_clean_data.csv")
write_csv(player_position, "data/player_position.csv")
