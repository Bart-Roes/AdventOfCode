setwd("./2023")
input = readr::read_lines("input4.txt") 

library(purrr)
library(dplyr)
library(stringr)
library(foreach)

# 1a
get_score = function(string){

  split = string |> str_split("[|:]")|> unlist()
  win = split[2] |> str_extract_all("[0-9]+") |> unlist() |> as.numeric()
  nrs = split[3] |> str_extract_all("[0-9]+") |> unlist() |> as.numeric()
  n_match = sum(nrs %in% win)
  score = if(n_match == 0) 0 else 2^(n_match-1)
  
  return(score)
}

get_score_vec = Vectorize(get_score)
results = get_score_vec(input)

results |> sum()

# 1b

get_matches = function(string){
  split = string |> str_split("[|:]")|> unlist()
  id = split[1]
  win = split[2] |> str_extract_all("[0-9]+") |> unlist() |> as.numeric()
  nrs = split[3] |> str_extract_all("[0-9]+") |> unlist() |> as.numeric()
  n_match = sum(nrs %in% win)
  
  result = tibble(card = id, matches = n_match)
  return(result)
}

match_table = input |> map_dfr(get_matches) |> 
  mutate(card_number = card |> str_extract("[0-9]+") |> as.numeric())

update_set = match_table |> mutate(n = 1)
remaining_set = update_set

for(i in 1:nrow(starting_set)){
  
  pick_card = remaining_set |> filter(card_number == i)
  
  if(pick_card$matches > 0){
    copies = rep((1+i):(i+pick_card$matches), pick_card$n)
    copies = tibble(card_number = copies) |> group_by(card_number) |> summarise(add = n())
    update_set = update_set |> left_join(copies, by = join_by(card_number)) |> 
      mutate(n = n + if_else(is.na(add),0,add)) |> select(-add)
  }
  remaining_set = update_set |> filter(card_number > i) 
  if (remaining_set$matches |> sum() == 0) break

}

update_set$n |> sum()


