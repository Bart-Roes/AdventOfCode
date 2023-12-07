setwd("../2023")
input = readr::read_lines("input2.txt") 

library(purrr)
library(dplyr)

# 2a
input_list = input |> stringr::str_split(":|;")

get_conditions = function(id, inp = input_list){
  conditions = inp[[id]][-1]
  
  conditions = foreach (i = 1:length(conditions), .combine = bind_rows) %do% {
    
    green = stringr::str_extract(conditions[i], "[0-9]+(?= green)") |> as.numeric()
    red = stringr::str_extract(conditions[i], "[0-9]+(?= red)") |> as.numeric()
    blue = stringr::str_extract(conditions[i], "[0-9]+(?= blue)") |> as.numeric()
    
    tibble(game = id, red = red, green = green, blue = blue) |> 
      tidyr::replace_na(list(
        green = 0, red = 0, blue = 0
      ))
  }
  
  return(conditions)
}

all_conditions = 1:100 |> purrr::map_dfr(get_conditions)

impossible_games = all_conditions |> 
  filter(red > 12 | green > 13 | blue > 14) |> 
  pluck("game") |> unique()

tibble(game = 1:100) |> 
  filter(! game %in% impossible_games) |> 
  pluck("game") |> 
  sum()


# 2b
min_req = all_conditions |> group_by(game) |> 
  summarise(red = max(red), green = max(green), blue = max(blue))

min_req |> 
  mutate(power = red*green*blue) |> 
  pluck("power") |> sum()





