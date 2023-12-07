setwd("./2023")
input = readr::read_lines("input1.txt")

# 1a
input_digits = input |> stringr::str_extract_all("[0-9]") 

get_calibration = function(digit_list){
  n = digit_list |> length()
  cal = paste0(digit_list[1], digit_list[n], collapse = "")
  return(as.numeric(cal))
}

results = lapply(
  input_digits, get_calibration
)

results |> unlist() |> sum()

# 1b
number_lookup = tibble::tibble(
  number = 1:9 |> as.character(),
  word = english::words(1:9)
)

library(dplyr)

convert_number = function(string, cal = get_calibration, lookup = number_lookup){
  string_out = string
  
  number_positions = lookup |> group_by(number) |>   
    mutate(pos = stringr::str_locate(string_out, word)[1]) 
  
  while( any(!is.na(number_positions$pos)) ){
    nr = number_positions |> filter(pos == min(number_positions$pos, na.rm = T))
    string_out = string_out |> stringr::str_replace(nr$word, nr$number)
    
    number_positions = lookup |> group_by(number) |>   
      mutate(pos = stringr::str_locate(string_out, word)[1]) 
  }
  
  digits = stringr::str_extract_all(string_out, "[0-9]") 
  return(digits)
}

convert_number_vec = Vectorize(convert_number)
results = convert_number_vec(input)

get_calibration(results[[1]])


out = lapply(
  results, get_calibration
)

out |> unlist() |> sum()



