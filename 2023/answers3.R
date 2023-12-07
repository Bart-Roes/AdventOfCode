setwd("../2023")
input = readr::read_lines("input3.txt") 

library(purrr)
library(dplyr)
library(stringr)

# 3a
all_points = input |> str_extract_all(".")

grid = matrix(ncol =  140, nrow = 140)
for (i in 1:nrow(grid)){
  grid[i,] = all_points[[i]]
}

symbols = all_points |> unlist() |> unique() |> 
  str_remove_all("[.]|[0-9]") |> unique() |> c()

symbol_loc = matrix(grid %in% symbols, nrow = 140,ncol = 140, F)
symbol_arr = which(x = symbol_loc, arr.ind = T)

all_number_value = foreach(i = 1:140, .combine = bind_rows) %do% {
  nrs = all_points[[i]] |> str_c(collapse = "") |> str_extract_all("[0-9]+") |> unlist()
  nrs_loc = all_points[[i]] |> str_c(collapse = "") |> str_locate_all("[0-9]+")  
  
  number_lookup = tibble(
    nrs = nrs,
    row = i,
    col_start = nrs_loc[[1]][,1],
    col_end = nrs_loc[[1]][,2]
  )
}


adj_number = foreach(i = 1:nrow(symbol_arr), .combine = bind_rows) %do% {
  test = symbol_arr[i,]
  all_number_value |> 
    filter(
      row == test["row"] & (test["col"] == col_start-1 | test["col"] == col_end+1) |
        row == test["row"]+1 & (
          between(col_start, test["col"]-1, test["col"]+1) | between(col_end, test["col"]-1, test["col"]+1)
        ) |
        row == test["row"]-1 & (
          between(col_start, test["col"]-1, test["col"]+1) | between(col_end, test["col"]-1, test["col"]+1)
        ) 
    )
}

adj_number |> unique() |> pluck("nrs") |> as.numeric() |> sum()


#3b
symbol_loc = matrix(grid %in% symbols, nrow = 140,ncol = 140, F)
symbol_arr = which(x = symbol_loc, arr.ind = T)
symbol_ind = which(grid %in% symbols, arr.ind = T)

adj_number = foreach(i = 1:nrow(symbol_arr), .combine = bind_rows) %do% {
  test = symbol_arr[i,]
  all_number_value |> 
    filter(
      row == test["row"] & (test["col"] == col_start-1 | test["col"] == col_end+1) |
        row == test["row"]+1 & (
          between(col_start, test["col"]-1, test["col"]+1) | between(col_end, test["col"]-1, test["col"]+1)
        ) |
        row == test["row"]-1 & (
          between(col_start, test["col"]-1, test["col"]+1) | between(col_end, test["col"]-1, test["col"]+1)
        ) 
    ) |> 
    mutate(index = symbol_ind[i])
}

adj_number |> 
  mutate(rn = paste0("val",row_number()), n = n(), .by = index) |> 
  filter(n == 2) |> 
  mutate(nrs = as.numeric(nrs)) |> 
  tidyr::pivot_wider(
    id_cols = index, values_from = nrs, names_from = rn
  ) |> 
  mutate(
    gear_ratio = val1*val2
  ) |> 
  pluck("gear_ratio") |> 
  sum()


