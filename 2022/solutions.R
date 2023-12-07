library(stringr)
library(dplyr)
library(magrittr)
library(tidyr)

# Day 1 ####
my_data <- readr::read_csv("day1.txt", col_names = F, skip_empty_rows = F) 

## Question 1
my_data %>% 
  replace_na(list(X1 = 0)) %>% 
  mutate(
    flg_new_elf = if_else(X1==0,1,0),
    elf_nr = cumsum(flg_new_elf)
  ) %>% 
  group_by(elf_nr) %>% 
  summarise(tot_cal = sum(X1)) %>% 
  arrange(desc(tot_cal))

## Question 2
my_data %>% 
  replace_na(list(X1 = 0)) %>% 
  mutate(
    flg_new_elf = if_else(X1==0,1,0),
    elf_nr = cumsum(flg_new_elf)
  ) %>% 
  group_by(elf_nr) %>% 
  summarise(tot_cal = sum(X1)) %>% 
  arrange(desc(tot_cal)) %>% 
  .$tot_cal %>% 
  # as.character() %>% 
  .[1:3] %>% sum

# Day 2 ####
# Day 3 ####

## Question 1
my_data <- readr::read_csv("day3.txt", col_names = F, skip_empty_rows = F) 

prios <- tibble(letter = c(letters, LETTERS)) %>% 
  mutate(prio = row_number())

my_data %>% 
  mutate(
    n_items = str_length(X1),
    comp1 = str_sub(X1, start = 1, end = n_items/2),
    comp2 = str_sub(X1, start = 1 + n_items/2)
  ) %>% 
  
  mutate(
    letter1 = str_extract_all(comp1, "[a-z|A-Z]{1}", simplify = F),
    letter2 = str_extract_all(comp2, "[a-z|A-Z]{1}", simplify = F),
  ) %>%
  rowwise() %>% 
  mutate(
    duplicate = (function(x ,y) {unlist(x)[which(unlist(x) %in% unlist(y))]}) (letter1, letter2) %>% unique
  ) %>% 
  inner_join(prios, by = c("duplicate" = "letter")) %>% 
  ungroup() %>% 
  summarise(sum(prio))

## Question 2
  
my_data %>% 
  mutate(
    letter1 = str_extract_all(X1, "[a-z|A-Z]{1}", simplify = F),
    group = floor((row_number()-1)/3),
    id = row_number()
  ) %>% 
  group_by(group) %>% 
  mutate(
    check = str_c(unlist(letter1), collapse = "") %>% str_remove(X1) %>% str_extract_all("[a-z|A-Z]{1}", simplify = F),
  ) %>% 
  group_by(group, id) %>% 
  unnest(letter1) %>% 
  mutate(score = letter1 %in% unlist(check)) %>% 
  group_by(group, id, letter1) %>% 
  summarise(score = max(score)) %>% 
  group_by(group, letter1) %>% 
  summarise(score = sum(score), .groups = "drop") %>% 
  filter(score == 3)%>% 
  inner_join(prios, by = c("letter1" = "letter")) %>% 
  summarise(sum(prio))
  
# Day 4 ####

rm(list = ls())
my_data <- readr::read_csv("day4.txt", col_names = F, skip_empty_rows = F) 

## Question 1
my_data %>% 
  mutate(rownum = row_number()) %>% 
  group_by(rownum) %>% 
  mutate(
    list1 = str_extract_all(X1, "[0-9]+"),
    list2 = str_extract_all(X2, "[0-9]+"),
    full1 = list(unlist(list1)[1]:unlist(list1)[2]),
    full2 = list(unlist(list2)[1]:unlist(list2)[2])
  ) %>% 
  mutate(
    test1 = min(unlist(full1)) >= min(unlist(full2)) & max(unlist(full1)) <= max(unlist(full2)),
    test2 = min(unlist(full1)) <= min(unlist(full2)) & max(unlist(full1)) >= max(unlist(full2)),
    flg_overlap = if_else(pmax(test1, test2)==1,1,0)
  ) %>% 
  ungroup() %>% 
  summarise(sum(flg_overlap))

## Question 2
my_data %>% 
  mutate(rownum = row_number()) %>% 
  group_by(rownum) %>% 
  mutate(
    list1 = str_extract_all(X1, "[0-9]+"),
    list2 = str_extract_all(X2, "[0-9]+"),
    full1 = list(unlist(list1)[1]:unlist(list1)[2]),
    full2 = list(unlist(list2)[1]:unlist(list2)[2])
  ) %>% 
  mutate(
    test1 = between(min(unlist(full1)), min(unlist(full2)), max(unlist(full2))),
    test2 = between(max(unlist(full1)), min(unlist(full2)), max(unlist(full2))),
    test3 = between(min(unlist(full2)), min(unlist(full1)), max(unlist(full1))),
    test4 = between(max(unlist(full2)), min(unlist(full1)), max(unlist(full1))),
    flg_overlap = if_else(pmax(test1, test2, test3, test4)==1,1,0)
  ) %>% 
  ungroup() %>% 
  summarise(sum(flg_overlap))



# Day 8 ####

rm(list = ls())
my_data <- readr::read_csv("day8.txt", col_names = F, skip_empty_rows = F) 

grid <- matrix(data = 0, nrow = nrow(my_data), ncol = nrow(my_data))
for (i in 1:nrow(my_data)){
  grid[i,] <- my_data$X1[i] %>% str_extract_all("[0-9]") %>% unlist() %>% as.numeric()
}

# Question 1

check_height <- function(row, col, grid){
  
  height = grid[row,col]
  
  if(row == 1 | col == 1 | row == nrow(grid) | col == ncol(grid)){
    visible = TRUE
  } else {
    numbers_left <- grid[row, 1:(col-1)]
    numbers_right <- grid[row, (col+1):ncol(grid)]
    numbers_up <- grid[1:(row-1), col]
    numbers_down <- grid[(row+1):nrow(grid), col]    
    
    visible <- height > pmin(
      max(numbers_left), max(numbers_right),
      max(numbers_up), max(numbers_down)
    )
  }

  return(visible)
}

grid_check <- expand_grid(
  row = 1:nrow(grid), col = 1:ncol(grid)
) 

grid_check$visible <- 
  purrr::pmap(.l = list(row = grid_check$row, col = grid_check$col), 
              .f = check_height, grid = grid) %>% unlist

grid_check$visible %>% sum

# Question 2

check_scenic <- function(row, col, grid){
  
  height = grid[row,col]
  numbers_left <- if(col-1 >= 1) grid[row, (col-1):1] else numeric(0)
  numbers_right <- if(col+1 <= ncol(grid)) grid[row, (col+1):ncol(grid)] else numeric(0)
  numbers_up <- if(row-1 >= 1) grid[(row-1):1, col] else numeric(0)
  numbers_down <- if(row+1 <= nrow(grid)) grid[(row+1):nrow(grid), col] else numeric(0)
  
  check_left <- which(height <= numbers_left) %>% min
  check_right <- which(height <= numbers_right) %>% min
  check_up <- which(height <= numbers_up) %>% min
  check_down <- which(height <= numbers_down) %>% min
  
  score = ifelse(is.finite(check_left), check_left,(col-1)) *
    ifelse(is.finite(check_right), check_right,(ncol(grid)-col)) *
    ifelse(is.finite(check_up), check_up,(row-1)) *
    ifelse(is.finite(check_down), check_down,(nrow(grid)-row)) 
  
  return(score)  
}

suppressWarnings(
  grid_check$score <- 
    purrr::pmap(.l = list(row = grid_check$row, col = grid_check$col), 
                .f = check_scenic, grid = grid) %>% unlist
)

grid_check$score %>% max
check_scenic(1,1,grid)






# Day 10 ####

rm(list = ls())
my_data <- readr::read_csv("day10.txt", col_names = F, skip_empty_rows = F) 

my_data <- my_data %>% 
  mutate(
    instruction = str_extract(X1, "[a-z]+"),
    value = str_extract(X1, "[^a-z]+") %>% trimws() %>% as.double(),
    duration = if_else(instruction == "noop",1,2),
    cycle_start = 1 + cumsum(duration) - duration,
    cycle_end = cumsum(duration)
  ) 

# Question 1
values <- vector(mode = "numeric", length = sum(my_data$duration)+1)
values[1] <- 1

for(i in 1:sum(my_data$duration)) {
  
  value_start = values[i]
  row = my_data %>% filter(cycle_start <= i, cycle_end >= i)
  
  if (i != row$cycle_end | is.na(row$value)){
    value_end <- value_start 
  } else {
    value_end <- value_start + row$value
  }
  
  values[i+1] <- value_end 
}

tibble(value = values) %>% 
  mutate(cycle = row_number()) %>% 
  mutate(score = cycle * value) %>% 
  filter(
    cycle %in% seq(from = 20, to = 220, by = 40)
  ) %>% 
  .$score %>% sum


# Question 2

values <- vector(mode = "numeric", length = sum(my_data$duration)+1)
values[1] <- 1
image_grid = matrix(data = ".", nrow = 6, ncol = 40)
sprite_pos = c(1,2,3)

for(i in 1:sum(my_data$duration)) {
  
  value_start = values[i]
  row = my_data %>% filter(cycle_start <= i, cycle_end >= i)
  
  if (i != row$cycle_end | is.na(row$value)){
    value_end <- value_start 
  } else {
    value_end <- value_start + row$value
  }
  
  if(i %% 40 == 0 & i %in% sprite_pos){
    image_grid[(i/40),40] <- "#"
  } else if(i %% 40 == 0 & !(i %in% sprite_pos)){
    image_grid[(i/40),40] <- "."
  } else if((i %% 40) %in% sprite_pos){
    image_grid[ceiling(i/40),(i %% 40)] <- "#"
  } else {
    image_grid[ceiling(i/40),(i %% 40)] <- "."
  }
  
  values[i+1] <- value_end 
  if(value_end != value_start){
    sprite_pos = c(value_end, (value_end+1), (value_end+2))
  }
    
}

# Day 11 ####

rm(list = ls())
my_data <- readr::read_csv("day11.txt", col_names = F, skip_empty_rows = F) 

monkeys <- my_data %>% 
  filter(str_detect(X1, "Monkey")) %>% 
  .$X1 %>% str_extract_all("[0-9]") %>% paste0("m",.)

monkey_attr <- vector("list", length = length(monkeys))
names(monkey_attr) <- monkeys

for (i in 1:length(monkey_attr)){
  monkey_attr[[i]]$bag <- my_data %>% 
    filter(str_detect(X1, "Starting")) %>% 
    .$X1 %>% str_extract_all("[0-9]+") %>% .[[i]] %>% as.numeric()
  
  monkey_attr[[i]]$fun <- my_data %>% 
    filter(str_detect(X1, "Operation")) %>% 
    .$X1 %>% str_extract_all("new.*") %>% .[[i]]

  monkey_attr[[i]]$divisor <- my_data %>% 
    filter(str_detect(X1, "Test")) %>% 
    .$X1 %>% str_extract_all("[0-9]+") %>% .[[i]] %>% as.numeric()
  
  monkey_attr[[i]]$if_true <-my_data %>% 
    filter(str_detect(X1, "true")) %>% 
    .$X1 %>% str_extract_all("[0-9]") %>% .[[i]] %>% as.numeric() + 1
  
  monkey_attr[[i]]$if_false <-my_data %>% 
    filter(str_detect(X1, "false")) %>% 
    .$X1 %>% str_extract_all("[0-9]") %>% .[[i]] %>% as.numeric() + 1
}

# Question 1
inspect_list <- tibble(monkey = numeric(0))

fun_round <- function(attributes, inspections){
  
  for (i in 1:length(attributes)){
    items <- attributes[[i]]$bag
    
    if(length(items) > 0){
      for (k in items){
        old = k
        eval(str2expression(attributes[[i]]$fun))
        new = new %/% 3
        
        if (new %% attributes[[i]]$divisor == 0){
          chosen_monkey <- attributes[[i]]$if_true
        } else {
          chosen_monkey <- attributes[[i]]$if_false
        }
        
        attributes[[chosen_monkey]]$bag <- append(attributes[[chosen_monkey]]$bag, values = new)
        inspections <- inspections %>% bind_rows(tibble(monkey = i-1))
      }
    }
    
    # return empty bag
    attributes[[i]]$bag <- numeric(0)
  }
  
  return(list(attr = attributes, insp = inspections))
}

results <- list(attr = monkey_attr, insp = inspect_list)

for (i in 1:20){
  results <- fun_round(attributes = results$attr, inspections = results$insp)
}

results$insp %>% 
  group_by(monkey) %>% count %>% 
  arrange(desc(n)) 

253*247




rm(list = ls())
my_data <- readr::read_csv("day11_test.txt", col_names = F, skip_empty_rows = F) 

monkeys <- my_data %>% 
  filter(str_detect(X1, "Monkey")) %>% 
  .$X1 %>% str_extract_all("[0-9]") %>% paste0("m",.)

monkey_attr <- vector("list", length = length(monkeys))
names(monkey_attr) <- monkeys

for (i in 1:length(monkey_attr)){
  monkey_attr[[i]]$bag <- my_data %>% 
    filter(str_detect(X1, "Starting")) %>% 
    .$X1 %>% str_extract_all("[0-9]+") %>% .[[i]] %>% as.numeric()
  
  monkey_attr[[i]]$fun <- my_data %>% 
    filter(str_detect(X1, "Operation")) %>% 
    .$X1 %>% str_extract_all("new.*") %>% .[[i]]
  
  monkey_attr[[i]]$divisor <- my_data %>% 
    filter(str_detect(X1, "Test")) %>% 
    .$X1 %>% str_extract_all("[0-9]+") %>% .[[i]] %>% as.numeric()
  
  monkey_attr[[i]]$if_true <-my_data %>% 
    filter(str_detect(X1, "true")) %>% 
    .$X1 %>% str_extract_all("[0-9]") %>% .[[i]] %>% as.numeric() + 1
  
  monkey_attr[[i]]$if_false <-my_data %>% 
    filter(str_detect(X1, "false")) %>% 
    .$X1 %>% str_extract_all("[0-9]") %>% .[[i]] %>% as.numeric() + 1
}

# Question 2
inspect_list <- tibble(monkey = numeric(0))

fun_round <- function(attributes, inspections, round){
  
  if (round == 1) {
    for (m in 1:length(attributes)){
      attributes[[m]]$bag = log(attributes[[m]]$bag)
    }
  }
  
  for (i in 1:length(attributes)){
    items <- attributes[[i]]$bag
    
    if(length(items) > 0){
      for (k in items){
        old = exp(k)
        eval(str2expression(attributes[[i]]$fun))
        # new = new %/% 3
        
        if (new %% attributes[[i]]$divisor == 0){
          chosen_monkey <- attributes[[i]]$if_true
        } else {
          chosen_monkey <- attributes[[i]]$if_false
        }
        
        attributes[[chosen_monkey]]$bag <- append(attributes[[chosen_monkey]]$bag, values = log(new))
        inspections <- inspections %>% bind_rows(tibble(monkey = i-1))
      }
    }
    
    # return empty bag
    attributes[[i]]$bag <- numeric(0)
  }
  
  return(list(attr = attributes, insp = inspections))
}

results <- list(attr = monkey_attr, insp = inspect_list)

for (i in 1:1000){
  results <- fun_round(attributes = results$attr, inspections = results$insp, round = i)
}

results$insp %>% 
  group_by(monkey) %>% count %>% 
  arrange(desc(n)) 

253*247

# Day 12 ####

rm(list = ls())
my_data <- readr::read_lines("Day12.txt") %>% as_tibble()

my_grid <- matrix(
  data = my_data$value %>% str_extract_all("", simplify = T), nrow = nrow(my_data), ncol = nchar(my_data$value[1])
)
  
test_data <- "SabqponmabcryxxlaccszExkacctuvwjabdefghi"
test_grid <- matrix(
  data = test_data %>% str_extract_all("", simplify = T), nrow = 5, ncol =8, byrow = T
)

my_grid <- my_grid %>% as_tibble %>% 
  mutate(
    across(.cols = everything(), Vectorize(function(x) if(x == "S") 0 else if (x == "E") 27 else which(letters == x)))
  ) %>% 
  mutate(
    across(.cols = everything(), as.numeric)
  )

check_slope <- function(x, y, grid){
  
  curr_height <- grid[x,y] %>% as.numeric()
  
  if(y == 1){
    left_height = 999
  } else {
    left_height = grid[x,y-1] %>% as.numeric()
  }
  
  if(y == ncol(grid)){
    right_height = 999
  } else {
    right_height = grid[x,y+1] %>% as.numeric()
  }
  
  if(x == 1){
    up_height = 999
  } else {
    up_height = grid[x-1,y] %>% as.numeric()
  }
  
  if(x == nrow(grid)){
    down_height = 999
  } else {
    down_height = grid[x+1,y] %>% as.numeric()
  }
  
  connections <- tibble(x = numeric(), y = numeric(), x_link = numeric(), y_link = numeric())
  if (right_height <= curr_height + 1) connections <- connections %>% bind_rows(tibble(x = x,y = y, x_link = x, y_link = y+1))
  if (left_height <= curr_height + 1) connections <- connections %>% bind_rows(tibble(x = x,y = y, x_link = x, y_link = y-1))
  if (up_height <= curr_height + 1) connections <- connections %>% bind_rows(tibble(x = x,y = y, x_link = x-1, y_link = y))
  if (down_height <= curr_height + 1) connections <- connections %>% bind_rows(tibble(x = x,y = y, x_link = x+1, y_link = y))
  
  return(connections)
}

all_connections <- tibble(x = numeric(), y = numeric(), x_link = numeric(), y_link = numeric())
indices <- expand_grid(
  x = 1:nrow(my_grid), y = 1:ncol(my_grid)
) %>% mutate(rn=row_number())

for (i in 1:nrow(indices)){
  all_connections <- bind_rows(all_connections, check_slope(x = indices$x[i], y = indices$y[i], grid = my_grid))
}

all_connections <- all_connections %>% 
  inner_join(indices, by = c("x", "y")) %>% 
  rename(from = rn) %>% 
  inner_join(indices, by = c("x_link"="x", "y_link"="y")) %>% 
  rename(to = rn)

library(igraph)

my_graph <- all_connections %>% 
  select(from, to) %>% 
  graph_from_data_frame() 

path_length <- function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  pairs <- cbind(values = path[-length(path)], ind = path[-1])
  length(pairs)
}
find_shortest_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)
  
  # base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]][[1]]) {
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)
      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # return shortest path
  shortest
}

start <- which(my_grid == 0, arr.ind = T)
end <- which(my_grid == 27, arr.ind = T)

result = find_shortest_path(
  my_graph,
  indices %>% filter(x == start[1], y == start[2]) %>% magrittr::extract2("rn"),
  indices %>% filter(x == end[1], y == end[2]) %>% magrittr::extract2("rn"),
  c()
)

-1 + result %>% length



# Day 25 ####

rm(list = ls())
options(scipen = 9999)
my_data <- readr::read_lines("Day25.txt") %>% as_tibble()

test <- my_data$value[1]
5^nchar(test)

test <- "1=-0-2"

fun_score <- function(item){
  if (item == "="){result = -2}
  else if (item == "-"){result = -1}
  else {result = as.numeric(item)}
  return(result)
}

fun_sum <- function(string){
  outcome <- numeric(nchar(string))
  for (i in 1:nchar(string)){
    item <- substr(string, nchar(string)-(i-1), nchar(string)-(i-1))
    outcome[i] <- fun_score(item) * 5^(i-1)
  }
  return(sum(outcome))
}

fun_sum <- Vectorize(fun_sum)

total <- my_data %>% 
  mutate(score = fun_sum(value)) %>% 
  magrittr::extract(2) %>% sum

total - 
  (2*5^19) - 
  (0*5^18) - 
  (-1*5^17) - 
  (-2*5^16) - 
  (-2*5^15) - 
  (0*5^14) - 
  (1*5^13) - 
  (-1*5^12) - 
  (2*5^11) - 
  (-1*5^10) - 
  (-2*5^09) - 
  (1*5^08) - 
  (-1*5^07) - 
  (2*5^06) - 
  (-1*5^05) - 
  (-1*5^04) - 
  (-1*5^03) - 
  (1*5^02) - 
  (-1*5^01) - 
  (0*5^00)

# test
fun_sum("20-==01-2-=1-2---1-0") - total


