library(tidyverse)
library(here)

# Reading in data
input_data <- read_csv(here("day1", "kasia.txt"), skip_empty_rows = FALSE,
                       col_names = c("calories"))

# Parsing

# Approach 1: add a new column that indicates which elf each load belongs to

list_of_elves <- input_data %>% 
  mutate(calories = case_when(is.na(calories) ~ "boo",
                              TRUE ~ as.character(calories))) %>% 
  pull(calories) %>% 
  toString() %>% 
  str_split(pattern = ", boo, ")

df_of_elves <- list_of_elves %>% 
  unlist() %>% 
  enframe() %>% 
  separate(col = value, sep = ", ")
  mutate(sum_calories = sum(as.numeric(value)))

# Approach 2: mimic Python and create a list of vectors

input_data_vector <- input_data %>% 
  pull(calories)

create_elf_bags <- function(elf_collection_vector){
  current_elf_bag <- c()
  elves <- list()
  for (current_snack in elf_collection_vector){
    if (is.na(current_snack)) {
      elves <- append(elves, list(current_elf_bag))
      current_elf_bag <- c()
    } else {
        current_elf_bag <- append(current_elf_bag, current_snack)
    }
  }
  elves <- append(elves, list(current_elf_bag))
  return(elves)
}






elf_bags <- create_elf_bags(input_data_vector)


df_elves <-elf_bags %>% 
  enframe() %>%
  rowwise()  %>% 
  mutate(calo_sum = sum(value)) %>%
  arrange(calo_sum)

largest <- df_elves %>%
  tail(n=1) %>%
  select(calo_sum)

largest3 <- df_elves %>%
  tail(n=3) %>%
  pull(calo_sum) %>%
  sum()
  
  

demo_data <- c(1, 2, 3, NA, 4, 5, 6, NA, 7, 8)
df_demo <- data.frame(calory = demo_data)

create_elf_numbers <- function(elf_collection_vector){
  elf_number <- 1
  all_numbers <- c()
  for (current_snack in elf_collection_vector){
    if (is.na(current_snack)) {
      elf_number <- elf_number +1
    }else{
       all_numbers <- c(all_numbers,elf_number)
    }
  }
  return(all_numbers)
}

which_elf_number <- create_elf_numbers(input_data$calories)
long_elves <- input_data %>% 
  drop_na() %>% 
  mutate(which_elf = which_elf_number)
