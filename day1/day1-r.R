library(tidyverse)
library(here)

# Reading in data
input_data <- read_csv(here("day1", "kasia.txt"), skip_empty_rows = FALSE,
                       col_names = c("calories"))

# Approach 1: mimic Python and create a list of vectors

# Create a vector of numbers
input_data_vector <- input_data %>% 
  pull(calories)

# Function to create a list of vectors, where each list element represents the 
# items carried by one elf
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

# Apply this function to turn our vector of numbers into a list of vectors
elf_bags <- create_elf_bags(input_data_vector)

# Turn the list into a df, and calculate sum of calories in each elf's bag
df_elves <-elf_bags %>% 
  enframe() %>%
  rowwise()  %>% 
  mutate(calo_sum = sum(value)) %>%
  arrange(calo_sum) # arrange in ascending order

# Find the elf with the most calories in their bag
largest <- df_elves %>%
  tail(n=1) %>%
  select(calo_sum)

# Find three elves with the most calories in their bags
largest3 <- df_elves %>%
  tail(n=3) %>%
  pull(calo_sum) %>%
  sum()

# Approach 2: Create a new column in the df to indicate which items belong
# to each elf

# Function to create a vector with elf numbers corresponding to the items
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

# Apply the function and obtain a vector
which_elf_number <- create_elf_numbers(input_data$calories)

# Add the new column to the input df
long_elves <- input_data %>% 
  drop_na() %>% 
  mutate(which_elf = which_elf_number)

# Calculate sum of calories for each elf

elf_calories <- long_elves %>% 
  group_by(which_elf) %>% 
  summarise(calorie_sum = sum(calories)) %>% 
  arrange(desc(calorie_sum)) 

# Pull the calories for top elf
top_elf <- elf_calories %>% 
  head(n=1) %>% 
  pull(calorie_sum)

# Pull the calories for top three elves
top_three_elves <- elf_calories %>% 
  head(n=3) %>% 
  summarise(sum_cal = sum(calorie_sum)) %>% 
  pull(sum_cal)

