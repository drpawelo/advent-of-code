library(tidyverse)
library(here)

### Part 1

# Reading in data
input_data <- read_csv(here("day3", "kasia.txt"), 
                       col_names = c("rucksack"))

# Split into compartments

processed_data <- input_data %>% 
  mutate(n_char = nchar(rucksack)) %>% 
  mutate(comp1 = str_split(substr(rucksack, 1, n_char/2), ""),
         comp2 = str_split(substr(rucksack, n_char/2 + 1, n_char), ""))

# Find common item
processed_data <- processed_data %>% 
  rowwise() %>% 
  mutate(common_item = intersect(comp1, comp2))

# Create lookup df
letters_vector <- c(letters, LETTERS)
priorities_vector <- 1:52

lookup_df <- data.frame(letters_vector, priorities_vector)

# Join 2 data frames
output_data <- processed_data %>% 
  left_join(lookup_df, by = c("common_item" = "letters_vector"))

# Calculate result
result <- sum(output_data$priorities_vector)
result


### Part 2

# Create the data
part2_data <- input_data %>% 
  # create a column with group membership
  mutate(elf_group = paste0("group",rep(1:(nrow(input_data)/3), each = 3))) %>% 
  # turn rucksack into vector of single letters
  mutate(rucksack = str_split(rucksack, ""))%>% 
  pivot_wider(names_from = elf_group, values_from = rucksack) %>% 
  pivot_longer(cols = everything())

part2_common_element <- part2_data %>% 
  rowwise() %>% 
  mutate(common_item = Reduce(intersect, value)) %>% 
  left_join(lookup_df, by = c("common_item" = "letters_vector"))

result_part2 <- sum(part2_common_element$priorities_vector)
result_part2
