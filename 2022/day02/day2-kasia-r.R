library(tidyverse)
library(here)

### Part 1
# part 1 instructions

# file looks like this:
# B Z
# B Z
# B Y
# C Y
# B Y

# them: A for Rock, B for Paper, and C for Scissors.
#  me:  X for Rock, Y for Paper, and Z for Scissors.

# basic points:  (1 for Rock, 2 for Paper, and 3 for Scissors) 

# Load the data

input_data <- read_delim(here("day2", "kasia.txt"), delim = " ",
                       col_names = c("them", "me"))

# Set up a df with basic scores
basic_score <- data.frame(me = c("X", "Y", "Z"), basic_score = c(1, 2, 3))

# Set up a df for win score
win_score <- data.frame(me = c("X", "Y", "Z"), A = c(3, 6, 0), B = c(0, 3, 6), 
                        C = c(6, 0, 3))

# Pivot the win score df for easier joining
win_score_long <- win_score %>% 
  pivot_longer(cols = c(A:C), names_to = "them", values_to = "win_score")

# Write a function to calculate total scores
add_all_scores <- function(play_data, basic_score_df, win_score_df){
  result_data <- play_data %>% 
    left_join(basic_score_df) %>% 
    left_join(win_score_df, by = c("me", "them")) %>% 
    mutate(total_score = basic_score + win_score)

  return(result_data)
    
}

# Apply the function to our input data
data_with_scores <- add_all_scores(input_data, basic_score, win_score_long)

# Calculate the sum as final result
final_result <- sum(data_with_scores$total_score)

### Part 2

# part 2
# file looks like this:
# B Z
# B Z
# B Y
# C Y
# B Y

# them: A for Rock, B for Paper, and C for Scissors.
# points: (1 for Rock, 2 for Paper, and 3 for Scissors) 

# new rules:
# X means you need to lose, 
# Y means you need to end the round in a draw, 
# and Z means you need to win.

input_data_part2 <- read_delim(here("day2", "kasia.txt"), delim = " ",
                         col_names = c("them", "instruction"))

win_score_long_with_words <- win_score_long %>% 
  mutate(me_words = case_when(me == "X" ~ "Rock",
                              me == "Y" ~ "Paper",
                              me == "Z" ~ "Scissors"))

basic_score_with_words <- basic_score %>% 
  mutate(me_words = case_when(me == "X" ~ "Rock",
                              me == "Y" ~ "Paper",
                              me == "Z" ~ "Scissors"))


part2_score_table <- win_score_long %>% 
  rename(instruction = me) %>% 
  mutate(instruction_human = case_when(instruction == "X" ~ "lose",
                                       instruction == "Y" ~ "draw",
                                       instruction == "Z" ~ "win")) %>% 
  select(-c(win_score)) %>% 
  mutate(win_score = case_when(instruction_human == "lose" ~ 0,
                                        instruction_human == "draw" ~ 3,
                                        instruction_human == "win" ~6))


# Write a function to calculate total scores
add_all_scores_part2 <- function(play_data, points_df){
  result_data <- play_data %>% 
    left_join(points_df)
    # left_join(win_score_df, by = c("me", "them")) %>% 
    # mutate(total_score = basic_score + win_score)
  
  return(result_data)
  
}

add_all_scores_part2(input_data_part2, part2_score_table)
