library(tidyverse)

text <- readLines("C:\\Users\\ewarren\\R\\Projects\\Advent of Code\\2022\\2022 1202 input.txt")

data <- tibble(
  rounds = text
) %>%
  separate(
    col = rounds,
    into = c("move_elf", "move_me"),
    sep = " "
  ) %>%
  mutate(
    move_elf = recode(
      move_elf,
      "A" = "Rock",
      "B" = "Paper",
      "C" = "Scissors"
    ),
    move_me = recode(
      move_me,
      "X" = "Rock",
      "Y" = "Paper",
      "Z" = "Scissors"
    )
  )

options <- tibble(
  move_elf = c(rep("Rock", 3), rep("Paper", 3), rep("Scissors", 3)),
  move_me = rep(c("Rock", "Paper", "Scissors"),3),
  outcome = c("Tie", "Win", "Lose", "Lose", "Tie", "Win", "Win", "Lose", "Tie"),
  outcome_points = recode(
    outcome,
    "Lose" = 0,
    "Tie" = 3,
    "Win" = 6
  )
)

data_games <- left_join(
  data,
  options,
  by = c("move_elf", "move_me")
) %>%
  mutate(
    points_elf = recode(
      move_elf,
      "Rock" = 1,
      "Paper" = 2,
      "Scissors" = 3
    ) + outcome_points,
    points_me = recode(
      move_me,
      "Rock" = 1,
      "Paper" = 2,
      "Scissors" = 3
    ) + outcome_points
    
  )

pt_1 <- sum(data_games$points_me)

data_2 <- tibble(
  rounds = text
) %>%
  separate(
    col = rounds,
    into = c("move_elf", "outcome"),
    sep = " "
  ) %>%
  mutate(
    move_elf = recode(
      move_elf,
      "A" = "Rock",
      "B" = "Paper",
      "C" = "Scissors"
    ),
    outcome = recode(
      outcome,
      "X" = "Lose",
      "Y" = "Tie",
      "Z" = "Win"
    )
  ) %>%
  left_join(
    options,
    by = c("move_elf", "outcome")
  ) %>%
  mutate(
    points_elf = recode(
      move_elf,
      "Rock" = 1,
      "Paper" = 2,
      "Scissors" = 3
    ) + outcome_points,
    points_me = recode(
      move_me,
      "Rock" = 1,
      "Paper" = 2,
      "Scissors" = 3
    ) + outcome_points
  )

pt_2 <- sum(data_2$points_me)






