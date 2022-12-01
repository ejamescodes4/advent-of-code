library(tidyverse)
library(RcppRoll)

text <- readLines("2021 1201 input.txt")

data <- tibble(
  depth = text,
  measurement = ifelse(
    as.numeric(depth) > lag(as.numeric(depth)),
    "increased",
    "decreased"
  )
)

pt_1 <- data %>%
  count(
    measurement
  )

data_adj <- data %>%
  select(
    -measurement
  ) %>%
  mutate(
    roll_sum = roll_sum(as.numeric(depth), 3, align = "right", fill = NA),
    measurement = ifelse(
      as.numeric(roll_sum) > lag(as.numeric(roll_sum)),
      "increased",
      "decreased"
    )
  )

pt_2 <- data_adj %>%
  count(
    measurement
  )
