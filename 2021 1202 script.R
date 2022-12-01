library(tidyverse)

text <- readLines("2021 1202 input.txt")

data <- tibble(
  full = text
) %>%
  separate(
    full, c("direction", "amount")
  ) %>%
 mutate(
   plane = ifelse(direction == "forward", "horizontal", "depth"),
   amount = as.numeric(amount),
   amount_adj = ifelse(direction == "up", amount*-1, amount)
 ) %>% 
  group_by(
    plane
  ) %>%
  summarise(
    sum(amount_adj)
  )

pt_1 <- data[1,2] * data[2,2]

data_adj <- tibble(
  full = text
) %>%
  separate(
    full, c("direction", "amount")
  ) %>%
  mutate(
    amount = as.numeric(amount),
    aim = ifelse(direction == "down",
                 amount,
                 ifelse(
                   direction == "up",
                   amount*-1,
                   0
                 )),
    aim_cum = cumsum(aim),
    horizontal = ifelse(direction == "forward",
                        amount,
                        0),
    depth = ifelse(direction == "forward",
                   aim_cum * amount,
                   0 
                   )
  ) %>%
  summarise(
    horizontal = sum(horizontal),
    depth = sum(depth),
    position = sum(horizontal) * sum(depth)
  )
