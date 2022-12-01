library(tidyverse)

text <- readLines("2022 1201 input.txt")

data <- tibble(
  calories = text,
  elf = 0
) %>%
  mutate(
    elf = ifelse(calories == "", elf + 1, elf),
    elf_adj = paste0("elf ", cumsum(elf))
  ) %>%
  filter(
    calories != ""
  ) %>%
  group_by(
    elf_adj
  ) %>%
  summarise(
    calories = sum(as.numeric(calories))
  ) %>%
  arrange(
    desc(calories)
  )

pt_1 <- slice_head(data)

pt_2 <- slice(data, c(1:3)) %>% summarise(sum(calories))

              