library(tidyverse)

text <- readLines("2021 1203 input.txt")

data <- tibble(
  full = text,
  b1 = substr(text, 1,1),
  b2 = substr(text, 2,2),
  b3 = substr(text, 3,3),
  b4 = substr(text, 4,4),
  b5 = substr(text, 5,5),
  b6 = substr(text, 6,6),
  b7 = substr(text, 7,7),
  b8 = substr(text, 8,8),
  b9 = substr(text, 9,9),
  b10 = substr(text, 10,10),
  b11 = substr(text, 11,11),
  b12 = substr(text, 12,12)
) %>%
  gather(
    key = position,
    value = bit,
    b1:b12
  ) 

gamma <- apply(data %>%
  group_by(
    position 
  ) %>%
  count(
    bit
  ) %>%
  slice(
    which.max(n)
  ) %>%
  select(
    -n
  ) %>%
  spread(
    key = position,
    value = bit
  ) %>%
  select(
    b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12
  ),
  1,
  paste0,
  collapse = ""
) %>%
  strtoi(.,base = 2)

epsilon <- apply(data %>%
                 group_by(
                   position 
                 ) %>%
                 count(
                   bit
                 ) %>%
                 slice(
                   which.min(n)
                 ) %>%
                 select(
                   -n
                 ) %>%
                 spread(
                   key = position,
                   value = bit
                 ) %>%
                 select(
                   b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12
                 ),
               1,
               paste0,
               collapse = ""
) %>%
  strtoi(.,base = 2)

pt_1 <- gamma * epsilon

oxygen <- apply(data %>%
  spread(
    key = position,
    value = bit
  ) %>%
  filter(
    b1 == ifelse(
      sum(as.numeric(b1)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b2 == ifelse(
      sum(as.numeric(b2)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b3 == ifelse(
      sum(as.numeric(b3)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b4 == ifelse(
      sum(as.numeric(b4)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b5 == ifelse(
      sum(as.numeric(b5)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b6 == ifelse(
      sum(as.numeric(b6)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b7 == ifelse(
      sum(as.numeric(b7)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b8 == ifelse(
      sum(as.numeric(b8)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b9 == ifelse(
      sum(as.numeric(b9)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b10 == ifelse(
      sum(as.numeric(b10)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b11 == ifelse(
      sum(as.numeric(b11)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  filter(
    b12 == ifelse(
      sum(as.numeric(b12)) >= nrow(.)/2,
      "1",
      "0"
    )
  ) %>%
  select(
    b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12
  ),
  1,
  paste0,
  collapse = ""
) %>%
  strtoi(.,base = 2 
)

co2 <- apply(data %>%
                  spread(
                    key = position,
                    value = bit
                  ) %>%
                  filter(
                    b1 == ifelse(
                      sum(as.numeric(b1)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b2 == ifelse(
                      sum(as.numeric(b2)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b3 == ifelse(
                      sum(as.numeric(b3)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b4 == ifelse(
                      sum(as.numeric(b4)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b5 == ifelse(
                      sum(as.numeric(b5)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b6 == ifelse(
                      sum(as.numeric(b6)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b7 == ifelse(
                      sum(as.numeric(b7)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) %>%
                  filter(
                    b8 == ifelse(
                      sum(as.numeric(b8)) >= nrow(.)/2,
                      "0",
                      "1"
                    )
                  ) 
                   %>%
                  select(
                    b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12
                  ),
                1,
                paste0,
                collapse = ""
) %>%
  strtoi(.,base = 2 
  )

pt_2 <- co2 * oxygen
