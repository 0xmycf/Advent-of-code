
library(stringr)
library(readr)


path <- "../input/day03.txt"
content <-  read_file(path)

matcha <- "mul\\(([:digit:]+),([:digit:]+)\\)"


matches <- str_match_all(content, matcha)

first <- as.numeric(matches[[1]][,2])
second <- as.numeric(matches[[1]][,3])
first
second

prod <- first * second

sum(prod)
