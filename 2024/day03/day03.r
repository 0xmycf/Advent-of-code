
library(stringr)
library(readr)


path <- "../input/day03.txt"
content <-  read_file(path)

matcha <- "mul\\(([:digit:]+),([:digit:]+)\\)"
matchb <- "mul\\(([:digit:]+),([:digit:]+)\\)|don't\\(\\)|do\\(\\)"


matches <- str_match_all(content, matcha)

first <- as.numeric(matches[[1]][, 2])
second <- as.numeric(matches[[1]][, 3])

prod <- first * second

sum(prod)

matches_b <- str_match_all(content, matchb)

first <- as.numeric(matches_b[[1]][, 2])
second <- as.numeric(matches_b[[1]][, 3])

prod <- first * second

do <- TRUE
sumb <- 0
for (x in seq_len(nrow(matches_b[[1]]))) {
  elem <- matches_b[[1]][x]
  if (str_detect(elem, "do\\(\\)")) {
    do <- TRUE
  }
  if (str_detect(elem, "don't\\(\\)")) {
    do <- FALSE
  }
  if (do && str_detect(elem, "mul\\([:digit:]+,[:digit:]+\\)")) {
    sumb <- sumb + prod[x]
  }
}

sumb
