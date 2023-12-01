# this doesnt work for part b!
library(tidyverse)
input <- read.table("../input/test/day1.txt") # nolint: commented_code_linter.
# input <- read.table("../input/day1.txt")

words <- c("one", "two", "three", "four",
           "five", "six", "seven", "eight", "nine")
wordsregex <- paste(words, collapse = "|")

# this is how you use it
# wordDf[wordDf == "seven", ] # nolint: commented_code_linter.
wordsDf <- data.frame(words, N = 1:9)

# i dont know how to map over the rows so i gotta do the naive approach
sum <- 0
# for (i in 1:nrow(input)) { # nolint: seq_linter.
for (i in 2:2) { # nolint: seq_linter.
  row <- input[i, 1]
  matches <- row %>% str_match_all(regex(str_c("\\d|", wordsregex)))
  first <- sapply(matches, head, 1)
  last <- sapply(matches, tail, 1)
  both <- c(first, last)
  asnum <- is.na(as.numeric(both))
  matches <- which(asnum %in% c(TRUE))
  toBeReplaced <- both[matches] # nolint: object_name_linter.
  thingstokeep <- which(matches %in% c(FALSE))
  keep <- both[thingstokeep]
  print(toBeReplaced)
  new <- filter(wordsDf, words %in% toBeReplaced)$N
  if (length(matches) == 1 && matches == 1) {
    first <- new
  } else if (length(matches) == 1 && matches == 2) {
    last <- new
  } else if (length(matches) == 2) {
    first <- new[1] %>% as.character()
    last <- new[2] %>% as.character()
  }
  # print(c(first, last))
  ans <- str_c(first, last) %>% as.numeric()
  print(ans)
  sum <- sum + ans
}

print(sum)

foo <- str_match_all("six8flfzdzl72eightnine", regex("\\d"))
print(foo[length(foo), 1])
print(sapply(foo, head, 1))
print(foo)


first <- "seven"
last <- "one"
two <- c(first, last)
test <- is.na(as.numeric(two))
matches <- which(test %in% c(TRUE))
toBeReplaced <- two[matches] # nolint: object_name_linter.
thingstokeep <- which(matches %in% c(FALSE))
keep <- two[thingstokeep]
# new <- wordsDf[wordsDf == toBeReplaced, ]
# new <- filter(wordsDf, words %in% toBeReplaced)$N
new <- wordsDf[ which(words %in% toBeReplaced),  ]
if (length(matches) == 1 && matches == 1) {
  first <- new
} else if (length(matches) == 1 && matches == 2) {
  last <- new
} else if (length(matches) == 2) {
  first <- new[1] %>% as.character()
  last <- new[2] %>% as.character()
}
print(c(first, last)) # nolint: commented_code_linter.


str_match_all("two1nine", regex(str_c("\\d|", wordsregex)))
ls <- as.list(wordsDf)

