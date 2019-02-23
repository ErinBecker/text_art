library(dplyr)
library(ggplot2)
library(tidytext)

# Read in some open source text
# Project Gutenberg documents start and end with informational text
# structure is
# informational text
# line starting with *
# book text
# line starting with *
# informational text
# need to find line numbers starting with * and keep only text between those lines

url <- "https://www.gutenberg.org/files/76/76-0.txt"
book <- readLines(url)
info_lines <- grep("^\\*", book)
book <- book[(info_lines[1]+1):(info_lines[2]-1)] %>%
  strsplit(" ") %>%
  unlist() %>%
  tolower()
book <- gsub("[[:punct:][:blank:][:digit:]]", "", book)

# sort words by character count
words <- data.frame("word" = as.character(book), "nchar" = nchar(book))
words$word <- as.character(words$word)
words <- words %>%
  filter(nchar != 0)
words <- words[order(words$nchar),]
words_list <- split(words, f = words$nchar)

# # test fib sequence
# select_word <- function(x) {
#   for(i in x) {
#     sample(words_list[[x[i]]][["word"]], 1) 
#   }
# }

select_word <- function(x) {
    sample(words_list[[x]][["word"]], 1)
}

sentence <- paste(select_word("1"), select_word("2"), select_word("3"),
      select_word("5"), select_word("8"), 
      sep = " ")

# write out to file
out_file <- "output/fib_test2.txt"

sentence <- paste(select_word("1"), select_word("2"), select_word("3"),
                  select_word("5"), select_word("8"), 
                  sep = " ")
write(sentence, out_file, append = TRUE)

# Read in some dictionary
