# start connection with file
con <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
# read some lines
data <- readLines(con, 20)
close(con)

# creating function that takes a file as input and
# returns a tokenized version of it
tokenize <- function(file){
        library(tokenizers)
        file <- tokenize_words(file)
        file
}

# tokenize data
data <- tokenize(data)


data

# inspect data
str(data)
class(data)
data[1]

# use tokenizers package to tokenize the data into single words
library(tokenizers)

data_token <- tokenize_words(data)

data_token[[1]]

