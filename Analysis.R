# start connection with file
conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
# read all lines and then sample 10%
set.seed(1234)
blogs <- readLines(con)
blogsSample <- blogs[rbinom(length(blogs)*0.1, length(blogs), 0.5)]
writeLines(blogsSample, con = "blogsSample.txt")
close(conBlogs)

conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
news <- readLines(conNews)
set.seed(1235)
newsSample <- news[rbinom(length(news)*0.1, length(news), 0.5)]
writeLines(newsSample, con = "newsSample.txt")
close(conNews)

conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
twitter <- readLines(conTwitter)
set.seed(1236)
twitterSample <- twitter[rbinom(length(twitter)*0.1, length(twitter), 0.5)]
writeLines(twitterSample, con = "twitterSample.txt")
close(conTwitter)

# clean workspace
rm(blogs, news, twitter)

# now we can always load the exact same samples
conBlogs <- file("blogsSample.txt", "r")
blogsSample <- readLines(conBlogs)
close(conBlogs)


# clean blogsSample
# remove special characters
# TODO: Write as function 'cleanText' or so
gsub("[[:punct:]]", "", blogsSample)
gsub("[^a-zA-Z0-9]", "", blogsSample)

# everything to lower case
tolower(blogsSample)
# remove numbers



# function for profanity filtering
profanityFilter <- function(wordList){
        
        # read list with profanity words
        con <- file("https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol", "r")
        profList <- readLines(con)
        close(con)
        
        library(stringr)
        
        str_replace_all(wordList, profList, replacement = NA_character_)
        
        wordList
        
}

profanityFilter(blogsSample)



# creating function that takes a file as input and
# returns a tokenized version of it
tokenize <- function(file){
        library(tokenizers)
        file <- tokenize_words(file)
        file
}

# tokenize blogsSample
blogsSample <- tokenize(blogsSample)


blogsSample


# test profanityFilter function
con <- file("https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol", "r")
profList <- readLines(con)
close(con)

text <- "I hate this motherfucking bitch ass guy."

profanityFilter(text)
text

str_replace_all(text, profList, replacement = NA_character_)

text
