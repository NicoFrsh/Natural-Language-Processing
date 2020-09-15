#### Modeling

# sample data ------------------------------------------------
if (!file.exists("Samples/blogsSample02.txt")){
        # start connection with file
        conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
        # read all lines and then sample 20%
        set.seed(1234)
        blogs <- readLines(conBlogs)
        blogsSample <- sample(blogs, length(blogs) * 0.2)
        writeLines(blogsSample, con = "Samples/blogsSample02.txt")
        close(conBlogs)
        rm(blogs, blogsSample)
}

if (!file.exists("Samples/newsSample02.txt")){
        conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
        news <- readLines(conNews)
        set.seed(1235)
        newsSample <- sample(news, length(news) * 0.2)
        writeLines(newsSample, con = "Samples/newsSample02.txt")
        close(conNews)
        rm(news, newsSample)
}

if (!file.exists("Samples/twitterSample02.txt")){
        conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
        twitter <- readLines(conTwitter)
        set.seed(1236)
        twitterSample <- sample(twitter, length(twitter) * 0.2)
        writeLines(twitterSample, con = "Samples/twitterSample02.txt")
        close(conTwitter)
        rm(twitter, twitterSample)
}

if (!file.exists("Samples/blogsSample03.txt")){
        # start connection with file
        conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
        # read all lines and then sample 20%
        set.seed(1234)
        blogs <- readLines(conBlogs)
        blogsSample <- sample(blogs, length(blogs) * 0.3)
        writeLines(blogsSample, con = "Samples/blogsSample03.txt")
        close(conBlogs)
        rm(blogs, blogsSample)
}

if (!file.exists("Samples/newsSample03.txt")){
        conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
        news <- readLines(conNews)
        set.seed(1235)
        newsSample <- sample(news, length(news) * 0.3)
        writeLines(newsSample, con = "Samples/newsSample03.txt")
        close(conNews)
        rm(news, newsSample)
}

if (!file.exists("Samples/twitterSample03.txt")){
        conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
        twitter <- readLines(conTwitter)
        set.seed(1236)
        twitterSample <- sample(twitter, length(twitter) * 0.3)
        writeLines(twitterSample, con = "Samples/twitterSample03.txt")
        close(conTwitter)
        rm(twitter, twitterSample)
}

if (!file.exists("Samples/blogsSample05.txt")){
        # start connection with file
        conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
        # read all lines and then sample 50%
        set.seed(1234)
        blogs <- readLines(conBlogs)
        blogsSample <- sample(blogs, length(blogs) * 0.5)
        writeLines(blogsSample, con = "Samples/blogsSample05.txt")
        close(conBlogs)
        rm(blogs, blogsSample)
}

if (!file.exists("Samples/newsSample05.txt")){
        conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
        news <- readLines(conNews)
        set.seed(1235)
        newsSample <- sample(news, length(news) * 0.5)
        writeLines(newsSample, con = "Samples/newsSample05.txt")
        close(conNews)
        rm(news, newsSample)
}

if (!file.exists("Samples/twitterSample05.txt")){
        conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
        twitter <- readLines(conTwitter)
        set.seed(1236)
        twitterSample <- sample(twitter, length(twitter) * 0.5)
        writeLines(twitterSample, con = "Samples/twitterSample05.txt")
        close(conTwitter)
        rm(twitter, twitterSample)
}

if (!file.exists("Samples/blogsSample08.txt")){
        # start connection with file
        conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
        # read all lines and then sample 50%
        set.seed(1234)
        blogs <- readLines(conBlogs)
        blogsSample <- blogs[rbinom(length(blogs)*0.8, length(blogs), 0.5)]
        writeLines(blogsSample, con = "Samples/blogsSample08.txt")
        close(conBlogs)
        rm(blogs, blogsSample)
}

if (!file.exists("Samples/newsSample08.txt")){
        conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
        news <- readLines(conNews)
        set.seed(1235)
        newsSample <- news[rbinom(length(news)*0.8, length(news), 0.5)]
        writeLines(newsSample, con = "Samples/newsSample08.txt")
        close(conNews)
        rm(news, newsSample)
}

if (!file.exists("Samples/twitterSample08.txt")){
        conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
        twitter <- readLines(conTwitter)
        set.seed(1236)
        twitterSample <- twitter[rbinom(length(twitter)*0.8, length(twitter), 0.5)]
        writeLines(twitterSample, con = "Samples/twitterSample08.txt")
        close(conTwitter)
        rm(twitter, twitterSample)
}



# clean data ---------------------
library(readtext)
library(quanteda)
library(readr)

all_data <- readtext(paste0(getwd(), "/Samples/*sSample05.txt"), encoding = "UTF-8")

# create a corpus 
corp <- corpus(all_data)

# remove non utf-8 characters
texts(corp) <- iconv(texts(corp), from = "UTF-8", to = "ASCII", sub = "")
#texts(corp) <- gsub("[\u2019\u0092]","'",texts(corp))
# remove punctuation
texts(corp) <- gsub('[\\,\"\\.\\*\\?!#:/();]', "", texts(corp))

# perform data cleaning using the quanteda package
# first create tokens object removing all non alphabetical characters
tok <- tokens(corp, what = "fasterword", remove_punct = TRUE, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_url = TRUE, split_hyphens = TRUE)

ntoken(tok)
rm(all_data, corp)
# to lower case
tok <- tokens_tolower(tok)

# remove profanity
con <- file("https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol", "r")
profList <- readLines(con)
close(con)

tok <- tokens_select(tok, profList, selection = "remove")

# convert to dfm
dfm <- dfm(tok)

# trim data
ntoken(dfm)
dfm <- dfm_trim(dfm, min_termfreq = 10, termfreq_type = "count")
ntoken(dfm)
# get ordered (by count) list of words
topfeat <- topfeatures(dfm, length(dfm))
# create coverage df
cvrg <- data.frame(n = 1:length(topfeat), total = cumsum(topfeat))
cvrg$prop <- cvrg$total / cvrg$total[length(topfeat)]
# find first word that surpasses the 90% mark
ind90 <- which(cvrg$prop > 0.9)[1]

# use only the 90% covering unique words
top90perc <- topfeat[1:ind90]

tok <- tokens_select(tok, names(top90perc), selection = "keep")

# clean workspace
rm(topfeat, top90perc, profList, dfm, cvrg)

# save n grams with frequencies
unigrams <- dfm(tok)
# unigrams <- dfm_trim(unigrams, min_termfreq = 10, termfreq_type = "count")
unigrams <- textstat_frequency(unigrams)
bigrams <- tokens_ngrams(tok, n = 2, concatenator = " ")
bigrams <- dfm_trim(dfm(bigrams), min_termfreq = 10, termfreq_type = "count")
bigrams <- textstat_frequency(dfm(bigrams))
# parallelize
RcppParallel::setThreadOptions(1)
trigrams <- tokens_ngrams(tok, n = 3, concatenator = " ")
trigrams <- dfm_trim(dfm(trigrams), min_termfreq = 5, termfreq_type = "count")
trigrams <- textstat_frequency(dfm(trigrams))

# too much memory, only use when using <= 20% of the data
quadgrams <- tokens_ngrams(tok, n = 4, concatenator = " ")
quadgrams <- dfm_trim(dfm(quadgrams), min_termfreq = 5, termfreq_type = "count")
quadgrams <- textstat_frequency(dfm(quadgrams))

pentagrams <- tokens_ngrams(tok, n = 5, concatenator = " ")
pentagrams <- dfm_trim(dfm(pentagrams), min_termfreq = 5, termfreq_type = "count")
pentagrams <- textstat_frequency(dfm(pentagrams))

# convert to data.table for performance issues
library(data.table)
unigrams <- as.data.table(unigrams)
bigrams <- as.data.table(bigrams)
trigrams <- as.data.table(trigrams)
quadgrams <- as.data.table(quadgrams)
pentagrams <- as.data.table(pentagrams)

# we only need the first two columns (feature and frequency)
unigrams <- unigrams[,1:2]
bigrams <- bigrams[ , 1:2]
trigrams <- trigrams[ , 1:2]
quadgrams <- quadgrams[ , 1:2]
pentagrams <- pentagrams[ , 1:2]

# save as RDS file

saveRDS(unigrams, "data/unigrams05.rds")
saveRDS(bigrams, "data/bigrams05.rds")
saveRDS(trigrams, "data/trigrams05.rds")
saveRDS(quadgrams, "data/quadgrams03.rds")
saveRDS(pentagrams, "data/pentagrams02.rds")

# improved algorithm (based on Katz' back-off model)
predictWordImproved <- function(toks, prevWords){
        
        # only use at most trigrams to predict
        if (ntoken(prevWords) > 5){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -5, end = -1)
                print(prevWords)
        }
        
        # backoff
        ngram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
        prevWords_ <- stringr::str_replace_all(prevWords, " ", "_")
        ngram_select <- tokens_select(ngram, pattern = phrase(paste0(prevWords_, "_*")))
        
        foundWords <- length(unique(ngram_select[[1]]))
        
        countDown <- ntoken(prevWords) - 1
        
        while (countDown > 0){
                message("Check for next N-gram:")
                #print(prevWords)
                # eliminate one of the previous words
                prevWords <- sub(".*? ", "", prevWords)
                #print(prevWords)
                ngram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
                prevWords_ <- stringr::str_replace_all(prevWords, " ", "_")
                print(prevWords_)
                ngram_select <- tokens_select(ngram, pattern = phrase(paste0(prevWords_, "_*")))
                foundWords <- length(unique(ngram_select[[1]])) + length(unique(ngram_select[[2]])) +
                        length(unique(ngram_select[[3]]))
                print(foundWords)
                countDown <- countDown - 1
                print(countDown)
                print(topfeatures(dfm(ngram_select)), 20)
        }
        
        top3 <- topfeatures(dfm(ngram_select), 15)
        
        # calculate probabilities
        
        prob3 <- top3 / sum(topfeatures(dfm(ngram_select)))
        prob3 <- round(prob3 * 100, digits = 2)
        
        print("Probabilities (in %):")
        print(prob3)
        
        # filter only the following word
        sub(".*_", "", names(top3))
        
}

#### TESTING
predictWordImproved(tok, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predictWordImproved(tok, "It would mean the")
predictWordImproved(tok, "can you follow me and make me the")
predictWordImproved(tok, "Offense still struggling but the")
predictWordImproved(tok, "Go on a romantic date at the")
predictWordImproved(tok, "I'll dust them off and be on my")
predictWordImproved(tok, "Love that film and haven't seen it in quite some")
predictWordImproved(tok, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
predictWordImproved(tok, "Be grateful for the good times and keep the faith during the")
predictWordImproved(tok, "If this isn't the cutest thing you've ever seen, then you must be")

predictWordImproved(tok, "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predictWordImproved(tok, "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
predictWordImproved(tok, "I'd give anything to see arctic monkeys this")
predictWordImproved(tok, "Talking to your mom has the same effect as a hug and helps reduce your")
predictWordImproved(tok, "When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
predictWordImproved(tok, "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
predictWordImproved(tok, "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
predictWordImproved(tok, "Every inch of you is perfect from the bottom to the")
predictWordImproved(tok, "I’m thankful my childhood was filled with imagination and bruises from playing")
predictWordImproved(tok, "I like how the same people are in almost all of Adam Sandler's")

ngram <- tokens_compound(tok, pattern = phrase("some *"))
ngram_select <- tokens_select(ngram, pattern = phrase("some_*"))
topfeatures(dfm(ngram_select), 20)
length(unique(ngram_select[[1]]))

#### new algorithm

predictNextWord <- function(prevWords){
        
        # load n grams
        bigrams <-readRDS("data/bigrams.rds")
        trigrams <- readRDS("data/trigrams.rds")
        quadgrams <- readRDS("data/quadgrams.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        
        n <- quanteda::ntoken(prevWords)
        message(paste0("PrevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 3){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -3, end = -1)
                print(prevWords)
        }
        
        
        countDown <- quanteda::ntoken(prevWords)
        
        
        #### TODO: Add foundWord < 3 later
        i <- 1
        while(countDown >= 0){
                
                message(paste0("Countdown: \t", countDown))
                
                if (i == 1 & quanteda::ntoken(prevWords) == 3){
                        message("Check quadgrams that contain:")
                        print(prevWords)
                        nextWords <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWords <- head(nextWords, 20)
                        print(nextWords)
                }
                
                if (i == 2 & quanteda::ntoken(prevWords) == 2){
                        message("Check trigrams that contain:")
                        print(prevWords)
                        nextWords <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWords <- head(nextWords, 20)
                        print(nextWords)
                }
                
                if (i == 3){
                        message("Check bigrams that contain:")
                        print(prevWords)
                        nextWords <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWords <- head(nextWords, 20)
                        print(nextWords)
                }
                
                countDown <- countDown - 1
                i <- i + 1
                
                prevWords <- sub(".*? ", "", prevWords)
                
        }
        
        message("Algorithm terminated.")
        
        nextWords
        
}

# new algorithm using Stupid Backoff
stupidBackoff <- function(prevWords){
        
        level <- quanteda::ntoken(prevWords)
        
        if (level == 1) message("Terminated.")
        
        else {
                
                message(paste0("Current level: ", level))
                
                if (level == 4) {
                        quadgrams <- readRDS("data/quadgrams.rds")
                        foundWords <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
                        if (length(foundWords) == 0) message("No match found in quadgram!")
                        else print(head(foundWords, 20))
                }
                else if (level == 3) {
                        trigrams <- readRDS("data/trigrams.rds")
                        foundWords <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        if (length(foundWords) == 0) message("No match found in trigram!")
                        else print(head(foundWords, 20))
                }
                else if (level == 2) {
                        bigrams <-readRDS("data/bigrams.rds")
                        foundWords <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        if (length(foundWords) == 0) message("No match found in bigram!")
                        else print(head(foundWords, 20))
                }
                
                else if (level == 1){
                        message("Check for most frequent unigrams to predict...")
                        # break
                }
                
                prevWords <- sub(".*? ", "", prevWords)
                
                stupidBackoff(prevWords)
                
        }
        
        foundWords
        
}

predictNextWord <- function(prevWords){
        
        # load n grams
        bigrams <-readRDS("data/bigrams.rds")
        trigrams <- readRDS("data/trigrams.rds")
        quadgrams <- readRDS("data/quadgrams.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        
        n <- quanteda::ntoken(prevWords)
        message(paste0("PrevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 3){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -3, end = -1)
                print(prevWords)
        }
        
        stupidBackoff(prevWords)
        
}


#### TEST
predictNextWord("Shut the")


predictNextWord("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predictNextWord("It would mean the")
predictNextWord("can you follow me and make me the")
predictNextWord("Offense still struggling but the")
predictNextWord("Go on a romantic date at the")
predictNextWord("I'll dust them off and be on my")
predictNextWord("Love that film and haven't seen it in quite some")
predictNextWord("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
predictNextWord("Be grateful for the good times and keep the faith during the")
predictNextWord("If this isn't the cutest thing you've ever seen, then you must be")

predictNextWord(tok, "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predictNextWord(tok, "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
predictNextWord(tok, "I'd give anything to see arctic monkeys this")
predictNextWord(tok, "Talking to your mom has the same effect as a hug and helps reduce your")
predictNextWord(tok, "When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
predictNextWord(tok, "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
predictNextWord(tok, "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
predictNextWord(tok, "Every inch of you is perfect from the bottom to the")
predictNextWord(tok, "I’m thankful my childhood was filled with imagination and bruises from playing")
predictNextWord(tok, "I like how the same people are in almost all of Adam Sandler's")
