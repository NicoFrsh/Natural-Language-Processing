# Analysis for complete corpus
#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
library(readtext)
library(quanteda)

all_data <- readtext(paste0(getwd(), "/Samples/*.txt"), encoding = "UTF-8")

#class(all_data)

# create a corpus 
corp <- corpus(all_data)

# remove non utf-8 characters
texts(corp) <- iconv(texts(corp), from = "UTF-8", to = "ASCII", sub = "")
texts(corp) <- gsub("[\u2019\u0092]","'",texts(corp))
# remove punctuation
texts(corp) <- gsub('[\\,\"\\.\\*\\?!#:/();]', "", texts(corp))

# perform data cleaning using the quanteda package
# first create tokens object removing all non alphabetical characters
tok <- tokens(corp, what = "fasterword", remove_punct = TRUE, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_url = TRUE, split_hyphens = TRUE)

# remove stop words
#tok <- tokens_select(tok, stopwords('english'),selection='remove')

# to lower case
tok <- tokens_tolower(tok)

# remove profanity
con <- file("https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol", "r")
profList <- readLines(con)
close(con)

tok <- tokens_select(tok, profList, selection = "remove")

# convert to dfm
dfm <- dfm(tok)

### Exploratory Analysis
# key words in context
head(kwic(tok, "school", window = 4))
# top featured words
top20 <- topfeatures(dfm, 20)

freq <- data.frame(feature = names(top20), frequency = top20)

library(ggplot2)

gg <- ggplot(freq, aes(x = reorder(feature, frequency), y = frequency)) + 
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = NULL, y = "Frequency", title = "Top 20 Features")

gg

# wordcloud
textplot_wordcloud(dfm, random_order = FALSE, max_words = 150, rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# create n-grams
biGram <- tokens_ngrams(tok, n = 2, concatenator = " ")
# 2-gram
head(biGram[[1]],30)
# 3-gram
triGram <- tokens_ngrams(tok, n = 3, concatenator = " ")
head(triGram[[1]],30)

# get frequencies
top20bigram <- topfeatures(dfm(biGram), 20)
freq <- data.frame(feature = names(top20bigram), frequency = top20bigram)
# plot in ggplot
gg <- ggplot(freq, aes(x = reorder(feature, frequency), y = frequency)) +
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(x = NULL, y = "Frequency", title = "Top 20 Bi-Grams")

gg

top20trigram <- topfeatures(dfm(triGram), 20)
freq <- data.frame(feature = names(top20trigram), frequency = top20trigram)

gg <- ggplot(freq, aes(x = reorder(feature, frequency), y = frequency)) +
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(x = NULL, y = "Frequency", title = "Top 20 Tri-Grams")

gg

# Can you think of a way to increase the coverage -- identifying words that may not be in the
# corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
# answer: trim words that have very low frequency as they are very specialized / specific
# check number of tokens / words
dfm <- dfm(tok)
ntoken(dfm)
dfm <- dfm_trim(dfm, min_termfreq = 5, termfreq_type = "count")
ntoken(dfm)

# QUESTION:
# How many unique words do you need in a frequency sorted dictionary
# to cover 50% of all word instances in the language? 90%? 
topfeat <- topfeatures(dfm, length(dfm))
cvrg <- data.frame(n = 1:length(topfeat), total = cumsum(topfeat))
cvrg$prop <- cvrg$total / cvrg$total[length(topfeat)]

plot(cvrg$n, cvrg$prop, type = "l", xlab = "Number of unique words", 
     ylab = "Proportion of total words", main = "Coverage by frequency sorted dictionary")
# find first word that surpasses the 50% mark
ind50 <- which(cvrg$prop > 0.5)[1]
# now for the 90%
ind90 <- which(cvrg$prop > 0.9)[1]
abline(v = ind50, col = "red")
abline(v = ind90, col = "blue")
ind50
ind90

# Find words from foreign languages.
# use english dictionary dataset
url <- "https://raw.githubusercontent.com/dwyl/english-words/master/words.txt"
con <- file(url, "r")
enDict <- readLines(con)
close(con)

# remove foreign words
sum(ntoken(tok))
tok <- tokens_select(tok, enDict, selection = "keep")
sum(ntoken(tok))
dfm <- dfm(tok)

##### Modelling

# naive approach
predictWord <- function(toks, prevWords){
        word_ngram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
        prevWords <- stringr::str_replace(prevWords, " ", "_")
        word_ngram_select <- tokens_select(word_ngram, pattern = phrase(paste0(prevWords, "_*")))
        
        top3 <- topfeatures(dfm(word_ngram_select), 3)
        
        # calculate probabilities
        
        prob3 <- top3 / sum(topfeatures(dfm(word_ngram_select)))
        prob3 <- round(prob3 * 100, digits = 2)
        
        print("Probabilies (in %):")
        print(prob3)
        
        # filter only the following word
        sub(".*_", "", names(top3))
}

predictWord(tok, "can you")
predictWord(tok, "i")
predictWord(tok, "tell me")
predictWord(tok, "thanks for")

#### PROBLEMS:
#       - as we only need 6730 words to cover 90% of all words (>30.000) we could use only those
#         those 6730 as our toks corpus
#       - how to handle unseen n-grams
#       - smoothing: some next words have probability 0 (count 0) even though they
#                    make perfect sense
#       - 

#### First improvement:
# reduce tokens
# remember that ind90 is the index for the most frequent words with a coverage of 90%

top90perc <- topfeat[1:ind90]

toks_red <- tokens_select(tok, names(top90perc), selection = "keep")

object.size(toks_red)
object.size(tok)
object.size(tok) - object.size(toks_red)
# reduced the object by almost 4 MB, not too much

# try old tokens object first and measure time
start_time <- Sys.time()
predictWord(tok, "i")
end_time <- Sys.time()
end_time - start_time

# now the reduced tokens object
start_time <- Sys.time()
predictWord(toks_red, "i")
end_time <- Sys.time()
end_time - start_time

# some significant improvement in computation time

# improved algorithm (based on Katz' back-off model)
predictWordImproved <- function(toks, prevWords, smoothing = TRUE){
        
        # only use at most trigrams to predict
        if (ntoken(prevWords) > 5){
                prevWords <- stringr::word(prevWords, start = -5, end = -1)
        }
        
        # backoff
        word_ngram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
        prevWords <- stringr::str_replace(prevWords, " ", "_")
        word_ngram_select <- tokens_select(word_ngram, pattern = phrase(paste0(prevWords, "_*")))
        
        foundWords <- length(word_ngram_select[[1]])
        
        countDown <- ntoken(prevWords)
        
        while (foundWords == 0 & countDown > 0){
                # eliminate one of the previous words
                prevWords <- sub(".*?_", "", prevWords)
                word_ngram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
                prevWords <- stringr::str_replace(prevWords, " ", "_")
                word_ngram_select <- tokens_select(word_ngram, pattern = phrase(paste0(prevWords, "_*")))
                foundWords <- length(word_ngram_select[[1]])
                countDown <- countDown - 1
        }
        
        top3 <- topfeatures(dfm(word_ngram_select), 3)
        
        # calculate probabilities
        
        prob3 <- top3 / sum(topfeatures(dfm(word_ngram_select)))
        prob3 <- round(prob3 * 100, digits = 2)
        
        print("Probabilies (in %):")
        print(prob3)
        
        # filter only the following word
        sub(".*_", "", names(top3))
        
}

#### TESTING
predictWordImproved(toks_red, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predictWordImproved(toks_red, "It would mean the")
predictWordImproved(toks_red, "can you follow me and make me the")
predictWordImproved(toks_red, "Offense still struggling but the")
predictWordImproved(toks_red, "Go on a romantic date at the")
predictWordImproved(toks_red, "I'll dust them off and be on my")
predictWordImproved(toks_red, "Love that film and haven't seen it in quite some")

# Lets try selective n-grams
# for example negation bigram
just_bigram <- tokens_compound(tok, pattern = phrase("* +* +beer"))
just_bigram_select <- tokens_select(just_bigram, pattern = phrase("*_*_beer"))
head(just_bigram_select[[1]],30)
topfeatures(dfm(just_bigram_select), 10)
# get topfeatures
topfeatures(dfm(just_bigram_select), 20)
# generate skip-gram
toks_skip <- tokens_ngrams(tok, n = 2, skip = 1:2)
head(toks_skip[[1]], 30)

# again, selective skip gram
need_skipgram <- tokens_compound(tok, pattern = phrase("need *"))
need_skipgram_select <- tokens_select(need_skipgram, pattern = phrase("need_*"))
head(need_skipgram_select[[1]], 30)

people_skipgram <-tokens_compound(tok, pattern = phrase("people *"))
people_skipgram_select <- tokens_select(people_skipgram, pattern = phrase("people_*"))
head(people_skipgram_select[[1]], 30)


# convert the frequency count to a proportion within documents
dfm_weight(dfm, scheme = "prop")
dfm_weight(dfm, scheme = "count") # same as just print(dfm)
dfm_tfidf(dfm)

# create feature co-occurrence matrix
fcm <- fcm(dfm_trim(dfm, min_termfreq = 200))
topfeatures(fcm)
feat <- names(topfeatures(fcm, 50))
fcm_select <- fcm_select(fcm, pattern = feat, selection = "keep")

# word embedding model
library(text2vec)
size <- log(colSums(dfm_select(dfm, feat, selection = "keep")))
set.seed(144)
textplot_network(fcm_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

# get word frequency grouped by document
tstat_freq <- textstat_frequency(dfm, n = 10, groups = docnames(dfm))

# plot in ggplot
dfm %>%
        textstat_frequency(n = 10, groups = docnames(dfm)) %>%
        ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
        geom_bar(stat = "identity") +
        facet_grid(. ~ group) +
        coord_flip() +
        labs(x = NULL, y = "Frequency")

#### TEST 
some_text <- "This. bullshit 12344 fucks don´t my head äâ bitch @nicofrsh #papijuancho."

some_text <- iconv(some_text, from = "UTF-8", to = "ASCII", sub = "")

# stringr before tokenizing
#library(stringr)
#some_text <- str_replace_all(some_text, "[^a-zA-Z\\s]", "")

text_tok <- tokens(some_text, remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE)
text_tok <- tokens_select(text_tok, profList, selection = "remove")
text_tok

texttt <- 'concerned. computer.* \"give hallo! #papijuancho out:'
gsub('[\"\\.\\*!\\?#:]', "", texttt)
