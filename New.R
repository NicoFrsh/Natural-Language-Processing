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

foreignWords <- tokens_select(tok, enDict, selection = "remove")
foreignWords
ntoken(foreignWords)
sum(ntoken(foreignWords))

# remove foreign words
sum(ntoken(tok))
tok <- tokens_select(tok, enDict, selection = "keep")
sum(ntoken(tok))


# Can you think of a way to increase the coverage -- identifying words that may not be in the
# corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
# answer: trim words that have very low frequency as they are very specialized / specific
# check number of tokens / words
ntoken(dfm)
dfm <- dfm_trim(dfm, min_termfreq = 5, termfreq_type = "count")
ntoken(dfm)

##### Modelling

# naive approach
predictWord <- function(toks, prevWords){
        word_bigram <- tokens_compound(toks, pattern = phrase(paste0(prevWords, " *")))
        prevWords <- stringr::str_replace(prevWords, " ", "_")
        word_bigram_select <- tokens_select(word_bigram, pattern = phrase(paste0(prevWords, "_*")))
        
        topfeatures(dfm(word_bigram_select), 3)
}

predictWord(tok, "just")
predictWord(tok, "i")
predictWord(tok, "why")
predictWord(tok, "do you")

# Lets try selective n-grams
# for example negation bigram
just_bigram <- tokens_compound(tok, pattern = phrase("just *"))
just_bigram_select <- tokens_select(just_bigram, pattern = phrase("just_*"))
head(just_bigram_select[[1]],30)
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
