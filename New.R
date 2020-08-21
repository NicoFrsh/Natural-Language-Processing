# Analysis for complete corpus
library(readtext)
library(quanteda)

all_data <- readtext(paste0(getwd(), "/Samples/*.txt"))

class(all_data)

# create a corpus 
corp <- corpus(all_data)
print(corp)

# perform data cleaning using the quanteda package
# first create tokens object removing all non alphabetical characters
tok <- tokens(corp, what = "fastestword", remove_punct = TRUE, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_url = TRUE)

# remove stop words
tok <- tokens_select(tok, stopwords('english'),selection='remove')

# to lower case
tok <- tokens_tolower(tok)

# TODO: remove profanity

# convert to dfm
dfm <- dfm(tok)

### Exploratory Analysis
# key words in context
head(kwic(tok, "school", window = 4))
# top featured words
topfeatures(dfm, 20)

# create n-grams
tok_ngram <- tokens_ngrams(tok, n = 2:3)
# 2-gram
head(tok_ngram[[1]],30)
# 3-gram
head(tok_ngram[[2]],30)
