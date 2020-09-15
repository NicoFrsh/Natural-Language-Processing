## Next Word Prediction
##### First function is a 5-gram model
predictNextWord <- function(prevWords){
        
        # load n grams
        unigrams <- readRDS("data/unigrams02.rds")
        bigrams <-readRDS("data/bigrams02.rds")
        trigrams <- readRDS("data/trigrams02.rds")
        quadgrams <- readRDS("data/quadgrams02.rds")
        pentagrams <- readRDS("data/pentagrams02.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        n <- quanteda::ntoken(prevWords)
        message(paste0("prevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 4){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -4, end = -1)
                print(prevWords)
        }
        
        i <- quanteda::ntoken(prevWords)
        
        message("Check pentagrams that contain:")
        print(prevWords)
        nextWords <- pentagrams[startsWith(feature, paste0(prevWords, " "))]
        
        if (dim(nextWords)[1] > 0 & i == 4){
                nextWords <- head(nextWords, 20)
                root <- quadgrams[feature == prevWords]
                nextWords$score <- nextWords$frequency / root$frequency
        }
        
        else {
                message("No pentagrams containing prevWords found!")
                if(i == 4) prevWords <- sub(".*? ", "", prevWords)
                message("Check quadgrams that contain:")
                print(prevWords)
                nextWords <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
                
                if (dim(nextWords)[1] > 0 & quanteda::ntoken(prevWords) == 3) {
                        
                        nextWords <- head(nextWords, 20)
                        root <- trigrams[feature == prevWords]
                        nextWords$score <- 0.4 * nextWords$frequency / root$frequency
                        
                }
                
                else {
                        message("No quadgrams containing prevWords found!")
                        prevWords <- sub(".*? ", "", prevWords)
                        message("Check trigrams that contain:")
                        print(prevWords)
                        
                        nextWords <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        
                        if (dim(nextWords)[1] > 0 & quanteda::ntoken(prevWords) == 2){
                                nextWords <- head(nextWords, 20)
                                root <- bigrams[feature == prevWords]
                                nextWords$score <- 0.4 * 0.4 * (nextWords$frequency / root$frequency)
                        }
                        
                        else {
                                message("No trigrams containing prevWords found!")
                                prevWords <- sub(".*? ", "", prevWords)
                                message("Look bigrams that contain:")
                                print(prevWords)
                                
                                nextWords <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                                
                                if (dim(nextWords)[1] > 0){
                                        nextWords <- head(nextWords, 20)
                                        root <- unigrams[feature == prevWords]
                                        nextWords$score <- 0.4 * 0.4 * 0.4 * (nextWords$frequency / root$frequency)
                                }
                                
                                #### TODO: Add another if else clause for when no bigram is found, so that then 
                                #          the top 5 unigrams will be chosen
                        }
                        
                }
                
        }
        
        
        
        
        message("Algorithm terminated.")
        nextWords
}

##### Second function is a 4-gram model
predictNextWord <- function(prevWords){
        
        # load n grams
        unigrams <- readRDS("data/unigrams03.rds")
        bigrams <-readRDS("data/bigrams03.rds")
        trigrams <- readRDS("data/trigrams03.rds")
        quadgrams <- readRDS("data/quadgrams03.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        n <- quanteda::ntoken(prevWords)
        message(paste0("prevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 3){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -3, end = -1)
                print(prevWords)
        }
        
        i <- quanteda::ntoken(prevWords)
        
        message("Check quadgrams that contain:")
        print(prevWords)
        nextWords <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
        
        if (dim(nextWords)[1] > 0 & i == 3){
                nextWords <- head(nextWords, 20)
                #root <- trigrams[feature == prevWords]
                #nextWords$score <- nextWords$frequency / root$frequency
        }
        
        else {
                message("No quadgrams containing prevWords found!")
                if(i == 3) prevWords <- sub(".*? ", "", prevWords)
                message("Check trigrams that contain:")
                print(prevWords)
                nextWords <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                
                if (dim(nextWords)[1] > 0 & quanteda::ntoken(prevWords) == 2) {
                        
                        nextWords <- head(nextWords, 20)
                        #root <- bigrams[feature == prevWords]
                        #nextWords$score <- nextWords$frequency / root$frequency
                        
                }
                
                else {
                        message("No trigrams containing prevWords found!")
                        prevWords <- sub(".*? ", "", prevWords)
                        message("Check bigrams that contain:")
                        print(prevWords)
                        
                        nextWords <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        
                        if (dim(nextWords)[1] > 0 & quanteda::ntoken(prevWords) == 1){
                                nextWords <- head(nextWords, 20)
                                #root <- unigrams[feature == prevWords]
                                #nextWords$score <- 0.4 * (nextWords$frequency / root$frequency)
                        }
                        
                        #### TODO: Add another if else clause for when no bigram is found, so that then 
                        #          the top 5 unigrams will be chosen
                        
                }
                
        }
        
        
        
        
        message("Algorithm terminated.")
        nextWords
}


predictNextWord4("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predictNextWord("It would mean the")
predictNextWord("can you follow me and make me the")
predictNextWord("Offense still struggling but the")
predictNextWord("Go on a romantic date at the")
predictNextWord("I'll dust them off and be on my")
predictNextWord("Love that film and haven't seen it in quite some")
predictNextWord("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
predictNextWord("Be grateful for the good times and keep the faith during the")
predictNextWord("If this isn't the cutest thing you've ever seen, then you must be")

predictNextWord("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predictNextWord("I asked about dessert and he started telling me about his")
predictNextWord("I'd give anything to see arctic monkeys this")
predictNextWord("Talking to your mom has the same effect as a hug and helps reduce your")
predictNextWord("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
predictNextWord("a presentation of evidence, and a jury to settle the")
predictNextWord("I can't even hold an uneven number of bags of groceries in each")
predictNextWord("Every inch of you is perfect from the bottom to the")
predictNextWord("I’m thankful my childhood was filled with imagination and bruises from playing")
predictNextWord("I like how the same people are in almost all of Adam Sandler's")


# Stupid Backoff Algorithm (4-gram)
predictNextWord4 <- function(prevWords, verbose = FALSE){
        
        require(data.table)
        
        # load n grams
        unigrams <- readRDS("data/unigrams03.rds")
        bigrams <-readRDS("data/bigrams03.rds")
        trigrams <- readRDS("data/trigrams03.rds")
        quadgrams <- readRDS("data/quadgrams03.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        n <- quanteda::ntoken(prevWords)
        if (verbose == TRUE) message(paste0("PrevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 3){
                prevWords <- stringr::word(prevWords, start = -3, end = -1)
                if (verbose == TRUE) {
                        message("Sentence too long, cutting to")
                        print(prevWords)
                }
        }
        
        
        countDown <- quanteda::ntoken(prevWords) + 1
        # initialize data table
        nextWords <- data.table::data.table(feature = character(), frequency = numeric(), score = numeric())
        
        #### TODO: Add foundWord < 3 later
        i <- 1
        while(countDown >= 0){
                
                if (verbose == TRUE) message(paste0("Countdown: \t", countDown))
                
                if (i == 1 & quanteda::ntoken(prevWords) == 3){
                        if (verbose == TRUE) {
                                message("Check quadgrams that contain:")
                                print(prevWords)
                        }
                        nextWordsTemp <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- head(nextWordsTemp, 20)
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- trigrams[feature %chin% prevWords]
                        nextWordsTemp[, score := nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords,
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 2 & quanteda::ntoken(prevWords) == 2){
                        if (verbose == TRUE) {
                                message("Check trigrams that contain:")
                                print(prevWords)
                        }
                        nextWordsTemp <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- nextWordsTemp[1:20,]
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- bigrams[feature %chin% prevWords]
                        nextWordsTemp[, score := 0.4 * nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 3){
                        if (verbose == TRUE) {
                                message("Check bigrams that contain:")
                                print(prevWords)
                        }
                        nextWordsTemp <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- nextWordsTemp[1:20,]
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- unigrams[feature %chin% prevWords]
                        nextWordsTemp[, score := 0.4 * 0.4 * nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 4){
                        if (verbose == TRUE) message("Check most frequent unigrams")
                        nextWordsTemp <- unigrams[1:20,]
                        nextWordsTemp[, 
                                      score := 0.4 * 0.4 * 0.4 * nextWordsTemp$frequency / sum(unigrams[, frequency])]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        if (verbose == TRUE) print(nextWords)
                }
                
                countDown <- countDown - 1
                i <- i + 1
                
                prevWords <- sub(".*? ", "", prevWords)
                
        }
        
        message("Algorithm terminated.")
        
        return(nextWords[order(-score)])
        
}

# Stupid Backoff Algorithm (5-gram)
predictNextWord5 <- function(prevWords){
        
        require(data.table)
        
        # load n grams
        unigrams <- readRDS("data/unigrams02.rds")
        bigrams <-readRDS("data/bigrams02.rds")
        trigrams <- readRDS("data/trigrams02.rds")
        quadgrams <- readRDS("data/quadgrams02.rds")
        pentagrams <- readRDS("data/pentagrams02.rds")
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        
        n <- quanteda::ntoken(prevWords)
        message(paste0("PrevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 4){
                message("Sentence too long, cutting to")
                prevWords <- stringr::word(prevWords, start = -4, end = -1)
                print(prevWords)
        }
        
        
        countDown <- quanteda::ntoken(prevWords) + 1
        # initialize data table
        nextWords <- data.table::data.table(feature = character(), frequency = numeric(), score = numeric())
        
        #### TODO: Add foundWord < 3 later
        i <- 1
        while(countDown >= 0){
                
                message(paste0("Countdown: \t", countDown))
                
                if (i == 1 & quanteda::ntoken(prevWords) == 4){
                        message("Check pentagrams that contain:")
                        print(prevWords)
                        nextWordsTemp <- pentagrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- head(nextWordsTemp, 20)
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- quadgrams[feature %chin% prevWords]
                        nextWordsTemp[, score := nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords,
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        #print(nextWords)
                }
                
                if (i == 2 & quanteda::ntoken(prevWords) == 3){
                        message("Check quadgrams that contain:")
                        print(prevWords)
                        nextWordsTemp <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- head(nextWordsTemp, 20)
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- trigrams[feature %chin% prevWords]
                        nextWordsTemp[, score := 0.4 * nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        #print(nextWords)
                }
                
                if (i == 3){
                        message("Check trigrams that contain:")
                        print(prevWords)
                        nextWordsTemp <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- head(nextWordsTemp, 20)
                        # cut off only the last word which is our next word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- bigrams[feature %chin% prevWords]
                        nextWordsTemp[, score := 0.4 * 0.4 * nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        #print(nextWords)
                }
                
                if (i == 4){
                        message("Check bigrams that contain:")
                        print(prevWords)
                        nextWordsTemp <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        nextWordsTemp <- head(nextWordsTemp, 20)
                        # cut off only the last word
                        nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                        root <- unigrams[feature %chin% prevWords]
                        nextWordsTemp[, 
                                      score := 0.4 * 0.4 * 0.4 * nextWordsTemp$frequency / root$frequency]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        #print(nextWords)
                }
                
                if (i == 5){
                        message("Check most frequent unigrams")
                        nextWordsTemp <- unigrams[1:20]
                        nextWordsTemp[, 
                                      score := 0.4 * 0.4 * 0.4 * 0.4 *
                                              nextWordsTemp$frequency / sum(unigrams[, frequency])]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords, 
                                           dplyr::filter(nextWordsTemp, 
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        #print(nextWords)
                }
                
                countDown <- countDown - 1
                i <- i + 1
                
                prevWords <- sub(".*? ", "", prevWords)
                
        }
        
        message("Algorithm terminated.")
        
        return(nextWords[order(-score)])
        
}

system.time(predictNextWord5("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))
system.time(predictNextWord4("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))
