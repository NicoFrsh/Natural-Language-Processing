# 5-gram Stupid Backoff Model
predictNextWord5 <- function(prevWords, unigrams, bigrams, trigrams, quadgrams, pentagrams, verbose = FALSE){
        
        # change to lower case
        prevWords <- tolower(prevWords)
        
        
        n <- quanteda::ntoken(prevWords)
        if (verbose == TRUE) message(paste0("PrevWords has ", n, " words."))
        
        # only use at most trigrams to predict
        if (n > 4){
                prevWords <- stringr::word(prevWords, start = -4, end = -1)
                if (verbose == TRUE){
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
                
                if (i == 1 & quanteda::ntoken(prevWords) == 4){
                        if (verbose == TRUE){
                                message("Check pentagrams that contain:")
                                print(prevWords)
                        }
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
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 2 & quanteda::ntoken(prevWords) == 3){
                        if (verbose == TRUE){
                                message("Check quadgrams that contain:")
                                print(prevWords)
                        }
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
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 3){
                        if (verbose == TRUE){
                                message("Check trigrams that contain:")
                                print(prevWords)
                        }
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
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 4){
                        if (verbose == TRUE){
                                message("Check bigrams that contain:")
                                print(prevWords)
                        }
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
                        if (verbose == TRUE) print(nextWords)
                }
                
                if (i == 5){
                        if (verbose == TRUE) message("Check most frequent unigrams")
                        nextWordsTemp <- unigrams[1:20]
                        nextWordsTemp[, 
                                      score := 0.4 * 0.4 * 0.4 * 0.4 *
                                              nextWordsTemp$frequency / sum(unigrams[, frequency])]
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
