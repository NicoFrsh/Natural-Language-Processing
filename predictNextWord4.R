# Stupid Backoff Algorithm (4-gram)

predictNextWord4 <- function(prevWords, unigrams, bigrams, trigrams, quadgrams, verbose = FALSE){

        # change to lower case and remove whitespace at beginning and end of string
        prevWords <- tolower(prevWords)
        prevWords <- stringr::str_trim(prevWords)

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


        countDown <- quanteda::ntoken(prevWords)
        # initialize data table
        nextWords <- data.table::data.table(feature = character(), frequency = numeric(), score = numeric())

        while( countDown >= 0 & quanteda::ntoken(prevWords) > 0){

                if (verbose == TRUE) message(paste0("Countdown: \t", countDown))

                # start with 4-grams if there are at least 3 words as input
                if (quanteda::ntoken(prevWords) == 3){
                        if (verbose == TRUE) {
                                message("Check quadgrams that contain:")
                                print(prevWords)
                        }
                        # search 4-grams that start with the three input words
                        nextWordsTemp <- quadgrams[startsWith(feature, paste0(prevWords, " "))]

                        if (!all(is.na(nextWordsTemp))){
                                nextWordsTemp <- head(nextWordsTemp, 6)
                                # cut off only the last word which is our next word
                                nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                                root <- trigrams[feature %chin% prevWords]
                                # compute score based on Stupid Backoff Model
                                nextWordsTemp[, score := nextWordsTemp$frequency / root$frequency]
                                # extend data table without adding already found words
                                nextWords <- rbind(nextWords,
                                                   dplyr::filter(nextWordsTemp,
                                                                 !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                                if (verbose == TRUE) print(nextWords)
                        }
                }

                # continue with trigrams
                if (quanteda::ntoken(prevWords) == 2){
                        if (verbose == TRUE) {
                                message("Check trigrams that contain:")
                                print(prevWords)
                        }
                        
                        nextWordsTemp <- trigrams[startsWith(feature, paste0(prevWords, " "))]
                        if (!all(is.na(nextWordsTemp))){
                                nextWordsTemp <- nextWordsTemp[1:6,]
                                # cut off only the last word which is our next word
                                nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                                root <- bigrams[feature %chin% prevWords]
                                # compute score based on Stupid Backoff Model
                                nextWordsTemp[, score := 0.4 * nextWordsTemp$frequency / root$frequency]
                                # extend data table without adding already found words
                                nextWords <- rbind(nextWords,
                                                   dplyr::filter(nextWordsTemp,
                                                                 !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                                if (verbose == TRUE) print(nextWords)
                        }
                }

                if (quanteda::ntoken(prevWords) == 1){
                        if (verbose == TRUE) {
                                message("Check bigrams that contain:")
                                print(prevWords)
                        }
                        nextWordsTemp <- bigrams[startsWith(feature, paste0(prevWords, " "))]
                        if (!all(is.na(nextWordsTemp))){
                                nextWordsTemp <- nextWordsTemp[1:6,]
                                # cut off only the last word which is our next word
                                nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
                                root <- unigrams[feature %chin% prevWords]
                                # compute score based on Stupid Backoff Model
                                nextWordsTemp[, score := 0.4 * 0.4 * nextWordsTemp$frequency / root$frequency]
                                # extend data table without adding already found words
                                nextWords <- rbind(nextWords,
                                                   dplyr::filter(nextWordsTemp,
                                                                 !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                                if (verbose == TRUE) print(nextWords)
                        }
                }

                # select most frequent unigrams if there was nothing found above
                if (quanteda::ntoken(prevWords) == 1 & all(is.na(nextWords))){
                        if (verbose == TRUE) message("Check most frequent unigrams")
                        nextWordsTemp <- unigrams[1:6,]
                        # extend data table without adding already found words
                        nextWords <- rbind(nextWords,
                                           dplyr::filter(nextWordsTemp,
                                                         !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
                        if (verbose == TRUE) print(nextWords)

                        break
                }

                countDown <- countDown - 1
                #i <- i + 1

                # cut off one word to continue with n-1-gram in the next loop iteration
                prevWords <- sub(".*? ", "", prevWords)

        }

        message("Algorithm terminated.")

        # return nextwords prediction in descending score order
        return(nextWords[order(-score)])

}

## attempting with indexed data tables, performance got worse so didnt use it 

# predictNextWord4 <- function(prevWords, unigrams, bigrams, trigrams, quadgrams, verbose = FALSE){
# 
#         # change to lower case and remove whitespace at beginning and end of string
#         prevWords <- tolower(prevWords)
#         prevWords <- stringr::str_trim(prevWords)
# 
#         n <- quanteda::ntoken(prevWords)
#         if (verbose == TRUE) message(paste0("PrevWords has ", n, " words."))
# 
#         # only use at most trigrams to predict
#         if (n > 3){
#                 prevWords <- stringr::word(prevWords, start = -3, end = -1)
#                 if (verbose == TRUE) {
#                         message("Sentence too long, cutting to")
#                         print(prevWords)
#                 }
#         }
# 
# 
#         countDown <- quanteda::ntoken(prevWords)
#         # initialize data table
#         nextWords <- data.table::data.table(feature = character(), frequency = numeric(), score = numeric())
# 
#         #### TODO: Add foundWord < 3 later
#         #i <- 1
#         while( countDown >= 0 & quanteda::ntoken(prevWords) > 0){
# 
#                 if (verbose == TRUE) message(paste0("Countdown: \t", countDown))
# 
#                 if (quanteda::ntoken(prevWords) == 3){
#                         if (verbose == TRUE) {
#                                 message("Check quadgrams that contain:")
#                                 print(prevWords)
#                         }
#                         nextWordsTemp <- quadgrams[startsWith(feature, paste0(prevWords, " "))]
# 
#                         if (!all(is.na(nextWordsTemp))){
#                                 nextWordsTemp <- nextWordsTemp[order(-frequency)]
#                                 nextWordsTemp <- head(nextWordsTemp, 10)
#                                 # cut off only the last word which is our next word
#                                 nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
#                                 root <- trigrams[feature %chin% prevWords]
#                                 nextWordsTemp[, score := nextWordsTemp$frequency / root$frequency]
#                                 # extend data table without adding already found words
#                                 nextWords <- rbind(nextWords,
#                                                    dplyr::filter(nextWordsTemp,
#                                                                  !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
#                                 if (verbose == TRUE) print(nextWords)
#                         }
#                 }
# 
#                 if (quanteda::ntoken(prevWords) == 2){
#                         if (verbose == TRUE) {
#                                 message("Check trigrams that contain:")
#                                 print(prevWords)
#                         }
#                         nextWordsTemp <- trigrams[startsWith(feature, paste0(prevWords, " "))]
#                         if (!all(is.na(nextWordsTemp))){
#                                 nextWordsTemp <- nextWordsTemp[order(-frequency)]
#                                 nextWordsTemp <- nextWordsTemp[1:10,]
#                                 # cut off only the last word which is our next word
#                                 nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
#                                 root <- bigrams[feature %chin% prevWords]
#                                 nextWordsTemp[, score := 0.4 * nextWordsTemp$frequency / root$frequency]
#                                 # extend data table without adding already found words
#                                 nextWords <- rbind(nextWords,
#                                                    dplyr::filter(nextWordsTemp,
#                                                                  !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
#                                 if (verbose == TRUE) print(nextWords)
#                         }
#                 }
# 
#                 if (quanteda::ntoken(prevWords) == 1){
#                         if (verbose == TRUE) {
#                                 message("Check bigrams that contain:")
#                                 print(prevWords)
#                         }
#                         nextWordsTemp <- bigrams[startsWith(feature, paste0(prevWords, " "))]
#                         if (!all(is.na(nextWordsTemp))){
#                                 nextWordsTemp <- nextWordsTemp[order(-frequency)]
#                                 nextWordsTemp <- nextWordsTemp[1:10,]
#                                 # cut off only the last word which is our next word
#                                 nextWordsTemp <- nextWordsTemp[, .(feature = sub(".* ", "", feature), frequency)]
#                                 root <- unigrams[feature %chin% prevWords]
#                                 nextWordsTemp[, score := 0.4 * 0.4 * nextWordsTemp$frequency / root$frequency]
#                                 # extend data table without adding already found words
#                                 nextWords <- rbind(nextWords,
#                                                    dplyr::filter(nextWordsTemp,
#                                                                  !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
#                                 if (verbose == TRUE) print(nextWords)
#                         }
#                 }
# 
#                 if (quanteda::ntoken(prevWords) == 1 & all(is.na(nextWords))){
#                         if (verbose == TRUE) message("Check most frequent unigrams")
#                         nextWordsTemp <- unigrams[1:10,]
#                         # extend data table without adding already found words
#                         nextWords <- rbind(nextWords,
#                                            dplyr::filter(nextWordsTemp,
#                                                          !feature %chin% intersect(nextWords$feature, nextWordsTemp$feature)))
#                         if (verbose == TRUE) print(nextWords)
# 
#                         break
#                 }
# 
#                 countDown <- countDown - 1
#                 #i <- i + 1
# 
#                 prevWords <- sub(".*? ", "", prevWords)
# 
#         }
# 
#         message("Algorithm terminated.")
# 
#         return(nextWords[order(-score)])
# 
# }
