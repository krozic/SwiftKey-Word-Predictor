# library(tibble)
library(dplyr)
library(tidytext)
# library(tidyr)
library(quanteda)
library(stringr)
# library(data.table)

load("../data/frequency_tables/bigram_freq_matrix.RData")
load("../data/frequency_tables/trigram_freq_matrix.RData")
load("../data/frequency_tables/quadgram_freq_matrix.RData")

quad_predict_multi <- function(input_word1, input_word2, input_word3){
        search_query <- paste(input_word1, input_word2, input_word3)
        freq_vector <- as.vector(quadgram_freq_matrix[search_query,])
        word_vector <- colnames(quadgram_freq_matrix)
        prob1 <- max(freq_vector[freq_vector != 0])
        prob2 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1])
        prob3 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1 & freq_vector != prob2])
        prediction <- data.frame(predictions = c(paste(word_vector[which(freq_vector == prob1)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob2)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob3)], collapse = ', ')),
                                 probability = c(prob1, prob2, prob3),
                                 table_type = 'quadgram')
        if (sum(prediction$prob == -Inf)){
                prediction2 <- tri_predict_multi(input_word2, input_word3)
                prediction <- rbind(prediction, prediction2)
                if (sum(prediction2$prob == -Inf)){
                        prediction3 <- bi_predict_multi(input_word3)
                        prediction <- rbind(prediction, prediction3)
                }
        }
        return(prediction)
}

tri_predict_multi <- function(input_word2, input_word3){
        search_query <- paste(input_word2, input_word3)
        freq_vector <- as.vector(trigram_freq_matrix[search_query,])
        word_vector <- colnames(trigram_freq_matrix)
        prob1 <- max(freq_vector[freq_vector != 0])
        prob2 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1])
        prob3 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1 & freq_vector != prob2])
        prediction <- data.frame(predictions = c(paste(word_vector[which(freq_vector == prob1)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob2)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob3)], collapse = ', ')),
                                 probability = c(prob1, prob2, prob3),
                                 table_type = 'trigram')
        if (sum(prediction$prob == -Inf)){
                prediction2 <- bi_predict_multi(input_word3)
                prediction <- rbind(prediction, prediction2)
        }
        return(prediction)
}

bi_predict_multi <- function(input_word3){
        search_query <- input_word3
        freq_vector <- as.vector(bigram_freq_matrix[search_query,])
        word_vector <- colnames(bigram_freq_matrix)
        prob1 <- max(freq_vector[freq_vector != 0])
        prob2 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1])
        prob3 <- max(freq_vector[freq_vector != 0 & freq_vector != prob1 & freq_vector != prob2])
        prediction <- data.frame(predictions = c(paste(word_vector[which(freq_vector == prob1)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob2)], collapse = ', '),
                                                 paste(word_vector[which(freq_vector == prob3)], collapse = ', ')),
                                 probability = c(prob1, prob2, prob3),
                                 table_type = 'bigram')
        return(prediction)
}

predict_words <- function(input_text){
        input_text <- gsub('[^a-zA-Z[:space:]]', '', input_text)
        input_text <- str_squish(input_text)
        input_words <- as.data.frame(input_text) %>% unnest_tokens(word, input_text)
        input_word1 <- input_words[nrow(input_words)-2,]
        input_word2 <- input_words[nrow(input_words)-1,]
        input_word3 <- input_words[nrow(input_words),]
        
        if(nrow(input_words) >= 3){
                tryCatch(
                        quad_predict_multi(input_word1, input_word2, input_word3),
                        error = function(e){
                                tryCatch(
                                        tri_predict_multi(input_word2, input_word3),
                                        error = function(e){
                                                tryCatch(
                                                        bi_predict_multi(input_word3),
                                                        error = function(e){
                                                                return('No predictions for query.')
                                                        }
                                                )
                                        }
                                )
                        }
                        )
        }else if(nrow(input_words) == 2){
                tryCatch(
                        tri_predict_multi(input_word2, input_word3),
                        error = function(e){
                                tryCatch(
                                        bi_predict_multi(input_word3),
                                        error = function(e){
                                                return('No predictions for query.')
                                        }
                                )
                        }
                        )
        }else if(nrow(input_words) == 1){
                tryCatch(
                        bi_predict_multi(input_word3),
                        error = function(e){
                                return('No predictions for query.')
                        }
                )
        }
}