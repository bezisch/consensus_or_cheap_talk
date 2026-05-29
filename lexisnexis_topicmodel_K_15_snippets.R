#This is the script for the approach of using just the snippets around just transition for k15. Taken from lexisnexis_topicmodel_K_10_bis_17 
# Create the csv with top-words based on frex + create csv with frex-scores for each word 

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")

library(quanteda)
library(stm)
library(tidyverse)
library(reshape2)
library(openxlsx)

# Load corpus
load("0 Data/LexisNexis/Data_Cleaned/All_merged_cleaned_splitted_2.RData") # This is the snippet code with additional stopword removal (say, year)

# Convert dfm into stm format
dfm_stm <- convert(dfm, to = "stm")

# Run stm function with K=15
model_15 <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = 15,
  seed = 123,
  max.em.its = 500,
  verbose = TRUE
)

# Inspect topics based on FREX weighting
labels <- labelTopics(model_15, n = 30)
topwords <- data.frame("features" = t(labels$frex))
proportion_base_stop <- as.data.frame(colSums(model_15$theta/nrow(model_15$theta))) # Relative topic shares
result <- cbind(topwords, proportion_base_stop)
result_with_header <- rbind(names(proportion_base_stop), result)

proportion_base_stop <- as.data.frame(colSums(model_15$theta/nrow(model_15$theta))) # Relative topic shares

# Combine feature titles with proportion base stop list entries
result <- cbind(topwords, t(proportion_base_stop))

# Save top 20 features across topics and forms of weighting
write.csv(topwords, file = "topwords_model15_snippets.csv", row.names = FALSE)

# # Most representative articles per topic
# top_doc <- findThoughts(model_15, articles_df$Article, n = 20, meta = meta_df$ID)
# 
# # Create dataframe to store topic data
# result_df <- data.frame()
# 
#
# for (i in 1:15) {
#   topic_name <- paste("Topic", i)
#   topic_indices <- top_doc$index[[topic_name]]
#   topic_articles <- top_doc$docs[[topic_name]]
#   topic_ids <- top_doc$meta[[topic_name]]
#   
#   topic_data <- data.frame(
#     Topic = i,
#     ID = topic_indices,
#     Article = topic_articles
#   )
#   
#   result_df <- rbind(result_df, topic_data)
# }
# 
#
# write.csv(result_df, file = "top_20_k_15_snippets.csv", row.names = FALSE)
# 
# # Find topics per document/article  
top_topic_by_article <- apply(model_15$theta, MARGIN = 1, FUN = which.max)
topic_by_article <- as.data.frame(model_15$theta)

diff_values <- setdiff(meta_df$ID, articles_df$ID)
print(diff_values)
nrow(meta_df)
missing_ids <- setdiff(meta_df$ID, dfm_stm$meta$ID)
print(missing_ids)

meta_df <- meta_df[meta_df$ID != 2099, ] 

topic_by_article <- cbind(meta_df, top_topic_by_article, model_15$theta)
# 
# # Save topics per document 
write.csv(topic_by_article[, !names(topic_by_article) %in% c("Source_File", "Graphic", "ArticleCount", "RelativeShare")], file = "topics_share_k_15_snippets.csv", row.names = FALSE)
# 
# # Prevalence of topics over time 
# model15_prevalence <- stm(
#   documents = dfm_stm$documents,
#   vocab = dfm_stm$vocab, 
#   K = 15,
#   prevalence = ~Date, 
#   data = meta_df,
#   verbose = TRUE
# )
# 
# effect <- estimateEffect(formula = ~Date, stmobj = model15_prevalence, metadata = meta_df)
# 
# write.csv(as.data.frame(effect), file = "effect_table_K_15_date_snippets.csv", row.names = FALSE)

# Normalize FREX scores between 0 and 1
normalize_frex <- function(frex_scores) {
  min_score <- min(frex_scores, na.rm = TRUE)
  max_score <- max(frex_scores, na.rm = TRUE)
  normalized_scores <- (frex_scores - min_score) / (max_score - min_score)
  return(normalized_scores)
}

#https://rdrr.io/cran/stm/man/stm.html
#https://juliasilge.com/blog/sherlock-holmes-stm/
#https://francescocaberlin.blog/2019/07/01/messing-around-with-stm-part-iiib-model-analysis/
# library(tidytext)
# 
# topic_word_matrix <- tidytext::tidy(model?) 
# 
# test <- topic_word_matrix %>%
#   group_by(topic) %>%
#   #top_n(10, beta) %>%
#   arrange(topic, beta) %>% 
#   ungroup() 

# Taken from: https://github.com/bstewart/stm/blob/master/R/labelTopics.R
#Calculate FREX Score
library(tidytext)

topic_word_matrix <- tidytext::tidy(model_15) 
tidy(model_15, matrix = "frex")



frexlabels <- try(calcfrex(logbeta, wordcounts))

extractTopics <- function(model15, topics = NULL, n = 200, frexweight = 0.5) {
  if (n < 1) stop("n must be 1 or greater")
  
  logbeta <- model15$beta$logbeta
  K <- model15$settings$dim$K
  vocab <- model15$vocab
  
  if (is.null(topics)) topics <- 1:nrow(logbeta[[1]])
  
  aspect <- length(logbeta) > 1
  
  out <- list()
  
  if (!aspect) {
    out$topic <- rep(NA_character_, K * n)
    out$word <- rep(NA_character_, K * n)
    out$frex <- rep(NA_real_, K * n)
    
    logbeta <- logbeta[[1]]
    wordcounts <- model15$settings$dim$wcounts$x
    
    frexlabels <- try(calcfrex(logbeta, frexweight, wordcounts), silent = TRUE)
    
    problabels <- apply(logbeta, 1, order, decreasing = TRUE)
    
    idx <- 1
    
    for (k in 1:K) {
      topic <- rep(k, n)
      word <- vocab[problabels[1:n, k]]
      
      if (inherits(frexlabels, "try-error")) {
        frex <- rep(NA_real_, n)
      } else {
        frex <- frexlabels[1:n, k]
      }
      
      out$topic[idx:(idx + n - 1)] <- topic
      out$word[idx:(idx + n - 1)] <- word
      out$frex[idx:(idx + n - 1)] <- frex
      
      idx <- idx + n
    }
  } else {
    # Add code for aspect models if needed
    stop("Aspect models not supported in this example")
  }
  
  return(data.frame(out))
}

extractedTopics <- extractTopics(model_15)
print(extractedTopics)
# have to normalize frex score between 0 and 1. 
normalize_frex <- function(frex_scores) {
  min_score <- min(frex_scores, na.rm = TRUE)
  max_score <- max(frex_scores, na.rm = TRUE)
  
  normalized_scores <- (frex_scores - min_score) / (max_score - min_score)
  
  return(normalized_scores)
}

normalized_frex <- normalize_frex(extractedTopics$frex)

#replace frex with normalized
extractedTopics$frex <- normalized_frex




# Save FREX scores 
write.csv(extractedTopics, file = "k15_snippets_frex.csv", row.names = FALSE)
