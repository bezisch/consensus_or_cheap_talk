# This script is for generating the output for the whole article approach: Copied code from lexisnexis_topicmodel_K_10_bis_17

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")

library(quanteda)
library(stm)
library(tidyverse)
library(reshape2)
library(openxlsx)

# Load corpus
load("0 Data/LexisNexis/Data_Cleaned/All_merged_cleaned_test3.RData") # 

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

# Save top 20 features across topics and forms of weighting
write.csv(topwords, file = "topwords_model15_whole.csv", row.names = FALSE)


proportion_base_stop <- as.data.frame(colSums(model15$theta/nrow(model_15$theta))) # Relative topic shares


# Most representative articles per topic
top_doc <- findThoughts(model_15, articles_df$Article, n = 20, meta = meta_df$ID)

#  df to store topic data
result_df <- data.frame()

# Extract information and add it to the df
for (i in 1:15) {
  topic_name <- paste("Topic", i)
  topic_indices <- top_doc$index[[topic_name]]
  topic_articles <- top_doc$docs[[topic_name]]
  topic_ids <- top_doc$meta[[topic_name]]
  
  # Combine the extracted information into a df
  topic_data <- data.frame(
    Topic = i,
    ID = topic_indices,
    Article = topic_articles
  )
  
  # Append the topic-specific dataframe to the result dataframe
  result_df <- rbind(result_df, topic_data)
}

# Save 
write.csv(result_df, file = "top_20_k_15_whole.csv", row.names = FALSE)

# Find topics per document/article  
top_topic_by_article <- apply(model_15$theta, MARGIN = 1, FUN = which.max)
topic_by_article <- as.data.frame(model_15$theta)
topic_by_article <- cbind(meta_df, top_topic_by_article, model_15$theta)

# Save
write.csv(topic_by_article[, !names(topic_by_article) %in% c("Source_File", "Graphic", "ArticleCount", "RelativeShare")], file = "topics_share_k_15_whole.csv", row.names = FALSE)

# Prevalence of topics over time 
model15_prevalence <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab, 
  K = 15,
  prevalence = ~Date, 
  data = meta_df,
  verbose = TRUE
)

effect <- estimateEffect(formula = ~Date, stmobj = model15_prevalence, metadata = meta_df)

# Save
write.csv(as.data.frame(effect), file = "effect_table_K_15_date_whole.csv", row.names = FALSE)

