# 1: scraper lexisnexis
source("A Lexisnexis Cleaning Descriptives/LexisNexis_scraper_3.R")

# 2: eliminate the duplicates, if not done before
source("A Lexisnexis Cleaning Descriptives/lexislexis_duplicates_4.R")

# 3: preprocess the data -> creating the corpus

# For the whole-text approach
source("A Lexisnexis Cleaning Descriptives/lexisnexis_preprocessing_7.R") 
  
# For the snippet approach 
source("A Lexisnexis Cleaning Descriptives/lexisnexis_preprocessing_8.R")  
source("A Lexisnexis Cleaning Descriptives/lexisnexis_preprocessing_9.R")  
  
# 4. general descriptives of articles -> e.g. articles/month, pattern, wordcloud, frequencies
source("A Lexisnexis Cleaning Descriptives/lexisnexis_descriptives_4.R") 
source("A Lexisnexis Cleaning Descriptives/lexisnexis_newspaper_freq_1.R") 

# 5. finding the optimal number of clusters K 
source("/B Topic Modelling/searchk_6.R") 