setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions/0 Data/LexisNexis/Data_Cleaned")
#setwd("/Users/charlott/Dropbox (Personal)/Paper_Just_Transitions/0 Data/LexisNexis/Data_Cleaned")

# This installs the package suitable for LexisNexis Uni, as specified here https://github.com/JBGruber/LexisNexisTools/issues/7
remotes::install_github("JBGruber/LexisNexisTools")

#library(LexisNexisTools)
# LexisNexisTools Version 0.2.3.9000

library(officer)
library(magrittr)
library(tools)
library(tidyverse)


# Open the merged files using  ReadRDS  
data_all <- readRDS("Merged_all_dupl_drop.rds") 


# 2550 articles

# remove the false positives.. found patterns by manual inspection of false positives
# with addition from closer reading of topic model results 
false_positives <- c("just works", "just in transit to", "first transit was", "natural transition", "just a mere transition", 
                     "security transition", "just in transit to", "into the transition was just", "Sebokeng", "cash-in-transit", "military ouster", "Gaddafi", "Assad",
                     "prison gangs", "armed robbery", "customer relationship", "as the ship was just in transit",
                     "not just traditional leaders", "rocky road to transition", "through the final transition, taking just",
                     "transition to Test cricket","I have just transitioned from one team to another",
                     "transition quite well, having just had elections", "refused to commit to a peaceful transfer of power"
                     )
contains_false_positives <- str_detect(data_all$articles.df$Article, paste(false_positives, collapse = "|", sep = "|"))
filtered_articles_df <- data_all$articles.df[!contains_false_positives, ]
filtered_meta_df <- data_all$meta.df[!contains_false_positives, ]

filtered_articles_df <- filtered_articles_df[!is.na(filtered_meta_df$Date), ]
filtered_meta_df <- filtered_meta_df[!is.na(filtered_meta_df$Date), ]

# List of article IDs to remove after screening with the APSIS review tool
articles_to_remove <- c(
  171, 237, 246, 248, 267, 305, 419, 460, 544, 545,
  546, 551, 553, 558, 561, 620, 696, 709, 739, 740,
  742, 792, 843, 910, 978, 982, 993, 1003, 1019, 1021,
  1056, 1104, 1108, 1109, 1118, 1124, 1199, 1206, 1252, 1269,
  1284, 1304, 1326, 1328, 1335, 1341, 1346, 1347, 1360, 1391,
  1394, 1401, 1411, 1430, 1446, 1448, 1449, 1461, 1462, 1468,
  1469, 1477, 1485, 1500, 1512, 1515, 1516, 1518, 1528, 1532,
  1549, 1554, 1560, 1573, 1574, 1591, 1614, 1648, 1654, 1694,
  1695, 1708, 1731, 1739, 1754, 1755, 1774, 1775, 1776, 1801,
  1804, 1807, 1810, 1815, 1853, 1860, 1867, 1883, 1885, 1889,
  1890, 1891, 1892, 1893, 1895, 1939, 1952, 1954, 1957, 1958,
  1960, 1962, 1972, 1976, 2003, 2012, 2014, 2017, 2018, 2021,
  2022, 2023, 2024, 2026, 2029, 2032, 2033, 2034, 2036, 2038,
  2039, 2046, 2047, 2049, 2052, 2054, 2056, 2061, 2062, 2073,
  2074, 2090, 2091, 2092, 2103, 2106, 2107, 2111, 2116, 2128,
  2129, 2137, 2138, 2144, 2145, 2156, 2158, 2170, 2171, 2172,
  2174, 2184, 2186, 2187, 2189, 2190, 2192, 2194, 2196, 2202,
  2204, 2205, 2206, 2210, 2212, 2213, 2215, 2218, 2219, 2221,
  2225, 2229, 2232, 2236, 2237, 2238, 2244, 2263, 2265, 2267,
  2285, 2288, 2290, 2291, 2292, 2320, 2331, 2333, 2334, 2335,
  2337, 2340, 2346, 2347, 2350, 2357, 2362, 2363, 2365, 2372,
  2382, 2394, 2405, 2407, 2409, 2413, 2416, 2424, 2425, 2427,
  2429, 2430, 2433, 2438, 2441, 2444, 2452, 2453, 2455, 2461,
  2463, 2465, 2467, 2473, 2481, 2482, 2483, 2488, 2492, 2497,
  2506, 2508, 2531, 2533, 2547, 2550, 2552, 2555, 2558, 2580,
  2582, 2583, 2585, 2701, 2703, 2705, 2706, 2784, 2823, 2824,
  2846, 2847, 3026, 3038, 3064, 3076, 3160, 3176, 3241, 3247,
  3271, 3276, 3279, 3284, 3289
)

# Filter out the articles with the specified IDs
filtered_articles_df <- filtered_articles_df[!(filtered_articles_df$ID %in% articles_to_remove), ]
filtered_meta_df <- filtered_meta_df[!(filtered_meta_df$ID %in% articles_to_remove), ]

#filtered_data_all <- list(articles.df = filtered_articles_df, meta.df = filtered_meta_df)


# subset of the entire dataset
filtered_data_all <- list(articles.df = filtered_articles_df, meta.df = filtered_meta_df)

#data_all <- subset(data_all,!grepl(paste0("\\b", paste(false_positives, collapse = "\\b|\\b"), "\\b"), data_all$articles.df$Article, ignore.case = TRUE))

data_all <- list(articles_df = filtered_articles_df, meta_df = filtered_meta_df)
data_all <- subset(data_all, !is.na(data_all$meta_df$Date))

# as a test
words<- "transition to Test cricket" # 1 article, checked
documents_with_word <- grep(words, data_all$articles_df$Article, value = TRUE)
documents_with_word

# remain 2483 articles

# Remove all articles containing less than 70 words -> Not really articles, most are like "previews"
data_all$meta_df$WordCount <- as.numeric(sub(" words", "", data_all$meta_df$Length))

data_all$meta_df <- data_all$meta_df %>%
  filter(WordCount >= 70) # test with 30, 35, 40 words...
data_all$articles_df <- data_all$articles_df %>%
  filter(ID %in% data_all$meta_df$ID)

# remove column again 
data_all$meta_df <- data_all$meta_df %>%
  select(-WordCount)

# ca. 342 removed

# Access data frames
#meta_df <- data_all$meta.df
#articles_df <- data_all$articles.df

#articles_df <- subset(data_all$articles.df, !is.na(data_all$meta.df$Date))
#meta_df <- subset(data_all$meta.df, !is.na(data_all$meta.df$Date))

articles_df <- data_all$articles_df
meta_df <- data_all$meta_df
#meta_df$Date <- 
#paragraphs_df <- data_all$paragraphs_df


# Next, we carry out some data cleaning ####

# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("corpus") # corpus analysis - term frequencies
#install.packages("stopwords") # removing stopwords (different stopword lists)

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(corpus) 
library(stopwords)
library(quanteda)

# Load the data as a corpus
docs <- Corpus(VectorSource(articles_df$Article))

#inspect(docs)

# remove URL
docs <- tm_map(docs, content_transformer(function(x) gsub("<!--EMBED:.*:EMBED-->", "", x, perl = TRUE))) # perl allows dot to match any character 


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@") # ?
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
head(stopwords("english", source = "snowball"),20)
docs <- tm_map(docs, removeWords, stopwords("english", source = "snowball"))
docs <- tm_map(docs, removeWords, c("said", "must", "will", "may", "new", "can", "one", "five", "will", "html", "also", "like","however")) # add more!  don't forget: delete these stopwords (beginning and end often same): "support journalism helps navigate world subscribe mg today just r first three months gain access story best journalism subscriber newsletters events weekly cryptic crossword pmprosignupformhidelabels pmprocheckoutfield labelfirstchild clip rectpx px px px position absolute height px width px overflow hidden jquerydocumentreadyfunction jquerypmproshortselectionchange function var levelid value jquerymgdiscountlinkattrhref https mgcoza accountconfirmorder levellevelid function mgbeforecheckout jquerypasswordvaljquerypasswordval jquerybconfirmemailvaljquerybemailval jquerypmprobtnsubmitcsspointereventsnone cursorallowed jquerypmprobtnsubmitvalplease wait gtagevent singlesubsbutton eventcategory singlesubsbutton https mgcoza accountconfirmorder looking another offer post coal workers worry just transition appeared first mail guardian"                                                                                                        
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# remove quotation marks
docs <- tm_map(docs, content_transformer(function(x) gsub("\"", "", x)))
# remove em-dash
docs <- tm_map(docs, content_transformer(function(x) gsub("—", "", x)))
# remove appostrophe (don't know, but in some cases was not removed by removePunctuation)
docs <- tm_map(docs, content_transformer(function(x) gsub("’", "", x)))

# Text stemming
# docs <- tm_map(docs, stemDocument)
#docs <- tm_map(docs, content_transformer(function(x) gsub("support journalism.*first mail guardian", "", x, perl = TRUE)))


# substitute (\\b - word bound)
docs <- tm_map(docs, content_transformer(gsub), pattern = "\\bafri\\w+", replacement = "africa")
## remove endpart of some guardian articles -> put into preprocessing script later
docs <- tm_map(docs, content_transformer(function(x) gsub("support journalism.*first mail guardian", "", x, perl = TRUE)))


#inspect(docs)


#save(docs, meta_df = meta_df, articles_df = articles_df, file = "All_merged_cleaned.RData")

#save(docs, meta_df = meta_df, articles_df = articles_df, file = "All_merged_cleaned_test.RData") # delete
#load("All_merged_cleaned_test.RData") # delete 

#load("All_merged_cleaned.RData")
# Remove documents with missing or invalid years
meta_df$Date <- substr(meta_df$Date, 1, 4) ## If want Date not as year, but Month/Day -> change here
meta_df <- meta_df[complete.cases(meta_df$Date), ]
meta_df$Date <- as.numeric(meta_df$Date)



#start_year <- 2018
#docs <- docs[meta_df$Date >= start_year]
#meta_df <- meta_df[meta_df$Date >= start_year, ]

# Get the indices of documents to keep
doc_indices <- which(complete.cases(meta_df$Date))

# same for articles
articles_indices <- which(complete.cases(meta_df$Date))

# Subset the documents in docs
docs <- docs[doc_indices]
articles_df <- articles_df[articles_indices,]

tokens <- tokens(docs$content)

#tokens <- tokens_wordstem(tokens, language = "en") 

#applying relative pruning
dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)

topfeatures(dfm, n = 20, scheme = "docfreq")

dfm <- dfm_remove(dfm, c("need", "africa","south", "sa", "just", "transition", "“", "”", "africa’", "–", "’")) # why is the "'" and "-"not removed by removePunctuation command? maybe just not in list there 
# updated stopword list based on topic model results (later: shift that to the cleaning part)
dfm <- dfm_remove(dfm, c("someth", "got", "perhap", "thing", "alway", "think",
                         "feel", "seem", "thought", "anyth", "match", "lot",
                         "knew", "know", "someon", "went", "actual", "might",
                         "ms", "mr", "bit", "came", "realli", "vs", "dr", "saw", "took",
                         "stay", "yes", "ago", "per", "date"))

# For sentiment analysis need stemless words -> dictionary needs original word
df <- as.data.frame(as.matrix(dfm))
str(df)
word_list <- unique(colnames(df))
word_df <- data.frame(word = word_list)

word_df$word_stemmed <- word_df$word %>%
  str_extract("\\b\\w+\\b") %>%
  wordStem(language = "en") # I am allowed to use this because the tokens_wordstem is based on that as well as dfm_wordstem: https://quanteda.io/reference/tokens_wordstem.html

word_df <- word_df %>%
  group_by(word_stemmed) %>%
  summarise(word = str_c(word, collapse = ", "))

write.csv(word_df, file = "dictionary_stem.csv", row.names = FALSE)

dfm <- dfm_wordstem(tokens, language = "en")

#save(docs, meta_df = meta_df, articles_df = articles_df, dfm = dfm, file = "All_merged_cleaned.RData")

#save(docs, meta_df = meta_df, articles_df = articles_df, dfm = dfm, file = "All_merged_cleaned_test2.RData") # delete
#load("All_merged_cleaned_test2.RData") # delete

# add newspaper classification metadata 

#load("All_merged_cleaned.RData")

## need to clean up newspaper names:
frequency_by_newspaper <- meta_df %>%
  group_by(Newspaper) %>%
  summarize(ArticleCount = n())

#meta_df <- merge(meta_df, frequency_by_newspaper, by = "Newspaper", all.x = TRUE)
meta_df <- meta_df %>%
  left_join(frequency_by_newspaper, by = "Newspaper")


meta_df <- meta_df %>%
  mutate(Newspaper = ifelse(grepl("^Bizcom", Newspaper, ignore.case = TRUE), "Bizcommunity", Newspaper)) %>%
  mutate(Newspaper = ifelse(grepl("^Cape Argus", Newspaper, ignore.case = TRUE), "Cape Argus", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^ITWeb", Newspaper, ignore.case = TRUE), "ITWeb", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^Insider", Newspaper, ignore.case = TRUE), "Insider", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^Pretoria", Newspaper, ignore.case = TRUE), "Pretoria News", Newspaper)) %>% 
  mutate(Newspaper = ifelse(ArticleCount < 5, "Other", Newspaper)) %>% 
  mutate(Newspaper = ifelse(is.na(Newspaper), "No Source", Newspaper))
  
# eventually drop CountArticle variable again..
meta_df <- meta_df %>%
  select(-ArticleCount)


news_class <- read.csv("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions/C Narrative Construction/merge_newspaper_classification.csv", sep = ";")

#meta_df <- merge(meta_df, news_class, by = "Newspaper", all.x = TRUE)
meta_df <- meta_df %>%
  left_join(news_class, by = "Newspaper")

#save(docs, meta_df = meta_df, articles_df = articles_df, dfm = dfm, file = "All_merged_cleaned.RData")
#save(docs, meta_df = meta_df, articles_df = articles_df, dfm = dfm, file = "All_merged_cleaned_test3.RData") # delete.. Currently the correct metadata is saved in this dataset. After checking with Lotti, change original dataset. 
# Had wrong IDs in metadata. The error occurred when adding the Newspaper classification to the metadata. Did not use left_join for some reason.
# Save it as a csv file to be opened in Python for the sentiment analysis

# Merge meta_df and articles_df
merged_df <- merge(meta_df, articles_df, by = "ID")


# Save the merged data frame as a CSV file
#setwd('C:/Users/charlott/Dropbox (Personal)/Paper_Just_Transitions/D Sentiment Analysis/data')
#write.csv(merged_df, file = "lexisnexis_for_python.csv", row.names = FALSE)

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions/D Sentiment Analysis/data")
write.csv(merged_df, file = "lexisnexis_for_python_new.csv", row.names = FALSE)
