### Descriptives All
rm()
setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")

library(ggplot2)
library(tm)
library(wordcloud)
library(corpus)

load("0 Data/LexisNexis/Data_Cleaned/All_merged_cleaned_splitted.RData")

### articles per month and year
# monthly versus cumulative article count
# articles/month

article_counts <- table(format(meta_df$Date, "%Y-%m")) ## Have to re-run whole preprocessing file, then it works! (was in year format previously, changed script already)

filtered_counts <- article_counts[names(article_counts) > "2020"]

# Calculate cumulative sum of article counts for filtered dates
cumulative_counts <- cumsum(article_counts[filtered_counts])

# plot of the cumulative articles/month
#lines(cumulative_counts, type = "s", col = "green")

# Plot articles/month
pdf("Text-Tables/Figures/All_descriptives/articles_month_2.pdf")
barplot(filtered_counts, main = "Article Counts by Month", xlab = "Month", ylab = "Count", ylim = c(0,300))
dev.off()


articles_11_2022 <- meta_df$Date > as.Date("2022-11-01") & meta_df$Date < as.Date("2022-11-30")
articles_nov_2022 <- articles_df$Article[articles_11_2022]

output <- "Text-Tables/Figures/All_descriptives/articles_nov_2022.txt"
sink(output)
articles_nov_2022[1:30]
sink()

words<- c("8.5") # 86 articles
words2 <- "Germany"
articles_with_word <- grep(words & words2, articles_df$Article, value = TRUE)
output <- "Text-Tables/Figures/All_descriptives/8_5_billion_2.txt"
sink(output)
articles_with_word
sink()


subset_articles1 <- articles_df[grep("JETP", articles_df$Article), ]
subset_articles2 <- articles_df[grep("JET", articles_df$Article), ]
subset_articles3 <- articles_df[grep("8.5", articles_df$Article), ]
subset_articles4 <- articles_df[grep("climate finance", articles_df$Article), ]
subset_articles5 <- articles_df[grep("R130", articles_df$Article), ] # $8.5bn converted to rand
subset_articles6 <- articles_df[grep("climate fund", articles_df$Article), ] # $8.5bn converted to rand




subset_meta_df <- meta_df[meta_df$ID %in% subset_articles1$ID | 
                            meta_df$ID %in% subset_articles2$ID |
                            meta_df$ID %in% subset_articles3$ID |
                            meta_df$ID %in% subset_articles4$ID |
                            meta_df$ID %in% subset_articles5$ID |
                            meta_df$ID %in% subset_articles6$ID, ]


all_jetp_articles <- subset_articles1$Article
all_jetp_articles <- c(all_jetp_articles, subset_articles2$Article)
all_jetp_articles <- c(all_jetp_articles, subset_articles3$Article)
all_jetp_articles <- c(all_jetp_articles, subset_articles4$Article)
all_jetp_articles <- c(all_jetp_articles, subset_articles5$Article)
all_jetp_articles <- c(all_jetp_articles, subset_articles6$Article)

output <- "Text-Tables/Figures/All_descriptives/all_jetp_articles_2.txt"
sink(output)
all_jetp_articles
sink()


# plot jetp, etc. articles over time

subset_article_counts <- table(format(subset_meta_df$Date, "%Y-%m")) # "%Y-%m" could add
pdf("Text-Tables/Figures/All_descriptives/articles_jetp_month_3.pdf")
barplot(subset_article_counts, 
        main = "Articles Containing JETP by Month", 
        sub = "containing either jetp/8.5bn/climate finance/r130/climate fund", 
        xlab = "Month", ylab = "Count", ylim = c(0,100))
dev.off()

# Same for Years
# articles/Year
article_counts <- table((meta_df$Date))

# cummulative sum/year
cumulative_counts <- cumsum(article_counts)

# Plot articles/year
pdf("Text-Tables/Figures/All_descriptives/articles_year_2.pdf")
barplot(article_counts, main = "Article Counts by Year", xlab = "Year", ylab = "Count", ylim = c(0,1000))
dev.off()

# plot of the cumulative articles/year
lines(cumulative_counts, type = "s", col = "green")

####

# Now, we are ready to create a first word cloud ####

#Build a term-document matrix

dtm <- TermDocumentMatrix(docs)
# Removing very rare terms (that occur only in <5% of docs)
dtm <- removeSparseTerms(dtm, sparse = 0.995)


m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 200)

# eclude terms from histogram (south africa and just transition)
d_excl <- subset(d, !(word %in% c("south", "africa", "just", "transition")))
head(d_excl, 10)

# Generate the Word cloud 
set.seed(1234)
pdf("Text-Tables/Figures/All_descriptives/Wordcloud_All_2.pdf", width = 8)
wordcloud(words = d_excl$word, freq = d_excl$freq, min.freq = 10,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), main = "")
title(main = "Wordcloud", sub = "Minimum Frequency = 10, maximum words = 100, excluding 'south','africa','just','transition'")
dev.off()


# Plot word frequencies
barplot(d_excl[1:10,]$freq, las = 2, names.arg = d_excl[1:10,]$word,
        col ="lightblue", main ="Most frequent words", sub = "excluding 'south','africa','just','transition'",
        ylab = "Word frequencies")

pdf("Text-Tables/Figures/All_descriptives/bar_words_All_2.pdf", width = 10, height = 8)
ggplot(d_excl[1:20, ], aes(x = freq, y = reorder(word, freq))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Most frequent words", x = "Frequency", y = "Word") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))
dev.off()



# most frequent 2-3 compound terms (excluding "just transition" and "south africa")


n_grams <- term_stats(docs, ngrams = 2:3)
exclude_terms <- c("just transition", "south africa")
n_grams <- subset(n_grams, !(term %in% exclude_terms))
head(n_grams)

# barplot for n_gram
barplot(n_grams[1:10,]$count, las = 2, names.arg = n_grams[1:10,]$term,
        col ="lightblue", main ="Most frequent terms", sub = "excluding 'just transition' and 'south africa'",
        ylab = "Term frequencies", cex.axis = 0.7, cex.lab = 0.7, cex.names = 0.55, cex.sub = 0.5)

# barplot for n_gram with ggplot
pdf("Text-Tables/Figures/All_descriptives/bar_terms_All_2.pdf", width = 10, height = 8)
ggplot(n_grams[1:20, ], aes(x = count, y = reorder(term, count))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Most frequent terms", x = "Frequency", y = "Term") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))
dev.off()


# wordcloud for n_gram 
set.seed(123)
pdf("Text-Tables/Figures/All_descriptives/wordcloud_terms_All_2.pdf", width = 8)
wordcloud(words = n_grams$term, freq = n_grams$count, min.freq = 10,         # min freq of 5, 10, 20?
          max.words=100, random.order=FALSE, random.color = F, rot.per=0.35, # Do word cloud for 50, 100, and 150 terms
          colors=brewer.pal(8, "Dark2" ))
title(main = "Wordcloud terms", sub = "Minimum Frequecy = 10 - maximum terms = 100 - excluding 'south africa' and 'just transition'")
dev.off()

#Set space for 2 rows and 3 columns.
# par(mfrow=c(2,3)) -> for the most frequent terms... 10 different plots

########################################################

# First, we want to lookup keywords, hence we write a loop to create subsets for each pattern we're interested in.
# ? df_eskom <- c("Eskom", "eskom", "ESKOM")

# eventually 
patterns <- c("jetp", "jet-ip", "jet-p", "justice", "left behind", "eskom", "ramaphosa", "coal", "mpumalanga","shedding") #"climate financing", "overseas investments"
subset_names <- c("JETP", "jet-ip", "jet-p", "justice", "left_behind","Eskom", "ramaphosa", "Coal", "Mpumalanga", "load-shedding")
subsets <- list() # empty subset list


for (i in seq_along(patterns)) {
  data@meta$stats <- lnt_lookup(data, pattern = patterns[i])
  subset <- data[!sapply(data@meta$stats, is.null), ]
  sub_name <- subset_names[i]
  subsets[[sub_name]] <- subset
  print(paste0("Object of class 'LNToutput': ", nrow(subset), " articles"))
  #View(subset@meta)
}

sum(sapply(subsets, nrow))

# Plot Occurences over time of the keywords

pdf("Text-Tables/Figures/All_descriptives/Normalized_Occurence_Keywords_All_Cleaned_2.pdf")
par(mfrow = c(2, 4)) # Set up 2 rows and 4 columns of plots, for plotting all 7 keywords

for (i in seq_along(subsets)) {
  subset <- subsets[[i]]
  subset@meta$year <- substr(subset@meta$Date, 1, 4) # Extract year (first 4 characters)
  total_articles <- nrow(subset) # Total number of articles in the subset
  counts <- table(subset@meta$year)
  normalized_counts <- counts / total_articles # Normalize counts by dividing by the total number of articles
  
  plot(as.numeric(names(counts)), normalized_counts, type = "o", 
       xlab = "Year", ylab = "Normalized Count",
       ylim = c(0,1),
       main = paste0("'", subset_names[i], "' Over Time"),
       cex.main = 1)
  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
}
dev.off()

# in absolute numbers

pdf("Text-Tables/Figures/All_descriptives/Occurence_Keywords_All_Cleaned_2.pdf")
par(mfrow = c(2, 5)) # Set up 2 rows and 4 columns of plots, for plotting all 7 keywords

for (i in seq_along(subsets)) {
  subset <- subsets[[i]]
  subset@meta$year <- substr(subset@meta$Date, 1, 4) # Extract year (first 4 characters)
  counts <- table(subset@meta$year)
  
  plot(as.numeric(names(counts)), counts, type = "o", 
       xlab = "Year", ylab = "Count",
       main = paste0("'", subset_names[i], "' Over Time"),
       cex.main = 1)
  axis(2, at = seq(0, max(counts), by = 25), labels = seq(0, max(counts), by = 25))
}
dev.off()


