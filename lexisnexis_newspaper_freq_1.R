### Descriptives Newspapers

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")

library(ggplot2)
library(tm)

load("Data/LexisNexis/Data_Cleaned/All_merged_cleaned.RData")
str(meta_df)

frequency_by_newspaper <- meta_df %>%
  group_by(Newspaper) %>%
  summarize(ArticleCount = n())



frequency_by_newspaper <- frequency_by_newspaper %>%
  mutate(Newspaper = ifelse(grepl("^Bizcom", Newspaper, ignore.case = TRUE), "Bizcommunity", Newspaper)) %>%
  mutate(Newspaper = ifelse(grepl("^Cape Argus", Newspaper, ignore.case = TRUE), "Cape Argus", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^ITWeb", Newspaper, ignore.case = TRUE), "ITWeb", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^Insider", Newspaper, ignore.case = TRUE), "Insider", Newspaper)) %>% 
  mutate(Newspaper = ifelse(grepl("^Pretoria", Newspaper, ignore.case = TRUE), "Pretoria News", Newspaper)) %>% 
  mutate(Newspaper = ifelse(is.na(Newspaper), "No Source", Newspaper)) %>%
  group_by(Newspaper) %>% 
  summarize(ArticleCount = sum(ArticleCount)) %>% 
  arrange(ArticleCount)

ggplot(frequency_by_newspaper, aes(x = reorder(Newspaper, ArticleCount), y = ArticleCount)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Article Count by Newspaper",
       x = "Newspaper",
       y = "Article Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("frequency_newspaper.pdf", width = 14, height = 12, units = "in")



frequency_by_newspaper <- frequency_by_newspaper %>%
  mutate(Newspaper = ifelse(ArticleCount < 5, "Other", Newspaper)) %>%
  group_by(Newspaper) %>%
  summarize(ArticleCount = sum(ArticleCount)) %>%
  arrange(ArticleCount)

total_articles <- sum(frequency_by_newspaper$ArticleCount)
frequency_by_newspaper <- frequency_by_newspaper %>%
  mutate(RelativeShare = ArticleCount / total_articles)

ggplot(frequency_by_newspaper, aes(x = reorder(Newspaper, RelativeShare), y = RelativeShare)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Relative Article Share by Newspaper",
       x = "Newspaper",
       y = "Relative Share") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("relative_share_newspaper_relative.pdf", width = 14, height = 12, units = "in")

write.csv(frequency_by_newspaper, file = "newspaper_classification.csv", )

