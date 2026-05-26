# Script for removing the duplicates. Takes time to run!

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")
#setwd("/Users/charlott/Dropbox (Personal)/Paper_Just_Transitions")

# This installs the package suitable for LexisNexis Uni, as specified here https://github.com/JBGruber/LexisNexisTools/issues/7
remotes::install_github("JBGruber/LexisNexisTools")

library(LexisNexisTools)
# LexisNexisTools Version 0.2.3.9000

library(officer)
library(magrittr)
library(tools)


data <- lnt_read("0 Data/LexisNexis/Scraper_Just_Transitions_SA/Merged/Merged_All.docx", verbose = FALSE)
#data <- lnt_read("/Users/charlott/Dropbox (Personal)/Paper_Just_Transitions/Data/LexisNexis/Scraper_Just_Transitions_SA/Merged//Merged_all.docx", verbose = FALSE)
2# Script for removing the duplicates. Takes time to run!


# taken from https://github.com/JBGruber/LexisNexisTools

# Convert raw file to LNToutput object
LNToutput <- data

# Test similarity of articles
duplicates.df <- lnt_similarity(
  texts = LNToutput@articles$Article,
  dates = LNToutput@meta$Date,
  IDs = LNToutput@articles$ID,
  rel_dist = FALSE,
  threshold = 0.92
)

## See below for the behavior of threshold versus duplicates categorization

# Threshold = 0.99; 1158 days processed; 748 duplicates found
# Threshold = 0.97; 1158 days processed; 815 duplicates found
# Threshold = 0.95; 1158 days processed; 853 duplicates found
# Threshold = 0.92; 1158 days processed; 892 duplicates found

## Check if the found duplicates are among them (see excel sheet LN Narrative Construction)

specific_ids <- c(1107, 727, 677)
are_ids_present <- specific_ids %in% duplicates.df$ID_original
print(are_ids_present)
duplicates.df <- duplicates.df[order(duplicates.df$ID_original), ]

# 727 is present, investigate:
id_to_check <- 727
text_to_print <- subset(duplicates.df, ID_original == id_to_check)$text_original
print(text_to_print)

print(subset(duplicates.df, ID_original == 727)$text_original)
print(subset(duplicates.df, ID_original == 727)$text_duplicate)


print(subset(meta.df, ID == 727)$Headline)

meta.df <- duplicates.df@meta



library(ggplot2) 

ggplot(duplicates.df, aes(x = Similarity)) +
  geom_density(fill = "blue", color = "black") +
  geom_vline(xintercept = 0.95, color = "red", linetype = "dashed") +
  labs(title = "Kernel Density Plot of Similarity", x = "Similarity", y = "Density")



## Inspect the duplicates in the lower similarity segment 

subset_df <- duplicates.df[duplicates.df$Similarity < 0.97, ]
subset_df <- subset_df[order(-subset_df$ID_original), ]

# for instance, this example shows one article with an added beginning, but the rest is the same
print(subset(subset_df, ID_original == 2672)$text_original)
print(subset(subset_df, ID_original == 2672)$text_duplicate)



# Remove instances with a high relative distance
#duplicates.df <- duplicates.df[duplicates.df$rel_dist < 0.2]

# Create three separate data.frames from cleaned LNToutput object
LNToutput <- LNToutput[!LNToutput@meta$ID %in%
                         duplicates.df$ID_duplicate]

LNToutput[1, ]

meta.df <- LNToutput@meta
articles.df <- LNToutput@articles
paragraphs.df <- LNToutput@paragraphs

meta.df<-meta.df[meta.df$Date > "2004-01-01",]
# Now merge back with articles_df to keep only the 2010> articles
articles.df <- articles.df %>%
  filter(ID %in% meta.df$ID)

meta.df <- meta.df %>%
  filter(ID %in% articles.df$ID)


# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
#saveRDS(list(meta.df = meta.df, articles.df = articles.df, paragraphs.df = paragraphs.df), "Merged_all_dupl_drop.rds")
saveRDS(list(meta.df = meta.df, articles.df = articles.df), "0 Data/LexisNexis/Data_Cleaned/Merged_all_dupl_drop.rds")


# Load the saved file
data_combined <- readRDS("data_combined.rds")

# Access individual data frames
meta.df <- data_combined$meta.df
articles.df <- data_combined$articles.df
paragraphs.df <- data_combined$paragraphs.df

