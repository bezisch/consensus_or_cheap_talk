


# This installs the package suitable for LexisNexis Uni, as specified here https://github.com/JBGruber/LexisNexisTools/issues/7

remotes::install_github("JBGruber/LexisNexisTools")

library(LexisNexisTools)
# LexisNexisTools Version 0.2.3.9000


# We start by merging the various single articles from the folders into one single docx. file, repeat this eight times using loops.

library(officer)
library(magrittr)
library(tools)

# Set the path to the folder containing the docx files
folder_path <- "C:/Users/charlott/Nextcloud/Shared/Paper_Tommy_Lotti_Just_Transitions_Archived/Data/LexisNexis/Scraper_Just_Transitions_SA/To_Be_Merged"

# Get a list of all subdirectories in the folder
subdir_list <- list.dirs(path = folder_path, recursive = FALSE)
print(subdir_list) ## have to be 8 different ones, since this version also includes the articles that have been published between 27 March and 6 of July

# Loop through each subdirectory
for (subdir in subdir_list) {
  
  # Define the original long path to the folder
  long_path <- "C:/Users/charlott/Nextcloud/Shared/Paper_Tommy_Lotti_Just_Transitions_Archived/Data/LexisNexis/Scraper_Just_Transitions_SA/Merged"
  
  # Convert the folder path to an absolute path
  abs_path <- normalizePath(long_path, winslash = "/")
  
  # Get the short path name for the folder
  short_path <- shortPathName(abs_path)
  
  # Replace any invalid characters in the subdirectory name
  subdir_name <- gsub("[^[:alnum:]]", "_", basename(subdir))
  
  # Get a list of all the docx files in the subdirectory
  file_list <- list.files(path = subdir, pattern = ".DOCX", full.names = TRUE)
  
  # Create a new empty document
  doc <- read_docx()
  
  # Loop through each file in the list and add its contents to the new document
  for (i in 1:length(file_list)) {
    doc <- doc %>% body_add_docx(file_list[i])
  }
  
  # Define the merged filename, remove the "To_Be_Merged_" prefix from the subdir_name using the gsub function
  merged_filename <- paste0("Merged_", gsub("^To_Be_Merged_", "", subdir_name), ".docx")
  
  # Define the merged file path
  merged_path <- file.path(abs_path, merged_filename)
  
  # Save the merged document for the current subdirectory
  print(doc, target = merged_path)
  
}

# Check size of the docs: must be approx. 10.000 kb.
# Manual step: delete the space at the beginning of the documents, otherwise R cannot read the documents. 7 times.
# This reduces the size of the documents from 10.000 kb to approx 3.000 kb.
# In the future: do this via R, see code chunks in the Helper_Snippets file.


# Next, we want to identify highly similar articles, see https://github.com/JBGruber/LexisNexisTools
# We do this separately for every document, as R chokes at doing the test for the full data set.
# Loop through each file in the list

new_path <- "C:/Users/charlott/Nextcloud/Shared/Paper_Tommy_Lotti_Just_Transitions_Archived/Data/LexisNexis/Scraper_Just_Transitions_SA"


for (file in file_list) {
  
  # Read in the document
  doc <- read_docx(file)

  
  # Perform the modifications
  data <- lnt_read(file, verbose = FALSE)
  
  # Direct duplicates can be filtered like this
  data <- data[!duplicated(data@articles$Article), ]
  
  duplicates_df <- lnt_similarity(texts = data@articles$Article,
                                  dates = data@meta$Date,
                                  IDs = data@articles$ID,
                                  threshold = 0.97) 
  # Inspect the results 
 # lnt_diff(duplicates_df, min = 0, max = Inf)
  
  duplicates_df <- duplicates_df[duplicates_df$rel_dist < 0.2]
  data <- data[!data@meta$ID %in% duplicates_df$ID_duplicate, ]
  
  # Add the modified text to the document
 doc <- body_add_docx(doc, value = as.character(data@articles$Article))
  
  # Save the modified document
print(data, target = new_path, basename(file))
}


# Now merge the 8 docx. files in order to have a unique file containing the > 3000 articles 

# Create a new empty document
doc <- read_docx()

# Get a list of all the docx files in the subdirectory
file_list <- list.files(path = long_path, pattern = ".docx", full.names = TRUE)

# Loop through each file in the list and add its contents to the new document
for (i in 1:length(file_list)) {
  doc <- doc %>% body_add_docx(file_list[i])
}

# Save the merged document. It has a size of 15.000 kb
# done on 07/07 at 10:56
print(doc, target = "C:/Users/charlott/Nextcloud/Shared/Paper_Tommy_Lotti_Just_Transitions_Archived/Data/LexisNexis/Scraper_Just_Transitions_SA/Merged/Merged_All.docx") 



# Do the manual step again, opening the document and deleting the leading blank space AFTER the first article.

# Now open the merged files using the lnt_read function as part of the lexisnexis package

data <- lnt_read("C:/Users/charlott/Nextcloud/Shared/Paper_Tommy_Lotti_Just_Transitions_Archived/Data/LexisNexis/Scraper_Just_Transitions_SA/Merged/Merged_All.docx", verbose = FALSE)
2
meta_df <- data@meta
# Print meta 
head(meta_df, n = 10)

articles_df <- data@articles


# Next, we want to identify highly similar articles, see https://github.com/JBGruber/LexisNexisTools

# Direct duplicates can be filtered like this
data <- data[!duplicated(data@articles$Article), ]

# Similarity measure for articles with small differences, providing texts, dates and IDs separately

duplicates_df <- lnt_similarity(texts = data@articles$Article,
                                dates = data@meta$Date,
                                IDs = data@articles$ID,
                                threshold = 0.97) 
#Checking similiarity for 2454 articles over 1084 dates...

# In this way, we are able to discover 16 duplicates

# Inspect the results 
lnt_diff(duplicates_df, min = 0, max = Inf)

# Chose a good cut-off value, then subset the duplicates_df data.frame and remove the respective articles

duplicates_df <- duplicates_df[duplicates_df$rel_dist < 0.2]
data <- data[!data@meta$ID %in% duplicates_df$ID_duplicate, ]


# Generate new dataframes without highly similar duplicates
meta_df <- data@meta
articles_df <- data@articles
paragraphs_df <- data@paragraphs

# Print meta to see how the data changed
head(meta_df, n = 10)

# Next, we want to lookup keywords, hence we write a loop to create subsets for each pattern we're interested in.
  
patterns <- c("JETP", "justice", "left behind", "ESKOM", "Ramaphosa", "Coal", "Mpumalanga") #"climate financing", "overseas investments"
subset_names <- c("JETP", "justice", "left_behind", "eskom", "ramaphosa", "Coal", "Mpumalanga")
subsets <- list()

for (i in seq_along(patterns)) {
  data@meta$stats <- lnt_lookup(data, pattern = patterns[i])
  subset <- data[!sapply(data@meta$stats, is.null), ]
  sub_name <- subset_names[i]
  subsets[[sub_name]] <- subset
  print(paste0("Object of class 'LNToutput': ", nrow(subset), " articles"))
 # View(subset@meta)
}



# word cloud 

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Load the data as a corpus
docs <- Corpus(VectorSource(articles_df))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("said", "must", "will", "may", "new", "can", "one", "five", "will")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Build a term-document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate the Word cloud 
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Plot word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

########################################################


df <- lnt_convert(data, to = "data.frame", what = "articles") # or what = "paragraphs"
View(df)
## or export it to Excel
rio::export(df, "LN.xlsx")

########################################################
# Initialize an empty list to store the results
results_list <- list()

# Get a list of all the docx files in the subdirectory
file_list <- list.files(path = long_path, pattern = ".docx", full.names = TRUE)

# Loop through each file in the list and add its contents to the new document
for (i in 1:length(file_list)) {
  # Read the current document
  current_doc <- read_docx(file_list[i])
  
  # Extract the text from the current document
  current_text <- docx_summary(current_doc)[[1]]$text
  
  # Extract the date and ID from the file name
  file_name <- basename(file_list[i])
  current_date <- str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")
  current_ID <- str_extract(file_name, "\\d{6}")
  
  # Apply the lnt_similarity function to the current document
  current_results <- lnt_similarity(texts = current_text,
                                    dates = current_date,
                                    IDs = current_ID,
                                    threshold = 0.97)
  
  # Append the current results to the list
  results_list[[i]] <- current_results
}

# Combine the results from all documents into a single data frame
all_results <- do.call(rbind, results_list)

# Filter out duplicates based on the relative distance threshold
all_results_filtered <- all_results[all_results$rel_dist < 0.2, ]

# Filter out duplicate IDs from the original data based on the filtered results
data_filtered <- data[!data@articles$ID %in% all_results_filtered$ID_duplicate, ]



