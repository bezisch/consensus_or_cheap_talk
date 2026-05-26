# Find optimal number of topics using statistical fit

setwd("/Users/giacomoraederscheidt/Dropbox/Paper_Just_Transitions")

library(quanteda)
library(stm)
library(tidyverse)
#library(ggplot2)
library(reshape2)
#library(stringr)

# load corpus 

load("0 Data/LexisNexis/Data_Cleaned/All_merged_cleaned_splitted.RData")


# convert dfm into stm format
dfm_stm <- convert(dfm, to = "stm")


# check out: https://juliasilge.com/blog/evaluating-stm/
#library(furrr)
#library(tidyverse)
#library(future)
#plan(multisession)
#many_models <- data_frame(K = c(4:16)) %>%
#  mutate(topic_model = future_map(K, ~stm(dfm_stm, K = .,
#                                          verbose = FALSE)))

K <- c(5:30)
fit <- searchK(dfm_stm$documents, dfm_stm$vocab, K = K, verbose = TRUE)

output <- "B Topic Modelling"

# Create graph
plot <- data.frame("K" = K, 
                   "Coherence" = unlist(fit$results$semcoh), # how often features describing a topic co-occur and topics thus appear to be internally coherent.
                   "Exclusivity" = unlist(fit$results$exclus)) # how much they differ from each other and topics thus appear to describe different things.

# Reshape to long format
plot <- melt(plot, id=c("K"))
plot

#Plot result
pdf(paste0(output, "search_k_base_6.pdf"))

ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "Number of topics K",
       title = "Statistical fit of models with different K") +
  theme_minimal()
dev.off()
# looking at coherence and exclusivity it seems like 10 or 11 topics have the best fit







