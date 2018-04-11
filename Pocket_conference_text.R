rm(list=ls())
graphics.off()

library(tidytext)
library(dplyr)
library(stringr)
library(readtext)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)

data(stop_words)

upload <- readtext("C:/Users/OWECHU/Documents/R/Working projects/Pocket_conference/Emoji_and_the_mind")
upload <- upload[which(upload[, 1] != "Meta.txt"), ]

tidy_count <- upload %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n))
  
ggplot(data = tidy_count, aes(word, n)) +
  geom_col() +
  labs(x = "", y = "Frequency", title = "Frequency of words used in the upload") +
  coord_flip()

ggsave("freq.tiff")

sentiment <- get_sentiments("afinn")

tidy_sentiment <- upload %>%
  unnest_tokens(word, text)%>%
  inner_join(sentiment)%>%
  subset(!duplicated(word))%>%
  subset(select = -c(doc_id))

tidy_sentiment$sign <- ifelse(tidy_sentiment$score < 0, "Negative","Positive")

ggplot(data = tidy_sentiment, aes(x = word, y = score, fill = sign)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "AFINN sentimet score", title = "Sentiment of words used in the upload") + 
  theme(legend.position = "none")

ggsave("sentiment.tiff")

upload_bigrams <- upload %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  count(bigram, sort = TRUE)

bigrams_separated <- upload_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_graph <- bigrams_separated %>%
  filter(n > 1) %>%
  graph_from_data_frame()

V(bigram_graph)$degree <- degree(bigram_graph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(size = degree)) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void() + 
  labs(title = "Network of bigrams used more than once")

ggsave("bigrams.tiff")