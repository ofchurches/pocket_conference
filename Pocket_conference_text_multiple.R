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
library(scales)

data(stop_words)

upload <- readtext("C:\\Users\\OWECHU\\Documents\\R\\Working projects\\Pocket_conference\\text")

tidy_count <- upload %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)# %>%
  # count(doc_id, word, sort = TRUE) %>%
  # filter(n > 2) %>%
  # mutate(word = reorder(word, n))

frequency <- tidy_count %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(doc_id, word) %>%
  group_by(doc_id) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(doc_id, proportion) %>% 
  gather(doc_id, proportion, c(2, 3, 4, 6)) %>%
  filter(is.na(word) == 0)

#frequency[is.na(frequency)] <- 0

ggplot(frequency, aes(x = proportion, y = `Emoji and mind.txt`)) +
  geom_jitter() +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  #scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~doc_id, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Emoji and mind.txt", x = NULL)