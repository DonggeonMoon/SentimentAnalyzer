setwd("C:/Users/MDG/Desktop/YCC")

text = read.csv("YCC.csv", header=FALSE, sep=",",quote="\"", encoding="UTF-8")
View(text)

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)
theme_set(theme_minimal())

text[6] = as.chrarcter(text[6])

text_df = data.frame(text[6])
names(text_df)[1] = "text"
text_df$text = as.caharacter(text_df$text)

tidy = text_df %>%
  unnest_tokens(word, text)

bing_word_counts = tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Words that contribute to positive and negative sentiment in the reviews")
