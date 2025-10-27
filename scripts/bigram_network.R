# AUTHOR: Natalie Miller
# PURPOSE: Create a preliminary network diagram
# OUTPUTS:

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(tidytext)
library(stringr)
library(widyr)
library(igraph)
library(ggraph)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Reading in data
policy_texts <- read.csv("../data/all_privacy_policies.csv")
app_list <- read.csv("../data/privacy_policy_list.csv")

# Clean names of apps in app.list to prepare for merge
app_list <- app_list |>
  mutate(
    app = App.Name |>
      str_replace_all("[^A-Za-z]", ".")
  ) |>
  mutate_all(~str_remove(., "\\.$")) |>
  mutate_all(~str_remove(., "\\.$"))

# Merge app.list and data frame with privacy policies
merged <- app_list |>
  left_join(policy_texts, by="app") |>
  select(app, Category, file, text)

data("stop_words")
preset_stopwords <- stop_words$word
custom_stopwords <- c("information", "additional", "including", "policy", "applicable",
                      "app", "social", "legal", "privacy", "device", "protection")

stopwords <- c(preset_stopwords, custom_stopwords)

merged_paragraphs <- merged |>
  mutate(paragraphs = str_split(text, "\\n\\s*")) |>
  unnest(paragraphs) |>
  filter(str_squish(paragraphs) != "") |>
  mutate(paragraph_id = row_number())

bigrams <- merged_paragraphs |>
  unnest_tokens(bigram, paragraphs, token="ngrams", n=2)

bigrams_clean <- bigrams |>
  separate(bigram, into=c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stopwords, !word2 %in% stopwords) |>
  unite(bigram, word1, word2, sep = " ")

co_occurrence <- bigrams_clean |>
  pairwise_count(item = bigram, feature = paragraph_id, sort = T)

bigram_counts <- bigrams_clean |>
  count(bigram, sort=T)

top_bigrams <- bigram_counts |>
  slice_max(n, n=20)

top_co_occurrence <- co_occurrence |>
  filter(item1 %in% top_bigrams$bigram & item2 %in% top_bigrams$bigram)

graph <- graph_from_data_frame(top_co_occurrence, directed=F)

V(graph)$deg <- igraph::degree(graph)

network <- ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha=0.2, color = "#5A9BD5", width=0.5) +
  geom_node_point(aes(size = deg), alpha=0.5, color = "#333333", size=0.8) +
  geom_node_text(
    aes(label = ifelse(deg > 2, name, "")),
    repel = TRUE, size = 8, max.overlaps = Inf
  ) +
  theme_void() +
  labs(title = "Network of Two-Word Phrases by Co-Occurrence in Paragraph") +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

network

ggsave("../figures/policy_bigram_network.png", plot=network)
