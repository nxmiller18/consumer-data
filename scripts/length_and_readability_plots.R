# AUTHOR: Natalie Miller
# PURPOSE: Basic plots on length and readability
# OUTPUTS: length_dists_by_category.png, read_time_vs_readability.png

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)
library(stringr)
library(quanteda)
library(quanteda.textstats)

# Read in data frames for average length by category visualization
policy.texts <- read.csv("all_privacy_policies.csv")
app.list <- read.csv("privacy_policy_list.csv")

# Measure word length of each privacy policy
policy.texts <- policy.texts |>
  mutate(
    word.count = str_count(text, boundary("word"))
  )

# Clean names of apps in app.list to prepare for merge
app.list <- app.list |>
  mutate(
    app = App.Name |>
      str_replace_all("[^A-Za-z]", ".")
  ) |>
  mutate_all(~str_remove(., "\\.$")) |>
  mutate_all(~str_remove(., "\\.$"))

# Merge app.list and data frame with privacy policies
merged <- app.list |>
  left_join(policy.texts, by="app") |>
  select(app, Category, file, text, word.count)

# Create violin plot of privacy policy lengths by category
policy_length_dist <- ggplot(merged, aes(x=reorder(Category, word.count, FUN=median), y=word.count)) +
  geom_violin(fill="skyblue", color="black", alpha=0.7, show.legend=FALSE) +
  labs(
    title="Distribution of Privacy Policy Word Counts by App Category",
    x="App Category",
    y="Word Count"
  ) +
  theme_minimal()

policy_length_dist

# Calculate app policy reading times

merged <- merged |>
  mutate(time=word.count/238) |>
  mutate(clean.text=str_squish(text))

policy_corpus <- corpus(merged$clean.text, docnames = merged$app)

readability_scores <- textstat_readability(policy_corpus, measure = c("Flesch", "FOG")) |>
  rename(app=document)

merged_readability <- merged |>
  left_join(readability_scores, by="app")

readability_scatter <- 
  ggplot(merged_readability, aes(x=time, y=Flesch, color=Category)) +
  geom_point(alpha=0.7, size=2) +
  theme_minimal(base_size=12) +
  labs(
    title="Reading Time and Readability Scores",
    x="Average Time to Read Policy (Minutes)",
    y="Flesch Readability Score (0-100)",
    color="App Category"
  )

readability_scatter

# Export graphs
ggsave("../figures/length_dists_by_category.png", plot=policy_length_dist)
ggsave("../figures/read_time_vs_readability.png", plot=readability_scatter)