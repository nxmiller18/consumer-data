# AUTHOR: Natalie Miller
# PURPOSE: Basic plots on length and readability
# OUTPUTS: length_dists_by_category.png, read_time_vs_readability.png

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Read in data frames for average length by category visualization
policy_texts <- read.csv("all_privacy_policies.csv")
app_list <- read.csv("privacy_policy_list.csv")

# Measure word length of each privacy policy
policy_texts <- policy_texts |>
  mutate(
    word.count = str_count(text, boundary("word"))
  )

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
  select(app, Category, file, text, word.count)

# Create violin plot of privacy policy lengths by category
policy_length_dist <- ggplot(merged, aes(x=reorder(Category, word.count, FUN=median), y=word.count)) +
  geom_violin(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  stat_summary(fun=median, geom="point", shape=23, size=2, fill="white") +
  labs(
    title="Distribution of Privacy Policy Word Counts by App Category",
    x="App Category",
    y="Word Count"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title=element_text(face="bold", size=20, hjust=0.5, color="#202124"),
    axis.title=element_text(face="bold", size=26),
    axis.text.x=element_text(angle=45, hjust=1, size=24, color="#333333"),
    axis.text.y=element_text(size=24, color="#333333"),
    panel.grid.major=element_line(color="#E6E6E6"),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="#FAFAF7", color=NA)
  )

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
  geom_point(alpha=0.7, size=3) +
  labs(
    title="Reading Time and Readability Scores",
    x="Average Time to Read Policy (Minutes)",
    y="Flesch Readability Score (0-100)",
    color="App Category"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 26),
    axis.text = element_text(size = 24, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA),
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 24)
  )

readability_scatter

# Export graphs
ggsave("../figures/length_dists_by_category.png", plot=policy_length_dist)
ggsave("../figures/read_time_vs_readability.png", plot=readability_scatter)