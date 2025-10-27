# AUTHOR: Natalie Miller
# PURPOSE: Basic plots on length and readability
# OUTPUTS:

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)
library(stringr)

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

