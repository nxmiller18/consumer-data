## AUTHOR: Emiline Labbe
## LAST UPDATED: Oct 27 2025
## PURPOSE: Text sentiment/emotion analysis (trust distribution by category)

setwd("/Users/emilabbe/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)
library(stringr)
library(syuzhet)
library(sentimentr)

#load data
policy.texts <- read.csv("all_privacy_policies.csv")
app.list <- read.csv("privacy_policy_list.csv")

#get word counts for each policy
policy.texts <- policy.texts %>%
  mutate(
    word.count = str_count(text, boundary("word"))
  )

#clean names of apps in app.list to prepare for merge
app.list <- app.list %>%
  mutate(
    app = App.Name %>%
      str_replace_all("[^A-Za-z]", ".")
  ) %>%
  mutate_all(~str_remove(., "\\.$")) %>%
  mutate_all(~str_remove(., "\\.$"))

#merge app.list and data frame with privacy policies
merged <- app.list %>%
  left_join(policy.texts, by="app") %>%
  select(app, Category, file, text, word.count)

merged <- merged %>%
  mutate(
        text = iconv(text, from = "", to = "UTF-8", sub = ""),
        char.count = nchar(text),  #count characters, can adjust for spaces if needed
        avg.word.length = char.count / word.count  #average chars per word
        )

#get sentiment scores using nrc lexicon to evaluate trust emotion
nrc_results <- get_nrc_sentiment(merged$text)
merged <- cbind(merged, nrc_results); colnames(merged)
merged$sentiment_score <- merged$positive - merged$negative

category_counts <- merged %>%
  count(Category) %>%
  mutate(Category_label = paste0(Category, "\n(n=", n, ")"))

merged <- merged %>%
  left_join(category_counts, by = "Category")

#filter out sleep due to low sample count
merged <- merged %>%
  filter(Category != "Sleep")

#aggregate average sentiment by category
avg_sentiment <- merged %>%
  group_by(Category) %>%
  summarize(
    avg_sentiment = mean(sentiment_score, na.rm = TRUE),
            n = n()
    )

#get average for trust emotion by category
trust_avg <- merged %>%
  group_by(Category) %>%
  summarize(avg_trust = mean(trust, na.rm = TRUE),
            count = n())

#plot distribution of trust emotion scores by category (removing sleep)
trust_distribution_plot <- ggplot(merged, aes(x = Category_label, y = trust, fill =Category)) +
  geom_boxplot(alpha=0.7) +
  labs(title = "Distribution of 'Trust' Emotion Scores by App Category",
       y="Trust Score",
       x="App Category") +
  theme(axis.text.x = element_text(angle=60)) +
  guides(fill = "none")

#display plot
trust_distribution_plot

#export
ggsave("../figures/trust_score_by_category.png", plot=trust_distribution_plot)
