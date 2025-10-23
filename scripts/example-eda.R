# AUTHOR: Natalie Miller
# PURPOSE: Initial EDA on scraped policies + external data on health app downloads

setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)

policy.texts <- read.csv("all_privacy_policies.csv")
app.list <- read.csv("privacy_policy_list.csv")

policy.texts <- policy.texts |>
  mutate(
    word.count = str_count(text, boundary("word"))
  )

app.list <- app.list |>
  mutate(
    app = App.Name |>
      str_replace_all("[^A-Za-z]", ".")
  ) |>
  mutate_all(~str_remove(., "\\.$")) |>
  mutate_all(~str_remove(., "\\.$"))

merged <- app.list |>
  left_join(policy.texts, by="app") |>
  select(app, Category, file, text, word.count)

avg_length <- merged |>
  group_by(Category) |>
  summarize(
    avg.word.count = as.numeric(mean(word.count)),
    n=n()
  )

ggplot(avg_length, aes(x=`Category`, y=avg.word.count)) +
  geom_col(show.legend=F) +
  labs(
    title="Average Privacy Policy Link by App Category",
    x="App Category",
    y="Average Word Count"
  ) +
  theme_minimal(base_size=14) |
  theme(
    axis.text.x=element_text(angle=30, hjust=1)
  )
