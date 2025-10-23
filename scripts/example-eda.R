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
      str_replace_all("[^A-Za-z0-9]", ".")
  )

merged <- app.list |>
  left_join(policy.texts, by="app") |>
  select(app, Category, file, text, word.count)
