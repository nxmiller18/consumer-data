# AUTHOR: Natalie Miller
# PURPOSE: Initial EDA on scraped policies + external data on health app downloads
# OUTPUTS: avg_length_by_category.png, downloads_by_year.png

setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)

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

str(avg_length)

length <- ggplot(avg_length, aes(x=reorder(`Category`, avg.word.count), y=avg.word.count)) +
  geom_col(show.legend=F) +
  labs(
    title="Average Privacy Policy Link by App Category",
    x="App Category",
    y="Average Word Count"
  ) +
  theme_minimal(base_size=12) +
  theme(
    axis.text.x=element_text(angle=30, hjust=1)
  )

length

app.downloads <- read_excel("health_app_downloads.xlsx", sheet="Data", skip=4)

downloads <- ggplot(app.downloads, aes(x=year, y=downloads, group=1)) +
  geom_line() +
  ylim(300,600) +
  labs(
    title="Number of Health App Downloads in the United States by Year",
    x=NULL,
    y="Number of Downloads (millions)"
  ) +
  theme_minimal(base_size=12)

# Export graphs
ggsave("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/figures/avg_length_by_category.png", 
       plot=length)
ggsave("C:Users/natal/OneDrive/Documents/GitHub/consumer-data/figures/downloads_by_year.png", 
       plot=downloads)