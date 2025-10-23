# AUTHOR: Natalie Miller
# PURPOSE: Initial EDA on scraped policies + external data on health app downloads
# OUTPUTS: avg_length_by_category.png, downloads_by_year.png

setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)

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

# Calculate average privacy policy length of each category of app
avg_length <- merged |>
  group_by(Category) |>
  summarize(
    avg.word.count = as.numeric(mean(word.count)),
    n=n()
  )

# Create bar chart of average lengths by category
policy_length <- ggplot(avg_length, aes(x=reorder(`Category`, avg.word.count), y=avg.word.count)) +
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

policy_length

# Read in data frame for downloads by year visualization
app.downloads <- read_excel("health_app_downloads.xlsx", sheet="Data", skip=4)

# Create line chart of number of health app downloads per year
policy_downloads <- ggplot(app.downloads, aes(x=year, y=downloads, group=1)) +
  geom_line() +
  ylim(300,600) +
  labs(
    title="Number of Health App Downloads in the United States by Year",
    x=NULL,
    y="Number of Downloads (millions)"
  ) +
  theme_minimal(base_size=12)

policy_downloads

# Export graphs
ggsave("../figures/avg_length_by_category.png", plot=policy_length)
ggsave("../figures/downloads_by_year.png", plot=policy_downloads)