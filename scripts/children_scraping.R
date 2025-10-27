# AUTHOR: Maia Forssman
# PURPOSE: preliminary text scraping for "children"
# OUTPUTS: mentions_children_by_category.png

setwd("~/Documents/GitHub/consumer-data/scripts/")

library(tidyverse)
library(readxl)
library(stringr)

# Reading in data
policy.texts <- read.csv("../data/all_privacy_policies.csv")
app.list <- read.csv("../data/privacy_policy_list.csv")

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
  select(app, Category, file, text)

# Determining whether "children" is found in each privacy policy
merged <- merged |>
  mutate(
    mentions_children = str_detect(text, regex("\\bchildren\\b", 
                                               ignore_case = TRUE))
  )

# % of policies that mention "children"
mean(merged$mentions_children, na.rm = TRUE)

#  % of policies mentioning "children" by category
children_by_category <- merged |>
  group_by(Category) |>
  summarize(
    prop_children = mean(mentions_children, na.rm = TRUE)
  ) |>
  arrange(desc(prop_children))

# Create bar chart of average mentions of "children" by category
children_plot <- ggplot(children_by_category, aes(x = reorder(Category, 
                                        prop_children), y = prop_children)) +
  geom_col(show.legend=F) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = 'Percent of Apps Whose Privacy Policies Mention "Children"',
    x = "App Category",
    y = "% Mentioning Children"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Display the plot
children_plot

# Export the graph
ggsave(path.expand("~/Documents/GitHub/consumer-data/figures/mentions_children_by_category.png"),
       plot = children_plot)




