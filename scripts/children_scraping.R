# AUTHOR: Maia Forssman
# PURPOSE: preliminary text scraping for "children"
# OUTPUTS: mentions_children_by_category.png

setwd("~/Documents/GitHub/consumer-data/scripts/")

library(tidyverse)
library(readxl)
library(stringr)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

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
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = 'Percent of Apps Whose Privacy Policies Mention "Children"',
    x = NULL,
    y = "% Mentioning Children"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 26),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24, color = "#333333"),
    axis.text.y = element_text(size = 24, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank()
  )

# Display the plot
children_plot

# Export the graph
ggsave("../figures/mentions_children_by_category.png", plot = children_plot)




