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

policy_texts <- policy_texts |>
  mutate(
    sensitivity_level = factor(
      sensitivity_level,
      levels=c("Minimal", "Low", "Medium", "High")
    )
  )

# Create violin plot of privacy policy lengths by category
policy_length_dist <- ggplot(
  policy_texts, 
  aes(x=sensitivity_level, y=word_count, fill=sensitivity_level)
  ) +
  geom_violin(color="#202124", alpha=0.7, show.legend=F) +
  stat_summary(fun=median, geom="point", shape=23, size=2, fill="white") +
  scale_fill_manual(values=c(
    "Minimal" = "#4CAF50",
    "Low" = "#FFC107",
    "Medium" = "#FF9800",
    "High" = "#F44336"
  )) +
  labs(
    title="Privacy Policy Length by Sensitive Data Usage",
    x="Data Sensitivity Level",
    y="Word Count"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title = element_text(face = "bold", size = 26),
    axis.text.x = element_text(size = 24, color = "#333333"),
    axis.text.y = element_text(size = 24, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank()
  )

policy_length_dist

# Calculate app policy reading times

policy_texts <- policy_texts |>
  mutate(time=word_count/238,
         clean_text=str_squish(text))

policy_corpus <- corpus(policy_texts$clean_text, docnames = policy_texts$app)

readability_scores <- textstat_readability(policy_corpus, measure = c("Flesch", "FOG")) |>
  rename(app=document)

policy_texts <- policy_texts |>
  left_join(readability_scores, by="app")

readability_scatter <- ggplot(
  policy_texts, 
  aes(x=time, y=Flesch, color=sensitivity_level, shape=sensitivity_level)
  ) +
  geom_point(alpha=0.7, size=3) +
  scale_color_manual(
    values=c(
    "Minimal" = "#4CAF50",
    "Low" = "#FFC107",
    "Medium" = "#FF9800",
    "High" = "#F44336"
    ),
    name="Sensitive Data"
  ) +
  scale_shape_manual(
    values = c("Minimal" = 16, "Low" = 17, "Medium" = 15, "High" = 18),
    name = "Sensitive Data"
  ) +
  labs(
    title="Reading Time and Readability Scores by Sensitive Data",
    x="Average Time to Read Policy (Minutes)",
    y="Flesch Readability Score (0-100)",
    color="App Category"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 26),
    axis.text = element_text(size = 24, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 24),
    legend.position = "right"
  )

readability_scatter

# Export graphs
ggsave("../figures/length_dists_by_sensitive_data.png", 
       plot = policy_length_dist, 
       width = 7, 
       height = 4, 
       dpi = 300)

ggsave("../figures/read_time_vs_readability_sensitive.png", 
       plot = readability_scatter, 
       width = 7, 
       height = 4, 
       dpi = 300)