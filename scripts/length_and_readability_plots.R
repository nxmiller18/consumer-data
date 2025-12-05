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
library(ggridges)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Read in data frame
policy_texts <- read.csv("all_privacy_policies.csv")

# Clean sensitivity_level data, calculate read times
policy_texts <- policy_texts |>
  mutate(
    sensitivity_level = factor(
      sensitivity_level,
      levels=c("Minimal", "Low", "Medium", "High")
    ),
    read_time = word_count/238,
    clean_text = str_squish(text)
  )

policy_corpus <- corpus(policy_texts$clean_text, docnames = policy_texts$app)

readability_scores <- textstat_readability(policy_corpus, measure = c("Flesch", "FOG")) |>
  rename(app=document)

policy_texts <- policy_texts |>
  left_join(readability_scores, by="app")

# Calculate summary statistics
summary_stats <- policy_texts |>
  mutate(sensitivity_level = as.character(sensitivity_level)) |>
  bind_rows(
    mutate(policy_texts, sensitivity_level="Overall")
  ) |>
  group_by(sensitivity_level) |>
  select(sensitivity_level, word_count, sensitive_term_count, read_time) |>
  summarise(across(everything(), list(
    median=~median(.x, na.rm=T),
    mean=~mean(.x, na.rm=T),
    sd=~sd(.x, na.rm=T)
  )), .groups="drop")

# Create violin plot of privacy policy lengths by category
policy_time_dist <- ggplot(
  policy_texts, 
  aes(x=sensitivity_level, y=read_time, fill=sensitivity_level)
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
    title="Privacy Policy Read Time by Sensititive Data Level",
    x="Data Sensitivity Level",
    y="Read Time (minutes)"
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

policy_time_dist

# Readability vs Read Time Scatter
readability_scatter <- ggplot(
  policy_texts, 
  aes(x=read_time, y=Flesch, color=sensitivity_level, shape=sensitivity_level)
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

# Readability Ridge Plot
readability_ridges <- ggplot(
  policy_texts,
  aes(x = Flesch, y = sensitivity_level, fill = sensitivity_level)
) +
  geom_density_ridges(
    scale = 1.5,
    rel_min_height = 0.01,
    color = "#202124",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 50,
    linetype = "dashed",
    color = "#202124",
    size = 1.2
  ) +
  annotate(
    "rect",
    xmin = -5, xmax = 50,
    ymin = 0, ymax = Inf,
    alpha = 0.1,
    fill = "#F44336"
  ) +
  scale_fill_manual(
    values = c(
      "Minimal" = "#4CAF50",
      "Low" = "#FFC107",
      "Medium" = "#FF9800",
      "High" = "#F44336"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    limits = c(-5, 70)
  ) +
  labs(
    title = "Most Privacy Policies Require College-Level Reading",
    subtitle = "Distribution of readability scores - policies left of the line require college education",
    x = "Flesch Reading Ease Score",
    y = ""
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 22),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 20, color = "#333333"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank()
  )

readability_ridges

# Export graphs
ggsave("../figures/read_time_dists_by_sensitive_data.png", 
       plot = policy_time_dist, 
       width = 7, 
       height = 4, 
       dpi = 300)

ggsave("../figures/read_time_vs_readability_sensitive.png", 
       plot = readability_scatter, 
       width = 7, 
       height = 4, 
       dpi = 300)

ggsave("../figures/reading_ease_dist.png", 
       plot = readability_ridges, 
       width = 7, 
       height = 4, 
       dpi = 300)