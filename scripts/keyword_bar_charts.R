# AUTHOR: Natalie Miller
# PURPOSE: Create plots measuring frequency of key words
# OUTPUTS: advert_mentions.png, delete_mentions.png, process_mentions.png, third_party_mentions.png, share_mentions.png

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(readxl)
library(stringr)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Reading in data
policy_texts <- read.csv("../data/all_privacy_policies.csv")
app_list <- read.csv("../data/privacy_policy_list.csv")

# Clean names of apps in app.list to prepare for merge
app_list <- app_list |>
  mutate(
    app = App.Name |>
      str_replace_all("[^A-Za-z]", ".")
  ) |>
  mutate_all(~str_remove(., "\\.$")) |>
  mutate_all(~str_remove(., "\\.$"))

# Merge app.list and data frame with privacy policies
merged <- app_list |>
  left_join(policy_texts, by="app") |>
  select(app, Category, file, text)

keywords <- merged |>
  mutate(
    count_third_party = str_count(text, regex("third[ -]?party", ignore_case = T)),
    count_partner = str_count(text, regex("partner", ignore_case = T)),
    count_share = str_count(text, regex("share", ignore_case=T)),
    count_process = str_count(text, regex("process", ignore_case = T)),
    count_advert = str_count(text, regex("advert", ignore_case = T)),
    count_delete = str_count(text, regex("delete", ignore_case = T))
  ) |>
  mutate(
    count_all_share = count_third_party + count_partner + count_share
  ) |>
  select(-file, -text)

keywords_by_category <- keywords |>
  group_by(Category) |>
  summarize(
    third_party = as.numeric(mean(count_third_party)),
    partner = as.numeric(mean(count_partner)),
    share = as.numeric(mean(count_share)),
    process = as.numeric(mean(count_process)),
    advert = as.numeric(mean(count_advert)),
    delete = as.numeric(mean(count_delete)),
    all_share = as.numeric(mean(count_all_share))
  )

third_party_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, third_party), y = third_party)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Third Party by Category",
    x = "App Category",
    y = "Average Mentions of Third Party"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

partner_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, partner), y = partner)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Partner by Category",
    x = "App Category",
    y = "Average Mentions of Partner"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

share_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, share), y = share)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Share by Category",
    x = "App Category",
    y = "Average Mentions of Share"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

process_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, process), y = process)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Process by Category",
    x = "App Category",
    y = "Average Mentions of Process"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

advert_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, advert), y = advert)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Advert by Category",
    x = "App Category",
    y = "Average Mentions of Advert"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

delete_plot <- ggplot(keywords_by_category, aes(x = reorder(Category, delete), y = delete)) +
  geom_col(fill="#5A9BD5", color="#202124", alpha=0.7, show.legend=F) +
  labs(
    title = "Average Mentions of Delete by Category",
    x = "App Category",
    y = "Average Mentions of Delete"
  ) +
  theme_minimal(base_family="atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12, color = "#333333"),
    axis.text.y = element_text(size = 12, color = "#333333"),
    panel.grid.major = element_line(color = "#E6E6E6"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA)
  )

advert_plot
delete_plot
partner_plot
process_plot
share_plot
third_party_plot

ggsave("../figures/advert_mentions.png", plot=advert_plot)
ggsave("../figures/delete_mentions.png", plot=delete_plot)
ggsave("../figures/partner_mentions.png", plot=partner_plot)
ggsave("../figures/process_mentions.png", plot=process_plot)
ggsave("../figures/share_mentions.png", plot=share_plot)
ggsave("../figures/third_party_mentions.png", plot=third_party_plot)