# AUTHOR: Natalie Miller
# PURPOSE: Analyze vague nouns in privacy policies
# OUTPUTS: vague_nouns_frequency.png, vague_nouns_kwic.csv

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(igraph)
library(ggraph)
library(stringr)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Read in privacy policy data
policy_texts <- read.csv("all_privacy_policies.csv")

# Define vague noun phrases to search for
vague_nouns <- c(
  "certain partners", "affiliates", "trusted third parties", 
  "service providers", "vendors", "other entities", 
  "business partners", "selected organizations"
)

# Create regex pattern for case-insensitive matching
vague_pattern <- paste0("\\b(", paste(vague_nouns, collapse = "|"), ")\\b")

# PART 1: FREQUENCY ANALYSIS

# Merge with sensitive data categorization
policy_texts <- policy_texts %>%
  mutate(
    deals_with_sensitive_data = factor(
      deals_with_sensitive_data,
      levels = c("Minimal", "Low", "Medium", "High")
    )
  )

# Count occurrences of each vague noun phrase in each policy
count_vague_nouns <- function(text, phrases) {
  counts <- map_int(phrases, ~str_count(text, regex(.x, ignore_case = TRUE)))
  names(counts) <- phrases
  return(counts)
}

# Apply to all policies
vague_counts <- policy_texts %>%
  select(app, deals_with_sensitive_data, text) %>%
  mutate(
    vague_noun_counts = map(text, ~count_vague_nouns(.x, vague_nouns))
  ) %>%
  select(app, deals_with_sensitive_data, vague_noun_counts) %>%
  unnest_wider(vague_noun_counts)

# Convert to long format for plotting
vague_counts_long <- vague_counts %>%
  pivot_longer(
    cols = -c(app, deals_with_sensitive_data),
    names_to = "vague_noun",
    values_to = "count"
  )

# Calculate total counts by vague noun and sensitivity category
counts_by_category <- vague_counts_long %>%
  group_by(vague_noun, deals_with_sensitive_data) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  group_by(vague_noun) %>%
  mutate(overall_total = sum(total)) %>%
  ungroup() %>%
  arrange(desc(overall_total))

# Create stacked bar chart
frequency_plot <- ggplot(
  counts_by_category, 
  aes(x = reorder(vague_noun, overall_total), y = total, fill = deals_with_sensitive_data)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Minimal" = "#81C784",
      "Low" = "#FFD54F", 
      "Medium" = "#FFB74D",
      "High" = "#E57373"
    ),
    name = "Sensitive Data"
  ) +
  labs(
    title = "Frequency of Vague Nouns by Sensitive Data Category",
    x = "Vague Noun Phrase",
    y = "Total Occurrences"
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    axis.title = element_text(face = "bold", size = 26),
    axis.text = element_text(size = 24, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E6E6E6"),
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 24),
    legend.position = "right"
  )

frequency_plot

# PART 2: KEY WORDS IN CONTEXT

# Create corpus
policy_corpus <- corpus(policy_texts$text, docnames = policy_texts$app)

# Generate KWIC for all vague noun phrases
kwic_results <- kwic(
  tokens(policy_corpus, remove_punct = FALSE),
  pattern = phrase(vague_nouns),
  window = 10,  # 10 words on each side
  valuetype = "regex",
  case_insensitive = TRUE
)

# Convert to dataframe and clean up
kwic_df <- as.data.frame(kwic_results) %>%
  select(docname, from, to, pre, keyword, post, pattern) %>%
  rename(app = docname, vague_noun = pattern)

# Save KWIC results
write.csv(kwic_df, "vague_nouns_kwic.csv", row.names = FALSE)

# Print sample KWIC for each vague noun
for (noun in vague_nouns) {
  cat("\nExamples for '", noun, "':\n", sep = "")
  examples <- kwic_df %>% 
    filter(vague_noun == noun) %>% 
    head(3)
  
  if (nrow(examples) > 0) {
    for (i in 1:nrow(examples)) {
      cat("  ", examples$app[i], ": ...", 
          str_trunc(examples$pre[i], 40, side = "left"), " [", 
          examples$keyword[i], "] ", 
          str_trunc(examples$post[i], 40, side = "right"), "...\n", sep = "")
    }
  } else {
    cat("  (No instances found)\n")
  }
}

# Save analysis data file
write.csv(kwic_df, "vague_nouns_kwic.csv", row.names = FALSE)

# Save plot
ggsave("../figures/vague_nouns_frequency.png", 
       plot = frequency_plot, 
       width = 7, 
       height = 4, 
       dpi = 300)

