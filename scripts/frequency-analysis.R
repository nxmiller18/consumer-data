# AUTHOR: Natalie Miller
# PURPOSE: Analyze "unavoidable sharing" language in privacy policies
# OUTPUTS: unavoidable_frequency.png, unavoidable_kwic.csv, unavoidable_position_heatmap.png

setwd("~/GitHub/consumer-data/data")
library(tidyverse)
library(quanteda)
library(stringr)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Read in privacy policy data
policy_texts <- read.csv("all_privacy_policies.csv")

# Define unavoidable sharing keywords/phrases
unavoidable_phrases <- c(
  "required by law",
  "required to",
  "legally required",
  "legal obligation",
  "comply with",
  "compliance with",
  "compelled to",
  "must disclose",
  "must share",
  "must provide",
  "obligated to",
  "obligation to",
  "necessary to comply",
  "as required by",
  "court order",
  "subpoena",
  "legal process",
  "law enforcement"
)

# ============================================================================
# PART 1: FREQUENCY ANALYSIS BY SENSITIVE DATA CATEGORY
# ============================================================================

# Ensure sensitive data categorization is present
policy_texts <- policy_texts %>%
  mutate(
    deals_with_sensitive_data = factor(
      deals_with_sensitive_data,
      levels = c("Minimal", "Low", "Medium", "High")
    )
  )

# Count occurrences of each phrase in each policy
count_unavoidable_phrases <- function(text, phrases) {
  counts <- map_int(phrases, ~str_count(text, regex(.x, ignore_case = TRUE)))
  names(counts) <- phrases
  return(counts)
}

# Apply to all policies
unavoidable_counts <- policy_texts %>%
  select(app, deals_with_sensitive_data, text) %>%
  mutate(
    phrase_counts = map(text, ~count_unavoidable_phrases(.x, unavoidable_phrases))
  ) %>%
  select(app, deals_with_sensitive_data, phrase_counts) %>%
  unnest_wider(phrase_counts)

# Convert to long format
unavoidable_counts_long <- unavoidable_counts %>%
  pivot_longer(
    cols = -c(app, deals_with_sensitive_data),
    names_to = "phrase",
    values_to = "count"
  )

# Calculate total counts by phrase and sensitivity category
counts_by_category <- unavoidable_counts_long %>%
  group_by(phrase, deals_with_sensitive_data) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  group_by(phrase) %>%
  mutate(overall_total = sum(total)) %>%
  ungroup() %>%
  filter(overall_total > 0) %>%  # Only keep phrases that appear at least once
  arrange(desc(overall_total))

# Create stacked bar chart
frequency_plot <- ggplot(
  counts_by_category, 
  aes(x = reorder(phrase, overall_total), y = total, fill = deals_with_sensitive_data)
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
    title = "Frequency of 'Unavoidable Sharing' Language",
    subtitle = "By Sensitive Data Category",
    x = "Phrase",
    y = "Total Occurrences"
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
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

# ============================================================================
# PART 2: KWIC (Keywords in Context) ANALYSIS
# ============================================================================

# Create corpus
policy_corpus <- corpus(policy_texts$text, docnames = policy_texts$app)

# Generate KWIC for all unavoidable phrases
kwic_results <- kwic(
  tokens(policy_corpus, remove_punct = FALSE),
  pattern = phrase(unavoidable_phrases),
  window = 15,  # 15 words on each side for more context
  valuetype = "regex",
  case_insensitive = TRUE
)

# Convert to dataframe and clean up
kwic_df <- as.data.frame(kwic_results) %>%
  select(docname, from, to, pre, keyword, post, pattern) %>%
  rename(app = docname, phrase = pattern)

# Save KWIC results
write.csv(kwic_df, "unavoidable_kwic.csv", row.names = FALSE)

cat("\n=== KWIC Analysis ===\n")
cat("Total KWIC instances found:", nrow(kwic_df), "\n\n")

# Print sample KWIC for top phrases
top_phrases <- counts_by_category %>%
  group_by(phrase) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total)) %>%
  head(8) %>%
  pull(phrase)

for (phrase_name in top_phrases) {
  cat("\nExamples for '", phrase_name, "':\n", sep = "")
  examples <- kwic_df %>% 
    filter(phrase == phrase_name) %>% 
    head(3)
  
  if (nrow(examples) > 0) {
    for (i in 1:nrow(examples)) {
      cat("  [", examples$app[i], "]\n", sep = "")
      cat("  ...", str_trunc(examples$pre[i], 50, side = "left"), " [", 
          examples$keyword[i], "] ", 
          str_trunc(examples$post[i], 50, side = "right"), "...\n\n", sep = "")
    }
  }
}

# ============================================================================
# PART 3: DOCUMENT POSITION HEATMAP
# ============================================================================

# Function to calculate relative position of phrase occurrences in text
get_phrase_positions <- function(text, phrases) {
  # Handle encoding issues
  text_clean <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
  if (is.na(text_clean) || text_clean == "") {
    return(tibble(phrase = character(), position = numeric()))
  }
  
  text_length <- nchar(text_clean, type = "chars")
  
  positions_list <- list()
  
  for (phrase in phrases) {
    # Find all matches
    matches <- str_locate_all(text_clean, regex(phrase, ignore_case = TRUE))[[1]]
    
    if (nrow(matches) > 0) {
      # Calculate relative positions (0 = start, 1 = end)
      relative_positions <- matches[, "start"] / text_length
      
      positions_list[[phrase]] <- tibble(
        phrase = phrase,
        position = relative_positions
      )
    }
  }
  
  if (length(positions_list) > 0) {
    return(bind_rows(positions_list))
  } else {
    return(tibble(phrase = character(), position = numeric()))
  }
}

# Extract positions for all policies
all_positions <- policy_texts %>%
  mutate(
    positions = map(text, ~get_phrase_positions(.x, unavoidable_phrases))
  ) %>%
  select(app, positions) %>%
  unnest(positions)

cat("\n=== Position Analysis ===\n")
cat("Total phrase occurrences with positions:", nrow(all_positions), "\n")

# Only keep phrases that appear enough times for meaningful analysis
phrase_counts_for_heatmap <- all_positions %>%
  count(phrase) %>%
  filter(n >= 5) %>%  # At least 5 occurrences
  pull(phrase)

all_positions_filtered <- all_positions %>%
  filter(phrase %in% phrase_counts_for_heatmap)

# Create bins for document position (aggregated across all phrases)
all_positions_binned <- all_positions_filtered %>%
  mutate(
    position_bin = cut(position, 
                       breaks = seq(0, 1, by = 0.1),
                       labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"),
                       include.lowest = TRUE)
  ) %>%
  count(position_bin)

# Create heatmap showing distribution across document (vertical)
position_heatmap <- ggplot(
  all_positions_binned,
  aes(x = 1, y = position_bin, fill = n)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = n), size = 8, family = "atkinson", fontface = "bold") +
  scale_fill_gradient(
    low = "#FFF5F5",
    high = "#E57373",
    name = "Occurrences"
  ) +
  scale_y_discrete(limits = rev) +  # Reverse so 0-10% is at top
  labs(
    title = "Document Position of 'Unavoidable Sharing' Language",
    subtitle = "Where in privacy policies do these phrases appear?",
    x = "",
    y = "Position in Document"
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 26),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 24, color = "#333333"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 24),
    legend.position = "right"
  )

position_heatmap

# Save frequency plot
ggsave("../figures/unavoidable_frequency.png", 
       plot = frequency_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)

# Save heatmap
ggsave("../figures/unavoidable_position_heatmap.png", 
       plot = position_heatmap, 
       width = 7, 
       height = 7, 
       dpi = 300)