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

# Define vague nouns (from previous analysis)
vague_nouns <- c(
  "service providers",
  "affiliates", 
  "vendors",
  "business partners",
  "certain partners",
  "trusted third parties",
  "other entities",
  "selected organizations"
)

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
  "as required by"
)

# ============================================================================
# EXTRACT PARAGRAPHS WITH BOTH VAGUE NOUNS AND UNAVOIDABLE LANGUAGE
# ============================================================================

# Function to extract detailed paragraph info
extract_paragraph_details <- function(text, app_name, sensitive_cat) {
  # Clean text
  text_clean <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
  if (is.na(text_clean) || text_clean == "") {
    return(tibble())
  }
  
  # Split into paragraphs
  paragraphs <- str_split(text_clean, "\n+")[[1]]
  text_length <- nchar(text_clean, type = "chars")
  
  # Create patterns
  vague_pattern <- regex(paste(vague_nouns, collapse = "|"), ignore_case = TRUE)
  
  results <- tibble()
  current_position <- 1
  
  for (i in seq_along(paragraphs)) {
    para <- paragraphs[i]
    para_length <- nchar(para, type = "chars")
    
    # Check if paragraph has BOTH vague noun AND unavoidable phrase
    has_vague <- str_detect(para, vague_pattern)
    
    if (has_vague) {
      # Find which unavoidable phrases appear (any of them)
      unavoidable_found <- unavoidable_phrases[str_detect(para, regex(unavoidable_phrases, ignore_case = TRUE))]
      
      if (length(unavoidable_found) > 0) {
        # Calculate relative position (middle of paragraph)
        relative_position <- (current_position + para_length / 2) / text_length
        position_bin <- cut(relative_position, 
                            breaks = seq(0, 1, by = 0.1),
                            labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"),
                            include.lowest = TRUE)
        
        # One row per PARAGRAPH (not per phrase)
        results <- bind_rows(results, tibble(
          app = app_name,
          sensitive_cat = sensitive_cat,
          paragraph_num = i,
          position = relative_position,
          position_bin = as.character(position_bin),
          phrases_in_paragraph = paste(unavoidable_found, collapse = "; "),
          num_phrases = length(unavoidable_found),
          paragraph_text = str_trunc(para, 200)
        ))
      }
    }
    
    current_position <- current_position + para_length + 1
  }
  
  return(results)
}

# Ensure sensitive data categorization is present
policy_texts <- policy_texts %>%
  mutate(
    deals_with_sensitive_data = factor(
      deals_with_sensitive_data,
      levels = c("Minimal", "Low", "Medium", "High")
    )
  )

# Extract all relevant paragraphs with details
cat("\n=== Extracting Paragraphs ===\n")
all_paragraphs <- policy_texts %>%
  mutate(
    para_details = pmap(list(text, app, deals_with_sensitive_data), 
                        ~extract_paragraph_details(..1, ..2, ..3))
  ) %>%
  select(para_details) %>%
  unnest(para_details)

# PART 1: FREQUENCY ANALYSIS

# Count paragraphs containing each phrase (a paragraph can contain multiple phrases)
# We'll count how many paragraphs contain each phrase
phrase_counts <- all_paragraphs %>%
  # For each paragraph, create one row per phrase it contains
  mutate(phrases_list = str_split(phrases_in_paragraph, "; ")) %>%
  unnest(phrases_list) %>%
  # Count unique paragraphs per phrase and category
  group_by(phrases_list, sensitive_cat) %>%
  summarise(n_paragraphs = n(), .groups = "drop") %>%
  rename(phrase = phrases_list)

# Calculate totals for ordering
phrase_totals <- phrase_counts %>%
  group_by(phrase) %>%
  summarise(total = sum(n_paragraphs), .groups = "drop")

# Create stacked bar chart
frequency_plot <- phrase_counts %>%
  left_join(phrase_totals, by = "phrase") %>%
  ggplot(aes(x = reorder(phrase, total), y = n_paragraphs, fill = sensitive_cat)) +
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
    title = "Paragraphs with 'Unavoidable Sharing' Language",
    subtitle = "Co-occurring with vague noun references, by sensitive data category",
    x = "Phrase",
    y = "Number of Paragraphs"
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
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

if (nrow(all_paragraphs) > 0) {
  # Create corpus from paragraphs
  relevant_corpus <- corpus(all_paragraphs$paragraph_text, 
                            docnames = paste0(all_paragraphs$app, "_", 
                                              seq_len(nrow(all_paragraphs))))
  
  # Generate KWIC for all unavoidable phrases
  kwic_results <- kwic(
    tokens(relevant_corpus, remove_punct = FALSE),
    pattern = phrase(unavoidable_phrases),
    window = 15,
    valuetype = "regex",
    case_insensitive = TRUE
  )
  
  # Convert to dataframe
  kwic_df <- as.data.frame(kwic_results) %>%
    select(docname, from, to, pre, keyword, post, pattern) %>%
    mutate(app = str_remove(docname, "_\\d+$")) %>%
    select(app, from, to, pre, keyword, post, pattern) %>%
    rename(phrase = pattern)
}

# PART 3: DOCUMENT POSITION HEATMAP

if (nrow(all_paragraphs) > 0) {
  # Create bins for document position (one entry per paragraph)
  position_binned <- all_paragraphs %>%
    count(position_bin) %>%
    complete(position_bin = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"), 
             fill = list(n = 0))
  
  # Create heatmap showing distribution across document
  position_heatmap <- ggplot(
    position_binned,
    aes(x = 1, y = factor(position_bin, levels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%")), fill = n)
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = n), size = 8, family = "atkinson", fontface = "bold") +
    scale_fill_gradient(
      low = "#FFF5F5",
      high = "#E57373",
      name = "Paragraphs"
    ) +
    scale_y_discrete(limits = rev) +
    labs(
      title = "Position of Unavoidable Language with Vague Nouns",
      subtitle = "Number of paragraphs at each document position",
      x = "",
      y = "Position in Document"
    ) +
    theme_minimal(base_family = "atkinson") +
    theme(
      plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
      plot.subtitle = element_text(size = 20, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", size = 26),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 24, color = "#333333"),
      panel.grid = element_blank(),
      legend.title = element_text(face = "bold", size = 25),
      legend.text = element_text(size = 24),
      legend.position = "right"
    )
}

position_heatmap

# Save KWIC results
write.csv(kwic_df, "unavoidable_kwic.csv", row.names = FALSE)

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
