# AUTHOR: Natalie Miller
# PURPOSE: Analyze "routine sharing" language in privacy policies
# OUTPUTS: routine_frequency.png, routine_kwic.csv, routine_position_heatmap.png

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

# Define routine sharing keywords/phrases
routine_phrases <- c(
  "improve our services",
  "improve services",
  "improve the services",
  "provide analytics",
  "analytics purposes",
  "analytical purposes",
  "enhance user experience",
  "enhance your experience",
  "improve user experience",
  "optimize performance",
  "optimize our",
  "develop new features",
  "new features",
  "provide support",
  "customer support",
  "technical support",
  "provide customer service",
  "maintain and improve",
  "operate and improve",
  "personalize",
  "customized experience",
  "tailor"
)

# ============================================================================
# EXTRACT PARAGRAPHS WITH BOTH VAGUE NOUNS AND ROUTINE LANGUAGE
# ============================================================================

# Function to extract paragraphs that contain BOTH vague nouns AND routine phrases
extract_relevant_paragraphs <- function(text) {
  # Split into paragraphs
  paragraphs <- str_split(text, "\n+")[[1]]
  
  # Create patterns
  vague_pattern <- regex(paste(vague_nouns, collapse = "|"), ignore_case = TRUE)
  routine_pattern <- regex(paste(routine_phrases, collapse = "|"), ignore_case = TRUE)
  
  # Find paragraphs with BOTH
  has_vague <- str_detect(paragraphs, vague_pattern)
  has_routine <- str_detect(paragraphs, routine_pattern)
  
  relevant_paragraphs <- paragraphs[has_vague & has_routine]
  
  return(relevant_paragraphs)
}

# Extract relevant paragraphs for all policies
relevant_paragraphs_data <- policy_texts %>%
  mutate(
    relevant_paragraphs = map(text, extract_relevant_paragraphs)
  ) %>%
  select(app, text, relevant_paragraphs) %>%
  mutate(
    num_relevant_paragraphs = map_int(relevant_paragraphs, length)
  )

cat("\n=== Paragraph Extraction ===\n")
cat("Policies with co-occurring paragraphs:", 
    sum(relevant_paragraphs_data$num_relevant_paragraphs > 0), 
    "out of", nrow(policy_texts), "\n")
cat("Total relevant paragraphs:", sum(relevant_paragraphs_data$num_relevant_paragraphs), "\n\n")

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

# Count occurrences of each phrase ONLY in relevant paragraphs (those with vague nouns)
count_routine_in_relevant_paragraphs <- function(relevant_paragraphs, phrases) {
  # Combine all relevant paragraphs into one text
  combined_text <- paste(relevant_paragraphs, collapse = " ")
  
  counts <- map_int(phrases, ~str_count(combined_text, regex(.x, ignore_case = TRUE)))
  names(counts) <- phrases
  return(counts)
}

# Apply to all policies using relevant paragraphs
routine_counts <- relevant_paragraphs_data %>%
  left_join(policy_texts %>% select(app, deals_with_sensitive_data), by = "app") %>%
  mutate(
    phrase_counts = map(relevant_paragraphs, ~count_routine_in_relevant_paragraphs(.x, routine_phrases))
  ) %>%
  select(app, deals_with_sensitive_data, phrase_counts) %>%
  unnest_wider(phrase_counts)

# Convert to long format
routine_counts_long <- routine_counts %>%
  pivot_longer(
    cols = -c(app, deals_with_sensitive_data),
    names_to = "phrase",
    values_to = "count"
  )

# Calculate total counts by phrase and sensitivity category
counts_by_category <- routine_counts_long %>%
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
    title = "Frequency of 'Routine Sharing' Language",
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

# Save frequency plot
ggsave("../figures/routine_frequency.png", 
       plot = frequency_plot, 
       width = 16, 
       height = 12, 
       dpi = 300)

# ============================================================================
# PART 2: KWIC (Keywords in Context) ANALYSIS
# ============================================================================

# Create corpus from ONLY relevant paragraphs (those with both vague nouns and routine language)
relevant_paragraphs_expanded <- relevant_paragraphs_data %>%
  select(app, relevant_paragraphs) %>%
  unnest(relevant_paragraphs) %>%
  filter(str_length(relevant_paragraphs) > 0)

if (nrow(relevant_paragraphs_expanded) > 0) {
  # Create corpus
  relevant_corpus <- corpus(relevant_paragraphs_expanded$relevant_paragraphs, 
                            docnames = paste0(relevant_paragraphs_expanded$app, "_", 
                                              seq_len(nrow(relevant_paragraphs_expanded))))
  
  # Generate KWIC for all routine phrases
  kwic_results <- kwic(
    tokens(relevant_corpus, remove_punct = FALSE),
    pattern = phrase(routine_phrases),
    window = 15,  # 15 words on each side for more context
    valuetype = "regex",
    case_insensitive = TRUE
  )
  
  # Convert to dataframe and clean up
  kwic_df <- as.data.frame(kwic_results) %>%
    select(docname, from, to, pre, keyword, post, pattern) %>%
    mutate(app = str_remove(docname, "_\\d+$")) %>%
    select(app, docname, from, to, pre, keyword, post, pattern) %>%
    rename(phrase = pattern)
  
  # Save KWIC results
  write.csv(kwic_df, "routine_kwic.csv", row.names = FALSE)
  
  cat("\n=== KWIC Analysis ===\n")
  cat("Total KWIC instances found (in co-occurring paragraphs):", nrow(kwic_df), "\n\n")
  
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
} else {
  cat("\n=== KWIC Analysis ===\n")
  cat("No co-occurring paragraphs found. Skipping KWIC analysis.\n")
}

# ============================================================================
# PART 3: DOCUMENT POSITION HEATMAP
# ============================================================================

# Function to calculate relative position of paragraphs containing both vague nouns and routine phrases
get_paragraph_positions <- function(text) {
  # Clean text for encoding issues first
  text_clean <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
  if (is.na(text_clean) || text_clean == "") {
    return(tibble(phrase = character(), position = numeric()))
  }
  
  # Split into paragraphs
  paragraphs <- str_split(text_clean, "\n+")[[1]]
  
  text_length <- nchar(text_clean, type = "chars")
  
  # Create patterns
  vague_pattern <- regex(paste(vague_nouns, collapse = "|"), ignore_case = TRUE)
  
  positions_list <- list()
  current_position <- 1
  
  for (i in seq_along(paragraphs)) {
    para <- paragraphs[i]
    para_length <- nchar(para, type = "chars")
    
    # Check if paragraph has both vague noun and routine phrase
    has_vague <- str_detect(para, vague_pattern)
    
    if (has_vague) {
      # Check which routine phrases appear in this paragraph
      for (phrase in routine_phrases) {
        if (str_detect(para, regex(phrase, ignore_case = TRUE))) {
          # Calculate relative position (middle of paragraph)
          relative_position <- (current_position + para_length / 2) / text_length
          
          positions_list[[length(positions_list) + 1]] <- tibble(
            phrase = phrase,
            position = relative_position
          )
        }
      }
    }
    
    # Update position (add paragraph length plus newlines)
    current_position <- current_position + para_length + 1
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
    positions = map(text, ~get_paragraph_positions(.x))
  ) %>%
  select(app, positions) %>%
  unnest(positions)

cat("\n=== Position Analysis ===\n")
cat("Total phrase occurrences with positions (in co-occurring paragraphs):", nrow(all_positions), "\n")

if (nrow(all_positions) > 0) {
  # Only keep phrases that appear enough times for meaningful analysis
  phrase_counts_for_heatmap <- all_positions %>%
    count(phrase) %>%
    filter(n >= 3) %>%  # At least 3 occurrences
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
      high = "#81C784",
      name = "Occurrences"
    ) +
    scale_y_discrete(limits = rev) +  # Reverse so 0-10% is at top
    labs(
      title = "Position of Routine Language with Vague Nouns",
      subtitle = "Paragraphs containing both routine phrases and vague noun references",
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
  
  position_heatmap
  
  # Save heatmap
  ggsave("../figures/routine_position_heatmap.png", 
         plot = position_heatmap, 
         width = 10, 
         height = 14, 
         dpi = 300)
}