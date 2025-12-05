# AUTHOR: Natalie Miller
# PURPOSE: Create combined position heatmap showing all three obfuscation strategies
# OUTPUTS: combined_position_heatmap.png

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(quanteda)
library(stringr)
library(showtext)

showtext_auto()
font_add_google("Atkinson Hyperlegible", "atkinson")

# Read in privacy policy data
policy_texts <- read.csv("all_privacy_policies.csv")

# Define vague nouns
vague_nouns <- c(
  "service providers",
  "affiliates", 
  "vendors",
  "business partners",
  "certain partners",
  "third parties",
  "other entities",
  "selected organizations"
)

# Define sharing verbs (will match variations: share, shares, shared, sharing, etc.)
sharing_verbs <- c(
  "shar(e|es|ed|ing)",
  "disclos(e|es|ed|ing)",
  "provid(e|es|ed|ing)",
  "transfer(s|red|ring)?"
)

# Create patterns
vague_pattern <- regex(paste(vague_nouns, collapse = "|"), ignore_case = TRUE)
sharing_pattern <- regex(paste(sharing_verbs, collapse = "|"), ignore_case = TRUE)

# Define unavoidable sharing phrases
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

# Define routine sharing phrases
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

# Function to extract positions for obligation/routine language (with sharing verbs)
extract_sharing_positions <- function(text, app_name, target_phrases) {
  text_clean <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
  if (is.na(text_clean) || text_clean == "") {
    return(tibble())
  }
  
  paragraphs <- str_split(text_clean, "\n+")[[1]]
  text_length <- nchar(text_clean, type = "chars")
  
  results <- tibble()
  current_position <- 1
  
  for (i in seq_along(paragraphs)) {
    para <- paragraphs[i]
    para_length <- nchar(para, type = "chars")
    
    has_sharing_verb <- str_detect(para, sharing_pattern)
    
    if (has_sharing_verb) {
      phrases_found <- target_phrases[str_detect(para, regex(target_phrases, ignore_case = TRUE))]
      
      if (length(phrases_found) > 0) {
        relative_position <- (current_position + para_length / 2) / text_length
        position_bin <- cut(relative_position, 
                            breaks = seq(0, 1, by = 0.1),
                            labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"),
                            include.lowest = TRUE)
        
        results <- bind_rows(results, tibble(
          app = app_name,
          paragraph_num = i,
          position = relative_position,
          position_bin = as.character(position_bin)
        ))
      }
    }
    
    current_position <- current_position + para_length + 1
  }
  
  return(results)
}

unavoidable_positions <- policy_texts |>
  mutate(
    para_details = map2(text, app, ~extract_sharing_positions(.x, .y, unavoidable_phrases))
  ) |>
  select(para_details) |>
  unnest(para_details) |>
  mutate(strategy = "Obligated Sharing")

routine_positions <- policy_texts |>
  mutate(
    para_details = map2(text, app, ~extract_sharing_positions(.x, .y, routine_phrases))
  ) |>
  select(para_details) |>
  unnest(para_details) |>
  mutate(strategy = "Routine Sharing")

vague_positions <- policy_texts |>
  mutate(
    para_details = map2(text, app, function(text, app_name) {
      text_clean <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
      if (is.na(text_clean) || text_clean == "") return(tibble())
      
      paragraphs <- str_split(text_clean, "\n+")[[1]]
      text_length <- nchar(text_clean, type = "chars")
      
      results <- tibble()
      current_position <- 1
      
      for (i in seq_along(paragraphs)) {
        para <- paragraphs[i]
        para_length <- nchar(para, type = "chars")
        
        if (str_detect(para, vague_pattern)) {
          relative_position <- (current_position + para_length / 2) / text_length
          position_bin <- cut(relative_position, 
                              breaks = seq(0, 1, by = 0.1),
                              labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"),
                              include.lowest = TRUE)
          
          results <- bind_rows(results, tibble(
            app = app_name,
            paragraph_num = i,
            position = relative_position,
            position_bin = as.character(position_bin)
          ))
        }
        
        current_position <- current_position + para_length + 1
      }
      
      return(results)
    })
  ) |>
  select(para_details) |>
  unnest(para_details) |>
  mutate(strategy = "Vague Nouns")

cat("Found", nrow(vague_positions), "paragraphs\n")

# Combine all three
all_positions <- bind_rows(
  vague_positions,
  unavoidable_positions,
  routine_positions
)

# Bin the positions
position_binned <- all_positions |>
  count(strategy, position_bin) |>
  complete(
    strategy, 
    position_bin = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"), 
    fill = list(n = 0)
  ) |>
  mutate(
    strategy = factor(strategy, levels = c("Vague Nouns", "Obligated Sharing", "Routine Sharing"))
  )

# Create combined heatmap with facets
combined_heatmap <- ggplot(
  position_binned,
  aes(x = strategy, y = factor(position_bin, levels = rev(paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"))), fill = n)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = n), 
    size = 6, 
    family = "atkinson", 
    fontface = "bold",
    color = ifelse(position_binned$n > max(position_binned$n) * 0.6, "white", "#202124")
  ) +
  scale_fill_gradient(
    low = "#FFF5F5",
    high = "#5A9BD5",
    name = "Paragraphs"
  ) +
  labs(
    title = "ctrl + f",
    subtitle = "Number of paragraphs at each position in privacy policies",
    x = "",
    y = "Position in Document"
  ) +
  theme_minimal(base_family = "atkinson") +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "#202124"),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    axis.title.y = element_text(face = "bold", size = 24, margin = margin(r = 10)),
    axis.text.x = element_text(size = 18, color = "#333333", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 18, color = "#333333"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

combined_heatmap

# Save
ggsave("../figures/combined_position_heatmap.png", 
       plot = combined_heatmap, 
       width = 5, 
       height = 4, 
       dpi = 300)