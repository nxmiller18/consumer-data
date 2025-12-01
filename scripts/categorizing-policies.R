# AUTHOR: Natalie Miller
# PURPOSE: Categorize scraped policies on whether or not they deal with sensitive data
# OUTPUTS: all_privacy_policies.csv [step 2: see scraping-policies.R]

setwd("~/GitHub/consumer-data/data")

library(tidyverse)
library(rvest)
library(stringr)

policy_texts <- read.csv("all_privacy_policies.csv")

# Create a list of sensitive keywords
sensitive_keywords <- c(
  "diagnosis", "prescription", "symptom", "treatment",
  "medication", "disease", "illness", "condition", "patient",
  "medical records", "medical history", "mental health", "therapy",
  "blood pressure", "glucose", "menstrual", "reproductive health",
  "pregnancy", "pharmacy", "biometric", "genetic", "DNA"
)

# Create a function to count the number of sensitive keywords in each policy
count_sensitive_terms <- function(text, keywords){
  text_clean <- iconv(text, from="UTF-8", to="UTF-8", sub="")
  if(is.na(text_clean) || text_clean == "") return(0)
  sum(str_count(text_clean, regex(paste(keywords, collapse = "|"), ignore_case=T)))
}

# Apply function to the texts
policy_texts <- policy_texts |>
  mutate(
    sensitive_term_count = map_int(text, ~count_sensitive_terms(.x, sensitive_keywords)),
    sensitivity_level = case_when(
      sensitive_term_count >= 10 ~ "High",
      sensitive_term_count >= 5 ~ "Medium",
      sensitive_term_count >= 2 ~ "Low",
      T ~ "Minimal"
    )
  )

# Export combined dataframe
write.csv(policy_texts, "all_privacy_policies.csv", row.names = F)