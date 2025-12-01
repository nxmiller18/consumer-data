# AUTHOR: Natalie Miller
# PURPOSE: Scrape privacy policies and compile into one data frame for textual analysis
# OUTPUTS: privacy policy .txt files, all_privacy_policies.csv

setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(rvest)
library(stringr)

# Read in list of privacy policies and links; clean app names
app_list <- read.csv("privacy_policy_list.csv") |>
  select(-Secondary.Link, -Manually.Saved.) |>
  mutate(
    App.Name = str_replace_all(App.Name, "[^\\p{L}\\s]", ""),
    App.Name = str_squish(App.Name)
    )

# Write function to scrape policies and save them as individual .txt files
get_text <- function(link, name, folder = "privacy_policies"){
  
  tryCatch({
    
    # Read webpage
    page <- read_html(link)
    
    # Extract text from relevant HTML elements
    text <- page |> 
      html_nodes("p, section, article") |> 
      html_text(trim=TRUE) |>
      paste(collapse = " ") |>
      str_squish()
    
    # Validate text length
    if(nchar(text)<200){
      warning(paste("Text suspiciously short for:", name))
      return(F)
    }
      
    # Save individual .txt file
    file_name <- file.path(folder, paste0(make.names(name), ".txt"))
    writeLines(text, file_name)
    return(T)
  }, 
  
  # Return an error message if policy not scraped
  error = function(e){
    warning(paste("Failed to scrape:", name))
    return(F)
  })
}

# Apply function to scrape policies
app_list$file_saved <- F

for (i in seq_len(nrow(app.list))) {
  app_name <- app_list$App.Name[i]
  app_link <- app_list$Privacy.Policy.Link[i]
  
  success <- get_text(app_link, app_name)
  app_list$file_saved[i] <- success
}

# NOTE: some scrapes were unsuccessful or returned text that was not the privacy policy.
# For those, we manually copied text from the webpages and saved the policies to the manual_privacy_policies folder.

# Combine all the individual .txt files into one dataframe

manual_files <- list.files("manual_privacy_policies", pattern="\\.txt", full.names=T)
auto_files <- list.files("privacy_policies", patter="\\.txt", full.names=T)

manual_file_names <- basename(manual_files) |> 
  str_remove("\\.txt$")
auto_file_names   <- basename(auto_files) |> 
  str_remove("\\.txt$")

auto_files <- auto_files[!auto_file_names %in% manual_file_names]

all_files <- c(manual_files, auto_files)

policy_texts <- tibble(
  file = all_files,
  app = basename(file) |> 
    str_remove("\\.txt$"),
  text = map_chr(all_files, ~ read_file(.x))
  ) |>
  mutate(
    word_count = str_count(text, "\\S+"),
    char_count = nchar(text)
  )
  
# Categorize on whether or not the app handles sensitive content

sensitive_keywords <- c(
  "health", "medical", "biometric", "genetic", "social security", "diagnosis",
  "prescription", "symptom", "children", "minor"
)

# Create a function to count the number of sensitive keywords in each policy

count_sensitive_terms <- function(text){
  text_lower <- tolower(text)
  sum(str_count(text_lower, regex(paste(sensitive_keywords, collapse = "|", ignore_case=T))))
}

policy_texts <- policy_texts |>
  mutate(
    sensitive_term_count = map_int(text, count_sensitive_terms),
    deals_with_sensitive_data = case_when(
      sensitive_term_count >= 10 ~ "High",
      sensitive_term_count >= 5 ~ "Medium",
      sensitive_term_count >= 2 ~ "Low",
      T ~ "Minimal"
    )
  )

# Export combined dataframe
write.csv(policy_texts, "all_privacy_policies.csv", row.names = F)

              