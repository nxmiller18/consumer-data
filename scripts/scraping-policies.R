# AUTHOR: Natalie Miller
# PURPOSE: Scrape privacy policies and compile into one data frame for textual analysis
# OUTPUTS: privacy policy .txt files, all_privacy_policies.csv [step 1: see categorizing-policies.R]

setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(rvest)
library(stringr)

# Read in list of privacy policies and links; clean app names
app_list <- read.csv("privacy_policy_list.csv") |>
  mutate(
    name = str_replace_all(App.Name, "[^\\p{L}\\s]", ""),
    name = str_squish(App.Name),
    manual = if_else(Manually.Saved. == "Yes", T, F, missing=F),
    link = Privacy.Policy.Link
    ) |>
  select(-Secondary.Link, -Manually.Saved., -App.Name, -Privacy.Policy.Link)

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

for (i in seq_len(nrow(app_list))) {
  app_name <- app_list$name[i]
  app_link <- app_list$link[i]
  manually_saved <- app_list$manual[i]
  
  if(manually_saved){
    app_list$file_saved[i] <- T
    next
  }
  
  success <- get_text(app_link, app_name)
  app_list$file_saved[i] <- success
}

# NOTE: some scrapes were unsuccessful or returned text that was not the privacy policy.
# For those, we manually copied text from the webpages and saved the policies to the manual_privacy_policies folder.

# Combine all the individual .txt files into one dataframe

manual_files <- list.files("manual_privacy_policies", pattern="\\.txt", full.names=T)
auto_files <- list.files("privacy_policies", patter="\\.txt", full.names=T)

manual_names <- basename(manual_files) |> str_remove("\\.txt$")
auto_names   <- basename(auto_files) |> str_remove("\\.txt$")

auto_files <- auto_files[!auto_names %in% manual_names]

all_files <- c(manual_files, auto_files)

read_file_safe <- function(file_path){
  
  tryCatch({
    text <- read_file(file_path, locale=locale(encoding="UTF-8"))
    text <- iconv(text, from="UTF-8", to="UTF-8", sub="")
    return(text)
  }, error=function(e){
    
    tryCatch({
      text <- read_file(file_path, locale=locale(encoding="latin1"))
      text <- iconv(text, from="latin1", to="UTF-8", sub="")
      return(text)
    }, error=function(e2){
      warning(paste("Could not read file:", file_path))
      return("")
    })
  })
}

policy_texts <- tibble(
  file = all_files,
  app = basename(file) |> 
    str_remove("\\.txt$"),
  text = map_chr(all_files, ~ read_file(.x))
  ) |>
  mutate(
    word_count = str_count(text, "\\S+"),
    char_count = nchar(text, type="chars", allowNA=T)
  ) |>
  filter(text != "") |>
  filter(app != "rubiTrack.6")

# Export combined dataframe
write.csv(policy_texts, "all_privacy_policies.csv", row.names = F)