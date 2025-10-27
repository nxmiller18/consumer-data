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
  mutate(App.Name = str_replace_all(App.Name, "[^\\p{L}\\s]", "")) |>
  mutate(App.Name = str_squish(App.Name))

# Write function to scrape policies and save them as individual .txt files
get_text <- function(link, name, folder = "privacy_policies") {
  tryCatch({
    page <- read_html(link)
    
    text <- page |> 
      html_nodes("p, section, article") |> 
      html_text(trim=TRUE) |>
      paste(collapse = " ")
      
    text <- str_squish(text)
    
    if (nchar(text) < 200) {
      warning(paste("Empty text for: ", name))
      return(FALSE)
    }
    
    file_name <- paste0(folder, "/", make.names(name), ".txt")
    writeLines(text, file_name)
    return(TRUE)
  }, error = function(e){
    warning(paste("File not saved for:", name))
    return(FALSE)
  })
}

# Apply function to scrape policies
app_list$file_saved <- FALSE

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

manual_file_names <- basename(manual_files) |> str_remove("\\.txt$") |> str_trim()
auto_file_names   <- basename(auto_files) |> str_remove("\\.txt$") |> str_trim()

auto_files <- auto_files[!auto_file_names %in% manual_file_names]

file_names <- c(manual_files, auto_files)

policy_texts <- tibble(
  file = file_names,
  app = basename(file) |> str_remove("\\.txt$") |> str_trim(),
  text = map_chr(file_names, read_file))

# Export combined dataframe
write.csv(policy_texts, "all_privacy_policies.csv", row.names = F)
                  