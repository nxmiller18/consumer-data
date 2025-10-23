setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(rvest)
library(stringr)

# Read in list of privacy policies and links; clean app names
app.list <- read.csv("privacy-policy-list.csv") |>
  select(-Secondary.Link, -Manually.Saved.) |>
  mutate(App.Name = str_replace_all(App.Name, "[^\\p{L}\\s]", "")) |>
  mutate(App.Name = str_squish(App.Name))

# Write function to scrape policies and save them as individual .txt files
get.text <- function(link, name, folder = "privacy_policies") {
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
    
    file.name <- paste0(folder, "/", make.names(name), ".txt")
    writeLines(text, file.name)
    return(TRUE)
  }, error = function(e){
    warning(paste("File not saved for:", name))
    return(FALSE)
  })
}

# Apply function to scrape policies
app.list$file_saved <- FALSE

for (i in seq_len(nrow(app.list))) {
  app_name <- app.list$App.Name[i]
  app_link <- app.list$Privacy.Policy.Link[i]
  
  success <- get.text(app_link, app_name)
  app.list$file_saved[i] <- success
}

# NOTE: some scrapes were unsuccessful or returned text that was not the privacy policy.
# For those, we manually copied text from the webpages and saved the policies to the
# manual_privacy_policies folder.

# Combine all the individual .txt files into one dataframe
file.names <- c(
  list.files("privacy_policies", pattern="\\.txt", full.names=T), 
  list.files("manual_privacy_policies", pattern="\\.txt", full.names=T)
  )

policy.texts <- tibble(
  file = file.names,
  app = basename(file) |> str_remove("\\.txt$") |> str_trim(),
  text = map_chr(file.names, read_file))

# Export combined dataframe
write.csv(policy.texts, "all_privacy_policies.csv", row.names = FALSE)  
