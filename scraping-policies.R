setwd("C:/Users/natal/OneDrive/Documents/GitHub/consumer-data/data")

library(tidyverse)
library(rvest)
library(stringr)

app.list <- read.csv("Privacy Policies.csv") |>
  select(-X)

get.text <- function(link, name, folder = "privacy_policies") {
  tryCatch({
    page <- read_html(link)
    text <- page |> html_nodes("p") |> html_text()
    text <- paste(text, collapse = "\n\n")
    file.name <- paste0(folder, "/", make.names(name), ".txt")
    writeLines(text, file.name)
  }, error = function(e){
    return(NA)
  })
}

for(i in seq_len(nrow(app.list))){
  get.text(app.list$Privacy.Policy.Link[i], app.list$App.Name[i])
}
