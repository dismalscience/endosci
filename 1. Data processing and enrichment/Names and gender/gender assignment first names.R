# This script uses data from Namespedia to collect the probability that that the different authors are male or female (based on their first names)

library(stringi)
library(stringr)
library(httpuv)
library(urltools)
library(rvset)
library(tidyverse)
library(purrr)

authors$GenderMasc <- NA  # define a variable that will store the probability that the first name of the author is male (female probability is defined on a binary scale by 1 - prob(male))

tick <- 1 # initialize the process

while (tick < nrow(authors)) { # carry out the steps for each author
  
  
  if (authors$FirstNameOnlyInitials[tick] == "false") { # process only for the names that feature a full first name (excluding initials only)
    
    text <- try(read_html(gsub(" ", "", paste("https://namespedia.com/details/",authors$FirstForGenderClassification[tick]))) %>% # extract the html document of Namespedia for the first name
      html_nodes(xpath = "//*[@id='content']") %>%
      html_text())

    gendermasc <- sub(".*feminine and (.*\\d+%).*", "\\1", text) # collect the relevant figure
    gendermasc <- gsub("%.*","",gendermasc)
    authors$GenderMasc[tick] <- gendermasc # assign the probability to the variable
    
    
  }
  
  
  print(tick)
  tick <- (tick + 1) # progress bar
  
}
