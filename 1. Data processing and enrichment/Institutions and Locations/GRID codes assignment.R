# Script to match the affiliations of the different authors to the GRID codes ; this is the automated part of the procedure. For affiliations that could not be matched with this algorithm, a manual check is conducted. 

library(stringr)
library(readxl)
library(tm)

pubsandaff <- read_excel("~consolidated affiliations.xlsm", sheet = "exportrenewedclassic") # this is the dataset with the list of affiliations matched to their authors and publications
pubsandaff$Duplicate <- NA
griddata <- read_excel("~GRID Clean updated.xlsx", sheet = "combined list") # this is the GRID database with the list of codes and institutions

# Harmonize the names of the institutioons in the Grid ID database with the texts in the extracted data from Dimensions

griddata$Name <- tolower(griddata$Name)
griddata$Name <- removePunctuation(griddata$Name)
griddata$Name <- stripWhitespace(griddata$Name)
griddata$duplicate <- duplicated(griddata$Name)
griddata[griddata$ID=="grid.482924.1",] <- NA # remove irrelevant values
griddata <- griddata[complete.cases(griddata),]


pubsandaff$translatedaffiliation <- tolower(pubsandaff$translatedaffiliation)
pubsandaff$translatedaffiliation <- removePunctuation(pubsandaff$translatedaffiliation)
pubsandaff$translatedaffiliation <- stripWhitespace(pubsandaff$translatedaffiliation)

i <- 1

for (i in 1:nrow(pubsandaff)) { # process for each row/affiliation line
  
  allmatchesrow <- vector() # create / reinitiate empty vector for all potential matches for this row
  
  k <- 1
  
  for (k in 1:nrow(griddata)) { # check presence of each grid title in the translated affiliations
    
    if (grepl(paste0("\\b",griddata$Name[k], "\\b"), pubsandaff$translatedaffiliation[i]) == TRUE) { # add grid ID to the results vector if there is a match
      
      allmatchesrow <- append(allmatchesrow, griddata$ID[k])
      
      
    }
    
  }
  
  # process results for the row
  
  allmatchesrow <- allmatchesrow[!is.na(allmatchesrow)]
  allmatchesrow <- unique(allmatchesrow)
  
  if(length(allmatchesrow) > 0) {
    
    pubsandaff[i,4:(length(allmatchesrow)+3)] <- as.list(allmatchesrow)
    if (any(griddata$duplicate[griddata$ID == unlist(allmatchesrow)])){
      
      pubsandaff$Duplicate[i] <- "YES"
    }
    
  }
  
  
  print(100*i/nrow(pubsandaff)) #progress bar
  
  
}
