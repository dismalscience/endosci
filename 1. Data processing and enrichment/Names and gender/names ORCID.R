# This script aims at completing missing names by retrieving data from the ORCID database
# It uses the list of previously extracted authors (authors object)

library(rorcid)
library(stringr)
orcid_auth()

tick <- 1

while (tick < nrow(authors)) { # review all names of the table
  
if (authors$FirstNameOnlyInitials[tick] == "true") { # Attempts at collecting ORCID data on authors' names only when there is missing information in the existing dataset (i.e., when the first names are only initials)
  
  orc.results <- orcid_search(family_name = authors$`Last Name`[tick], text = "endometriosis") # search ORCID, contextualizing by mentionning endometriosis in the keywords
  
  if (length(orc.results) > 0) { # if there are some potential replacement values for the names
     
    orc.results$first <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", orc.results$first, perl = TRUE) # capitalize names of results
    orc.results$capitals <-gsub("(\\b[A-Z])[^A-Z]+", "\\1", orc.results$first) # extract capital letters
    orc.results$searchedcapitals <- authors$FirstNameInitials[tick] # copy initials that are searched
    
    for (i in 1:nrow(orc.results)) { # check if there are matches
      
      if (orc.results$capitals[i] == orc.results$searchedcapitals[i]) {
        
        orc.results$matchstatus[i] <- 1
        
      } else {
        
        orc.results$matchstatus[i] <- 0
        
      }
      
      
    }
    
    if (sum(as.numeric(orc.results$matchstatus)) == 1) { # if there is only one match
      
      authors$`First Name`[tick] <- orc.results$first[orc.results$matchstatus == 1] # replace name
      authors$FirstNameOnlyInitials[tick] <- "false"
      authors$status[tick] <- "REPLACED BY ORCID"
       
      
    } else {
      
      authors$status[tick] <- "INADEQUATE ORCID MATCH" # mention that no adequate match was found on orcid
      
      
    }
    
  } else {
    
    authors$status[tick] <- "NOT FOUND ON ORCID" # mention that no match was found on orcid
    
  }
  
  
}
  print(authors$status[tick])
  print((tick/nrow(authors))*100)
  tick <- (tick + 1) # update counter for progress
 
  
}


