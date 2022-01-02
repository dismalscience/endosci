# script to complete the abstract from the original dimensions database using RCrossref

library(rcrossref)

library(readxl) # importing the base data
publis <- read_excel("~endodata.xlsx", 
                     na = "NA")

tick <- 1 # initialize ticker for loop
count.repl <- 0 # counter for the number of replaced abstracts

while (tick < nrow(publis)) { # process each publication of the database
  
  if (is.na(publis$Abstract[tick]) == TRUE) { # if the abstract is missing only
    
    extractedbastr <-try(cr_abstract(doi = publis$DOI[tick])) # check abstract using RCrossref
    
    if (class(extractedbastr) == "try-error") { # if there is an error during extraction; do not do anything
      
    } else { # otherwise, if there is a relevant abstract, retrieve it and integrate it to the database
      
      publis$Abstract[tick] <- cr_abstract(doi = publis$DOI[tick])
      publis$crossrefreplaced[tick] <- "TRUE"
      count.repl <- (count.repl + 1)
      print(count.repl)
    }
    

    
  }
  
  tick <- (tick +1)
  
  
}