# script to complete the abstract from the original dimensions database with semantic scholar

library(fulltext)

library(readxl) # importing the base data
publis <- read_excel("~endodata.xlsx", 
                     na = "NA")
publis$semanticreplaced <- NA

tick <- 1 # initialize ticker for loop
count.repl <- 0 # counter for the number of replaced abstracts

while (tick < nrow(publis)) { # process each publication of the file
  
  if (is.na(publis$Abstract[tick])) { # if the abstract is missing only
    
    extractedbastr <- try(ft_abstract(publis$DOI[tick], from="semanticscholar")) # extract the abstract for the DOI of the publication using semantic scholar
    out <- try(extractedbastr$semanticscholar[[1]][[2]])
    
    Sys.sleep(4)
    
    
    if (!is.null(out)) { # if there is a relevant abstract, retrieve it and integrate it to the database
      
      publis$Abstract[tick] <- out
      publis$semanticreplaced[tick] <- "TRUE"
      count.repl <- (count.repl + 1)
      print(c("REPLACED SINCE INITIALISATION:",count.repl))
      
      
    } else { # if no relevant abstract is found, do nothing
      
      print("NOT FOUND IN SEMANTISCHOLAR")
      
    }
    
    
    
  } else {
    
    print("ABSTRACT NOT MISSING")
    
  }
  
  print(c("PROGRESS:",tick/nrow(publis)))
  tick <- (tick +1)
  
  
}
