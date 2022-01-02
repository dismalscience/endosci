# This script is an attempt to automatize the grouping of similar authors. It cannot identify unique authors due to ambiguities in names. In spite of effort, this algorithm does not perform well (there are probably numerous mistakes!). it was only used for the basic cases. Otherwise, names were compared manually when possible (i.e., when there were no major ambiguities). 

#Prepare the column of first names for comparison; it exploits the endo dataset with authors' names
# the id variable identifies cases where there are potential matches for regrouping (same last names, same initials for first names)
library(stringi)
library(stringr)
endo$firstnameproc <- stri_trans_general(endo$cleanfirst, "Latin-ASCII")
endo$firstnameproc <- gsub("[[:space:]]", "", endo$firstnameproc)
endo$firstnameproc <- gsub("[[:punct:]]", "", endo$firstnameproc)
endo[consolidatedfirst] <- NA #create new empty column for results

#Initialisation of variables for the algorithm
n = nrow(endo) #counter for the number of rows
i = 1 #index for the processing of rows, initialisation
k = 1 # index to process initials

for (k in 1:n) {
  
if ((gsub("[^A-Z]","", endo$firstnameproc[k]) == endo$firstnameproc[k]) && (nchar(endo$firstnameproc[k]) > 3)){ # if the name is only initials and more than 3 letters; make it lower cases
  
  endo$firstnameproc[k] <- str_to_title(endo$firstnameproc[k])
  
  }
  
}


#Implementation of the algorithm

repeat { # for all rows of the database
  
  if(endo$id[i]<2) { # if the potential matches between first names for a given last name are not present, simply copy the existing clean first names
    endo$consolidatedfirst[i] <- endo$cleanfirst[i]
    print(i)
    i <- (i+1)
    
  } else if (endo$id[i] > 1) { # algorithm when the potential matches are greater than 1
  
      
    vectornameslength <- c(nchar(endo$firstnameproc[seq(i,(i+endo$id[i])-1,1)])) # create vector for the length of processed first names
    vectornamesinitials <- c((gsub("[^A-Z]","", endo$firstnameproc[seq(i,(i+endo$id[i])-1,1)]))) # create vector for the initials of the processed names
    
    checkonlyinitials <- nchar(vectornamesinitials) == vectornameslength # identification of names that are only initials
    positionsinitialsonly <- i+which(checkonlyinitials %in% TRUE)-1 # get indexes for names that are only initials
    positionsfullnames <- i+which(checkonlyinitials %in% FALSE)-1 # get indexes for names that are full names (i.e., not only initials)
    
    # 1. process full names
    
    vectorunique <- unique(endo$firstnameproc[positionsfullnames]) # extract unique values among full names
    positionsuniquenames <- (i +which(endo$firstnameproc[i:(i+endo$id[i])] %in% vectorunique)-1) # detect the position of unique names
    tableunique <- data.frame("pos" = positionsuniquenames, "names" = endo$firstnameproc[positionsuniquenames], "initials" = gsub("[^A-Z]","", endo$firstnameproc[positionsuniquenames]), nbinitials = nchar(gsub("[^A-Z]","", endo$firstnameproc[positionsuniquenames])) ) # create position table for unique names
    
         #1.1 the first step is to consolidate middle names (for cases where there are more than 1 initial)
    
    groupsofmiddlenames <- as.vector(unique(tableunique$initials[tableunique$nbinitials>1])) # identify groups of initials with potential middle names
    idmiddledifference <- vector() # initiate vector for identification
    
    
    if (length(groupsofmiddlenames) >0) { # if there are potential middle names only
    
    
    for (tickmiddle in (1:(length(groupsofmiddlenames)))) { # identification of groups of middle names that have internal differences or not
     idmiddledifference <- append(idmiddledifference, length(unique(tableunique$names[tableunique$initials==groupsofmiddlenames[tickmiddle]])))
      
    }
    
    
    for (tickmiddle in (1:(length(groupsofmiddlenames)))) { 
      
      
      if (idmiddledifference[tickmiddle] == 1) { # adopt as clean names the ones that have unique values (no internal differences)
        endo$consolidatedfirst[(tableunique$pos[tableunique$initials == (groupsofmiddlenames[tickmiddle])])] <- endo$cleanfirst[(tableunique$pos[tableunique$initials == (groupsofmiddlenames[tickmiddle])])]

      } else { # if there are internal differences
        
        
        
        # process groups with internal differences (check if identical first parts; and if same initial after)
        
        
        for(tickmiddleinternaldiff in 1:(length(groupsofmiddlenames[which(idmiddledifference > 1)]))) { # loop for each group with internal differences;  check number of different unique values within the group
          
          
          listnamesmoremiddle <- as.vector(unique(tableunique$names[tableunique$initials == groupsofmiddlenames[which(idmiddledifference > 1)][tickmiddleinternaldiff]])) # extract list unique names for the group
          
          processtablemoremiddle <- data.frame("names" = (listnamesmoremiddle[order(-nchar(listnamesmoremiddle), listnamesmoremiddle)]), "id" = 1:(length(listnamesmoremiddle))) # create table with names ranked and indexes
          tickmoremiddle <- 2 # initialize tick
          tickinternalmoremiddle <- 1 # initialize tick
          
          
          
          endo$consolidatedfirst[(tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[1])])] <- endo$cleanfirst[(tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[1])])] # copy longest name in consolidated column
          
          
          for (tickmoremiddle in 2:length(listnamesmoremiddle)) { # loop for each name that could be replaced
            
            for (tickinternalmoremiddle in 1:tickmoremiddle) { # loop for each potential value for replacement
              
              if (((str_extract_all(processtablemoremiddle$names[tickinternalmoremiddle], '[[:lower:]]+')[[1]][1]) == (str_extract_all(processtablemoremiddle$names[tickmoremiddle], '[[:lower:]]+')[[1]][1])) && (((str_extract_all(processtablemoremiddle$names[tickinternalmoremiddle], '[[:lower:]]+')[[1]][2]) == (str_extract_all(processtablemoremiddle$names[tickmoremiddle], '[[:lower:]]+')[[1]][2])) || ((is.na(str_extract_all(processtablemoremiddle$names[tickinternalmoremiddle], '[[:lower:]]+')[[1]][2]))  ||(is.na(str_extract_all(processtablemoremiddle$names[tickmoremiddle], '[[:lower:]]+')[[1]][2])   ))) && (((str_extract_all(processtablemoremiddle$names[tickinternalmoremiddle], '[[:lower:]]+')[[1]][3]) == (str_extract_all(processtablemoremiddle$names[tickmoremiddle], '[[:lower:]]+')[[1]][3])) || ((is.na(str_extract_all(processtablemoremiddle$names[tickinternalmoremiddle], '[[:lower:]]+')[[1]][3]))  ||(is.na(str_extract_all(processtablemoremiddle$names[tickmoremiddle], '[[:lower:]]+')[[1]][3])   )))) { # check for potential matches
                
              }
              
              processtablemoremiddle[tickmoremiddle,(tickinternalmoremiddle)+2] <- tickinternalmoremiddle # extract the id of the potential match and store it in dedicated column
              
            }
            
            
          }
          
          for (tickmoremiddle in 2:length(listnamesmoremiddle)) { # loop for each name that could be replaced
            
            
            
            if (sum(!is.na(processtablemoremiddle[tickmoremiddle,3:ncol(processtablemoremiddle)])) == 1) { #check if there are no ambiguities
              
              
              indextobeusedasreplacement <- (processtablemoremiddle[tickmoremiddle,3:ncol(processtablemoremiddle)])[!is.na((processtablemoremiddle[tickmoremiddle,3:ncol(processtablemoremiddle)]))] # index to be used as a replacement
              
              endo$consolidatedfirst[(tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[tickmoremiddle])])] <- endo$cleanfirst[tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[indextobeusedasreplacement])]]   # copy the longest corresponding name in the adequate consolidated name row
              
            } else {
              
              endo$consolidatedfirst[(tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[tickmoremiddle])])] <- endo$cleanfirst[(tableunique$pos[tableunique$names == as.vector(processtablemoremiddle$names[tickmoremiddle])])]   # copy the longest corresponding name in the adequate consolidated name row
              # copy the original name in the corresponding consolidated name row
              
            }
            {
              
              
            }
            
            
          }
          
        }
        
        }
        
            }
    }
    
       #1.2. the second step is to expand full names with a single initial with middle names; if there are no ambiguities
    
    
    if ((length(tableunique$names[tableunique$nbinitials == 1])) > 0) { #check if there are full names with a single initial
      
      processtablefullinitials <- data.frame("names" = as.vector(tableunique$names[tableunique$nbinitials == 1])) # create table for processing
      
      
      if (length(vectorunique) > 5) { # if there are many possible unique values
      
      
      tickfullnomiddle <- 1
      
      for (tickfullnomiddle in 1:nrow(processtablefullinitials))
        
      {
        
        endo$consolidatedfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]] <- endo$cleanfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]]
        
      }
      
      
      } else if ((length(groupsofmiddlenames)>0) && (length(vectorunique) < 6))  { # if there are limited numbers of unique values AND full names with middle names, process accordingly
      
     processtableallmiddle <- data.frame("names" = unique(tableunique$names[tableunique$nbinitials >1]), "id" = 1:length(unique(tableunique$names[tableunique$nbinitials >1])))    # create table all names with middle name
        
     tickfullnomiddle <- 1
     tickpotfullmiddle <- 1
     
     for (tickfullnomiddle in 1:nrow(processtablefullinitials)) { # loop to check all potential values to be replaced
       
       for (tickpotfullmiddle in 1:length(processtableallmiddle)) {
         
         if (!is.na(str_extract_all(processtableallmiddle$names[tickpotfullmiddle], '[[:lower:]]+')[[1]][1])) {
         
         if ((str_extract_all(processtablefullinitials$names[tickfullnomiddle], '[[:lower:]]+')[[1]][1]) == (str_extract_all(processtableallmiddle$names[tickpotfullmiddle], '[[:lower:]]+')[[1]][1])) { # check for potential matches
         
           processtablefullinitials[tickfullnomiddle,(ncol(processtablefullinitials)+1)] <- processtableallmiddle$id[tickpotfullmiddle] # copy the relevant index
           
         }
         }
         
       }
       
       
     }
     
     
     tickfullnomiddle <- 1
     tickpotfullmiddle <- 1
     
     for (tickfullnomiddle in 1:nrow(processtablefullinitials)) { # loop to check all potential values to be replaced
    
       if (ncol(processtablefullinitials) >1) {
       
       
       if ((sum(!is.na(processtablefullinitials[tickfullnomiddle,2:ncol(processtablefullinitials)]))) == 1) { # if there are no ambiguities
         
         endo$consolidatedfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]] <- endo$cleanfirst[as.vector(tableunique$pos[tableunique$names == (processtableallmiddle$names[processtableallmiddle$id == as.vector(((processtablefullinitials[tickfullnomiddle,2:ncol(processtablefullinitials)])[!is.na(processtablefullinitials[tickfullnomiddle,2:ncol(processtablefullinitials)])]))])])[1]]
         
         
       } else { # if there are ambiguities; process and copy accordingly
         
         
         
         endo$consolidatedfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]] <- endo$cleanfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]]
         
      
         
       } 
       

       
       } else {
         
         endo$consolidatedfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]] <- endo$cleanfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]]
         
         
       }
     
     }
        
      } else { # other unforeseen cases : just copy
      
        tickfullnomiddle <- 1
        
        for (tickfullnomiddle in 1:nrow(processtablefullinitials))
          
        {
          
          endo$consolidatedfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]] <- endo$cleanfirst[tableunique$pos[tableunique$names == as.vector(processtablefullinitials$names[tickfullnomiddle])]]
          
        }
        
  
      }
      
    }
      
    # 2. complete names with initials only; provided there are no ambiguities
    
    
    
    if (length(positionsinitialsonly) >0) { # check if there are lines that are only initials
      
    
      processtableonlyinitials <- data.frame("names" = endo$firstnameproc[positionsinitialsonly], "pos" = positionsinitialsonly)
      cleannamesforinionly <- unique(endo$consolidatedfirst[positionsfullnames])
      initialscleanforinionly <- gsub("[^A-Z]","", cleannamesforinionly)
      
      if (length(initialscleanforinionly) >0) {
      
      potentialmatchesonlyini <- data.frame("names" = cleannamesforinionly, "initials" = initialscleanforinionly, "id" = 1:length(initialscleanforinionly))
      
      tickinionly <- 1
      tickmatchesfull <- 1
      
      for (tickinionly in 1:nrow(processtableonlyinitials)) {  # loop to check potential matches for every initials only case
        
          for (tickmatchesfull in 1:nrow(potentialmatchesonlyini)) {
            
            if (processtableonlyinitials$names[tickinionly] == potentialmatchesonlyini$initials[tickmatchesfull]) {# check if the initials are the same
              
              processtableonlyinitials[tickinionly,ncol(processtableonlyinitials)+1] <- as.numeric(rownames(potentialmatchesonlyini)[tickmatchesfull]) # copy the id in the process table (additional column)
              
              
            } else { # if no potential matches, just copy the original initial
              
              endo$consolidatedfirst[processtableonlyinitials$pos[tickinionly]] <- endo$cleanfirst[processtableonlyinitials$pos[tickinionly]]
              
            }
            
            if ((ncol(processtableonlyinitials) >2) && (nrow(potentialmatchesonlyini) >0)) { # if there are potential matches, check if ambiguities and process accordingly
              
              if ((sum(!is.na(processtableonlyinitials[tickinionly,3:ncol(processtableonlyinitials)]))) == 1) { # cases where there are no ambguities
                
                endo$consolidatedfirst[processtableonlyinitials$pos[tickinionly]] <- as.character(potentialmatchesonlyini$names[potentialmatchesonlyini$id == as.numeric((processtableonlyinitials[tickinionly,3:ncol(processtableonlyinitials)])[!is.na(processtableonlyinitials[tickinionly,3:ncol(processtableonlyinitials)])])])
                
              } else { # if ambiguities; just copy initial
                
                endo$consolidatedfirst[processtableonlyinitials$pos[tickinionly]] <- endo$cleanfirst[processtableonlyinitials$pos[tickinionly]]
                
                
              
            }
            
          }
          
        
        
        
      }
      
      
        
      }
      
      
      
      } else {
        
        potentialmatchesonlyini <- data.frame()
        tickinionly <- 1
        tickmatchesfull <- 1
        
        for (tickinionly in 1:nrow(processtableonlyinitials)) {  # loop to check potential matches for every initials only case
          endo$consolidatedfirst[processtableonlyinitials$pos[tickinionly]] <- endo$cleanfirst[processtableonlyinitials$pos[tickinionly]]
          
        }
      }
    }
    
    # block for progress towards the next names and cleaning variables
    
    print(seq(i,i+endo$id[i]-1, by = 1)) # show progress to the user
    i <- (i+endo$id[i])  # increase the ticking counter depending on number of potential matches
  
    
    potentialmatchesonlyini <- NULL
    processtableallmiddle <- NULL
    processtablefullinitials <- NULL
    processtablemoremiddle <- NULL
    processtableonlyinitials <- NULL
    tableallmiddle <- NULL
    tableunique <- NULL
    checkonlyinitials <- NULL
    cleannamesforinionly <- NULL
    groupsofmiddlenames <- NULL
    idmiddledifference <- NULL
    initialscleanforinionly <- NULL
    listnamesmoremiddle <- NULL
    positionsfullnames <- NULL
    positionsinitialsonly <- NULL
    positionsuniquenames <- NULL
    stepforclean <- NULL
    vectornamesinitials <- NULL
    vectornameslength <- NULL
    vectorunique <- NULL
    
    
    
    
  }
  

  if (i == 93931) {
    break
    
  }
  
}
  
    
 

