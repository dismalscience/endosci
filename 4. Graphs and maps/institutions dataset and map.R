# This script is dedicated to the analysis of institutions at the individual level. It first creates the dataset of institutions with detailed information. Then, it generates the map.

############## LOADING OF REQUIRED LIBRARIES

library(leaflet)
library(RColorBrewer)
library(htmlwidgets)
library(inlmisc)
library(leaflet.extras)
library(readxl)
library(tidyverse)
library(widgetframe)

############## IMPORTING REQUIRED DATASETS

sourcedatainst <- read_excel("~endodata.xlsx", na = "NA") # import the full dataset with publication characteristics
individual_inst_data <- read_excel("~final affiliations list.xlsm", sheet = "list codes and caracs", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text"), na = "NA") # import the full list of institutions
indauthordata <- read_excel("~authors publications pairs.xlsx", col_types = c("text", "numeric", "numeric", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric"), na = "NA") # import the dataset with individual authors, their gender and affiliations

########## PROCESSING TOPICS

institutions.topic <- data.frame("Code" = individual_inst_data$Code) # creation of the dataset that will store the topics information

topicsallpubs <- round((colMeans(sourcedatainst[sapply(sourcedatainst, is.numeric)], na.rm=TRUE)[11:53]) * 100, digits = 2) # extract the share of topics for all publications (even those with no geographical matches)
data.toextract <- data.frame() # initialise an empty dataset that will contain the information to be processed

i <- 1 # set the ticker

for (i in 1:nrow(institutions.topic)) { # preliminary creation of the dataset with topics share
  
  data.toextract <- sourcedatainst %>% filter_at(c("Code AFF 1", "Code AFF 2", "Code AFF 3", "Code AFF 4", "Code AFF 5", "Code AFF 6", "Code AFF 7", "Code AFF 8", "Code AFF 9", "Code AFF 10", "Code AFF 11", "Code AFF 12", "Code AFF 13", "Code AFF 14", "Code AFF 15", "Code AFF 16", "Code AFF 17"),any_vars(.%in% individual_inst_data$Code[i])) # retrieve the data with the publications solely linked to the institution under scrutiny
  
  j <- 11
  
  for (j in 11:53) { # extract average values at the institution level for each topic 
    
    institutions.topic[i,j-9] <-   round(100 * colMeans(data.toextract[sapply(data.toextract, is.numeric)], na.rm=TRUE)[j], digits = 2) # extract and store the average shares of all topics (broad and detailed)
  }
  
  
  j <- 1
  
  for (j in 1:43) { # process each column for overrepresentation ratios
    
    institutions.topic[i,44+j] <- round(institutions.topic[i,1+j] / topicsallpubs[j], digits = 2) # compute and format ratio
    
  }

  print((i/nrow(institutions.topic))*100) # show progress
}

colnames(institutions.topic) <- c("Code", colnames(sourcedatainst)[119:161], paste("Ratio",colnames(sourcedatainst)[119:161])) # assign names to columns of the dataset


# identification of the top topics for each institution and their underlying characteristics

# BROAD TOPICS

inst.broad <- institutions.topic[,3:10] # subset for broad topics

institutions.topic$CommonBroad <- colnames(inst.broad)[max.col(inst.broad,ties.method="first")] # extract the name of the most common broad topic for each institution

i <- 1 

for (i in 1:nrow(institutions.topic)) {
  
  institutions.topic[i,89] <- max(inst.broad[i,]) # extract the share for the corresponding broad topic
  
}

colnames(institutions.topic)[89] <- "ShareMostCommonBroad"

inst.broad.over <- institutions.topic[,46:53] # subset for broad topics over representation

institutions.topic$SpecificBroad <- substring(colnames(inst.broad.over)[max.col(inst.broad.over,ties.method="first")],7) # extract the name of the most specific broad topic for each institution

i <- 1 

for (i in 1:nrow(institutions.topic)) {
  
  institutions.topic[i,91] <- max(inst.broad.over[i,]) # extract the ratio for the corresponding broad topic
  
}

colnames(institutions.topic)[91] <- "RatioMostSpecificBroad"

i <- 1 

for (i in 1:nrow(institutions.topic)) { # retrieve the share for each over represented topic
  
  if (length(inst.broad[i,substring(colnames(inst.broad.over)[max.col(inst.broad.over,ties.method="first")],7)[i]]) > 0) {
    
    institutions.topic[i,92] <- inst.broad[i,substring(colnames(inst.broad.over)[max.col(inst.broad.over,ties.method="first")],7)[i]]
  }
}

colnames(institutions.topic)[92] <- "ShareMostSpecificBroad"

# Specific topics

inst.detail <- institutions.topic[,14:44] # subset for specific topics

institutions.topic$CommonDetailed <- colnames(inst.detail)[max.col(inst.detail,ties.method="first")] # extract the name of the most common detailed topic for each institution

i <- 1 

for (i in 1:nrow(institutions.topic)) {
  
  institutions.topic[i,94] <- max(inst.detail[i,]) # extract the share for the corresponding detailed topic
  
}

colnames(institutions.topic)[94] <- "ShareCommonDetailed"

inst.detailed.over <- institutions.topic[,57:87] # subset for detailed topics over representation

institutions.topic$SpecificDetailed <- substring(colnames(inst.detailed.over)[max.col(inst.detailed.over,ties.method="first")],7) # extract the name of the most specific detailed topic for each institution


i <- 1 

for (i in 1:nrow(institutions.topic)) {
  
  institutions.topic[i,96] <- max(inst.detailed.over[i,]) # extract the ratio for the corresponding detailed topic
  
}

colnames(institutions.topic)[96] <- "RatioMostSpecificDetailed"

i <- 1 

for (i in 1:nrow(institutions.topic)) { # retrieve the share for each over represented topic
  
  if (length(inst.detail[i,substring(colnames(inst.detailed.over)[max.col(inst.detailed.over,ties.method="first")],7)[i]]) > 0) {
    
    institutions.topic[i,97] <- inst.detail[i,substring(colnames(inst.detailed.over)[max.col(inst.detailed.over,ties.method="first")],7)[i]]
  }
}

colnames(institutions.topic)[97] <- "ShareMostSpecificDetailed"


############### PROCESSING OF THE INSTITUTION DATA ############

individual_inst_data$Freq <- NA # create empty column for number of endo publications
individual_inst_data$Citations <- NA # create empty column for number of total citations
individual_inst_data$RecentCitations <- NA # create empty column for number of recent citations
individual_inst_data$AverageCitations <- NA # create empty column for average citations
individual_inst_data$FreqTopics <- NA # create empty column for number of endo publications with abstract data allowing the identification of topics

individual_inst_data$MaleContrib <- NA # create empty column for number of male contributions
individual_inst_data$FemaleContrib <- NA # create empty column for number of female contributions
individual_inst_data$NAContrib <- NA # create empty column for number of undetermined gender contributions
individual_inst_data$MaleShareContrib <- NA # create empty column for share of male contribution
individual_inst_data$FemaleShareContrib <- NA # create empty column for share of female contribution


# importing topic related data

individual_inst_data$CommonBroad <- institutions.topic$CommonBroad
individual_inst_data$ShareMostCommonBroad <- institutions.topic$ShareMostCommonBroad
individual_inst_data$SpecificBroad <- institutions.topic$SpecificBroad
individual_inst_data$RatioMostSpecificBroad <- institutions.topic$RatioMostSpecificBroad
individual_inst_data$ShareMostSpecificBroad <- institutions.topic$ShareMostSpecificBroad
individual_inst_data$CommonDetailed <- institutions.topic$CommonDetailed
individual_inst_data$ShareCommonDetailed <- institutions.topic$ShareCommonDetailed
individual_inst_data$SpecificDetailed <- institutions.topic$SpecificDetailed
individual_inst_data$RatioMostSpecificDetailed <- institutions.topic$RatioMostSpecificDetailed
individual_inst_data$ShareMostSpecificDetailed <- institutions.topic$ShareMostSpecificDetailed


data.toextract <- data.frame() # initialise an empty dataset that will contain the information to be processed

i <- 1 # reset the ticker

for (i in 1:nrow(individual_inst_data)) { # processing each individual institution from the list
  
  data.toextract <- sourcedatainst %>% filter_at(c("Code AFF 1", "Code AFF 2", "Code AFF 3", "Code AFF 4", "Code AFF 5", "Code AFF 6", "Code AFF 7", "Code AFF 8", "Code AFF 9", "Code AFF 10", "Code AFF 11", "Code AFF 12", "Code AFF 13", "Code AFF 14", "Code AFF 15", "Code AFF 16", "Code AFF 17"),any_vars(.%in% individual_inst_data$Code[i])) # retrieve the data with the publications solely linked to the institution under scrutiny
  
  individual_inst_data$Freq[i] <- nrow(data.toextract) # the number of publications is equivalent to the number of rows of the extraction
  individual_inst_data$Citations[i] <- sum(data.toextract$`Times cited`, na.rm = TRUE) # the number of citations is equivalent to the sum of all citations from the matched publications, removing NAs
  individual_inst_data$RecentCitations[i] <- sum(data.toextract$`Recent citations`, na.rm = TRUE) # the number of recent citations is equivalent to the sum of all recent citations from the matched publications, removing NAs
  individual_inst_data$AverageCitations[i] <- round(individual_inst_data$Citations[i] / individual_inst_data$Freq[i], digits = 2)  # simple computation of the average
  individual_inst_data$FreqTopics[i] <- individual_inst_data$Freq[i] - sum(is.na(data.toextract$`1. Causes and mechanisms`))  # computation of the number of publications with an abstract allowing the analysis of topics
  
  
  print(100*(i/nrow(individual_inst_data))) # show progress
}

########### PROCESSING OF THE GENDER DATA AT THE INSTITUTION LEVEL ############## 

data.toextract <- data.frame() # initialise an empty dataset that will contain the information to be processed

i <- 1 # reset the ticker

for (i in 1:nrow(individual_inst_data)) { # processing each individual institution from the list
  
  data.toextract <- indauthordata %>% filter_at(c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13"),any_vars(.%in% individual_inst_data$Code[i])) # retrieve the data with the authors solely linked to the institution under scrutiny
  
  individual_inst_data$MaleContrib[i] <- sum(data.toextract$Gender == "Male") # compute number of male contributions
  individual_inst_data$FemaleContrib[i] <- sum(data.toextract$Gender == "Female") # compute number of female contributions
  individual_inst_data$NAContrib[i] <- sum(data.toextract$Gender == "Not determined") # compute number of ND contributions
  individual_inst_data$MaleShareContrib[i] <- round(100*((individual_inst_data$MaleContrib[i])/(individual_inst_data$MaleContrib[i] + individual_inst_data$FemaleContrib[i])), digits = 2) # compute the share
  individual_inst_data$FemaleShareContrib[i] <- round(100*((individual_inst_data$FemaleContrib[i])/(individual_inst_data$MaleContrib[i] + individual_inst_data$FemaleContrib[i])), digits = 2) # compute the share
  
  
  print(100*(i/nrow(individual_inst_data))) # show progress
}


########### VISUALIZATION OF DATA THROUGH A MAP ################

indivinstscaled <- individual_inst_data[individual_inst_data$Freq > 1,] # restrict the sample to institutions with more than one publication to ensure smooth visualization

indivinstscaled <- indivinstscaled[!(indivinstscaled$`Organisation type` %in% c("Not applicable","Not specified (city only)","Not specified (country only)", "Not specified (region only)")),] # exclude codes with poor localization
indivinstscaled <- indivinstscaled[!(indivinstscaled$Name == "Centocor Europe"),] # exclude institution with wrong localization (sea)

pal <- colorFactor(palette = c("#8dd3c7", "#ffd92f", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#dfc27d"), levels = sort(as.vector(unique(indivinstscaled$`Organisation type`)))) # definition of the ad-hoc palette

pal <- colorFactor(palette = c("#bebada", "#ffd92f", "#80b1d3", "#8dd3c7", "#fdb462", "#fb8072", "#b3de69", "#dfc27d"), levels = sort(as.vector(unique(indivinstscaled$`Organisation type`)))) # definition of the alternative ad-hoc palette


map.inst <- leaflet(indivinstscaled) %>% addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(Freq)*1.13+1.8, fillColor = ~pal(`Organisation type`), fillOpacity = 0.77, weight = 0.5, stroke = FALSE, color = "grey", group="organisations", label = ~Name, popup=paste0("Name: ",indivinstscaled$Name,"<br>", "Organisation type: ",indivinstscaled$`Organisation type`, "<br>", "Country: ",indivinstscaled$Country, "<br>", "<br>", "Number of endometriosis publications: ",indivinstscaled$Freq,"<br>", "Average citations per publication: ",indivinstscaled$AverageCitations, "<br>", "Gender contribution: ", indivinstscaled$MaleShareContrib,"% male / ", indivinstscaled$FemaleShareContrib,"% female", "<br>", "<br>", "Most common broad topic: ",substring(indivinstscaled$CommonBroad,4), " (", indivinstscaled$ShareMostCommonBroad, "% of output)", "<br>", "Most common detailed topic: ",substring(indivinstscaled$CommonDetailed,6)," (",indivinstscaled$ShareCommonDetailed,"% of output)","<br>","Most specific detailed topic: ",substring(indivinstscaled$SpecificDetailed,6)," (", indivinstscaled$ShareMostSpecificDetailed, "% of output - ", indivinstscaled$RatioMostSpecificDetailed, " times the avg.)")) %>%
  addLegend(pal = pal, values = ~`Organisation type`, opacity = 1, title = "The World of Endometriosis Research") %>%
  addFullscreenControl()

map.inst <- inlmisc::AddSearchButton(map.inst, group = "organisations", zoom = 15,textPlaceholder = "Search organization") # creation of the map

htmlwidgets::saveWidget((frameableWidget(map.inst)), selfcontained = F, libdir = "lib", 'map.inst.html') # export the map

