# This script carries out the analysis of topics at the national level (most common, most overrepresented). It produces the four related maps.

# loading relevant libraries

library(readxl)
library(tidyverse)
library(rgdal)
library(leaflet)
library(htmltools)
library(leaflet.extras)
library(widgetframe)

# importing full dataset of endometriosis publications

fulldatatopics <- read_excel("~endodata.xlsx", na = "NA") # import the full dataset with publications and countries
list.countries <- unique(as.vector(as.matrix(fulldatatopics[,85:95]))) # extract list of countries to cover
list.countries <- list.countries[!is.na(list.countries)] # remove NAs

national.topics <- data.frame("Country" = list.countries) # create a dataset to countain all the data

# process the shares by topics for each country of the list

i <- 1

for (i in 1:nrow(national.topics)) { # run for each country
  
  datatoextractinst <- data.frame() # initialise extract of database to retrieve information
  datatoextractinst <- fulldatatopics %>% filter_at(c("Country 1","Country 2","Country 3", "Country 4", "Country 5", "Country 6", "Country 7", "Country 8", "Country 9", "Country 10", "Country 11"),any_vars(.%in% national.topics$Country[i])) # Select only publications matching the country under study
  
  national.topics[i,2] <- nrow(datatoextractinst) # number of relevant publications is equivalent to number of rows
  national.topics[i,3] <- national.topics[i,2] - sum(is.na(datatoextractinst$`1. Causes and mechanisms`)) # computation of the number of publications with an abstract allowing the analysis of topics
  
  j <- 11
  
  for (j in 11:53) { # extract average values for each topic
    
    national.topics[i,j-7] <- colMeans(datatoextractinst[sapply(datatoextractinst, is.numeric)], na.rm=TRUE)[j]
  }
  
}

colnames(national.topics) <- c("Country","TotalPubs","PubsWithAbstracts", colnames(datatoextractinst)[119:161]) # assign names to columns of the dataset
national.topics[,4:46] <- round(national.topics[,4:46]*100, digits = 2) # convert to percentages and trim decimals

# process the over-representation ratios

topicsallpubs <- (colMeans(fulldatatopics[sapply(fulldatatopics, is.numeric)], na.rm=TRUE)[11:53]) * 100 # extract the share of topics for all publications (even those with no geographical matches)

i <- 1

for (i in 1:nrow(national.topics)) { # process each country
  
  j <- 1
  
  for (j in 1:43) { # process each column
    
    national.topics[i,46+j] <- round(national.topics[i,3+j] / topicsallpubs[j], digits = 2) # compute and format ratio
    
  }
  
}

colnames(national.topics)[47:89] <- paste("Ratio",colnames(datatoextractinst)[119:161]) # attribute names to columns


# identification of the top topics for each country and their underlying characteristics

################ BROAD TOPICS

national.broad <- national.topics[,5:12] # subset for broad topics

national.topics$CommonBroad <- colnames(national.broad)[max.col(national.broad,ties.method="first")] # extract the name of the most common broad topic for each country

i <- 1 

for (i in 1:nrow(national.topics)) {

national.topics[i,91] <- max(national.broad[i,]) # extract the share for the corresponding broad topic

}

colnames(national.topics)[91] <- "ShareMostCommonBroad"

national.broad.over <- national.topics[,48:55] # subset for broad topics over representation

national.topics$SpecificBroad <- substring(colnames(national.broad.over)[max.col(national.broad.over,ties.method="first")],7) # extract the name of the most specific broad topic for each country

i <- 1 

for (i in 1:nrow(national.topics)) {
  
  national.topics[i,93] <- max(national.broad.over[i,]) # extract the ratio for the corresponding broad topic
  
}

colnames(national.topics)[93] <- "RatioMostSpecificBroad"

i <- 1 

for (i in 1:nrow(national.topics)) { # retrieve the share for each over represented topic
  
  if (length(national.broad[i,substring(colnames(national.broad.over)[max.col(national.broad.over,ties.method="first")],7)[i]]) > 0) {
  
  national.topics[i,94] <- national.broad[i,substring(colnames(national.broad.over)[max.col(national.broad.over,ties.method="first")],7)[i]]
  }
}

colnames(national.topics)[94] <- "ShareMostSpecificBroad"

############### SPECIFIC TOPICS ##################

national.detail <- national.topics[,16:46] # subset for specific topics

national.topics$CommonDetailed <- colnames(national.detail)[max.col(national.detail,ties.method="first")] # extract the name of the most common detailed topic for each country

i <- 1 

for (i in 1:nrow(national.topics)) {
  
  national.topics[i,96] <- max(national.detail[i,]) # extract the share for the corresponding broad topic
  
}

colnames(national.topics)[96] <- "ShareCommonDetailed"

national.detailed.over <- national.topics[,59:89] # subset for detailed topics over representation

national.topics$SpecificDetailed <- substring(colnames(national.detailed.over)[max.col(national.detailed.over,ties.method="first")],7) # extract the name of the most specific detailed topic for each country


i <- 1 

for (i in 1:nrow(national.topics)) {
  
  national.topics[i,98] <- max(national.detailed.over[i,]) # extract the ratio for the corresponding detailed topic
  
}

colnames(national.topics)[98] <- "RatioMostSpecificDetailed"

i <- 1 

for (i in 1:nrow(national.topics)) { # retrieve the share for each over represented topic
  
  if (length(national.detail[i,substring(colnames(national.detailed.over)[max.col(national.detailed.over,ties.method="first")],7)[i]]) > 0) {
    
    national.topics[i,99] <- national.detail[i,substring(colnames(national.detailed.over)[max.col(national.detailed.over,ties.method="first")],7)[i]]
  }
}

colnames(national.topics)[99] <- "ShareMostSpecificDetailed"

######## Adding country codes (ISO3) and exporting the file

isocodes <- read_excel("~iso_3digit_alpha_country_codes.xlsx") # list matching ISO3 codes with countries
national.topics$ISO <- isocodes$`Code Value`[match(national.topics$Country,isocodes$Definition)] # assign ISO codes to countries

################ VISUALIZING THE DATA THROUGH MAPS


# importing the world shp file

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
world_spdf_topics <- readOGR(dsn= "/Shpfile" , layer="TM_WORLD_BORDERS_SIMPL-0.3", verbose=FALSE)

# assigning the data to the spatial file

world_spdf_topics$CommonBroad <- national.topics$CommonBroad[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$ShareMostCommonBroad <- national.topics$ShareMostCommonBroad[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$SpecificBroad <- national.topics$SpecificBroad[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$ShareMostSpecificBroad <- national.topics$ShareMostSpecificBroad[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$RatioMostSpecificBroad <- national.topics$RatioMostSpecificBroad[match(world_spdf_topics$ISO3,national.topics$ISO)]

world_spdf_topics$CommonDetailed <- national.topics$CommonDetailed[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$ShareCommonDetailed <- national.topics$ShareCommonDetailed[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$SpecificDetailed <- national.topics$SpecificDetailed[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$ShareMostSpecificDetailed <- national.topics$ShareMostSpecificDetailed[match(world_spdf_topics$ISO3,national.topics$ISO)]
world_spdf_topics$RatioMostSpecificDetailed <- national.topics$RatioMostSpecificDetailed[match(world_spdf_topics$ISO3,national.topics$ISO)]

# setting common formatting for the maps (CSS and palettes)

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

palcommonbroad <- colorFactor(palette = c("#66c2a5", "#fc8d62" , "#8da0cb", "#e78ac3" , "#a6d854", "#ffd92f"), levels = c("1. Causes and mechanisms", "2. Anatomy and classification", "3. Symptoms and daily life", "4. Diagnosis and analyses", "5. Treatment", "6. Connections to other diseases")) # define the color palette for the most common broad topics

palspecificbroad <- colorFactor(palette = c("#66c2a5", "#fc8d62" , "#8da0cb", "#e78ac3" , "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"), levels = c("1. Causes and mechanisms", "2. Anatomy and classification", "3. Symptoms and daily life", "4. Diagnosis and analyses", "5. Treatment", "6. Connections to other diseases", "7. Health and economic burden", "8. Miscellaneous")) # define the color palette for the most specific broad topics

palcommondetailed <- colorFactor(palette = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#2c7fb8", "#de2d26", "#b3cde3", "#8856a7", "#810f7c", "#feebe2", "#fbb4b9", "#f768a1", "#ae017e",  "#a1d76a", "#4d9221", "#c51b7d", "#fde0ef", "#ffeda0", "#feb24c", "#f03b20", "#af8dc3"), levels = sort(as.vector(unique(world_spdf_topics$CommonDetailed)))) # define the color palette for the most common detailed topics

palspecificdetailed <- colorFactor(palette = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "#fee0d2", "#fc9272", "#de2d26", "#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "#feebe2", "#fbb4b9", "#f768a1", "#ae017e", "#e6f5d0", "#a1d76a", "#4d9221", "#c51b7d", "#fde0ef", "#ffeda0", "#feb24c", "#f03b20", "#5ab4ac", "#d8b365", "#af8dc3", "#7fbf7b"), levels = sort(as.vector(unique(world_spdf_topics$SpecificDetailed)))) # define the color palette for the most specific broad topics

# generate the labels for the different maps (4)

labs.commonbroad <- lapply(seq(nrow(world_spdf_topics@data)), function(i) { # generate the labels for the map (most common broad topics)
  paste0( '<p>', world_spdf_topics$NAME[i], " most common topic: ",substring(world_spdf_topics@data$CommonBroad[i],4),'<br/>', 
          '(',  round(world_spdf_topics@data$ShareMostCommonBroad[i], digits = 2),"% of total sci. output)") 
})

labs.specificbroad <- lapply(seq(nrow(world_spdf_topics@data)), function(i) { # generate the labels for the map (most specific broad topics)
  paste0( '<p>', world_spdf_topics$NAME[i], " most specific topic: ",substring(world_spdf_topics@data$SpecificBroad[i],4),'<br/>', 
          '(',  round(world_spdf_topics@data$ShareMostSpecificBroad[i], digits = 2),"% of total sci. output, i.e., ", round(world_spdf_topics@data$RatioMostSpecificBroad[i], digits = 2), " times the average)") 
})

labs.commondetailed <- lapply(seq(nrow(world_spdf_topics@data)), function(i) { # generate the labels for the map (most common detailed topics)
  paste0( '<p>', world_spdf_topics@data$NAME[i], " most common detailed topic: ","<br/>",substring(world_spdf_topics@data$CommonDetailed[i],6),'<br/>', 
          '(',  round(world_spdf_topics@data$ShareCommonDetailed[i], digits = 2),"% of total sci. output)") 
})

labs.specificdetailed <- lapply(seq(nrow(world_spdf_topics@data)), function(i) { # generate the labels for the map (most specific detailed topics)
  paste0( '<p>', world_spdf_topics@data$NAME[i], " most specific topic: ","<br/>", substring(world_spdf_topics@data$SpecificDetailed[i],6),'<br/>', 
          '(',  round(world_spdf_topics@data$ShareMostSpecificDetailed[i], digits = 2),"% of total sci. output, i.e., ", round(world_spdf_topics@data$RatioMostSpecificDetailed[i], digits = 2), " times the average)") 
})


# generate the maps

map.commonbroadtopic.light <- leaflet(world_spdf_topics) %>% addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.2, fillOpacity = 1, fillColor = ~palcommonbroad(CommonBroad), label = lapply(labs.commonbroad, htmltools::HTML), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
  addLegend(title = "Most common broad topic", pal = palcommonbroad, values = ~CommonBroad, opacity = 1.0) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create map (most common broad topics)

map.specificbroadtopic.light <- leaflet(world_spdf_topics) %>% addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.2, fillOpacity = 1, fillColor = ~palspecificbroad(SpecificBroad), label = lapply(labs.specificbroad, htmltools::HTML), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
  addLegend(title = "Most specific broad topic", pal = palspecificbroad, values = ~SpecificBroad, opacity = 1.0) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create map (most specific broad topics)

map.commondetailedtopic.light <- leaflet(world_spdf_topics) %>% addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.2, fillOpacity = 1, fillColor = ~palcommondetailed(CommonDetailed), label = lapply(labs.commondetailed, htmltools::HTML), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create map (most common detailed topics)

map.specificdetailedtopic.light <- leaflet(world_spdf_topics) %>% addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.2, fillOpacity = 1, fillColor = ~palspecificdetailed(SpecificDetailed), label = lapply(labs.specificdetailed, htmltools::HTML), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))  %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create map (most specific detailed topics)

htmlwidgets::saveWidget((frameableWidget(map.commonbroadtopic.light)), selfcontained = F, libdir = "lib", 'map.commonbroadtopic.light.html') # export map
htmlwidgets::saveWidget((frameableWidget(map.specificbroadtopic.light)), selfcontained = F, libdir = "lib", 'map.specificbroadtopic.light.html') # export map
htmlwidgets::saveWidget((frameableWidget(map.commondetailedtopic.light)), selfcontained = F, libdir = "lib", 'map.commondetailedtopic.light.html') # export map
htmlwidgets::saveWidget((frameableWidget(map.specificdetailedtopic.light )), selfcontained = F, libdir = "lib", 'map.specificdetailedtopic.light .html') # export map
