# This script creates the dataset for the analysis of publications at the national level. It then proceeds to build the relevant maps and graphs. 

library(readxl)
library(tidyverse)
library(plotly)
library(rgdal)
library(leaflet)
library(htmltools)
library(leaflet.extras)
library(widgetframe)

# importing the relevant dataset

countrypublis <- read_excel("~publications by country.xlsx") # this intermediate dataset summarises the number of publications and citations by country and by year. It was derived from the source database.

# importing the world dataset (alternative, lighter shp approach)

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip") # this is the shapefile with the world countries
world_spdf <- readOGR(dsn= "Shpfile" , layer="TM_WORLD_BORDERS_SIMPL-0.3", verbose=FALSE) # import the shapefile to R

# Chloropeth map - number of cumulative publications since 1925

cumulativecountrydata <- subset(countrypublis,countrypublis$Year == 2020) # extract the data for the yeat 2020 only (last year of the sample)

world_spdf$CumFreq <- cumulativecountrydata$CumulativeFreq[match(world_spdf$ISO3, cumulativecountrydata$ISO3)] # cumulative number of endometriosis publications (1925-2020)

labs.totalpublis <- lapply(seq(nrow(world_spdf@data)), function(i) { # generate the labels for the map (multiple rows)
  paste0( '<p>', world_spdf@data$NAME[i], '<br/>', 
          world_spdf@data$CumFreq[i], ' publications (1925-2020) - ', 
          round(100*(world_spdf@data$CumFreq[i]/29567), digits = 2), "% of the total", '</p>' ) 
})

pal <- colorNumeric("Reds", NULL) # set the palette
pseudoLog10 <- function(x) { asinh(x/2)/log(10) } # define the function to smooth the shades of the map

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

map_countries.light <- leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(pseudoLog10(CumFreq)),
              label = lapply(labs.totalpublis, htmltools::HTML), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Number of Publications", pal = pal, values = ~pseudoLog10(CumFreq), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map of the cumulative number of publications by country

htmlwidgets::saveWidget((frameableWidget(map_countries.light)), selfcontained = F, libdir = "lib", 'map_countries.light.html') # export the map


# Chloropeth map - number of cumulative citations since 1925

world_spdf$CumCitations <- cumulativecountrydata$CumulativeCitations[match(world_spdf$ISO3,cumulativecountrydata$ISO3)] # cumulative number of endometriosis citations (1925-2020)

palcit <- colorNumeric("Greens", NULL) # define the palette for citations

labs.totalcit <- lapply(seq(nrow(world_spdf@data)), function(i) { # generate the labels for the map (multiple rows)
  paste0( '<p>', world_spdf@data$NAME[i], '<br/>', 
          world_spdf@data$CumCitations[i], ' total citations received (over the period 1925-2020) - ', 
          round(100*(world_spdf@data$CumCitations[i]/573051), digits = 2), "% of the total", '</p>' ) 
})

map_citcountries.light <- leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~palcit(pseudoLog10(CumCitations)),
              label = lapply(labs.totalcit, htmltools::HTML), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Number of Citations", pal = palcit, values = ~pseudoLog10(CumCitations), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map of the cumulative number of citations by country

htmlwidgets::saveWidget((frameableWidget(map_citcountries.light)), selfcontained = F, libdir = "lib", 'map_citcountries.light.html') # export the map


# Data processing for recent publications and per capita analyses

percapitapublis <- subset(countrypublis, subset = Year %in% 2010:2019) # extract all the data for publications of the 2010s
percapitapublis.sum <- percapitapublis %>% group_by(Country) %>% summarise(nbpub = sum(Freq), nbcit = sum(Citations)) # compute the total number of publications from the 2010s, and of citations collected by publications from the 2010s
unpopdata <- read_excel("~WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx") # import data from the UN on population
unpopdata$avg2010s <- rowMeans(unpopdata[,68:78]) # compute the mean population for the 2010s at the country level
percapitapublis.sum$pop2010s <- unpopdata$avg2010s[match(percapitapublis.sum$Country,unpopdata$`Region, subregion, country or area *`)] # assign the average population in the 2010s to each country in the dataset

world_spdf$pubs2010s <- percapitapublis.sum$nbpub[match(world_spdf$NAME,percapitapublis.sum$Country)] # extract the total number of publications from the 2010s and assign them to countries
world_spdf$cits2010s <- percapitapublis.sum$nbcit[match(world_spdf$NAME,percapitapublis.sum$Country)] # extract the total number of citations for publications from the 2010s and assign them to countries

percapitapublis.sum$pubsperpop2010s <- round(percapitapublis.sum$nbpub/10/(percapitapublis.sum$pop2010s/1000), digits = 2) # publications per million pop (yearly average)

percapitapublis.sumpc <- subset(percapitapublis.sum, percapitapublis.sum$pubsperpop2010s < 3) # create a subset for the map per capita, excluding countries  with aberrant values

world_spdf$pcpublis2010s <- percapitapublis.sumpc$pubsperpop2010s[match(world_spdf$NAME,percapitapublis.sumpc$Country)] # extract the average publications per capita and assign them to countries


# Basic chloropeth maps from the 2010s (number and citations)

labs.totpublis2010s <- lapply(seq(nrow(world_spdf@data)), function(i) { # generate the labels for the map (multiple rows)
  paste0( '<p>', world_spdf@data$NAME[i], '<br/>', 
          world_spdf@data$pubs2010s[i], ' total publications in the 2010s - ', 
          round(100*(world_spdf@data$pubs2010s[i]/13618), digits = 2), "% of the total", '</p>' ) 
})

map_countries_2010stot.light <- leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(pseudoLog10(pubs2010s)),
              label = lapply(labs.totpublis2010s, htmltools::HTML), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Number of Publications in the 2010s", pal = pal, values = ~pseudoLog10(pubs2010s), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map of the total number of publications from the 2010s

htmlwidgets::saveWidget((frameableWidget(map_countries_2010stot.light)), selfcontained = F, libdir = "lib", 'map_countries_2010stot.light.html') # export the map


labs.totcits2010s <- lapply(seq(nrow(world_spdf@data)), function(i) { # generate the labels for the map (multiple rows)
  paste0( '<p>', world_spdf@data$NAME[i], '<br/>', 
          world_spdf@data$cits2010s[i], ' total citations received for publications from the 2010s',"<br/>","(", 
          round(100*(world_spdf@data$cits2010s[i]/166620), digits = 2), "% of the total)", '</p>' ) 
})

map_citations_2010stot.light <- leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~palcit(pseudoLog10(cits2010s)),
              label = lapply(labs.totcits2010s, htmltools::HTML), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Total citations for publications of the 2010s", pal = palcit, values = ~pseudoLog10(cits2010s), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map for the total number of citations for publications from the 2010s

htmlwidgets::saveWidget((frameableWidget(map_citations_2010stot.light)), selfcontained = F, libdir = "lib", 'map_citations_2010stot.light.html') # export the map


# Per capita map for publications (2010s)

pal2 <- colorNumeric("PuRd", NULL) # define the palette

map_pcpublis.light <- leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal2(pseudoLog10(pcpublis2010s)),
              label = ~paste0(NAME, ": ", formatC(pcpublis2010s, big.mark = ","), " publications each year per million inhabitants in the 2010s"), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Yearly publications per million inhabs. (2010s)", pal = pal2, values = ~(pcpublis2010s), opacity = 1.0) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map for the number of publications per capita in the 2010s

htmlwidgets::saveWidget((frameableWidget(map_pcpublis.light)), selfcontained = F, libdir = "lib", 'map_pcpublis.light.html') # export the map


# Map controlling for economic output

gdpdatawb <- read_excel("~API_NY.GDP.MKTP.CD_DS2_en_excel_v2_3158925.xlsx", sheet = "data euros", na = "NA") # import GDP data in EUR 2020

world_spdf$GDP2010 <- gdpdatawb$`Average GDP 2010s in 2020 euros`[match(world_spdf$ISO3,gdpdatawb$`Country Code`)] # import GDP data
world_spdf$pGDPpublis2010s <- (world_spdf$pubs2010s/10 / world_spdf$GDP2010s) # compute ratio for publications

world_spdf$pGDPpublis2010s <- round(world_spdf$pGDPpublis2010s * 1000000000000, digits = 2) # convert to publis per trillion of GDP
world_spdf$pGDPpublis2010s[!is.finite(world_spdf$pGDPpublis2010s)]<- NA # remove infinite values
world_spdf$pGDPpublis2010s[world_spdf$ISO3 == "TUV"] <- NA # remove tuvalu for aberrant value


css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

palGDP <- colorNumeric("YlOrBr", NULL) # define palette

map.yearlypublisperGDP.light <-  leaflet(world_spdf) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~palGDP(pseudoLog10(pGDPpublis2010s)),
              label = ~paste0(NAME, ": ", pGDPpublis2010s, " yearly publications per trillion of GDP (real 2020 euros) in the 2010s"), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Yearly publications per trillion of GDP in the 2010s", pal = palGDP, values = ~pseudoLog10(pGDPpublis2010s), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # map for the publications per GDP trillion

htmlwidgets::saveWidget((frameableWidget(map.yearlypublisperGDP.light)), selfcontained = F, libdir = "lib", 'map.yearlypublisperGDP.light.html') # export the map
