# This script carries out the analyses of gender at the country level. It produces some maps and graphs with a focus on two countries (France and the United States of America). 

library(readxl)
library(tidyverse)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(leaflet.extras)
library(htmltools)
library(plotly)
library(widgetframe)

# Importing gender data at the country level

authorsindividualcountry <- read_excel("~gender country level.xlsx") # this dataset is produced in the script dedicated to sensitivity analyses for gender. It contains the information on gender contribution by country

# Importing the world shp file

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
world_spdf_gender <- readOGR(dsn= "/Shpfile" , layer="TM_WORLD_BORDERS_SIMPL-0.3", verbose=FALSE)

# Matching the data to the shp file

world_spdf_gender$ShareMale <- authorsindividualcountry$MaleShare[match(world_spdf_gender$ISO3,authorsindividualcountry$ISO3)]
world_spdf_gender$ShareMale2010s <- authorsindividualcountry$MaleShare2010s[match(world_spdf_gender$ISO3,authorsindividualcountry$ISO3)]

# Create the maps

palgender <- colorNumeric("PuOr", NULL, reverse = TRUE) # adequate color palette

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

map_gender.light <- leaflet(world_spdf_gender) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~palgender(ShareMale),
              label = ~paste0(NAME, ": ", round(ShareMale,digits=2), "% male / ", 100 - round(ShareMale,digits=2), "% female"), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Contribution to endo R&D", colors = c("#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788"), labels = c("100% male", "", "75% male", "", "Balanced", "", "75% female", "", "100% female"), opacity = 1.0) %>% htmlwidgets::prependContent(html_fix) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create the map with the gender contribution for all publications at the country level

htmlwidgets::saveWidget((frameableWidget(map_gender.light)), selfcontained = F, libdir = "lib", 'map_gender.light.html') # export the map


map_genderrecent.light <- leaflet(world_spdf_gender) %>%
  addPolygons(color = "#444444", weight = 1, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~palgender(ShareMale2010s),
              label = ~paste0(NAME, ": ", round(ShareMale2010s,digits=2), "% male / ", 100 - round(ShareMale2010s,digits=2), "% female"), labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto")) %>%
  addLegend(title = "Contribution to recent endo R&D (2010s)", colors = c("#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788"), labels = c("100% male", "", "75% male", "", "Balanced", "", "75% female", "", "100% female"), opacity = 1.0) %>% addFullscreenControl() %>% setMapWidgetStyle(list(background= "#deebf7")) # create the map with the gender contribution for the publications of the 2010s at the country level


htmlwidgets::saveWidget((frameableWidget(map_genderrecent.light)), selfcontained = F, libdir = "lib", 'map_genderrecent.light.html') # export the map


# Analysis of gender contribution in context for a sample of countries (FR, US...)

genderspecificcountries <- read_excel("~gender data FR US.xlsx", sheet = "shares") # importing the dataset with specific contextual data for France and the United States
genderspecificcountries$Share <- round(100 * genderspecificcountries$Share, digits = 2) # formatting the percentages

genderfrance <- subset(genderspecificcountries,genderspecificcountries$Country == "France") # subset for France
genderus <- subset(genderspecificcountries,genderspecificcountries$Country == "USA") # subset for the United States

genderfrance$Group <- factor(genderfrance$Group, levels = unique(as.character(genderfrance$Group)))
genderus$Group <- factor(genderus$Group, levels = unique(as.character(genderus$Group)))

web.inter.chart.genderfrance <- plot_ly(data=genderfrance, y = ~Group, x = ~Share, color = ~Gender, type = "bar", orientation = "h", colors = c("#9970ab","#e08214"), hovertemplate = "%{x}% of %{y}")%>% layout(yaxis = list(title = ''), xaxis = list(title = "Share by Gender (%)"), barmode = 'stack') %>% config(displayModeBar = FALSE) %>% layout(shapes = list(type = "line", fillcolor = "black", line = list(color = "black"), opacity = 1, x0 = 50, x1 = 50, xref = 'x', y0 = -0.45, y1 = 8.45, yref = 'y')) # create the contextualized graph for France

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.genderfrance)), selfcontained = F, libdir = "lib", 'web.inter.chart.genderfrance.html') # export the graph

web.inter.chart.genderus <- plot_ly(data=genderus, y = ~Group, x = ~Share, color = ~Gender, type = "bar", orientation = "h", colors = c("#9970ab","#e08214"), hovertemplate = "%{x}% of %{y}")%>% layout(yaxis = list(title = ''), xaxis = list(title = "Share by Gender (%)"), barmode = 'stack') %>% config(displayModeBar = FALSE) %>% layout(shapes = list(type = "line", fillcolor = "black", line = list(color = "black"), opacity = 1, x0 = 50, x1 = 50, xref = 'x', y0 = -0.45, y1 = 8.45, yref = 'y')) # create the contextualized graph for the USA

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.genderus)), selfcontained = F, libdir = "lib", 'web.inter.chart.genderus.html') # export the graph
