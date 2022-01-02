# This script builds the simple graph showing the correlation between citations and number of publications at the country level
# It requires the dataset produced for the maps at the country level (see "countries map" script)

library(plotly)
library(readxl)
library(htmlwidgets)
library(widgetframe)


# Creation of the dataframe

correlation.countries <- data.frame("Country" = world_spdf@data$NAME, "SharePublis" = round(100*(world_spdf@data$CumFreq/29567), digits = 2), "ShareCitations" = round(100*(world_spdf@data$CumCitations/573051), digits = 2), "TotalPubs" = world_spdf@data$CumFreq) # this requires the file produced in the map countries script
continentcountries <- read_excel("~continents.xlsx") # import file matching the country names with continents
colnames(continentcountries)[1] <- "Country"
continentcountries <- continentcountries[,1:2]
correlation.countries <- inner_join(correlation.countries,continentcountries, by = "Country") # merge the dataframes to have the countries with their continents, number of publications and citations

# Creation of the correlation graph

eqline <- list(type = "line", line = list(color = "black"), xref = "x", yref = "y", x0 = 0, y0 = 0, x1 = 100, y1 = 100) # this is an equality line

web.inter.graph.corrcountries <- plot_ly(data = correlation.countries, x = ~SharePublis, y = ~ShareCitations, color = ~Region, colors = c("#CC503E", "#3969AC", "#F2B701", "#11A579", "#7F3C8D"), size = ~TotalPubs,  hovertemplate = ~paste("Country: ", Country, '<br>Total Publications:', TotalPubs, "<br>Share Publications:",SharePublis,"%<br>Share Citations:",ShareCitations,"%")) %>% layout(shapes = eqline, xaxis = list(title="Share of Publications", range = list(0,45)), yaxis = list(title="Share of Citations", range = list(0,45))) %>% config(displayModeBar = FALSE) %>% layout(legend = list(xanchor = "center", x = 0.5, y=1.15, orientation = "h")) # creation of the graph correlating number of publications and citations at the country level

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.graph.corrcountries)), selfcontained = F, libdir = "lib", 'web.inter.graph.corrcountries.html')
