# This script conducts analyses with a breakdown by type of institutions. It produces the related graphs as well. 

library(readxl)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(widgetframe)

# Computation of statistics of publications for each type of institution

databasewithinsttype <- read_excel("~endodata with types institutions.xlsx", na = "NA") # this is a slightly revised version of the endo database with the types of institutions explicitly included in ad-hoc columns
codesandcaracs <- read_excel("~final affiliations list.xlsm", sheet = "list codes and caracs") # import the list of institutions with their characteristics

typeinstdata <- data.frame("Type" = unique(codesandcaracs$`Organisation type`)) # initialise table for results

i <- 1

for (i in 1:nrow(typeinstdata)) { # process data for each type of institutions
  
  datatoextractinst <- data.frame() # initialise extract of database to retrieve information
  
  datatoextractinst <- databasewithinsttype %>% filter_at(c("TYP1","TYP2","TYP3", "TYP4", "TYP5", "TYP6", "TYP7", "TYP8", "TYP9", "TYP10", "TYP11", "TYP12", "TYP13", "TYP14", "TYP15", "TYP16", "TYP17"),any_vars(.%in% typeinstdata$Type[i])) # extract all publications involving at least one organization from the studied type
  
  typeinstdata[i,2] <- nrow(datatoextractinst) # number of relevant publications is equivalent to number of rows
  typeinstdata[i,3] <- typeinstdata[i,2] - sum(is.na(datatoextractinst$`1. Causes and mechanisms`)) # computation of the number of publications with an abstract allowing the analysis of topics
  
  # computation of the shares of topics for each type of organisation
  
  typeinstdata[i, 4] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[1] # extraction of total topic contribution
  typeinstdata[i, 5] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[2] # extraction of total topic contribution
  typeinstdata[i, 6] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[3] # extraction of total topic contribution
  typeinstdata[i, 7] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[4] # extraction of total topic contribution
  typeinstdata[i, 8] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[5] # extraction of total topic contribution
  typeinstdata[i, 9] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[6] # extraction of total topic contribution
  typeinstdata[i, 10] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[7] # extraction of total topic contribution
  typeinstdata[i, 11] <- colMeans(datatoextractinst[, 137:144], na.rm = TRUE)[8] # extraction of total topic contribution
  
  typeinstdata[i, 12] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[1] # extraction of total topic contribution
  typeinstdata[i, 13] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[2] # extraction of total topic contribution
  typeinstdata[i, 14] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[3] # extraction of total topic contribution
  typeinstdata[i, 15] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[4] # extraction of total topic contribution
  typeinstdata[i, 16] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[5] # extraction of total topic contribution
  typeinstdata[i, 17] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[6] # extraction of total topic contribution
  typeinstdata[i, 18] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[7] # extraction of total topic contribution
  typeinstdata[i, 19] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[8] # extraction of total topic contribution
  typeinstdata[i, 20] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[9] # extraction of total topic contribution
  typeinstdata[i, 21] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[10] # extraction of total topic contribution
  typeinstdata[i, 22] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[11] # extraction of total topic contribution
  typeinstdata[i, 23] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[12] # extraction of total topic contribution
  typeinstdata[i, 24] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[13] # extraction of total topic contribution
  typeinstdata[i, 25] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[14] # extraction of total topic contribution
  typeinstdata[i, 26] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[15] # extraction of total topic contribution
  typeinstdata[i, 27] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[16] # extraction of total topic contribution
  typeinstdata[i, 28] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[17] # extraction of total topic contribution
  typeinstdata[i, 29] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[18] # extraction of total topic contribution
  typeinstdata[i, 30] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[19] # extraction of total topic contribution
  typeinstdata[i, 31] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[20] # extraction of total topic contribution
  typeinstdata[i, 32] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[21] # extraction of total topic contribution
  typeinstdata[i, 33] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[22] # extraction of total topic contribution
  typeinstdata[i, 34] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[23] # extraction of total topic contribution
  typeinstdata[i, 35] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[24] # extraction of total topic contribution
  typeinstdata[i, 36] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[25] # extraction of total topic contribution
  typeinstdata[i, 37] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[26] # extraction of total topic contribution
  typeinstdata[i, 38] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[27] # extraction of total topic contribution
  typeinstdata[i, 39] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[28] # extraction of total topic contribution
  typeinstdata[i, 40] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[29] # extraction of total topic contribution
  typeinstdata[i, 41] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[30] # extraction of total topic contribution
  typeinstdata[i, 42] <- colMeans(datatoextractinst[, 148:178], na.rm = TRUE)[31] # extraction of total topic contribution

  }

colnames(typeinstdata) <- c("Type","NbPublis","NbWithTopics", colnames(datatoextractinst[, 137:144]), colnames(datatoextractinst[, 148:178])) # clean names for the columns of the table

typeinstdata <- typeinstdata[!(typeinstdata$Type=="Not specified (city only)" | typeinstdata$Type == "Not specified (country only)" | typeinstdata$Type == "Not applicable" | typeinstdata$Type == "Not specified (region only)"),] # remove types that cannot be interpreted
typeinstdata$Type <- as.character(typeinstdata$Type)

topicsworld <- append((colMeans(databasewithinsttype[sapply(databasewithinsttype, is.numeric)], na.rm=TRUE)[137:144]), (colMeans(databasewithinsttype[sapply(databasewithinsttype, is.numeric)], na.rm=TRUE)[148:178])) # retrieve the topic shares for all publications (with an abstract)
typeinstdata[nrow(typeinstdata)+1,] <- append(c("All types", "36929", "28521"), topicsworld) # insert the data regarding all publications in the dataframe

typeinstdata$NbPublis <- as.numeric(typeinstdata$NbPublis) # ensuring adequate format
typeinstdata$NbWithTopics <- as.numeric(typeinstdata$NbWithTopics) # ditto
typeinstdata <- typeinstdata %>% arrange(desc(NbPublis)) # arrange order
typeinstdata$Type <- factor(typeinstdata$Type, levels = unique(typeinstdata$Type)[order(typeinstdata$NbPublis, decreasing = TRUE)])
typeinstdata[,4:42] <- typeinstdata[,4:42]*100 # convert to percentages
typeinstdata[,4:42] <- round(typeinstdata[,4:42], digits = 2) # clean percentages

# Creation of the graphs - number of publications by type

web.inter.chart.quantbytype <- plot_ly(typeinstdata[!(typeinstdata$Type == "All types"),], x = ~Type, y = ~NbPublis, type = 'bar', hovertemplate = "%{y} endometriosis publications involving at least one organization from the %{x} type <extra></extra>", marker = list(color = c("#80B1D3","#FB8072","#8DD3C7","#ffd92f","#B3DE69","#FDB462","#dfc27d","#BEBADA"))) %>% layout(xaxis = list(title=""), yaxis = list(title="Number of publications")) %>% config(displayModeBar = FALSE) # graph for the number of publication by type

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.quantbytype)), selfcontained = F, libdir = "lib", 'web.inter.chart.quantbytype.html') # export the graph

# Creation of the graphs - broad topics by type

typeinstbroadtopicsdata <- pivot_longer(typeinstdata[,c(1,4:11)], cols=2:9, names_to = "Topic", values_to = "Share")
typeinstbroadtopicsdata <- typeinstbroadtopicsdata[!(typeinstbroadtopicsdata$Type == "Archive"),] # drop irrelevant type of institution
typeinstbroadtopicsdata <- typeinstbroadtopicsdata[!(typeinstbroadtopicsdata$Type == "Other"),] # drop irrelevant type of institution

web.inter.chart.broadtopictypes <- plot_ly(typeinstbroadtopicsdata, x = ~Share, y = ~Type, color = ~Topic, legendgroup = ~Topic,  type = 'bar', orientation = 'h', hovertemplate = "%{x}% of the scientific output for %{y}") %>% layout(barmode = "stack") %>% layout(xaxis = list(title="Share of Abstracts' Content"), yaxis = list(title="")) %>% config(displayModeBar = FALSE) # graph for the broad topic share by type

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.broadtopictypes)), selfcontained = F, libdir = "lib", 'web.inter.chart.broadtopictypes.html') # export the graph

# Creation of the graphs - most overrepresented detailed topics

typeinstdatarepresentedtopics <- read_excel("~detailed topics by types formatted.xlsx", na = "NA", sheet="longformat") # this is the dataset produced in the previously mentioned script. It has been reformatted to the adequate form with Excel for simplicity. 
typeinstdatarepresentedtopics$Topic <- factor(typeinstdatarepresentedtopics$Topic, levels = unique(typeinstdatarepresentedtopics$Topic)[order(typeinstdatarepresentedtopics$Topic, decreasing = TRUE)]) # rearrange order

typeinstdatarepresentedtopics <- typeinstdatarepresentedtopics[!(typeinstdatarepresentedtopics$Type == "Archive"),] # drop irrelevant type of institution
typeinstdatarepresentedtopics <- typeinstdatarepresentedtopics[!(typeinstdatarepresentedtopics$Type == "Other"),] # drop irrelevant type of institution

inter.chart.typeovertopics <- plot_ly(typeinstdatarepresentedtopics, y = ~Topic, x = ~round(RepRatio, digits = 2), type = 'bar', text = ~Type, orientation = "h", hovertemplate = "%{x} times the average among institutions of the %{text} type<extra></extra>" , transforms = list(list(type = 'filter', target = ~Type, operation = '=', value = unique(typeinstdatarepresentedtopics$Type)[1])),  marker = list(color = ~RepRatio))  %>% layout(yaxis = list(title = ''), xaxis = list(range = list(0,4), title = 'Deviation from average (ratio)')) %>% layout(shapes = list(type = "line", fillcolor = "black", line = list(color = "black"), opacity = 1, x0 = 1, x1 = 1, xref = 'x', y0 = -0.45, y1 = 31, yref = 'y')) %>% config(displayModeBar = FALSE) %>% layout(
  updatemenus = list(
    list(type = 'dropdown',
         active = 0, y = 1.1,
         x = -0.3,
         buttons = list(
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[1]),
                label = unique(typeinstdatarepresentedtopics$Type)[1]),
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[2]),
                label = unique(typeinstdatarepresentedtopics$Type)[2]),
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[3]),
                label = unique(typeinstdatarepresentedtopics$Type)[3]),
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[4]),
                label = unique(typeinstdatarepresentedtopics$Type)[4]),
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[5]),
                label = unique(typeinstdatarepresentedtopics$Type)[5]),
           list(method = "restyle",
                args = list("transforms[0].value", unique(typeinstdatarepresentedtopics$Type)[6]),
                label = unique(typeinstdatarepresentedtopics$Type)[6])
         )
    )
  )
)
# interactive version

htmlwidgets::saveWidget((frameableWidget(inter.chart.typeovertopics)),  selfcontained = F, libdir = "lib", 'web.inter.chart.typeovertopics.html') # export the graph