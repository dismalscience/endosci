# This script analyses the gender breakdown of contributors by type of institutions. It then produces a dedicated graph. 

# Loading the required libraries

library(readxl)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(widgetframe)

# Importing the source datasets

sourcedatainst <- read_excel("~endodata.xlsx", na = "NA") # import the full dataset with publication characteristics
individual_inst_data <- read_excel("~final affiliations list.xlsm", sheet = "list codes and caracs", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text"), na = "NA") # import the full list of institutions
indauthordata <- read_excel("~authors publications pairs.xlsx", col_types = c("text", "numeric", "numeric", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric"), na = "NA") # import the dataset with individual authors, their gender and affiliations

# Initialisation of the processing

genderbytypeinst <- data.frame("Type" = unique(individual_inst_data$`Organisation type`)) # initialise table for results
genderbytypeinst$NbPublis <- NA
genderbytypeinst$MaleContrib <- NA
genderbytypeinst$FemaleContrib <- NA
genderbytypeinst$UndeterminedContrib <- NA
genderbytypeinst$MaleShareContrib <- NA
genderbytypeinst$FemaleShareContrib <- NA

datatoextractinst <- data.frame() # initialise extract of database to retrieve information

# Processing the gender contribution for each type of institution

i <- 1 # initialize the process

for (i in 1:nrow(genderbytypeinst)) { # perform the operation for each type of institution
  
  listcodesinst <- (individual_inst_data  %>% filter(`Organisation type` == genderbytypeinst$Type[i]))$Code # extract the list of codes for the type of institution
  
  datatoextractinst <- indauthordata %>% filter_at(c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13"),any_vars(.%in% listcodesinst)) # retrieve the data with the authors solely linked to the type of institution under scrutiny
  
  genderbytypeinst$NbPublis[i] <- length(unique(datatoextractinst$Publication))
  genderbytypeinst$MaleContrib[i] <- sum(datatoextractinst$Gender == "Male")
  genderbytypeinst$FemaleContrib[i] <- sum(datatoextractinst$Gender == "Female")
  genderbytypeinst$UndeterminedContrib[i] <- sum(datatoextractinst$Gender == "Not determined")
  genderbytypeinst$MaleShareContrib[i] <- round(100*(genderbytypeinst$MaleContrib[i] / (genderbytypeinst$MaleContrib[i] + genderbytypeinst$FemaleContrib[i])), digits = 2)
  genderbytypeinst$FemaleShareContrib[i] <-  round(100*(genderbytypeinst$FemaleContrib[i] / (genderbytypeinst$MaleContrib[i] + genderbytypeinst$FemaleContrib[i])), digits = 2)
  
  
  print((i/nrow(genderbytypeinst))*100) # show progress percentage
}

genderbytypeinst <- genderbytypeinst[!(genderbytypeinst$Type=="Not specified (city only)" | genderbytypeinst$Type == "Not specified (country only)" | genderbytypeinst$Type == "Not applicable" | genderbytypeinst$Type == "Not specified (region only)" | genderbytypeinst$Type == "Archive" | genderbytypeinst$Type == "Other"),] # remove types of institutions that cannot be interpreted easily
genderbytypeinst$Type <- as.character(genderbytypeinst$Type)

genderbytypeinst[nrow(genderbytypeinst)+1,] <- c("All types", length(unique(indauthordata$Publication)), sum(indauthordata$Gender == "Male"), sum(indauthordata$Gender == "Female"), sum(indauthordata$Gender == "Not determined"), round(100*(sum(indauthordata$Gender == "Male") / (sum(indauthordata$Gender == "Male") + sum(indauthordata$Gender == "Female"))), digits = 2), round(100*(sum(indauthordata$Gender == "Female") / (sum(indauthordata$Gender == "Male") + sum(indauthordata$Gender == "Female"))), digits = 2)) # add comparison data for all publications

genderbytypeinst <- genderbytypeinst %>% mutate_at(c(2:7), as.numeric) # convert numbers to numeric value type

genderbytypeinst <- genderbytypeinst %>% arrange(desc(NbPublis)) # arrange order
genderbytypeinst$Type <- factor(genderbytypeinst$Type, levels = unique(genderbytypeinst$Type)[order(genderbytypeinst$NbPublis, decreasing = TRUE)])

genderbytypeinst.long <- pivot_longer(genderbytypeinst, cols=6:7, names_to = "Gender", values_to = "Values") # convert to long format for simplicity in producing the plotly graph

genderbytypeinst.long$Gender[genderbytypeinst.long$Gender == "FemaleShareContrib"] <- "Female" # renaming the variable for easier graph implementation
genderbytypeinst.long$Gender[genderbytypeinst.long$Gender == "MaleShareContrib"] <- "Male" # ditto


# Creation of the output (graph)

web.inter.chart.genderbytypeofinst <- plot_ly(data=genderbytypeinst.long, y = ~Type, x = ~Values, color = ~Gender, text = ~Gender, type = "bar", orientation = "h", colors = c("#9970ab","#e08214"), hovertemplate = "%{x}% of %{text} contribution in the %{y} institutions<extra></extra>")%>% layout(yaxis = list(title = ''), xaxis = list(title = "Contribution by Gender (%)"), barmode = 'stack') %>% config(displayModeBar = FALSE) %>% layout(shapes = list(type = "line", fillcolor = "black", line = list(color = "black"), opacity = 1, x0 = 50, x1 = 50, xref = 'x', y0 = -0.45, y1 = 6.4, yref = 'y')) # creation of the graph linking the type of institution with the gender breakdown of contributors

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.genderbytypeofinst)), selfcontained = F, libdir = "lib", 'web.inter.chart.genderbytypeofinst.html') # export the graph
