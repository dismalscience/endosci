# This script analyses the additional data from the Clinicaltrials.gov database. It compiles the related graphs.

library(readxl)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(widgetframe)

# Importing the dataset

clinicaltrialsdata <- read_excel("~clinicaltrialsdata.xlsx") # importing the clinical trials dataset with full information on the individual studies

# Processing data for the evolution of the number of started trials by year

clinicalevo <- clinicaltrialsdata %>% group_by(StartYear) %>% summarise(nb = n()) # counting the evolution of clinical trials over time
clinicalevo <- clinicalevo[!is.na(clinicalevo$StartYear),]
clinicalevo$StartYear <- as.numeric(clinicalevo$StartYear)
clinicalevo <- subset(clinicalevo,clinicalevo$StartYear<2021) # drop recent year
clinicalevo <- clinicalevo %>% complete(StartYear = 1990:2020, fill = list(nb = 0)) # add missing years

# Processing data for the types of interventions tested in trials

trialsevotypes <- read_excel("~clinicaltrialsdata.xlsx", sheet = "evolutiontypes") # importing the data with the share of trials containing the different interventions by year
trialsevotypes$ShareTrials <- round(100 * trialsevotypes$ShareTrials, digits = 2) # converting to percentages

# Generation of graphs

web.inter.chart.nbclinic <- plot_ly(data = clinicalevo, x = ~StartYear, y = ~nb, type = "bar", marker = list(color = "#3182bd", width = 4), hovertemplate = "%{y} new endometriosis-related clinical trials started in %{x} <extra></extra>") %>% layout(yaxis = list(title = 'Number of trials started'), xaxis = list(title = "Starting Year")) %>% config(displayModeBar = FALSE) # create the graph for the evolution of the number of started trials by year

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.nbclinic)), selfcontained = F, libdir = "lib", 'web.inter.chart.nbclinic.html') # export graph

web.inter.chart.typestrials <- plot_ly(data = trialsevotypes, x = ~StartYear, y = ~ShareTrials, color = ~InterventionType, type = "bar", text = ~InterventionType, name = ~InterventionType, hovertemplate = "%{text}: type of intervention used in %{y}% of clinical trials started in %{x}<extra></extra>") %>% layout(barmode = "stack", yaxis = list(title = 'Share of clinical trials'), xaxis = list(title = "")) %>% config(displayModeBar = FALSE) %>% layout(legend = list(orientation = 'h')) # create the graph for the evolution of the shares of trials testing the different types of intervention 

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.typestrials)), selfcontained = F, libdir = "lib", 'web.inter.chart.typestrials.html')  # export graph

