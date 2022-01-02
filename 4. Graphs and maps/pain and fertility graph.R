# This script allows the study of the evolution of publications specifically dedicated to pain and to fertility. It relies on an ad-hoc identification of the relevant topics derived from the LDA. 

# Libraries and functions

library(readxl)
library(purrr)
library(tidyverse)
library(htmlwidgets)
library(widgetframe)
library(plotly)

comput.rowsums <- function(x,y){ # this custom function performs computations for topic shares
  rowSums(topicdescription[,(pluck(x,y)+1)])
}

# Importing the relevant datasets

publis <- read_excel("endodata.xlsx") # this is the full dataset with publications' characteristics
classifiedpaperslist <- read_excel("topic description LDA.xlsx") # this dataset is the list of topics (with their numbers)
topicdescription <- read_excel("topic description LDA.xlsx", sheet = "pubsprobas") # this dataset is the probability of each topic in each publication

# Creating the list of topics

painindices <- c(231,158,83,227,28,310,64,44, 247) # the indices of topics that correspond to research specifically dedicated to pain
fertilindices <- c(167, 61, 285, 196, 60, 239, 115, 8, 91, 108, 308, 65, 248, 272, 220, 319, 30) # the indices of topics that correspond to research specifically dedicated to fertility
pvsfertindices <- list(painindices,fertilindices) # full list of indices

# Computing the share of each topic in the individual publications

painferttopics <- data.frame(matrix(ncol = 2))
colnames(painferttopics) <- c("Pain","Fertility")
painferttopics <- cbind(publication = topicdescription$Publication, painferttopics) # a dataframe is created with the identifiers of unique publications and a column for the share of abstracts belonging to pain, another for fertility

i <- 1

for (i in 1:length(pvsfertindices)) {
  painferttopics[,i+1] <- comput.rowsums(pvsfertindices,i)  # compute the shares to fertility/pain
}

# Extraction of paper basic information, computation of shares by year, decades / overall and for the top 10% most cited papers

painferttopics <- merge(painferttopics,publis, by.x="publication", by.y="Publication ID") # merge the full dataset with publications characteristics with the created one for fertility/pain
painferttopics <- gather(painferttopics, topic, share, c("Pain","Fertility")) # format the topic column for fertility/pain
pvfertstreamdata <- painferttopics %>% # summarise the information
  group_by(topic, PubDecade) %>%
  summarise(Value = mean(share, na.rm=TRUE))
pvfertstreamdata$Valuetop10 <- (painferttopics %>%
                                 group_by(topic, PubDecade) %>%
                                 filter(`Times cited` > quantile(`Times cited`, probs = 0.9)) %>%
                                 summarise(Valuetop10 = mean(share, na.rm=TRUE)))$Valuetop10


web.inter.chart.painvsfertil <- plot_ly(pvfertstreamdata, x = ~PubDecade, y = ~round(Value*100,digits=2), color=~topic, colors = c("#8856a7","#b3cde3"), text=~topic, type = "bar", hovertemplate ="%{y}% of the scientific output dedicated to %{text} in the %{x}s <extra></extra>") %>% layout(xaxis = list(title = 'Decade of Publication'), yaxis = list(title="Share of Publications' Abstracts"), barmode = 'stack') %>% config(displayModeBar = FALSE) # graph for the evolution over time of RD specifically dedicated to pain or fertility

htmlwidgets::saveWidget((frameableWidget(web.inter.chart.painvsfertil)),'web.inter.chart.painvsfertil.html', selfcontained = F, libdir = "lib") # export the graph