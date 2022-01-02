# This script assigns the regrouped topics (broad and detailed) to each publication. Then, it summarizes the evidence to produce graphs and the evolution of topics over time (bar charts and trend lines).
# This builds on the topics derived from the LDA

library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(widgetframe)
library(ggplot2)

# Import the datasets with topics data and publications' characteristics

classifiedpaperslist <- read_excel("topic description LDA.xlsx") # file with the list of topics derived from the LDA and the regrouped topics obtained by manual analysis (broad and detailed topics)
topicdescription <- read_excel("topic description LDA.xlsx", sheet = "pubsprobas") # dataset with the probabilities assigned to each publication for each topic identified in the LDA
endodata <- read_excel("endodata.xlsx", na = "NA") # import the full dataset with papers' publication decade and identifier
endodata <- endodata[c("Publication ID","PubDecade")] # extract only the relevant columns

# Definition of functions that will be useful to process the data (i.e., extract the list of topics for the new topics, and compute their updated probabilities)

extr.macrotopics <- function(x){ # this function extracts the broad topics
  classifiedpaperslist$`Topic number`[classifiedpaperslist$`Macro-category`== x]
}

extr.topics <- function(x){ # this function extracts the detailed topics
  classifiedpaperslist$`Topic number`[classifiedpaperslist$`Topic category`== x]
}

comput.rowsums <- function(x,y){ # this function computes the updated probabilities
  rowSums(topicdescription[,(pluck(x,y)+1)])
}

# Automatic extraction of labels for the topics

macrotopicslabels <- unique(classifiedpaperslist$`Macro-category`)
topicslabels <- unique(classifiedpaperslist$`Topic category`)

# Automatic creation of the lists of indices for the corresponding (macro)topics (i.e., the indices of the topics identified in the LDA)

macrotopicsindices <- map(macrotopicslabels, extr.macrotopics)
topicsindices <- map(topicslabels, extr.topics)

# Creation of the datasets and automatic attribution of (macro)topics to each publication

macrotopics <- data.frame(matrix(ncol = (length(macrotopicslabels))))
colnames(macrotopics) <- c(macrotopicslabels)
macrotopics <- cbind(publication = topicdescription$Publication, macrotopics)
i <- 1

for (i in 1:length(macrotopicsindices)) {
  macrotopics[,i+1] <- comput.rowsums(macrotopicsindices,i)
}


topics <- data.frame(matrix(ncol = (length(topicslabels))))
colnames(topics) <- c(topicslabels)
topics <- cbind(publication = topicdescription$Publication, topics)
i <- 1

for (i in 1:length(topicsindices)) {
  topics[,i+1] <- comput.rowsums(topicsindices,i)
}

# Extraction of paper basic information, computation of shares by year, decades / overall and for the top 10% most cited papers

# Broad topics

macrotopics <- merge(macrotopics,endodata, by.x="publication", by.y="Publication ID") # merge with the basic information on papers
macrotopics <- gather(macrotopics, topic, share, macrotopicslabels) # convert to adequate format for graphs
macrostreamdata <- macrotopics %>% # summarize the broad topics by decade
  group_by(topic, PubDecade) %>%
  summarise(Value = mean(share, na.rm=TRUE))
macrostreamdata$Valuetop10 <- (macrotopics %>% # same process, but for the top 10% most cited publications of each decade
  group_by(topic, PubDecade) %>%
  filter(`Times cited` > quantile(`Times cited`, probs = 0.9)) %>%
  summarise(Valuetop10 = mean(share, na.rm=TRUE)))$Valuetop10

# Detailed topics

topics <- merge(topics,endodata, by.x="publication", by.y="Publication ID") # merge with the basic information on papers
topics <- gather(topics, topic, share, topicslabels) # convert to adequate format for graphs
topicstreamdata <- topics %>% # summarize the broad topics by decade
  group_by(topic, PubDecade) %>%
  summarise(Value = mean(share, na.rm=TRUE))
topicstreamdata$Valuetop10 <- (topics %>% # same process, but for the top 10% most cited publications of each decade
                                 group_by(topic, PubDecade) %>%
                                 filter(`Times cited` > quantile(`Times cited`, probs = 0.9)) %>%
                                 summarise(Valuetop10 = mean(share, na.rm=TRUE)))$Valuetop10
								 
excludetopics <- c("Types of studies","NOT MEANINGFUL","Research method") # list of broad topics to be excluded from graphs, as they do not carry meaningful information
topicstreamdata$macrotopic <- substring(topicstreamdata$topic,1,2) # creation of a variable signalling the broad topic in the dataset with the detailed topics

# Creation of graphs with the evolution of broad topics over time 

web.macrograph <- plot_ly(macrostreamdata[macrostreamdata$topic != "_TO EXCLUDE",], x = ~PubDecade, y = ~round(Value*100,digits=2), color=~topic, colors = "Set2", text=~(substring(topic,4)), legendgroup=~(substring(topic,4)), type = "bar", hovertemplate ="%{y}% of the scientific output dedicated to %{text} in the %{x}s <extra></extra>") %>% layout(xaxis = list(title = 'Decade of Publication'), yaxis = list(title="Share of Abstracts' Content"), barmode = 'stack') %>% config(displayModeBar = FALSE) # graph for broad topics - all publications

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.macrograph)),'web.macrograph.html', selfcontained = F, libdir = "lib") # export the graph

web.macrographtop10 <- plot_ly(macrostreamdata[macrostreamdata$topic != "_TO EXCLUDE",], x = ~PubDecade, y = ~round(Valuetop10*100,digits=2), color=~topic, colors = "Set2", legendgroup=~(substring(topic,4)), text=~(substring(topic,4)), type = "bar", hovertemplate ="%{y}% of the most cited scientific output (top 10%) dedicated to %{text} in the %{x}s <extra></extra>") %>% layout(xaxis = list(title = 'Decade of Publication'), yaxis = list(title="Share of the Abstracts from the Most Cited Publications"), barmode = 'stack') %>% config(displayModeBar = FALSE) # graph for broad topics - top 10% publications

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.macrographtop10)),'web.macrographtop10.html', selfcontained = F, libdir = "lib") # export the graph


# Creation of graphs with the evolution of detailed topics over time 

web.detailedtopics <- topicstreamdata[ !grepl(paste(excludetopics, collapse="|"), topicstreamdata$topic),] %>% # graph for detailed topics - all publications
  plot_ly(
    type = 'bar', 
    x = ~PubDecade, 
    y = ~round(Value*100,digits=2),
    text = ~substring(topic,6), hovertemplate ="%{y}% of the scientific output dedicated to %{text} in the %{x}s <extra></extra>", color = ~topic, colors = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "#fee0d2", "#fc9272", "#de2d26", "#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "#feebe2", "#fbb4b9", "#f768a1", "#ae017e", "#e6f5d0", "#a1d76a", "#4d9221", "#e9a3c9", "#c51b7d", "#fde0ef", "#ffeda0", "#feb24c", "#f03b20", "#5ab4ac", "#d8b365", "#af8dc3", "#7fbf7b"), legendgroup = ~topic,
    transforms = list(
      list(
        type = 'filter',
        target = ~macrotopic,
        operation = '=',
        value = unique(topicstreamdata$macrotopic)[1]
      )
    ))%>%
  layout(barmode = "stack", xaxis = list(title = "Decade of Publication"), yaxis = list (title = "Share of Abstracts' Content"),
         updatemenus = list(
           list(
             type = 'dropdown',
             active = 0,
             buttons = list(
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[1]),
                    label = "1. Causes and mechanisms"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[2]),
                    label = "2. Anatomy and classification"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[3]),
                    label = "3. Symptoms and daily life"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[4]),
                    label = "4. Diagnosis and analyses"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[5]),
                    label = "5. Treatment"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[6]),
                    label = "6. Connections to other diseases"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[7]),
                    label = "7. Health and economic burden"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[8]),
                    label = "8. Miscellaneous")
             )
           )
         )
  ) %>% config(displayModeBar = FALSE)


htmlwidgets::saveWidget((frameableWidget(web.detailedtopics)),'web.detailedtopics.html', selfcontained = F, libdir = "lib") # export the graph


web.detailedtopicstop10 <- topicstreamdata[ !grepl(paste(excludetopics, collapse="|"), topicstreamdata$topic),] %>% # graph for detailed topics - top 10% publications
  plot_ly(
    type = 'bar', 
    x = ~PubDecade, 
    y = ~round(Valuetop10*100,digits=2),
    text = ~substring(topic,6), hovertemplate ="%{y}% of the most cited scientific output dedicated to %{text} in the %{x}s <extra></extra>", color = ~topic, colors = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "#fee0d2", "#fc9272", "#de2d26", "#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "#feebe2", "#fbb4b9", "#f768a1", "#ae017e", "#e6f5d0", "#a1d76a", "#4d9221", "#e9a3c9", "#c51b7d", "#fde0ef", "#ffeda0", "#feb24c", "#f03b20", "#5ab4ac", "#d8b365", "#af8dc3", "#7fbf7b"), legendgroup = ~topic,
    transforms = list(
      list(
        type = 'filter',
        target = ~macrotopic,
        operation = '=',
        value = unique(topicstreamdata$macrotopic)[1]
      )
    ))%>%
  layout(barmode = "stack", xaxis = list(title = "Decade of Publication"), yaxis = list (title = "Share of most cited Publications' Abstracts"),
         updatemenus = list(
           list(
             type = 'dropdown',
             active = 0,
             buttons = list(
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[1]),
                    label = "1. Causes and mechanisms"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[2]),
                    label = "2. Anatomy and classification"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[3]),
                    label = "3. Symptoms and daily life"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[4]),
                    label = "4. Diagnosis and analyses"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[5]),
                    label = "5. Treatment"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[6]),
                    label = "6. Connections to other diseases"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[7]),
                    label = "7. Health and economic burden"),
               list(method = "restyle",
                    args = list("transforms[0].value", unique(topicstreamdata$macrotopic)[8]),
                    label = "8. Miscellaneous")
             )
           )
         )
  ) %>% config(displayModeBar = FALSE)

htmlwidgets::saveWidget((frameableWidget(web.detailedtopicstop10)),'web.detailedtopicstop10.html', selfcontained = F, libdir = "lib") # export the graph

### Production of the trend lines 


# Generation of the graphs for broad topics

palbroad <- c("#66c2a5", "#fc8d62" , "#8da0cb", "#e78ac3" , "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3")

i <- 1

for (i in 2:length(unique(macrostreamdata$topic))) {
  
  ggplot(data = macrostreamdata[macrostreamdata$topic == unique(macrostreamdata$topic)[i],], aes(x=PubDecade, y=Value*100)) + geom_point(col=palbroad[i-1]) + geom_smooth(method=loess, col=palbroad[i-1]) + labs(title=paste0(substring(unique(macrostreamdata$topic)[i],4)), x="Publication Decade", y = "Share of Abstracts' Content (%)") + theme_bw() + scale_x_continuous(breaks=seq(1920,2020,10))
  
  ggsave(paste0("broadtopicstrends",i-1,".png"))
}
  
# Generation of the graphs for detailed topics

paldetailed <- c("#f3f38a", "#99cd7b", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "#dca388", "#fc9272", "#de2d26", "#67dbfb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "#ff985d", "#fbb4b9", "#f768a1", "#ae017e", "#adff34", "#a1d76a", "#4d9221", "#e9a3c9", "#c51b7d", "#fe2b98", "#ffeda0", "#feb24c", "#f03b20", "#5ab4ac", "#d8b365", "#af8dc3", "#7fbf7b")

i <- 1

for (i in 1:(length(unique(topicstreamdata$topic))-3)) {
  
  ggplot(data = topicstreamdata[topicstreamdata$topic == unique(topicstreamdata$topic)[i],], aes(x=PubDecade, y=Value*100)) + geom_point(col=paldetailed[i]) + geom_smooth(method=loess, col=paldetailed[i]) + labs(title=paste0(substring(unique(topicstreamdata$topic)[i],6)), x="Publication Decade", y = "Share of Abstracts' Content (%)") + theme_bw() + scale_x_continuous(breaks=seq(1920,2020,10))
  
  ggsave(paste0("detailedtopicstrends",i,".png"))
}
