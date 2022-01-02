# This script investigates the gender breakdown of endometriosis publications' contributors on two dimensions (evolution over time and linkages with the topics). It produces related graphs.

library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(stringr)
library(gridExtra)
library(htmlwidgets)
library(widgetframe)

# Investigation of the evolution of contribution by gender

genderevodata <- read_excel("~endodata.xlsx", na = "NA") # importation of the full database of endometriosis publications, with the gender of authors

genderevodata <- genderevodata %>% group_by(PubYear) %>% summarise(nbmale = sum(`Number Male`), nbfemale = sum(`Number  Female`), nbundet = sum(`Number Not Determined`)) # compile number of contributors by gender and by year
genderevodata$totalmalfemale <- genderevodata$nbmale + genderevodata$nbfemale # sum of contributors with an attributable gender (drop undetermined)
genderevodata$sharemale <- round(100*(genderevodata$nbmale/genderevodata$totalmalfemale), digits = 2) # share of male contributors
genderevodata$sharefemale <- round(100*(genderevodata$nbfemale/genderevodata$totalmalfemale),digits = 2) # share of female contributors

web.inter.chart.evogender <- plot_ly(genderevodata, x = ~PubYear, y = ~sharemale, type = 'bar', text = "male", name = 'Male Contribution', marker = list(color ="#e08214"), hovertemplate = "%{y}% of %{text} contribution in %{x} <extra></extra>") %>% add_trace(y = ~sharefemale, text = "female", name = 'Female Contribution', marker = list(color ="#8073ac")) %>% layout(yaxis = list(title = 'Share total contribution'), xaxis = list(title = "Publication Year"), barmode = 'stack', bargap = 0) %>% config(displayModeBar = FALSE) # graph of the evolution of contribution by gender over time

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.evogender)), selfcontained = F, libdir = "lib", 'web.inter.chart.evogender.html') # export the graph

# Investigation of the correlation between the gender composition of the team and the studied topics at the publication level

gendercorreltopics <- read_excel("~endodata.xlsx", na = "NA") # import the dataset to study the linkage between gender and topics
gendercorreltopics$sharemale <- round((gendercorreltopics$`Number Male`)/(gendercorreltopics$`Number  Female` + gendercorreltopics$`Number Male`), digits = 2)*100 # compute the share of male contribution at the publication level, i.e., the gender composition of the teams
gendercorreltopics[,120:161] <- round(gendercorreltopics[,120:161]*100, digits=2) # reformat properly the topic shares

i <- 1

for (i in 1:42) { # generation of a graph linking gender composition of the team and abstract's share for each topic
  
  trgtopic <- paste0("`",colnames(gendercorreltopics)[119+i],"`") # set target
  trgtitle <- paste0(gsub('^.|.$', '', trgtopic)) # format title of the graph
  
  assign(paste0("plotcorreltopic",i), ggplot(data = gendercorreltopics, aes_string(x="sharemale", y = trgtopic)) + geom_smooth() + ggtitle(trgtitle) + xlab("Share of Male Authors in Publications' Teams (%)") + ylab("Share of Output dedicated to topic (%)")) # define graph
  
  print(i)
}

# generate the grids

genderplotbroadtopics <- grid.arrange(plotcorreltopic1, plotcorreltopic2, plotcorreltopic3, plotcorreltopic4, plotcorreltopic5, plotcorreltopic6, plotcorreltopic7, plotcorreltopic8)
genderplottopics1 <- grid.arrange(plotcorreltopic12, plotcorreltopic13, plotcorreltopic14, plotcorreltopic15, plotcorreltopic16, plotcorreltopic17)
genderplottopics2 <- grid.arrange(plotcorreltopic18, plotcorreltopic19, plotcorreltopic20, nrow = 2)
genderplottopics3 <- grid.arrange(plotcorreltopic21, plotcorreltopic22, plotcorreltopic23, plotcorreltopic24, plotcorreltopic25)
genderplottopics4 <- grid.arrange(plotcorreltopic26, plotcorreltopic27, plotcorreltopic28, plotcorreltopic29)
genderplottopics5 <- grid.arrange(plotcorreltopic30, plotcorreltopic31, plotcorreltopic32, plotcorreltopic33, plotcorreltopic34, plotcorreltopic35)
genderplottopics6 <- grid.arrange(plotcorreltopic36, plotcorreltopic37, plotcorreltopic38, nrow = 2)
genderplottopics7 <- grid.arrange(plotcorreltopic39, plotcorreltopic40, ncol = 2)
genderplottopics8 <- grid.arrange(plotcorreltopic41, plotcorreltopic42, ncol = 2)

# save the consolidated plots

ggsave(filename = "genderplotbroadtopics.png", width = 12, height = 12, dpi = "retina", plot = genderplotbroadtopics)
ggsave(filename = "genderplottopics1.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics1)
ggsave(filename = "genderplottopics2.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics2)
ggsave(filename = "genderplottopics3.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics3)
ggsave(filename = "genderplottopics4.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics4)
ggsave(filename = "genderplottopics5.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics5)
ggsave(filename = "genderplottopics6.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics6)
ggsave(filename = "genderplottopics7.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics7)
ggsave(filename = "genderplottopics8.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics8)