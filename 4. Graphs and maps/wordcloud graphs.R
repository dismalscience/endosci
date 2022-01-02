# This script generates and exports wordclouds for the different detailed topics. 
# It requires the data processed to generate the graphs on the evolution of topics (i.e., topic graphs script)

library(webshot)
library(dplyr)
library(wordcloud2)
library(htmlwidgets)

# Remove the irrelevant topics from the used lists obtained from the previous script (topic graphs.R)

cloudtopiclabels <- topicslabels[4:34]
cloudtopicindices <- topicsindices[4:34]


# Loop to derive wordclouds for the different detailed topics

i <- 1 

for (i in 1:length(cloudtopiclabels)) { # Process each topic with the same procedure
  
  j <- 1
  
  wordcloud.data <- data.frame("words" = NA, "probabilities" = NA) # Initialise an empty dataframe to collect words and weights for each topic
  
  for (j in 1:length(cloudtopicindices[[i]])) { # Process the different components of the topic (i.e., each topic is composed of base topics from the LDA)
    
    topicToViz <- cloudtopicindices[[i]][j]
    top40terms <- sort(tmResult325$terms[topicToViz,], decreasing=TRUE)[1:40] # extract the top 40 words of the topics
    words <- names(top40terms)
    probabilities <- sort(tmResult325$terms[topicToViz,], decreasing=TRUE)[1:40]
    
    wordcloud.topic <- data.frame(words, probabilities) # Dataframe with the data on the specific components of the topic
    wordcloud.data <- rbind(wordcloud.data,wordcloud.topic) # Append in a unique dataframe
    
  } 
  
  sumwordcloud <- wordcloud.data %>% group_by(words) %>% summarize(sum(probabilities)) # Sum of the potentially common words between the components of the topic (base LDA topics)
  sumwordcloud <- na.omit(sumwordcloud) # remove NAs
  sumwordcloud <-  sumwordcloud[!(sumwordcloud$words %in% c("endometriosi", "indic", "determin")),] # remove meaningless words
  sumwordcloud <- sumwordcloud[order(-sumwordcloud$`sum(probabilities)`),] # order words by frequency
  if(nrow(sumwordcloud) > 80) { 
  sumwordcloud <- sumwordcloud[1:80,] # select only the top 80 words in total if there are more
  }
  mutate(sumwordcloud, color = cut(`sum(probabilities)`, breaks = c(0, quantile(sumwordcloud$`sum(probabilities)`, c(.6,0.8,0.95,.99))[1], quantile(sumwordcloud$`sum(probabilities)`, c(.6,0.8,0.95,.99))[2], quantile(sumwordcloud$`sum(probabilities)`, c(.6,0.8,0.95,.99))[3], quantile(sumwordcloud$`sum(probabilities)`, c(.6,0.8,0.95,.99))[4], Inf),
                           labels = c("#abdda4", "#2b83ba", "#33a02c",
                                      "#fdae61", "#d7191c"),
                           include.lowest = TRUE)) -> temp # generate the color palette with adequate breaks
  
  my_graph <- wordcloud2(sumwordcloud, color = temp$color, backgroundColor = "white", fontFamily = 'Helvetica', shape = "circle") # generate wordcloud
  saveWidget(my_graph,paste0("wordcloud",i,".html"),selfcontained = F) # export to html
  webshot(paste0("wordcloud",i,".html"),paste0("wordcloud",i,".png"), delay =5, vwidth = 800, vheight=800) # export to png
  
  print(i) # show progress
  
}
