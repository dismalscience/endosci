# This script focuses on the analysis of scientific disciplines mobilized by the different publications. It creates the related graphs. 

library(plotly)
library(dplyr)
library(readxl)
library(htmlwidgets)
library(widgetframe)

# import and process data

endodata <- read_excel("endodata.xlsx") # import the full dataset of publications with information on the disciplines

colnames(endodata)[102:118] <- c("math", "phys", "chem", "envi", "agri", "ict", "biomed", "educ", "econ", "mgt", "human", "psy", "law", "arts", "lang", "hist", "philo") # rename the columns with disciplines for simplicity

myvars <- colnames(endodata)[41:57] # list of names for disciplines

# transform the data to analyze it

scicount.all <- endodata %>% group_by(PubDecade) %>% 
  summarise_each_(funs(sum(.=="X",na.rm=TRUE)), myvars) # compute the number of publications involving the different disciplines by publication decade
scicount.top <- endodata %>% group_by(PubDecade) %>%
    filter(`Times cited` > quantile(`Times cited`, probs = 0.9)) %>% 
    summarise_each_(funs(sum(.=="X",na.rm=TRUE)), myvars) # compute the number of publications involving the different disciplines by publication decade, for the top 10% most cited publications for each decade

scicount.all$total <- (endodata %>% count(PubDecade))$n # compute the total number of publications per decade
scicount.top$total <- (endodata  %>% filter(`Times cited` > quantile(`Times cited`, probs = 0.9))%>% count(PubDecade))$n # compute the total number of publications per decade (top 10% most cited within each decade only)

scicount.all <- scicount.all %>% rowwise() %>% mutate(avgdiversity = (sum(c_across(math:philo))/total)) # compute the average number of disciplines per publication
scicount.top <- scicount.top %>% rowwise() %>% mutate(avgdiversity = (sum(c_across(math:philo))/total)) # compute the average number of disciplines per publication (top 10% most cited)
diversity <- data.frame(PubDecade = scicount.all$PubDecade, all = scicount.all$avgdiversity, top10 = scicount.top$avgdiversity) # unified dataframe for average diversity (disciplines per publication)

# Generate the graphs

web.inter.chart.alldisciplines <- plot_ly(scicount.all, x = ~PubDecade, y = ~(math/total)*100, name = 'Mathematical Sciences', type = 'bar', marker = list(color = '#fb9a99'), hovertemplate = paste("Share of publications in the %{x}s: %{y:.2f}%")) %>% add_trace(y = ~(phys/total)*100, name = 'Physical Sciences', marker = list(color = '#e31a1c')) %>% add_trace(y = ~(chem/total)*100, name = 'Chemical Sciences', marker = list(color = '#a6cee3')) %>% add_trace(y = ~(envi/total)*100, name = 'Environmental Sciences', marker = list(color = '#33a02c'))  %>% add_trace(y = ~(agri/total)*100, name = 'Agricultural and Veterinary Sciences', marker = list(color = '#b2df8a')) %>% add_trace(y = ~(ict/total)*100, name = 'ICT, Technology and Engineering', marker = list(color = '#b15928')) %>% add_trace(y = ~(biomed/total)*100, name = 'Biological and Medical-Health Sciences', marker = list(color = '#1f78b4')) %>% add_trace(y = ~(educ/total)*100, name = 'Education', marker = list(color = '#ffff99')) %>% add_trace(y = ~(econ/total)*100, name = 'Economics', marker = list(color = '#ff7f00')) %>% add_trace(y = ~(mgt/total)*100, name = 'Commerce, Management, Tourism and Services', marker = list(color = '#fdbf6f')) %>% add_trace(y = ~(human/total)*100, name = 'Studies in Human Society', marker = list(color = '#cab2d6')) %>% add_trace(y = ~(psy/total)*100, name = 'Psychology and Cognitive Sciences', marker = list(color = '#6a3d9a')) %>% add_trace(y = ~(law/total)*100, name = 'Law and Legal Studies', marker = list(color = '#969696')) %>% add_trace(y = ~(arts/total)*100, name = 'Studies in Creative Arts and Writing', marker = list(color = '#ce1256')) %>% add_trace(y = ~(lang/total)*100, name = 'Language, Communication and Culture', marker = list(color = '#67001f')) %>% add_trace(y = ~(hist/total)*100, name = 'History and Archaeology', marker = list(color = '#bcbddc')) %>% add_trace(y = ~(philo/total)*100, name = 'Philosophy and Religious Studies', marker = list(color ='#fcbba1')) %>% config(displayModeBar = FALSE) %>% layout(xaxis = list(title = "Decade of Publication"), yaxis = list (title = "Share of Publications")) %>% layout(barmode = "stack") # create the graph of the share of publications involving the different disciplines by decade

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.alldisciplines)),'web.inter.chart.alldisciplines.html', selfcontained = F, libdir = "lib") # export the graph

  
web.inter.chart.top <- plot_ly(scicount.top, x = ~PubDecade, y = ~(math/total)*100, name = 'Mathematical Sciences', type = 'bar', marker = list(color = '#fb9a99'), hovertemplate = paste("Share of publications in the %{x}s: %{y:.2f}%")) %>% add_trace(y = ~(phys/total)*100, name = 'Physical Sciences', marker = list(color = '#e31a1c')) %>% add_trace(y = ~(chem/total)*100, name = 'Chemical Sciences', marker = list(color = '#a6cee3')) %>% add_trace(y = ~(envi/total)*100, name = 'Environmental Sciences', marker = list(color = '#33a02c'))  %>% add_trace(y = ~(agri/total)*100, name = 'Agricultural and Veterinary Sciences', marker = list(color = '#b2df8a')) %>% add_trace(y = ~(ict/total)*100, name = 'ICT, Technology and Engineering', marker = list(color = '#b15928')) %>% add_trace(y = ~(biomed/total)*100, name = 'Biological and Medical-Health Sciences', marker = list(color = '#1f78b4')) %>% add_trace(y = ~(educ/total)*100, name = 'Education', marker = list(color = '#ffff99')) %>% add_trace(y = ~(econ/total)*100, name = 'Economics', marker = list(color = '#ff7f00')) %>% add_trace(y = ~(mgt/total)*100, name = 'Commerce, Management, Tourism and Services', marker = list(color = '#fdbf6f')) %>% add_trace(y = ~(human/total)*100, name = 'Studies in Human Society', marker = list(color = '#cab2d6')) %>% add_trace(y = ~(psy/total)*100, name = 'Psychology and Cognitive Sciences', marker = list(color = '#6a3d9a')) %>% add_trace(y = ~(law/total)*100, name = 'Law and Legal Studies', marker = list(color = '#969696')) %>% add_trace(y = ~(arts/total)*100, name = 'Studies in Creative Arts and Writing', marker = list(color = '#ce1256')) %>% add_trace(y = ~(lang/total)*100, name = 'Language, Communication and Culture', marker = list(color = '#67001f')) %>% add_trace(y = ~(hist/total)*100, name = 'History and Archaeology', marker = list(color = '#bcbddc')) %>% add_trace(y = ~(philo/total)*100, name = 'Philosophy and Religious Studies', marker = list(color ='#fcbba1')) %>% config(displayModeBar = FALSE) %>% layout(xaxis = list(title = "Decade of Publication"), yaxis = list (title = "Share of most cited Publications (top 10%)")) %>% layout(barmode = "stack") # create the graph of the share of publications (top 10% most cited) involving the different disciplines by decade

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.top)),'web.inter.chart.top.html', selfcontained = F, libdir = "lib") # export the graph

 
web.inter.chart.diversity <- plot_ly(diversity, x = ~PubDecade, y = ~round(all, digits = 2), name = 'All Publications', type = 'scatter', mode = 'lines', line=list(color="#66bd63", width = 4), hovertemplate = paste("%{y} Scientific Disciplines per Publication in the %{x}s")) %>% add_trace(y = ~round(top10,digits=2), name = 'Most Cited Publications (Top 10%)', mode = 'lines', line=list(color="#f46d43", width = 4))  %>% layout(xaxis = list(title = "Decade of Publication"), yaxis = list (title = "Number of disciplines per publication")) %>% config(displayModeBar = FALSE) %>% layout(legend = list(x = 0.1, y = 0.9)) %>% layout(hovermode = "x unified") # create the line graph with the evolution of average disciplinary diversity by decade (all publications and top 10%)

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.diversity)),'web.inter.chart.diversity.html', selfcontained = F, libdir = "lib") # export the graph