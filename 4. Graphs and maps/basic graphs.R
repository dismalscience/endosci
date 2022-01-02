# Production of basic graphs regarding the evolution of publications

library(htmlwidgets)
library(plotly)
library(dplyr)
library(widgetframe)
library(readxl)

endodata <- read_excel("~endodata.xlsx", na = "NA") # Requires the import of the full database of publications (endodata)

# data for the evolution of the number of publications

datanbpublis <- endodata %>% group_by(PubYear) %>% summarise(Freq = n()) # calculate the yearly umber of publications 
datanbpublis$nbdimensions <- c(84334, 91018, 94205, 98598, 101386, 106011, 107081, 110543, 106029, 106279, 111489, 114418, 114375, 116433, 115170, 106652, 103363, 97578, 90211, 83758, 90720, 123782, 144270, 162922, 168519, 203645, 227435, 235039, 245116, 251186, 259747, 266462, 279806, 286904, 305691, 323313, 342857, 357018, 393109, 418862, 448456, 468940, 499936, 525382, 542516, 584251, 592475, 614861, 632325, 649384, 673407, 691479, 716042, 752527, 776277, 806226, 834626, 858702, 894301, 934047, 963500, 1004607, 1061522, 1124028, 1175464, 1225823, 1263860, 1289455, 1337201, 1376476, 1452258, 1494416, 1550999, 1594648, 1625117, 1756578, 1813948, 1879145, 2016228, 2219489, 2364473, 2570426, 2871434, 2881228, 3089135, 3222149, 3608826, 3711560, 3982594, 4181035, 4302623, 4525785, 4927341, 5273966, 5677812, 6191695) # data on the total number of publications included in the Dimensions Database on 18-04-2021
datanbpublis$shareendo <- datanbpublis$Freq/datanbpublis$nbdimensions # share of publications for endometriosis research
datanbpublis$indiceendo <- datanbpublis$Freq*(100/556) # indice for number of endo publications (100 = 2000)
datanbpublis$indiceallpubs <- datanbpublis$nbdimensions*(100/1756578) # indice for number of all publications (100 = 2000)

# data related to funding

unescord <- read_excel("funding endometriosis.xlsx", sheet = "RD expenditure") # UNESCO data on share of R&D in world GDP
eurogdp <- read_excel("GDP data.xls", sheet = "Formatted GDP") # data on world GDP in euros

endofunding <- datanbpublis[,c(1,4)] # create a dataset with only publication year and the corresponding share of scientific publications corresponding to endometriosis R&D
endofunding[72:94,3] <- unescord$Value # attribute the share of world GDP dedicated to R&D to the relevant years
endofunding[75:96,4] <- as.vector(as.numeric(eurogdp$GDPcurrenteuros)) # attribute the world GDP in current euros to the relevant years
endofunding[75:96,5] <- as.vector(as.numeric(eurogdp$GDPreal2020euros)) # attribute the world GDP in real 2020 euros to the relevant years

colnames(endofunding) <- c("Year","Shareendo","Percrd","GDP.currentEUR", "GDP.real2020EUR") # rename columns

endofunding$EndoSpendingEURGDP <- endofunding$GDP.currentEUR * (endofunding$Percrd/100) * (endofunding$Shareendo) *(1/3) * (1/1000000) # this estimate is based on the world GDP dedicated to RD, and the estimated share of RD that is linked to endometriosis in the original dataset derived from the Dimensions raw data. It is adjusted by a factor of 1/3 to account for the fact that the World estimate conflates public and private RD, and it is very likely that the endometriosis-research is predominantely public (1/3 is a reaonsable guess for most countries). Data is converted to millions
endofunding$EndoSpendingEURrealGDP <- endofunding$GDP.real2020EUR * (endofunding$Percrd/100) * (endofunding$Shareendo) *(1/3) * (1/1000000) # same thing with real 2020 euros

endofunding$EndoSpendingEURGDP <- round(endofunding$EndoSpendingEURGDP, digits = 0) # rounding values
endofunding$EndoSpendingEURrealGDP <- round(endofunding$EndoSpendingEURrealGDP, digits = 0) # rounding values


fundingcompare <- data.frame(type = c("Global Endometriosis R&D", "Kiribati GDP", "Nauru GDP","Cuba military", "Senegal military", "US NIH Prostate Cancer R&D", "US NIH Depression R&D"), amounts = c(150, 171, 108, 112, 336, 228, 92), category = c("Endometriosis R&D", "Economic", "Economic", "Military", "Military", "Medical R&D", "Medical R&D")) # Creation of the dataset for the contextualization of funding guesstimate. Data sources for GDP: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2018&start=2018 ; https://report.nih.gov/funding/categorical-spending#/ ; https://www.sipri.org/databases/milex 
fundingcompare <- fundingcompare[order(fundingcompare$amounts),] 
fundingcompare$type <- factor(fundingcompare$type, levels = c(as.character(fundingcompare$type))) # process to get the increasing order in the graph

# realisation of the graphs and exports

web.inter.chart.nb.endopublis <- plot_ly(datanbpublis[datanbpublis$PubYear != 2020,], x = ~PubYear, y = ~Freq, name = "Endometriosis-related publications", type = "bar", marker=list(color="#6a51a3"), hovertemplate ="%{y} documents published in %{x} <extra></extra>") %>% layout(xaxis = list(title = "Publication Year"), yaxis = list (title = "Number of yearly Publications"))  %>% config(displayModeBar = FALSE) # chart on the evolution of the number of publications over time

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.nb.endopublis)),'web.inter.chart.nb.endopublis.html', selfcontained = F, libdir = "lib")

web.inter.chart.indices.publis <- plot_ly(datanbpublis[datanbpublis$PubYear != 2020,], x = ~PubYear, y = ~indiceendo, name = "Endometriosis-related publications", type = "scatter", mode = "lines", line=list(color="#6a51a3", width = 4)) %>% add_trace(y = ~indiceallpubs, name = "All Scientific Publications", mode = "lines", line=list(color="#e34a33", width = 4)) %>% layout(xaxis = list(title = "Publication Year"), yaxis = list (title = "Number of yearly publications index (100 in 2000)")) %>% layout(legend = list(x = 0.1, y = 0.9)) %>% layout(hovermode = "x unified") %>% config(displayModeBar = FALSE) # chart on the growth of publications dedicated to endo as compared to all publications

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.indices.publis)),'web.inter.chart.indices.publis.html', selfcontained = F, libdir = "lib")

web.inter.chart.endofunding.EUR <- plot_ly(endofunding, x = ~Year, y = ~EndoSpendingEURGDP, name = "Current euros", type = "scatter", mode = "lines", line=list(color="#31a354", width = 4), text = "current euros", hovertemplate ="%{y} million of %{text} spent on endometriosis R&D in %{x} <extra></extra>")  %>% add_trace(y = ~EndoSpendingEURrealGDP, name = "Real 2020 euros", text="real 2020 euros", mode = "lines", line=list(color="#e34a33", width = 4))  %>% layout(xaxis = list(title = "Year"), yaxis = list (title = "Millions of Euros")) %>% config(displayModeBar = FALSE) %>% layout(yaxis = list(range = list(0,160))) %>% layout(hovermode = "x unified") # chart on the funding guesstimate

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.endofunding.EUR)),'web.inter.chart.endofunding.EUR.html', selfcontained = F, libdir = "lib")

web.inter.chart.compare.funding <- plot_ly(fundingcompare, x = ~type, y = ~amounts, color=~category, colors=c("#feb24c","#6a51a3","#2b8cbe","#e34a33"), type = 'bar', hovertemplate =" %{x}: %{y} million of real 2020 euros in 2018 <extra></extra>") %>% layout(xaxis = list(title = ""), yaxis = list(title = "Millions of real 2020 euros"))  %>% config(displayModeBar = FALSE) # chart on the contextualization of funding

htmlwidgets::saveWidget(partial_bundle(frameableWidget(web.inter.chart.compare.funding)),'web.inter.chart.compare.funding.html', selfcontained = F, libdir = "lib")

