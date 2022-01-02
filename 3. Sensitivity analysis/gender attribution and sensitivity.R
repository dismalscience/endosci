# This script conducts sensitivity analyses for graphs and analyses involving gender. To proceed, it tests whether results change when considering a tougher threshold to assign a male or female gender to an author (90% probability instead of 75%). Overall, results are very robust. Setting a higher threshold tends to reduce the share of females.

# loading the relevant libraries

library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# import the source dataset with author-publication pairs

authorspublispairs <- read_excel("~authorsindividualcountry.xlsx") # this is a processed dataset where each line is an author involved in a single publication (i.e., the same author can be found in multiple lines if s/he is involved in more than one publication)
authorspublispairs$MascProba <- as.numeric(authorspublispairs$MascProba)
endodatabase <- read_excel("~endodata.xlsx", na = "NA") # this is the complete database of endometriosis publications

# Attribute gender with different probabilities (sensitivity analysis) - pairs of authors

authorspublispairs$Gender <- NULL # remove existing data from the standard database
authorspublispairs$Gender <- NA # this this the standard threshold at 75%
authorspublispairs$Gender90 <- NA # this is the enhanced threshold at 90%
authorspublispairs$Gender[authorspublispairs$MascProba > 74] <- "Male"
authorspublispairs$Gender[authorspublispairs$MascProba < 26] <- "Female"
authorspublispairs$Gender[authorspublispairs$MascProba > 26 & authorspublispairs$MascProba < 75] <- "Not determined"

authorspublispairs$Gender90[authorspublispairs$MascProba > 89] <- "Male"
authorspublispairs$Gender90[authorspublispairs$MascProba < 11] <- "Female"
authorspublispairs$Gender90[authorspublispairs$MascProba > 10 & authorspublispairs$MascProba < 90] <- "Not determined"

# Attribute gender with different probabilities (sensitivity analysis) - generic publications dataset (variables G1 to G10 contain the probability of being a male author)

endodatabase$G1[endodatabase$G1 == "Gender not determined"] <- NA
endodatabase$G2[endodatabase$G2 == "Gender not determined"] <- NA
endodatabase$G3[endodatabase$G3 == "Gender not determined"] <- NA
endodatabase$G4[endodatabase$G4 == "Gender not determined"] <- NA
endodatabase$G5[endodatabase$G5 == "Gender not determined"] <- NA
endodatabase$G6[endodatabase$G6 == "Gender not determined"] <- NA
endodatabase$G7[endodatabase$G7 == "Gender not determined"] <- NA
endodatabase$G8[endodatabase$G8 == "Gender not determined"] <- NA
endodatabase$G9[endodatabase$G9 == "Gender not determined"] <- NA
endodatabase$G10[endodatabase$G10 == "Gender not determined"] <- NA

endodatabase$G1 <- as.numeric(endodatabase$G1)
endodatabase$G2 <- as.numeric(endodatabase$G2)
endodatabase$G3 <- as.numeric(endodatabase$G3)
endodatabase$G4 <- as.numeric(endodatabase$G4)
endodatabase$G5 <- as.numeric(endodatabase$G5)
endodatabase$G6 <- as.numeric(endodatabase$G6)
endodatabase$G7 <- as.numeric(endodatabase$G7)
endodatabase$G8 <- as.numeric(endodatabase$G8)
endodatabase$G9 <- as.numeric(endodatabase$G9)
endodatabase$G10 <- as.numeric(endodatabase$G10)

i <- 1 # initialise the process

for (i in 1:nrow(endodatabase)) { # process each publication
  
  endodatabase$NbMale[i] <- sum(endodatabase[i,45:54] > 74, na.rm = T)
  endodatabase$NbFemale[i] <- sum(endodatabase[i,45:54] < 26, na.rm = T)

  endodatabase$NbMale90[i] <- sum(endodatabase[i,45:54] > 89, na.rm = T)
  endodatabase$NbFemale90[i] <- sum(endodatabase[i,45:54] < 11, na.rm = T)

  print(i/nrow(endodatabase))
}

# overall good correlation; higher threshold > penalize women 

# Sensitivity analysis for the evolution of gender breakdown over time

evogendercheck <- endodatabase %>% group_by(PubYear) %>% summarise(sharemale = round(100 * sum(NbMale, na.rm = T)/(sum(NbMale, na.rm = T)+sum(NbFemale, na.rm = T)), digits = 2), sharefemale = round(100 * sum(NbFemale, na.rm = T)/(sum(NbMale, na.rm = T)+sum(NbFemale, na.rm = T)), digits = 2), sharemale90 = round(100 * sum(NbMale90, na.rm = T)/(sum(NbMale90, na.rm = T)+sum(NbFemale90, na.rm = T)), digits = 2), sharefemale90 = round(100 * sum(NbFemale90, na.rm = T)/(sum(NbMale90, na.rm = T)+sum(NbFemale90, na.rm = T)), digits = 2)) # compile number by year

# very similar results, slight male overrepresentation in the sensitivity analysis at 90%

# Sensitivity analysis for the gender breakdown by country

list.countries <- unique(as.vector(as.matrix(authorspublispairs[,19:25])))
genderattrib <- data.frame(Country = list.countries)

datatoextractinst <- data.frame() # initialise extract of database to retrieve information


i <- 1 # initialize the process

for (i in 1:nrow(genderattrib)) { # perform the operation for each country: the full database is filtered for the corresponding country

  datatoextractinst <- authorspublispairs %>% filter_at(c("COUNT1", "COUNT2", "COUNT3", "COUNT4", "COUNT5", "COUNT6", "COUNT7"),any_vars(.%in% genderattrib$Country[i])) # retrieve the data with the authors solely linked to the studied country
  
  # retrieve the number of author-publication pairs by gender for the standard probability and sensitivity analysis
  
  genderattrib$Male[i] <- sum(datatoextractinst$Gender == "Male", na.rm = TRUE)
  genderattrib$Female[i] <- sum(datatoextractinst$Gender == "Female", na.rm = TRUE)
  genderattrib$NotDetermined[i] <- sum(datatoextractinst$Gender == "Not determined", na.rm = TRUE)
  
  genderattrib$Male90[i] <- sum(datatoextractinst$Gender90 == "Male", na.rm = TRUE)
  genderattrib$Female90[i] <- sum(datatoextractinst$Gender90 == "Female", na.rm = TRUE)
  genderattrib$NotDetermined90[i] <- sum(datatoextractinst$Gender90 == "Not determined", na.rm = TRUE)

  # perform the same analysis but restricting the sample to publications from the 2010s
  
  datatoextractinst <- datatoextractinst %>% filter(PubDecade == 2010)
  
  genderattrib$Male2010s[i] <- sum(datatoextractinst$Gender == "Male", na.rm = TRUE)
  genderattrib$Female2010s[i] <- sum(datatoextractinst$Gender == "Female", na.rm = TRUE)
  genderattrib$NotDetermined2010s[i] <- sum(datatoextractinst$Gender == "Not determined", na.rm = TRUE)
  
  genderattrib$Male902010s[i] <- sum(datatoextractinst$Gender90 == "Male", na.rm = TRUE)
  genderattrib$Female902010s[i] <- sum(datatoextractinst$Gender90 == "Female", na.rm = TRUE)
  genderattrib$NotDetermined902010s[i] <- sum(datatoextractinst$Gender90 == "Not determined", na.rm = TRUE)
}

# compute the gender ratios

genderattrib$MaleShare <- round(100 * (genderattrib$Male / (genderattrib$Male + genderattrib$Female)), digits = 2)
genderattrib$FemaleShare <- round(100 * (genderattrib$Female / (genderattrib$Male + genderattrib$Female)), digits = 2)

genderattrib$MaleShare90 <- round(100 * (genderattrib$Male90 / (genderattrib$Male90 + genderattrib$Female90)), digits = 2)
genderattrib$FemaleShare90 <- round(100 * (genderattrib$Female90 / (genderattrib$Male90 + genderattrib$Female90)), digits = 2)

genderattrib$MaleShare2010s <- round(100 * (genderattrib$Male2010s / (genderattrib$Male2010s + genderattrib$Female2010s)), digits = 2)
genderattrib$FemaleShare2010s <- round(100 * (genderattrib$Female2010s / (genderattrib$Male2010s + genderattrib$Female2010s)), digits = 2)

genderattrib$MaleShare902010s <- round(100 * (genderattrib$Male902010s / (genderattrib$Male902010s + genderattrib$Female902010s)), digits = 2)
genderattrib$FemaleShare902010s <- round(100 * (genderattrib$Female902010s / (genderattrib$Male902010s + genderattrib$Female902010s)), digits = 2)

write_xlsx(genderattrib, "genderattrib.xlsx") # export the file

# results very similar - likely that the 90% threshold tends to favour males slighlty but difference very small


# Sensitivity analyses for the gender breakdown by type of instittuions

individual_inst_data <- read_excel("~final affiliations list.xlsm", sheet = "list codes and caracs", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "text", "text"), na = "NA") # import the full list of institutions, each of them with a unique identifier

genderbytypeinst <- data.frame("Type" = unique(individual_inst_data$`Organisation type`)) # initialise table for results
genderbytypeinst$NbPublis <- NA
genderbytypeinst$MaleContrib <- NA
genderbytypeinst$FemaleContrib <- NA
genderbytypeinst$UndeterminedContrib <- NA
genderbytypeinst$MaleShareContrib <- NA
genderbytypeinst$FemaleShareContrib <- NA

genderbytypeinst$MaleContrib90 <- NA
genderbytypeinst$FemaleContrib90 <- NA
genderbytypeinst$UndeterminedContrib90 <- NA
genderbytypeinst$MaleShareContrib90 <- NA
genderbytypeinst$FemaleShareContrib90 <- NA


datatoextractinst <- data.frame() # initialise extract of database to retrieve information

# Processing the gender contribution for each type of institution

i <- 1 # initialize the process

for (i in 1:nrow(genderbytypeinst)) { # perform the operation for each type of institution
  
  listcodesinst <- (individual_inst_data  %>% filter(`Organisation type` == genderbytypeinst$Type[i]))$Code # extract the list of codes for the type of institution
  
  datatoextractinst <- authorspublispairs %>% filter_at(c("V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13"),any_vars(.%in% listcodesinst)) # retrieve the data with the authors solely linked to the type of institution under scrutiny
  
  genderbytypeinst$NbPublis[i] <- length(unique(datatoextractinst$Publication))
  genderbytypeinst$MaleContrib[i] <- sum(datatoextractinst$Gender == "Male", na.rm = T)
  genderbytypeinst$FemaleContrib[i] <- sum(datatoextractinst$Gender == "Female", na.rm = T)
  genderbytypeinst$UndeterminedContrib[i] <- sum(datatoextractinst$Gender == "Not determined", na.rm = T)
  genderbytypeinst$MaleShareContrib[i] <- round(100*(genderbytypeinst$MaleContrib[i] / (genderbytypeinst$MaleContrib[i] + genderbytypeinst$FemaleContrib[i])), digits = 2)
  genderbytypeinst$FemaleShareContrib[i] <-  round(100*(genderbytypeinst$FemaleContrib[i] / (genderbytypeinst$MaleContrib[i] + genderbytypeinst$FemaleContrib[i])), digits = 2)
  genderbytypeinst$MaleContrib90[i] <- sum(datatoextractinst$Gender90 == "Male", na.rm = T)
  genderbytypeinst$FemaleContrib90[i] <- sum(datatoextractinst$Gender90 == "Female", na.rm = T)
  genderbytypeinst$UndeterminedContrib90[i] <- sum(datatoextractinst$Gender90 == "Not determined", na.rm = T)
  genderbytypeinst$MaleShareContrib90[i] <- round(100*(genderbytypeinst$MaleContrib90[i] / (genderbytypeinst$MaleContrib90[i] + genderbytypeinst$FemaleContrib90[i])), digits = 2)
  genderbytypeinst$FemaleShareContrib90[i] <-  round(100*(genderbytypeinst$FemaleContrib90[i] / (genderbytypeinst$MaleContrib90[i] + genderbytypeinst$FemaleContrib90[i])), digits = 2)
  
  print((i/nrow(genderbytypeinst))*100) # show progress percentage
}

genderbytypeinst <- genderbytypeinst[!(genderbytypeinst$Type=="Not specified (city only)" | genderbytypeinst$Type == "Not specified (country only)" | genderbytypeinst$Type == "Not applicable" | genderbytypeinst$Type == "Not specified (region only)" | genderbytypeinst$Type == "Archive" | genderbytypeinst$Type == "Other"),] # remove types of institutions that cannot be interpreted easily

genderbytypeinst$Type <- as.character(genderbytypeinst$Type)

genderbytypeinst[nrow(genderbytypeinst)+1,] <- c("All types", length(unique(authorspublispairs$Publication)), sum(authorspublispairs$Gender == "Male", na.rm = T), sum(authorspublispairs$Gender == "Female", na.rm = T), sum(authorspublispairs$Gender == "Not determined", na.rm = T), round(100*(sum(authorspublispairs$Gender == "Male", na.rm = T) / (sum(authorspublispairs$Gender == "Male", na.rm = T) + sum(authorspublispairs$Gender == "Female", na.rm = T))), digits = 2), round(100*(sum(authorspublispairs$Gender == "Female", na.rm = T) / (sum(authorspublispairs$Gender == "Male", na.rm = T) + sum(authorspublispairs$Gender == "Female", na.rm = T))), digits = 2), sum(authorspublispairs$Gender90 == "Male", na.rm = T), sum(authorspublispairs$Gender90 == "Female", na.rm = T), sum(authorspublispairs$Gender90 == "Not determined", na.rm = T), round(100*(sum(authorspublispairs$Gender90 == "Male", na.rm = T) / (sum(authorspublispairs$Gender90 == "Male", na.rm = T) + sum(authorspublispairs$Gender90 == "Female", na.rm = T))), digits = 2), round(100*(sum(authorspublispairs$Gender90 == "Female", na.rm = T) / (sum(authorspublispairs$Gender90 == "Male", na.rm = T) + sum(authorspublispairs$Gender90 == "Female", na.rm = T))), digits = 2)) # add comparison data for all publications

# Results are confirmed


# Sensitivity analyses for the linkage between gender composition of teams and covered topics

endodatabase[,120:161] <- round(endodatabase[,120:161]*100, digits=2) # reformat properly
endodatabase$sharemale <- round((endodatabase$NbMale)/(endodatabase$NbMale + endodatabase$NbFemale), digits = 2)*100 # compute the share of male contribution by publication
endodatabase$sharemale90 <- round((endodatabase$NbMale90)/(endodatabase$NbMale90 + endodatabase$NbFemale90), digits = 2)*100 # compute the share of male contribution by publication


i <- 1

for (i in 1:42) { # generation of a graph for each topic with each threshold
  
  trgtopic <- paste0("`",colnames(endodatabase)[119+i],"`") # set target
  trgtitle <- paste0(gsub('^.|.$', '', trgtopic)) # format title
  
  assign(paste0("plotcorreltopic90",i), ggplot(data = endodatabase, aes_string(x="sharemale90", y = trgtopic)) + geom_smooth(color = "red") + ggtitle(trgtitle) + xlab("Share of Male Contribution in Publications (%)") + ylab("Share of Output dedicated to topic (%)")) # define graph
  
  assign(paste0("plotcorreltopic",i), ggplot(data = endodatabase, aes_string(x="sharemale", y = trgtopic)) + geom_smooth() + ggtitle(trgtitle) + xlab("Share of Male Contribution in Publications (%)") + ylab("Share of Output dedicated to topic (%)")) # define graph
  
  
  print(i)
}


# generate the grids with the individual graphs

genderplotbroadtopics <- grid.arrange(plotcorreltopic1, plotcorreltopic2, plotcorreltopic3, plotcorreltopic4, plotcorreltopic5, plotcorreltopic6, plotcorreltopic7, plotcorreltopic8)
genderplottopics1 <- grid.arrange(plotcorreltopic12, plotcorreltopic13, plotcorreltopic14, plotcorreltopic15, plotcorreltopic16, plotcorreltopic17)
genderplottopics2 <- grid.arrange(plotcorreltopic18, plotcorreltopic19, plotcorreltopic20, nrow = 2)
genderplottopics3 <- grid.arrange(plotcorreltopic21, plotcorreltopic22, plotcorreltopic23, plotcorreltopic24, plotcorreltopic25)
genderplottopics4 <- grid.arrange(plotcorreltopic26, plotcorreltopic27, plotcorreltopic28, plotcorreltopic29)
genderplottopics5 <- grid.arrange(plotcorreltopic30, plotcorreltopic31, plotcorreltopic32, plotcorreltopic33, plotcorreltopic34, plotcorreltopic35)
genderplottopics6 <- grid.arrange(plotcorreltopic36, plotcorreltopic37, plotcorreltopic38, nrow = 2)
genderplottopics7 <- grid.arrange(plotcorreltopic39, plotcorreltopic40, ncol = 2)
genderplottopics8 <- grid.arrange(plotcorreltopic41, plotcorreltopic42, ncol = 2)

genderplotbroadtopics90 <- grid.arrange(plotcorreltopic901, plotcorreltopic902, plotcorreltopic903, plotcorreltopic904, plotcorreltopic905, plotcorreltopic906, plotcorreltopic907, plotcorreltopic908)
genderplottopics901 <- grid.arrange(plotcorreltopic9012, plotcorreltopic9013, plotcorreltopic9014, plotcorreltopic9015, plotcorreltopic9016, plotcorreltopic9017)
genderplottopics902 <- grid.arrange(plotcorreltopic9018, plotcorreltopic9019, plotcorreltopic9020, nrow = 2)
genderplottopics903 <- grid.arrange(plotcorreltopic9021, plotcorreltopic9022, plotcorreltopic9023, plotcorreltopic9024, plotcorreltopic9025)
genderplottopics904 <- grid.arrange(plotcorreltopic9026, plotcorreltopic9027, plotcorreltopic9028, plotcorreltopic9029)
genderplottopics905 <- grid.arrange(plotcorreltopic9030, plotcorreltopic9031, plotcorreltopic9032, plotcorreltopic9033, plotcorreltopic9034, plotcorreltopic9035)
genderplottopics906 <- grid.arrange(plotcorreltopic9036, plotcorreltopic9037, plotcorreltopic9038, nrow = 2)
genderplottopics907 <- grid.arrange(plotcorreltopic9039, plotcorreltopic9040, ncol = 2)
genderplottopics908 <- grid.arrange(plotcorreltopic9041, plotcorreltopic9042, ncol = 2)

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

ggsave(filename = "genderplotbroadtopics90.png", width = 12, height = 12, dpi = "retina", plot = genderplotbroadtopics90)
ggsave(filename = "genderplottopics901.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics901)
ggsave(filename = "genderplottopics902.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics902)
ggsave(filename = "genderplottopics903.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics903)
ggsave(filename = "genderplottopics904.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics904)
ggsave(filename = "genderplottopics905.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics905)
ggsave(filename = "genderplottopics906.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics906)
ggsave(filename = "genderplottopics907.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics907)
ggsave(filename = "genderplottopics908.png", width = 12, height = 12, dpi = "retina", plot = genderplottopics908)
