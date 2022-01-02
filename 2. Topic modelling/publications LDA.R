# Script for the processing of abstracts data
# LDA approach for the identification of topics within the abstracts

library(readxl) # importing the base data with full abstracts in English
publis <- read_excel("~endodata.xlsx", 
                     na = "NA")

# Initialisation of required packages
library(tm)
library(qdap)
library(topicmodels)
library(reshape2)
library(pals)
library(ldatuning)
library(tidyr)

# prepare the data to create the corpus
abstracts <- data.frame("doc_id" = publis$`Publication ID`, "text" = publis$AbstractEN, "title" = publis$TitleEN)
abstracts <- abstracts[complete.cases(abstracts),] 
abstracts$text <- paste(abstracts$text, abstracts$title)
abstracts$title <- NULL
corpus <- Corpus(DataframeSource(abstracts)) # creation of the corpus

# download / create stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
extra_stopwords <- c("p", "abstract", "result", "results", "objective", "objectives", "na", "&na", "background", "aim", "study", "introduction", "conclusion", "conclusions", "findings")

# Preprocessing corpus
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removeWords, extra_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = FALSE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# create the DTM
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
abstracts <- abstracts[sel_idx, ]

# compute the optimal number of topics

result_topics <- FindTopicsNumber(
  DTM,
  topics = c(seq(from = 10, to = 40, by = 10), seq(from = 45, to = 75, by = 5), seq(from = 100, to = 250, by = 25), seq(from = 275, to = 325, by = 10), seq(from = 400, to = 500, by = 100)),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result_topics)

# Runs of the topic modelling for the relevant numbers of topics / results show that the 325 topics are more relevant
set.seed(77)
topicModel30 <- LDA(DTM, 30, method="Gibbs", control = list(iter = 500, verbose = 25))
topicModel325 <- LDA(DTM, 325, method="Gibbs", control = list(iter = 500, verbose = 25))

# Analyses of topics for the two models

# Model with 30 topics

tmResult30 <- posterior(topicModel30)
beta30 <- tmResult30$terms   # get beta from results
theta30 <- tmResult30$topics # data attributing topic probability for each publication
exampleTermData30 <- terms(topicModel30, 20)

# Model with 325 topics (used for further analyses)

tmResult325 <- posterior(topicModel325)
beta325 <- tmResult325$terms   # get beta from results
theta325 <- tmResult325$topics # data attributing topic probability for each publication
exampleTermData325 <- terms(topicModel325, 20)

