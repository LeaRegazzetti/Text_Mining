list.of.packages <- c("lda","LDAvis",'servr','tm','ldatuning','devtools','topicmodels','qdapTools')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(lda)
library(LDAvis)
library(servr)
library(tm)
devtools::install_github("nikita-moor/ldatuning")
library("ldatuning")
library("topicmodels")
library(qdapTools)
library(wordcloud)

setwd("/Users/pierre/Documents/M2/Cours_M2/Text_Mining/bd")
load("final.Rda")

################## Vector / Corpus
#Stop words
vsdocs <-VectorSource(final$Content)
corpus <- Corpus(vsdocs)

# Delete stopwords
corpus <- tm_map(corpus, removeWords)

# Text stemming
#corpus <- tm_map(corpus, stemDocument)
#print(corpus$content[3])

#DocumentTerm
mdt1 <- DocumentTermMatrix(corpus)
print(mdt1$dimnames$Terms)
M1 <- as.matrix(mdt1) #Matrice
print(nrow(M1))
print(ncol(M1))
as.matrix(mdt1$dimnames)
cbind(as.matrix(mdt1$dimnames$Docs), as.matrix(mdt1$dimnames$Terms)) 

#list terms
print(colnames(M1))

#frequency terms
freq <- apply(M1,2,sum)
print(sort(freq,decreasing=TRUE)[1:50])

#To reduce the dimension of the DTM, we can move the less frequent terms such that 
#the sparsity is less than 0.95
review_mdt
review_mdt = removeSparseTerms(mdt1, 0.99)
review_mdt
findFreqTerms(review_mdt, lowfreq = 100, highfreq = Inf) #mot between lowfreq and high

result <- FindTopicsNumber(
  review_mdt,
  topics = seq(from = 50, to = 80, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)