list.of.packages <- c("lda","LDAvis",'servr','tm','ldatuning','devtools','topicmodels')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(lda)
library(LDAvis)
library(servr)
library(tm)
devtools::install_github("nikita-moor/ldatuning")
library("ldatuning")
library("topicmodels")

setwd("/Users/pierre/Documents/M2/Cours_M2/Text Mining/bd")
load("final.Rda")

# pre-processing:
reviews <- gsub("'", "", final$Content)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents

#tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")

#delete extra word
extra_stop_words <- c('ans','jour','lors','chez','etes','etre','ainsi','plus','afin'
                      ,'temps','emploi','plein','non','selon','tous','mois','tant','sein','leurs','hors',
                      'tous','entre','meme','net','si','te','etc','www','hui','toutes','toute','tout','sous','dont')

doc.list2 <- c()
for (i in unlist(doc.list)){
  if (nchar(i)>1 & !i %in% extra_stop_words){
    doc.list2<-c(doc.list2,i)
  }
}

# compute the table of terms:
term.table <- table(doc.list2)
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that occur fewer than 5 times:
del <- term.table < 5 | term.table=="aa"
term.table <- term.table[!del]
vocab <- names(term.table)

dd <- as.data.frame(table(vocab))
# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

#https://stats.stackexchange.com/questions/59684/what-are-typical-values-to-use-for-alpha-and-beta-in-latent-dirichlet-allocation
alpha <-50/K
eta <- 0.1

# Fit the model:
set.seed(357)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = 52, vocab = vocab, 
                                   num.iterations = 1000, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

#saveRDS(fit, "fit.rds")
#fit <- readRDS("fit.rds")

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)

serVis(json, out.dir = 'LDAvis', open.browser = TRUE)
#servr::daemon_stop(1)
