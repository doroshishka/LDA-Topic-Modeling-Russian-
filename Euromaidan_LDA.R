library(topicmodels) #topic modeling
library(quanteda) #preprocessing and building dtm/dfm
library(dplyr)
library(ldatuning) #find topic numbers
library(tidytext)
library(ggplot2)
library(DT)
library(tidyverse)
library(SnowballC)
library (devtools)
library(qdapRegex)
library(stringr)
library(rlist)
library(rtweet)
library(corpus)

Sys.setlocale("LC_CTYPE", "russian")
options(scipen=999)

setwd("C:/Users/Larisa/OneDrive/Documents/University of Wisconsin-Madison/SMAD/Russian sockpuppet project")
data = read.csv("C:/Users/Larisa/OneDrive/Documents/University of Wisconsin-Madison/SMAD/Russian sockpuppet project/data_test.csv", encoding = "UTF-8", header = TRUE, sep = ",")

data_relevant <- subset(data, final_label == "1", select=c(tweet.id, user.id, user.handle, tweet, created.at, retweet.created.at, retweet.user.verified, retweet))
write.csv(data_relevant, fileEncoding = "UTF-8",'data_test.csv')

notRT <- subset(data, retweet.created.at == "\\N", select=c(tweet.id, user.id, user.handle, tweet, created.at))
RT <- subset(data, retweet.user.verified == "false", select=c(tweet.id, user.id, user.handle, retweet, created.at))
RTverified <- subset(data, retweet.user.verified == "true", select=c(tweet.id, user.id, user.handle, retweet, created.at))

merged.RT <- rbind(RT,RTverified)
names(RT)[names(RT) == "retweet"] <- "tweet"
mergedtweet <- rbind(RT,notRT)

tweets <- rm_url(mergedtweet$tweet, pattern=pastex("@rm_twitter_url", "@rm_url")) %>% as.data.frame()
tweets2 <- as.character(tweets$.)#change to character

corpus <- quanteda::corpus(tweets2) #build "corpus" using quanteda (becuase of the encoding issue)
tokens <- tokens(corpus,remove_numbers=T,remove_punct=T) 
tokens <- tokens_tolower(tokens) 
tokens <- tokens_wordstem(tokens, language = "russian")

head(tokens)

stopwords(language = "ru")

dfm <- dfm(tokens, remove = stopwords ("russian")) #converting to document feature matrix

dfm_trim <- dfm_trim(dfm, min_termfreq = 15, max_docfreq = 150) #removing terms that occurred less than 2 times and occurred over 25 documents

rowtotals <- apply(dfm_trim, 1, sum)
dfm_n <- dfm_trim[rowtotals > 0,]

k <- 10

control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE, #the starting value for alpha is 50/k suggested by Griffiths & Steyvers (2004)
                          verbose = 0, prefix = tempfile(),
                          save = 0, keep = 0,  #no information is printed during the algorithm; no immediate results are saved
                          seed = 999, #random seed for reproducibility
                          nstart = 1, #number of repeated runs with random initializations
                          best = TRUE, #returns only the best one model
                          delta = 0.1, #specifies the parameter of the prior distribution of the term distribution over topics. The default is 0.1
                          iter = 2000, #iterations 
                          burnin = 100, #the first 100 iterations are discarded 
                          thin = 2000) #then every 2000th iteration is returned

lda <- LDA(dfm_n, k=k, method="Gibbs", control = control_LDA_Gibbs)

terms(lda, 10)

topics<- tidy(lda, matrix="beta")
top_terms <- topics %>% group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

result <- FindTopicsNumber(
  dfm_n,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

n_topics <- c(4, 10, 12, 15)
lda_compare <- n_topics %>%
  map(LDA, x = dfm_n, control = list(seed = 1109))
data_frame(k = n_topics,
           perplex = map_dbl(lda_compare, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line() +
  labs(title = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics (smaller is better)",
       x = "Number of topics",
       y = "Perplexity")
