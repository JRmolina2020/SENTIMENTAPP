library(rtweet)
library(plyr)
library(stringr)
library(highcharter)
library(shiny)
library(highcharter)
library(twitteR)
library("tm")
library(shinythemes)
library("SnowballC")
library("wordcloud")
library(wordcloud2)
library("RColorBrewer")
library(memoise)
library(shinyWidgets)
api_key             <- "lhoFGi7WwTCNweSwjLAf0pcsO"
api_secret          <- "HX5YP7MaRb0pri3G73tbIIqvv8qbUGiptsY4fsL9bCN6YjN9zv"
access_token        <- "920582805981188097-kydfkJ915usop34k1VQd9uMBVKTxnaL"
access_token_secret <- "73LOfzcjfIPlUUNoDVOVXYt5uEpBPaiM16EnSk9HD7pUO"


create_token(
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)




tweet_df <- search_tweets('#MingaSomosTodos', n = 1000,lang="es",
                          include_rts = FALSE)
texto <- tweet_df$text

colombia<-get_trends(368148)




limpiarTweets = function(tweet){
  tweet_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet)
  tweet_txt <- gsub("@\\w+", "", tweet_txt)
  tweet_txt <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", tweet_txt)
  tweet_txt <- gsub("[^a-zA-Z0-9 ]", "", tweet_txt)
  tweet_txt <- gsub("[[:punct:]]", "", tweet_txt)
  tweet_txt <- gsub("amp ", "", tweet_txt)
  tweet_txt <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", tweet_txt)
  tweet_txt <- tweet_txt[!is.na(tweet_txt)]
  tweet_txt <- iconv(tweet_txt, 'UTF-8', 'ASCII')
  tweet_txt <- gsub("[ \t]{2,}", "", tweet_txt)
  tweet_txt <- gsub("^\\s+|\\s+$", "",tweet_txt)
  tweet_txt <- tolower(tweet_txt)
  tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweet_txt)
  tweet_txt = gsub("http[^[:blank:]]+", "", tweet_txt)
  tweet_txt = gsub("[^[:alnum:]]", " ", tweet_txt)
  tweet_txt <- gsub('\\d+', '', tweet_txt)
  return(tweet_txt)
}

texto<-limpiarTweets(texto)
v <- reactiveValues(
  texto = texto
)


sentimientosScore <- function(frases, mNeg, neg, pos, mPos){
  scores_final <- matrix('', 0, 5)
  scores <- laply(frases, function(frase, mNeg, neg, pos, mPos){
    frase_inicial <- frase   
    #elimina caracteres innecesarios
    frase <- limpiarTweets(frase)
    frase <- tolower(frase)
    listaPalabras <- str_split(frase, '\\s+')
    palabras <- unlist(listaPalabras)
    #construye un vector con los matches entre la frase y las categorias
    mPosMatches <- match(palabras, mPos)
    posMatches <- match(palabras, pos)
    mNegMatches <- match(palabras, mNeg)
    negMatches <- match(palabras, neg)
    mPosMatches <- sum(!is.na(mPosMatches))
    posMatches <- sum(!is.na(posMatches))
    mNegMatches <- sum(!is.na(mNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(mNegMatches, negMatches, posMatches, mPosMatches)
    nlinea <- c(frase_inicial, score)
    scores_final <- rbind(scores_final, nlinea)
    return(scores_final)
  }, mNeg, neg, pos, mPos)
  return(scores)
}
afinn_list <- read.csv(file='./diccionario.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('palabra', 'score')
afinn_list$palabra <- tolower(afinn_list$palabra)
mNeg <- afinn_list$palabra[afinn_list$score==-5 | afinn_list$score==-4]
neg <- c(afinn_list$palabra[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
pos <- c(afinn_list$palabra[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1])
mPos <- c(afinn_list$palabra[afinn_list$score==5 | afinn_list$score==4])  

valor=0

getTermMatrix <- memoise(function(texto) {
  text = iconv(texto, to="ASCII//TRANSLIT") 
  text <- text
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus  <- tm_map(myCorpus, stripWhitespace)
  stopwords("spanish")
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("spanish"))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  tdm <- TermDocumentMatrix(myCorpus)
  findFreqTerms(tdm, lowfreq=20)
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
