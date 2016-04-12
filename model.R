library(tm)
library(SnowballC)
library(RWeka)
library(stringr)

library(stringi)
library(slam)

load('ngram.RData')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

CleanInput <- function(input)
{
  input <- removePunctuation(input)#remove punctuation
  input <- removeNumbers(input)#remove number
  input <- tolower(input)#to lower case
  #input <- stemDocument(input)#hmmm, get rid?
  input <- stripWhitespace(input)
  input <- input[grepl('[[:alpha:]]',input)]
  input <- trim(input)
  return(input)
}


predict4 <-function(input,profanity,unigramDF, bigramDF, trigramDF, fourgramDF, fivegramDF, maxResults) {
  input <- CleanInput(input)
  seekfive <- grepl(paste0("^",input,"$"),fivegramDF$fourgram)
  subfive<-fivegramDF[seekfive,]
  input2 <- sub(input, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seekfour <- grepl(paste0("^",input2,"$"),fourgramDF$trigram) 
  subfour<-fourgramDF[seekfour,]
  input3 <- sub(input2, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seektri<-grepl(paste0("^",input3,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  input4 <- sub(input3, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seekbi <- grepl(paste0("^",input4,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.0256#weighted Good-Turing probabability of unigram
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]#ordered weighted Good-Turing probability
  useunia <- useuni[1:maxResults,]#top-MaxResults of weighted Good-Turing probability
  
  subfive$s <- subfive$freq/sum(subfive$freq)
  subfour$s <- 0.4*subfour$freq/sum(seekfour)#weighted Good-Turing probability of bigram
  subtri$s <- 0.16*subtri$freq/sum(seektri)
  subbi$s <- 0.064*subbi$freq/sum(seekbi)
  
  names <- c(subfive$name,subfour$name, subtri$name, subbi$name, useunia$unigram)
  score <- c(subfive$s,subfour$s,subtri$s, subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}

predict3 <-function(input,profanity,unigramDF, bigramDF, trigramDF, fourgramDF, maxResults) {  
  input <- CleanInput(input)
  seekfour <- grepl(paste0("^",input,"$"),fourgramDF$trigram) 
  subfour<-fourgramDF[seekfour,]
  input2 <- sub(input, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seektri<-grepl(paste0("^",input2,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  input3 <- sub(input2, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seekbi <- grepl(paste0("^",input3,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.064#weighted Good-Turing probabability of unigram
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]#ordered weighted Good-Turing probability
  useunia <- useuni[1:maxResults,]#top-MaxResults of weighted Good-Turing probability
  
  subfour$s <- subfour$freq/sum(seekfour)#weighted Good-Turing probability of bigram
  subtri$s <- 0.4*subtri$freq/sum(seektri)
  subbi$s <- 0.16*subbi$freq/sum(seekbi)
  
  names <- c(subfour$name, subtri$name, subbi$name, useunia$unigram)
  score <- c(subfour$s,subtri$s, subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}

predict2 <-function(input,profanity,unigramDF, bigramDF, trigramDF, maxResults) {  
  input <- CleanInput(input)
  seektri<-grepl(paste0("^",input,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  input2 <- sub(input, pattern = "^[[:alpha:]]* ", replacement = "")
  
  seekbi <- grepl(paste0("^",input2,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.16#weighted Good-Turing probabability of unigram
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]#ordered weighted Good-Turing probability
  useunia <- useuni[1:maxResults,]#top-MaxResults of weighted Good-Turing probability
  
  subtri$s <- subtri$freq/sum(seektri)
  subbi$s <- 0.4*subbi$freq/sum(seekbi)
  
  names <- c(subtri$name, subbi$name, useunia$unigram)
  score <- c(subtri$s, subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}

predict1 <-function(input,profanity,unigramDF, bigramDF, maxResults) {  
  input <- CleanInput(input)
  seekbi <- grepl(paste0("^",input,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,] 
  
  subbi$s <- subbi$freq/sum(seekbi)
  
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.4
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]
  useunia <- useuni[1:maxResults,]
  
  names <- c(subbi$name, useunia$unigram)
  score <- c(subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)]  
  return(final)
}


#predict<-function(input,profanity,unigramDF, maxResults = 3) {  
#{
#  return(head(unigramDF[order(unigramDF$freq,decreasing = T),1],maxResults))#return top-maXResults for unigram
#}
