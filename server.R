

library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library (tidyr)
library(stringr)
library(tidyverse)
library(tokenizers)
library(quanteda)
library(LIWCalike)
library(ggplot2)
library(readr)
library(readtext)
library(hunspell)
library(stopwords)
library(ANLP)
library(rJava)
library(qdap)
library(tm)
library(rmarkdown)
library(RWeka)


blogs <- file("~/Corpus/Capstone/Coursera-Swiftkey/final/en_US/en_US.blogs.txt","r")
blogToken <- readLines(blogs, 4000)
close(blogs)
news <- file("~/Corpus/Capstone/Coursera-Swiftkey/final/en_US/en_US.news.txt","r")
newsToken <- readLines(news, 4000)
close(news)
tweets <- file("~/Corpus/Capstone/Coursera-Swiftkey/final/en_US/en_US.twitter.txt", "r")
tweetsToken <- readLines(tweets, 4000)
close(tweets)


## One Word Data Files

dirtyWords <- c("shit","piss", "fuck","cocksucker","cunt","motherfucker","tits","fucking") #dirty words
blogBad<-hunspell(blogToken) #misspelled and Foreign words
blogsTokens <- tokens(blogToken, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 1)
blogsDfm <- dfm(blogsTokens, remove = dirtyWords)
blogsDfm <- dfm(blogsTokens, remove = blogBad)
newsBad<-hunspell(newsToken)
newsTokens <- tokens(newsToken, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 1)
newsDfm <- dfm(newsTokens, remove = c("dirtyWords","newsBad"))
tweetsBad<-hunspell(tweetsToken)
tweetsTokens <- tokens(tweetsToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 1)
tweetsDfm <- dfm(tweetsTokens, remove = dirtyWords)
tweetsDfm <- dfm(tweetsToken, remove = tweetsBad)

## Two Words Data Files

blogsTokens2 <- tokens(blogToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 2)
blogsDfm1 <- dfm(blogsTokens2, remove = c("dirtyWords","blogBad"))
newsTokens2 <- tokens(newsToken, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 2)
newsDfm1 <- dfm(newsTokens2, remove = c("dirtyWords","newsBad"))
tweetsTokens2 <- tokens(tweetsToken, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 2)
tweetsDfm1 <- dfm(tweetsTokens2, remove = c("dirtyWords","tweetsBad"))

## Three Words Data Files

blogsTokens3 <- tokens(blogToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 3)
blogsDfm2 <- dfm(blogsTokens3, remove = c("dirtyWords","blogBad"))
newsTokens3 <- tokens(newsToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 3)
newsDfm2 <- dfm(newsTokens3, remove = c("dirtyWords","newsBad"))
tweetsTokens3 <- tokens(tweetsToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, remove_url = TRUE, n = 3)
tweetsDfm2 <- dfm(tweetsTokens3, remove = c("dirtyWords","tweetsBad"))

## Four Words Data Files

blogsTokens4 <- tokens(blogToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, n = 4)
blogsDfm3 <- dfm(blogsTokens4, remove = c("dirtyWords","blogsBad"))
newsTokens4 <- tokens(newsToken,remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_twitter = TRUE, n = 4)
newsDfm3 <- dfm(newsTokens4, remove = c("dirtyWords","newsBad"))
tweetsTokens4 <- tokens(tweetsToken, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE,remove_twitter = TRUE, n = 4)
tweetsDfm3 <- dfm(tweetsTokens4, remove = c("dirtyWords","tweetsBad"))


## One Word Frequency Sorted Tables



blogsFreq <- textstat_frequency(blogsDfm)
newsFreq <- textstat_frequency(newsDfm)
tweetsFreq <- textstat_frequency(tweetsDfm)


## Two Words Frequency Sorted Tables

blogsFreq1 <- textstat_frequency(blogsDfm1)
newsFreq1 <- textstat_frequency(newsDfm1)
tweetsFreq1 <- textstat_frequency(tweetsDfm1)


## Plot Three Words Frequency Sorted Tables


blogsFreq2 <- textstat_frequency(blogsDfm2)
newsFreq2 <- textstat_frequency(newsDfm2)
tweetsFreq2 <- textstat_frequency(tweetsDfm2)


blogsFreq3 <- textstat_frequency(blogsDfm3)
newsFreq3 <- textstat_frequency(newsDfm3)
tweetsFreq3 <- textstat_frequency(tweetsDfm3)



firstWord <- c(blogsFreq, newsFreq,tweetsFreq)
secondWord <- c(blogsFreq1, newsFreq1, tweetsFreq1)
thirdWord <- c(blogsFreq2, newsFreq2, tweetsFreq2)
fourthWord <- c(blogsFreq3,newsFreq3, tweetsFreq3)


predictWord <- function(testPhrase)
{
  
  wordCount <-  word_count(testPhrase)
  
  
  if (wordCount == 3)
  {
    
    testPhrase <- gsub(" ","_",testPhrase)
    fourthWord <- data.frame(feature = fourthWord$feature, frequency =
                               fourthWord$frequency)
    fourthWordA <- filter(fourthWord, grepl(testPhrase, fourthWord$feature))
    print("The top choice for the fourth word: ")
    print(str_split(fourthWordA$feature[1],"_")[[1]][4]) #print 4th word   
    
    
  }
  
  if (wordCount == 2)
  {
    testPhrase <- gsub(" ","_",testPhrase)
    thirdWord <- data.frame(feature = thirdWord$feature, frequency =
                              thirdWord$frequency)
    thirdWordA <- filter(thirdWord, grepl(testPhrase, thirdWord$feature))
    print("The top choice for the third word: ")
    print(str_split(thirdWordA$feature[1], "_")[[1]][3])  #prints third word
  }  
  
  if (wordCount == 1)
  {
    secondWord <- data.frame(feature = secondWord$feature, frequency =  
                               secondWord$frequency)
    secondWordA <- filter(secondWord, grepl(testPhrase, secondWord$feature))
    print("The top choice for the second word:")
    secondPredict <- (str_split(secondWordA$feature[1], "_")[[1]][2])
    print(secondPredict)
    
    
  } 
  if(wordCount == 0)
  {
    print("Data is missing.  Type at least one word.") 
  }
  
  
}

  


shinyServer(
  function(input, output)  {
      output$inputvalue <- renderPrint({input$testPhrase})
      output$nextWord <- renderPrint({predictWord(input$testPhrase)})
    
    
    
    
    
    
  }
)
