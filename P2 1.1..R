library(stringr)
library(stopwords)
library(wordcloud)
library(RColorBrewer)

library(dplyr)
library(tidytext)
library(tidyr)
library(purrr)
file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_declaration.txt','r' ) 
flines <- readLines(file_txt)
N <- length(flines)
print(flines)

#1.1.
word_line <- ""

for (i in 1:N){
  text_line <- flines[i]
  text_line <- stringr::str_replace_all(text_line, "[^a-zA-Z\\s]", "")
  text_line <- stringr::str_replace_all(text_line, "\\s+", " ")
  text_line <- trimws(text_line, which=c("both"))
  word_line <- paste(word_line,text_line, sep=" ")
}
 word_list <- stringr::str_split(word_line," ")[[1]] #split a string based on spaces into individual words
 #When you use [[1]] with str_split(),
 #you're accessing the first element of the list directly,
 #which will give you the split result as a character vector rather than a list
 

print(word_list)

#1.2.
file_stopW <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/stopwords.txt','r' ) 
stop_w <- flines <- readLines(file_stopW) 
Nstop <- length(stop_w)
print(stop_w)

#remove stop words
for (i in 1:Nstop)
{
  indices<- which(word_list == stop_w[i]) #indices of words that matches stop word ('which' gives you the indices)

  if(length(indices)>0)
  {
    word_list <-word_list[-indices] #remove the identified indices 
  }
}

print(word_list)

#bag of words are unique words
bag_words <- unique(word_list)
print(bag_words)
bag_words<- bag_words[2:length(bag_words)] # remove space character(it is in index 1)
print(bag_words)

#1.3.
#find how many times each word in bag_words occurs in word_list
N_bag <-length(bag_words)
freq <- rep(0,N_bag) #create an array the length N_bag containing 0's

for (i in 1:N_bag)
{
  indices2<- which(word_list == bag_words[i])
  print(length(indices2))
  freq[i] <- length(indices2)
}
print(freq)
df <- data.frame(bag_words,freq)
print(df)

sorted_df <- df[order(-df$freq),]# put - to show it is in descending order
print(sorted_df)


wordcloud(words=sorted_df$bag_words, freq = sorted_df$freq, min.freq = 1, max.words = 30, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

top30W <- 30
plot(1:top30W, sorted_df$freq[1:top30W], type="l", main="Term Frequency plot", xlab="", ylab="Term Count", xaxt="n")
axis(1, at=seq(1,top30W,by=1),label=sorted_df$bag_words[1:top30W],las=2, cex.axis = 0.8)

#1.4.
clean_Words <- function(flines) # make string/text into words
{
  N <- length(flines)
  
  word_line <- ""
  for (i in 1:N){
    text_line <- flines[i]
    text_line <- stringr::str_replace_all(text_line, "[^a-zA-Z\\s]", "")
    text_line <- stringr::str_replace_all(text_line, "\\s+", " ")
    text_line <- trimws(text_line, which=c("both"))
    word_line <- paste(word_line,text_line, sep=" ")
  }
  word_list <- stringr::str_split(word_line," ")[[1]]
  
  return(word_list)  
}

#remove stop words
remove_sw <-function(word_list)
{
  Nstop <- length(stop_w)
  
  for (i in 1:Nstop)
  {
    indices<- which(word_list == stop_w[i]) #indices of words that matches stop word ('which' gives you the indices)
    
    if(length(indices)>0)
    {
      word_list <-word_list[-indices] #remove the identified indices 
    }
  }
  return(word_list) 
}

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_law_speeches/law_1.txt','r' ) 
flines <- readLines(file_txt)
print(flines)
law1_words <- tolower(law1_words)#lower case everyword
law1_words <- clean_Words(flines)
law1_words <- remove_sw(law1_words)

print(law1_words)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_law_speeches/law_2.txt','r' ) 
flines <- readLines(file_txt)
print(flines)
law2_words <- tolower(law2_words)#lower case everyword
law2_words <- clean_Words(flines)
law2_words <- remove_sw(law2_words)

print(law2_words)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_law_speeches/law_3.txt','r' ) 
flines <- readLines(file_txt)
print(flines)
law3_words <- tolower(law3_words)#lower case everyword
law3_words <- clean_Words(flines)
law3_words <- remove_sw(law3_words)

print(law3_words)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_law_speeches/law_4.txt','r' ) 
flines <- readLines(file_txt)
print(flines)
law4_words <- tolower(law4_words)#lower case everyword
law4_words <- clean_Words(flines)
law4_words <- remove_sw(law4_words)

print(law4_words)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/un_law_speeches/law_5.txt','r' ) 
flines <- readLines(file_txt)
print(flines)
law5_words <- tolower(law5_words)#lower case everyword
law5_words <- clean_Words(flines)
law5_words <- remove_sw(law5_words)

print(law5_words)


law1_words <- law1_words[2:length(law1_words)] #remove space in position 1
law1_sentence <- paste(law1_words, collapse = " ")

law2_words <- law2_words[2:length(law2_words)] #remove space in position 1
law2_sentence <- paste(law2_words, collapse = " ")

law3_words <- law3_words[2:length(law3_words)] #remove space in position 1
law3_sentence <- paste(law3_words, collapse = " ")

law4_words <- law4_words[2:length(law4_words)] #remove space in position 1
law4_sentence <- paste(law4_words, collapse = " ")

law5_words <- law5_words[2:length(law5_words)] #remove space in position 1
law5_sentence <- paste(law5_words, collapse = " ")



corpus <- data.frame(
  document = c("law1","law2","law3","law4","law5"),
  text=c(law1_sentence, law2_sentence, law3_sentence, law4_sentence, law5_sentence)
)


corpus_df <- corpus %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  count(document, word)
print(corpus_df)

corpus_tfidf <- corpus_df %>%
  bind_tf_idf(word, document, n) %>%
  arrange(desc(tf_idf))
print(corpus_tfidf)



wordcloud(words=corpus_tfidf$word, freq = corpus_tfidf$tf_idf, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

top30W <- 30
plot(1:top30W, corpus_tfidf$tf_idf[1:top30W], type="l", main="Term Frequency plot", xlab="", ylab="Term TFIDF", xaxt="n")
axis(1, at=seq(1,top30W,by=1),label=corpus_tfidf$word[1:top30W],las=2, cex.axis = 0.8)
