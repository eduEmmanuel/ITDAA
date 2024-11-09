require("ggplot2")
require("reshape2")
require("lda")
require("tm")
require("topicmodels")
#2.1.
file_stopW <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/stopwords.txt','r' ) 
stop_w <- flines <- readLines(file_stopW) 

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

perf_clean_task <-function(word_list)
{
  word_list<- tolower(word_list)# lower case
  word_list <- clean_Words(word_list)
  word_list <- remove_sw(word_list)
  word_list <- word_list[2:length(word_list)] #remove space in position 1
  word_list <- paste(word_list, collapse = " ")# make words into a sentance
  return(word_list)
}



file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_1.txt','r' ) 
news1 <- readLines(file_txt)

news_1 <- perf_clean_task(news1)
print(news_1)


file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_2.txt','r' ) 
news_2 <- readLines(file_txt)
print(news_2)
news_2 <- perf_clean_task(news_2)
print(news_2)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_3.txt','r' ) 
news_3 <- readLines(file_txt)
print(news_3)
news_3 <- perf_clean_task(news_3)
print(news_3)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_4.txt','r' ) 
news_4 <- readLines(file_txt)
print(news_4)
news_4 <- perf_clean_task(news_4)
print(news_4)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_5.txt','r' ) 
news_5 <- readLines(file_txt)
print(news_5)
news_5 <- perf_clean_task(news_5)
print(news_5)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_6.txt','r' ) 
news_6 <- readLines(file_txt)
print(news_6)
news_6 <- perf_clean_task(news_6)
print(news_6)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_7.txt','r' ) 
news_7 <- readLines(file_txt)
print(news_7)
news_7 <- perf_clean_task(news_7)
print(news_7)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_8.txt','r' ) 
news_8 <- readLines(file_txt)
print(news_8)
news_8 <- perf_clean_task(news_8)
print(news_8)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_9.txt','r' ) 
news_9 <- readLines(file_txt)
print(news_9)
news_9 <- perf_clean_task(news_9)
print(news_9)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/sa_news/news_10.txt','r' ) 
news_10 <- readLines(file_txt)
print(news_10)
news_10 <- perf_clean_task(news_10)
print(news_10)

corpus <- data.frame(
  file_name = c("news_1","news_2","news_3","news_4","news_5","news_6","news_7","news_8","news_9","news_10"),
  content=c(news_1,news_2,news_3,news_4,news_5,news_6,news_7,news_8,news_9,news_10)
)

print(corpus$content[2])

#2.2.
content_vector <- setNames(corpus$content, corpus$file_name)
corpus <- Corpus(VectorSource(content_vector))
dtm <- DocumentTermMatrix(corpus) #format required by LDA
#DocumentTermMatrix shows the number of occurrances of each tems in each document
inspect(dtm)

lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))

# Look at the topics
terms(lda_model, 10)
################################
#posterior tells us the topic distribution in documents(probability of a document being about a topic),
#and topic term(word) distribution (tells us which words are likely to below to each topic)

#for topic 1
words = posterior(lda_model)$terms[1, ]
top_words1 = head((sort(words,decreasing =T )),n=50)
print(top_words1)

#for topic 2
words = posterior(lda_model)$terms[2, ]
top_words2 = head((sort(words,decreasing =T )),n=50)
print(top_words2)

#for topic 3
words = posterior(lda_model)$terms[3, ]
top_words3 = head((sort(words,decreasing =T )),n=50)
head(top_words3)
##################################

#2.3.
#Topic 1
wordcloud(names(top_words1), top_words1, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

#Topic 2
wordcloud(names(top_words2), top_words2, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

#Topic 3
wordcloud(names(top_words3), top_words3, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8,"Dark2"))