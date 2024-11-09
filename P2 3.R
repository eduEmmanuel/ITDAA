library(plyr)
library(tm)


file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_1.txt','r' ) 
news1 <- readLines(file_txt)


print(news_1)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_2.txt','r' )
news_2 <- readLines(file_txt)

print(news_2)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_3.txt','r' )
news_3 <- readLines(file_txt)

print(news_3)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_4.txt','r' )
news_4 <- readLines(file_txt)

print(news_4)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_5.txt','r' )
news_5 <- readLines(file_txt)

print(news_5)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_6.txt','r' )
news_6 <- readLines(file_txt)

print(news_6)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_7.txt','r' )
news_7 <- readLines(file_txt)

print(news_7)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_8.txt','r' )
news_8 <- readLines(file_txt)

print(news_8)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_9.txt','r' )
news_9 <- readLines(file_txt)

print(news_9)

file_txt <- file('C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project 2/Q3/sa_news/news_10.txt','r' )
news_10 <- readLines(file_txt)

print(news_10)


corpus  <- list(
  "news1" = news_1,
  "news2" = news_2,
  "news3" = news_3,
  "news4" = news_4,
  "news5" = news_5,
  "news6" = news_6,
  "news7" = news_7,
  "news8" = news_8,
  "news9" = news_9,
  "news10" = news_10
)

print(corpus)
# Define the keyword to search for

###Change keyword here
keyword <- "Gauteng"



#3.2.
mapper <- function(docname, content, keyword) {
  print("-------------------in")
  print(content)
  print("-------------------in")
  results <- list()
  lines <- strsplit(content, "\n")[[1]]
  
  for (line_id in seq_along(lines)) {
    line <- lines[line_id]
    words <- strsplit(line, "\\s+")[[1]]
    for (col_id in seq_along(words)) {
      if (tolower(words[col_id]) == tolower(keyword)) {
        results <- append(results, list(list(docname = docname, line_id = line_id, col_id = col_id)))
      }
    }
  }
  print(results)
  return(results)
  
}

# Apply the Mapper function to each document
mapped_results <- list()
for (docname in names(corpus)) {
  content <- corpus[[docname]]
  mapped_results <- c(mapped_results, mapper(docname, content, keyword))
}
print("here3")
#3.3.
# Reduce step: Organize results by document and line for clarity
reduced_results <- list()
for (res in mapped_results) {
  docname <- res$docname
  line_id <- res$line_id
  col_id <- res$col_id
  key <- paste(docname, line_id, sep = "_")
  
  if (!is.null(reduced_results[[key]])) {
    reduced_results[[key]] <- c(reduced_results[[key]], col_id)
  } else {
    reduced_results[[key]] <- col_id
  }
}

#3.4.
# Print the final results
for (key in names(reduced_results)) {
  parts <- strsplit(key, "_")[[1]]
  docname <- parts[1]
  line_id <- as.integer(parts[2])
  col_positions <- reduced_results[[key]]
  print(paste("The keyword is in the doc:", docname, "Line:", line_id, "Columns:", paste(col_positions, collapse = ", ")))

}