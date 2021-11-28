all_data <- data.frame(word=character(),
                  ham=integer(), 
                  spam=integer(), 
                  stringsAsFactors=FALSE
) 
result <- data.frame(file_name=character(),
                     detected_spam=integer(), #if spam ->1
                     detected_correct=integer(), #if not spam ->1
                     stringsAsFactors=FALSE
)

#function gets the name of file and returns the content of that specific file in a vector 
get_file_text <- function(path,file_name){
  text <- readLines(paste(path, file_name, sep = ""),encoding = "UTF-8")
  return(text)
}

add_to_dataframe <- function(new_word,all_data){
  temp_df <- data.frame(new_word,0,0)
  names(temp_df) <-c("word","ham","spam")
  return(rbind(all_data,temp_df))
}

add_to_spam <- function(new_word,all_data){
  if(!any(all_data[,1]==new_word)){
    all_data <- add_to_dataframe(new_word,all_data)
  }
  all_data[all_data$word ==new_word,3] = all_data[all_data$word ==new_word,3]+1
  return(all_data)
}

add_to_ham <- function(new_word,all_data){
  if(!any(all_data[,1]==new_word)){
    all_data <- add_to_dataframe(new_word,all_data)
  }
  all_data[all_data$word ==new_word,2] = all_data[all_data$word ==new_word,2]+1
  return(all_data)
}

#we mark links as "THISISLINK" and email as "THISISEMAIL" and numbers as "NUMBER"
extract_links <- function(line){
  line<-gsub("<","",line)
  line <- gsub("(\\s|^)(\\S*http[s]?:\\S+\\.\\S+)(\\s|$)"," THISISLINK ",line)
  line <- gsub("(\\s|^)(\\S*www\\.\\S+\\.\\S+)(\\s|$)"," THISISLINK ",line)
  line <- gsub("(\\s+|^)(\\d+)(\\s+|$)"," NUMBER ",line)
  line <- gsub("(\\s+|^)([\u06F0-\u06F9]+)(\\s+|$)"," NUMBER ",line)     #handle persian numbers
  line <- gsub("(\\s|^)(\\S*www\\.\\S+\\.\\S+)(\\s|$)"," THISISLINK ",line)
  line <- gsub("(\\s|^)(\\S+@\\S+\\.\\S+)(\\s|$)"," THISISEMAIL ",line)
  ?gsub
  return(line)
}
#replacing all characters that dont change the meaning of words
adjust_text <- function(line){
  line <- extract_links(line)
  line <- gsub("(\\s|^)\\S(\\s|$)"," ",line)   #remove one character words
  line <- gsub("\\(|\\)|\\.|,|/|\\\\|\\?|؟|،|\\!|\\[|\\]|\\{|\\}|؛","",line) #remove extra signs 
  line <- gsub(":|@|#|\\$|\\^|&|\\*|<|>|\\+|-|~|`|×|÷|;|'|_|«|»","",line)
  line <- gsub("\u0650|\u0651|\u064F|\u064E|\u064D|\u064C|\u064B","",line) #remove some persian characters that don't affect the meaning of words
  line <- gsub("\u06C0|\u0629","\u0647",line)   #ه و ۀ و ة
  line <- gsub("\u0622|\u0625|\u0623","\u0627",line)    #آ و ا
  line <- gsub("\u064A|\u0626","\u06CC",line)     #ی و ي
  line <- gsub("\u0624","\u0648",line)   #و ؤ
  line <- gsub("\u0621","",line)
  line <- gsub("\u0025","",line)
  line <- gsub("\\s+"," ",line)
  line <- extract_links(line)
  line <- gsub("\\d|[\u06F0-\u06F9]","",line)
  return(line)
}

#this function will return a list of words that don't have any recurrence and and also links are handled
get_all_words <- function(all_lines){
  word_list <- list()
  #for each line of file we have to adjust the line and split the words by space
  for(i in 1:length(all_lines)){
    if(i==1){#remove first character of the text
      all_lines[i] <- substr(all_lines[i],2,nchar(all_lines[i]))
    }
    Encoding(all_lines[i]) <- "UTF-8"
    all_lines[i] = adjust_text(all_lines[i])
    different_words <- strsplit(all_lines[i]," ")
    word_list <- append(word_list,different_words)
  }
  return((unique(unlist(word_list))))
}

#learn 300 files of spam emails
learn_spam <- function(all_data){
  path <- "D://spamDetector/emails/spamtraining"
  for(i in 1:300){
    name <- paste("/spamtraining (",i,").txt",sep = "")
    all_lines <- get_file_text(path,name)
    all_words <- get_all_words(all_lines)
    for(j in 1:length(all_words)){
      if(all_words[j]!= ""){
        all_data <- add_to_spam(all_words[j],all_data)
      }
    }
    print(i)
    print("is done" )
    print("------------------------")
  }
  return(all_data)
}

#learn 300 files of ham emails
learn_ham <- function(all_data){
  path <- "D://spamDetector/emails/hamtraining"
  for(i in 1:300){
    name <- paste("/hamtraining (",i,").txt",sep = "")
    all_lines <- get_file_text(path,name)
    all_words <- get_all_words(all_lines)
    for(j in 1:length(all_words)){
      if(all_words[j]!= ""){
        all_data <- add_to_ham(all_words[j],all_data)
      }
    }
    print(i)
    print("is done" )
    print("------------------------")
  }
  
  return(all_data)
  
}

test_spam <- function(all_data,result){
  correct_out <-0
  path <- "D://spamDetector/emails/spamtesting"
  for( i in 1:200){
    name <-paste("/spamtesting (",i,").txt",sep = "")
    all_lines <- get_file_text(path,name)
    all_words <- get_all_words(all_lines)
    spam <- calculate_spam_ifemail(all_data,all_words)
    ham <- calculate_ham_ifemail(all_data,all_words)
    detected_spam <- 1
    detected_correct <- 1
    if(spam<ham){
      detected_correct <-0 
      detected_spam <- 0
    }else{
      correct_out <- correct_out+1
    }
    #add to result dataframe 
    temp_df <- data.frame(name,detected_spam,detected_correct)
    names(temp_df) <-c("file_name","detected_spam","detected_correct")
    result<- (rbind(result,temp_df))
  }
  print("correct spam detections: ")
  print(correct_out)
  print("percentage of correct spam detections:")
  print(correct_out/200)
  print("----------------------------------")
  return(result)
}

test_ham <- function(all_data,result){
  correct_out <-0
  path <- "D://spamDetector/emails/hamtesting"
  for( i in 1:200){
    name <-paste("/hamtesting (",i,").txt",sep = "")
    all_lines <- get_file_text(path,name)
    all_words <- get_all_words(all_lines)
    spam <- calculate_spam_ifemail(all_data,all_words)
    ham <- calculate_ham_ifemail(all_data,all_words)
    detected_spam <- 0
    detected_correct <- 1
    if(spam==Inf || spam>ham){
      detected_correct <- 0
      detected_spam <- 1
    }else{
      correct_out <- correct_out+1
    }
    #add to result dataframe 
    temp_df <- data.frame(name,detected_spam,detected_correct)
    names(temp_df) <-c("file_name","detected_spam","detected_correct")
    result<- (rbind(result,temp_df))
  }
  print("correct ham detections: ")
  print(correct_out)
  print("percentage of correct ham detections:")
  print(correct_out/200)
  print("----------------------------------")
  return(result)
}

calculate_email_count_ifspam <- function(all_data,all_words){
  count <- 1
  for(i in 1:length(all_words)){
    if(any(all_data[1]==all_words[i])){
      word_stat <- all_data[all_data$word %in% all_words[i],] 
      if(word_stat[3]!=0)
      {count = count*(1+word_stat[3])
      }
    }
  }
  return (count)
}
calculate_email_count_ifham <- function(all_data,all_words){
  count <- 1
  for(i in 1:length(all_words)){
    if(any(all_data[1]==all_words[i])){
      word_stat <- all_data[all_data$word %in% all_words[i],] 
      if(word_stat[2]!=0){
        count = count*(1+word_stat[2])
      }
    }
  }
  return (count)
}

# n(S|e)
calculate_spam_ifemail <- function(all_data,all_words){
  soorat <- calculate_email_count_ifspam(all_data,all_words)
  makhraj <- soorat+ calculate_email_count_ifham(all_data,all_words)
  if(soorat==Inf){
    result <- 1
  }else
  {result <- soorat/makhraj}
  return (result)
}
#n(H|E)
calculate_ham_ifemail <- function(all_data,all_words){
  soorat <- calculate_email_count_ifham(all_data,all_words)
  makhraj <- soorat+ calculate_email_count_ifspam(all_data,all_words)
  if(soorat==Inf){
    result <- 1
  }else
 { result <- soorat/makhraj}
  return (result)
}

main <- function(all_data,result){
 # all_data <- learn_spam(all_data)
  #all_data <- learn_ham(all_data)
  result <-test_spam(all_data,result)
 result <- test_ham(all_data,result)
  correct <- sum(result$detected_correct)
  print("number of correct decisions out of 400:")
  print(correct)
  print("percentage:")
  print(correct/4)
  print("----------------------------------")
  return(result)
}

result<- main(all_data,result)
  
