
path.profile <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
dir.post <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
dir.models <- "/Users/Mehdi/Desktop/ML/Project/Models/"
training.size <- 7500

####################### MAIN #######################

read.profiles <- function(profilesPath, start, end) {
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profiles <- read.csv(path.profile, header = T, colClasses = col.class)[start : end, ]
  age <- cut(profiles$age, 
             breaks = c(-Inf, 24, 34, 49, Inf), 
             labels = c("xx-24", "25-34", "35-49", "50-xx"), 
             right = T)
  profiles$age <- age
  profiles$X <- NULL
  profiles$ope <- NULL
  profiles$con <- NULL
  profiles$ext <- NULL
  profiles$agr <- NULL
  profiles$neu <- NULL
  
  return (profiles)
}

read.post <- function(userid) {
  path <- paste(userid, ".txt", sep = "")
  post <- paste(scan(path, what="character", sep = "\n"), collapse = "")
  post <- gsub("[[:punct:]]", " ", post)
  post <- gsub('([[:alpha:]])\\1+', '\\1', post)
  post <- iconv(post, "WINDOWS-1252","UTF-8")
  return (post)
}

BigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

createBigramDocumentTermMatrix <- function(profiles, post) {
  
  library(NLP)
  library(tm)
  library(SnowballC)
  
  post.corpus <- Corpus(VectorSource(post))
  post.corpus <- tm_map(post.corpus, content_transformer(tolower))
  post.corpus <- tm_map(post.corpus, PlainTextDocument)
  post.corpus <- tm_map(post.corpus, removeNumbers)
  post.corpus <- tm_map(post.corpus, removePunctuation)
  post.corpus <- tm_map(post.corpus, stripWhitespace)
  post.corpus <- tm_map(post.corpus, removeWords, stopwords(kind = "en"))
  post.corpus <- tm_map(post.corpus, stemDocument)
  
  
  tdm <- DocumentTermMatrix(post.corpus)
  tdm <- removeSparseTerms(tdm, sparse=0.99)
  lowfreq <- findFreqTerms(tdm, lowfreq = 0, highfreq = 100)
  
  a <- tm_map(post.corpus, removeWords, lowfreq)
  
  tdm2 <- DocumentTermMatrix(a, control = list(tokenize = BigramTokenizer))
  tdm2 <- removeSparseTerms(tdm2, sparse=0.995)
  
}

createUnigramDocumentTermMatrix <- function(profiles, post) {
  library(NLP)
  library(tm)
  library(SnowballC)
  
  post.corpus <- Corpus(VectorSource(post))
  post.corpus <- tm_map(post.corpus, content_transformer(tolower))
  post.corpus <- tm_map(post.corpus, PlainTextDocument)
  post.corpus <- tm_map(post.corpus, removeNumbers)
  post.corpus <- tm_map(post.corpus, removePunctuation)
  post.corpus <- tm_map(post.corpus, stripWhitespace)
  post.corpus <- tm_map(post.corpus, removeWords, stopwords(kind = "en"))
  post.corpus <- tm_map(post.corpus, stemDocument)
  
  
  tdm <- DocumentTermMatrix(post.corpus)
}

createGenderModel <- function(profiles, documentTermMatrix) {
  
  library(e1071)
  model <- naiveBayes(documentTermMatrix, profiles$gender)
  #saveRDS(model, paste(dir.models,"txt_gender_model.rds", sep = ""))
  
  return(model)
}

createAgeModel <- function(profiles, documentTermMatrix) {
  library(e1071)
  model <- naiveBayes(documentTermMatrix, profiles$age, laplace = 0.1)
  
  #saveRDS(model, paste(dir.models,"txt_age_model.rds", sep = ""))
  
  return(model)
}

profiles <- read.profiles(path.profile, 1, training.size)
setwd(dir.post)
post <- do.call("rbind",lapply(profiles$userid, FUN = read.post))

bigrams <- createBigramDocumentTermMatrix(profiles, post)
unigrams <- createUnigramDocumentTermMatrix(profiles, post)
freqTerms <- findFreqTerms(unigrams, lowfreq = 100, highfreq = Inf)

bigrams.matrix <- as.matrix(bigrams)
unigrams.matrix <- as.matrix(unigrams)
unigrams.matrix <- unigrams.matrix[, freqTerms]

features <- cbind(unigrams.matrix, bigrams.matrix)

model.age <- createAgeModel(profiles, features)
model.gender <- createGenderModel(profiles, features)

###################### TEST #########################


test.profiles <- read.profiles(path.profile, 7501, 9500)
test.post <- do.call("rbind",lapply(test.profiles$userid, FUN = read.post))
test.bigrams <- createBigramDocumentTermMatrix(test.profiles, test.post)
test.unigrams <- createUnigramDocumentTermMatrix(test.profiles, test.post)

t.unigram.matrix <- as.matrix(test.unigrams)
t.unigram.matrix <- t.unigram.matrix[, freqTerms]
t.bigram.matrix <- as.matrix(test.bigrams)

test.matrix <- cbind(t.unigram.matrix, t.bigram.matrix)

age.results <- predict(model.age, test.matrix)
corrects <- age.results[age.results == test.data$age]
length(corrects)/length(age.results)

gender.results <- predict(model.gender, test.matrix)
corrects <- gender.results[gender.results == test.profiles$gender]
length(corrects)/length(gender.results)

