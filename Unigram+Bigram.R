
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
  post <- iconv(post, "WINDOWS-1252","UTF-8")
  return (post)
}

corpusToDataframe <- function(corpus) { 
  df <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)  
}

BigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

createDocumentBigrams <- function(profiles, postsDirectory) {
  
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
  
  DTM <- DocumentTermMatrix(post.corpus, control = list(tokenize = BigramTokenizer))
  DTM <- removeSparseTerms(DTM, sparse = 0.997)
  return(DTM)
}
createDocumentUnigram <- function(profiles, postsDirectory) {
  setwd(postsDirectory)
  
  
  library(NLP)
  library(tm)
  library(SnowballC)
  
  post.corpus <- Corpus(VectorSource(post))
  post.corpus <- tm_map(post.corpus, content_transformer(tolower))
  post.corpus <- tm_map(post.corpus, PlainTextDocument)
  post.corpus <- tm_map(post.corpus, removeNumbers)
  post.corpus <- tm_map(post.corpus, stripWhitespace)
  post.corpus <- tm_map(post.corpus, removeWords, stopwords(kind = "en"))
  post.corpus <- tm_map(post.corpus, stemDocument)
  
  DTM <- DocumentTermMatrix(post.corpus)
  return(DTM)
}
createGenderModel <- function(profiles, documentTermMatrix) {
  
  user_post <- cbind.data.frame(profiles, documentTermMatrix)
  library(e1071)
  model <- naiveBayes(user_post[, c(-1,-2,-3)], user_post$gender)
  saveRDS(model, paste(dir.models,"txt_gender_model.rds", sep = ""))
  
  return(model)
}


profiles <- read.profiles(path.profile, 1, training.size)
setwd(dir.post)
post <- do.call("rbind",lapply(profiles$userid, FUN = read.post))

unigrams <- createDocumentUnigram(profile, dir.post)
unigrams.matrix <- as.matrix(unigrams)


freqTerms <- findFreqTerms(unigrams, lowfreq = 500, highfreq = Inf)

unigrams.matrix <- unigrams.matrix[, freqTerms]

bigrams <- createDocumentBigrams(profiles, dir.post)
bigrams.matrix <- as.matrix(bigrams)

features <- cbind(bigrams.matrix, unigrams.matrix)

model.gender <- createGenderModel(profiles, features)

###################### TEST #########################

t.profiles <- read.profiles(path.profile, 7501, 9500)
post <- do.call("rbind",lapply(t.profiles$userid, FUN = read.post))

t.bigrm <- createDocumentBigrams(t.profiles, dir.post)
t.bigrm.mrtx <- as.matrix(t.bigrm)

t.unigrm <- createDocumentUnigram(t.profiles, dir.post)
t.unigrm.mrtx <- as.matrix(t.unigrm)


t.features <- cbind(t.bigrm.mrtx, t.unigrm.mrtx)

gender.results <- predict(model.gender, t.features)
corrects <- gender.results[gender.results == t.profiles$gender]
length(corrects)/length(gender.results)