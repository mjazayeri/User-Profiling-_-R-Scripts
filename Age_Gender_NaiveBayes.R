
#path.profile <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
path.profile <- "/Users/Mehdi/Desktop/ML/Project/data/public-test-data/profile/profile.csv"
#dir.post <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
dir.post <- "/Users/Mehdi/Desktop/ML/Project/data/public-test-data/text"
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

read.profiles <- function(profilesPath) {
  profiles <- read.csv(path.profile, header = T)
  profiles$age <- as.numeric(profiles$age)
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

corpusToDataframe <- function(corpus) { 
  df <-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                  stringsAsFactors=F)  
}

createDocumentTerm <- function(profiles, postsDirectory) {
  setwd(postsDirectory)
  
  post <- do.call("rbind",lapply(profiles$userid, FUN = read.post))
  
  library(NLP)
  library(tm)
  library(SnowballC)
  
  post.corpus <- Corpus(VectorSource(post))
  post.corpus <- tm_map(post.corpus, content_transformer(tolower))
  post.corpus <- tm_map(post.corpus, PlainTextDocument)
  post.corpus <- tm_map(post.corpus, removeNumbers)
  #post.corpus <- tm_map(post.corpus, removePunctuation)
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

createAgeModel <- function(profiles, documentTermMatrix) {
  user_post <- cbind.data.frame(profiles, documentTermMatrix)
  
  library(e1071)
  model <- naiveBayes(user_post[, c(-1,-2,-3)], user_post$age, laplace = 0.1)
  
  saveRDS(model, paste(dir.models,"txt_age_model.rds", sep = ""))
  
  return(model)
}

profiles <- read.profiles(path.profile)
DTM <- createDocumentTerm(profiles, dir.post)
termMatrix <- as.matrix(DTM)
#freqTerms <- findFreqTerms(DTM, lowfreq = 100, highfreq = Inf)
freqTerms <- read.csv("/Users/Mehdi/Desktop/ML/Project/Processed Data/freqTerms.csv")[,2]

foundTermsIndex <- colnames(termMatrix) %in% freqTerms
features <- termMatrix[, foundTermsIndex]
featuresName <- colnames(features)
missingTermsIndex <- !(freqTerms %in% featuresName)
missingTerms <- featuresName[missingTermsIndex]

missingMatrix <- matrix(0, nrow(termMatrix), length(missingTerms))
colnames(missingMatrix) <- missingTerms

features <- cbind(features, missingMatrix)

features <- termMatrix[, freqTerms]
user_term <- cbind.data.frame(profiles, features)
write.csv(user_term, "/Users/Mehdi/Desktop/ML/Project/Processed Data/user_term.csv")


model.age <- createAgeModel(profiles, features)
model.gender <- createGenderModel(profiles, features)
#rm(DTM, matrix, user_post, post.corpus, post, profiles)
###################### TEST #########################



test.data <- read.profiles(path.profile, 7501, 9500)
test.dtm <- createDocumentTerm(test.data, dir.post)
test.matrix <- as.matrix(test.dtm)

test.matrix <- test.matrix[, names(model.age$tables)]

age.results <- predict(model.age, test.matrix)
corrects <- age.results[age.results == test.data$age]
length(corrects)/length(age.results)

gender.results <- predict(model.gender, test.matrix)
corrects <- gender.results[gender.results == test.data$gender]
length(corrects)/length(gender.results)
