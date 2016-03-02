
path.profile <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
dir.post <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
dir.models <- "/Users/Mehdi/Desktop/ML/Project/Models/"

fold.size <- 3166
fold_1.start <- 1
fold_2.start <- fold_1.start + fold.size + 1
fold_3.start <- fold_2.start + fold.size + 1
####################### MAIN #######################

read.profiles <- function(profilesPath, start, end) {
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profiles <- read.csv(path.profile, header = T, colClasses = col.class)[start : end, ]
#   age <- cut(profiles$age, 
#              breaks = c(-Inf, 24, 34, 49, Inf), 
#              labels = c("xx-24", "25-34", "35-49", "50-xx"), 
#              right = T)
  age <- cut(profiles$age, 
             breaks = c(-Inf, 24, 34, 49, Inf), 
             labels = c("A", "B", "C", "D"), 
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
  post.corpus <- tm_map(post.corpus, stripWhitespace)
  post.corpus <- tm_map(post.corpus, removeWords, stopwords(kind = "en"))
  post.corpus <- tm_map(post.corpus, stemDocument)
  
  DTM <- DocumentTermMatrix(post.corpus)
  return(DTM)
}

createGenderModel <- function(profiles, documentTermMatrix, name) {
  
  user_post <- cbind.data.frame(profiles, documentTermMatrix)
  library(e1071)
  model <- naiveBayes(user_post[, c(-1,-2,-3)], user_post$gender)
  saveRDS(model, paste(dir.models, name, ".rds", sep = ""))
  
  return(model)
}

createAgeModel <- function(profiles, documentTermMatrix, name) {
  user_post <- cbind.data.frame(profiles, documentTermMatrix)
  
  library(e1071)
  model <- naiveBayes(user_post[, c(-1,-2,-3)], user_post$age)
  
  saveRDS(model, paste(dir.models, name, ".rds", sep = ""))
  
  return(model)
}

profiles <- read.profiles(path.profile, 1, 9500)
DTM <- createDocumentTerm(profiles, dir.post)
term.matrix <- as.matrix(DTM)
freqTerms <- findFreqTerms(DTM, lowfreq = 70, highfreq = Inf)
term.matrix <- term.matrix[, freqTerms]

#fold 1 metrix
fold1.term.matrix <- term.matrix[fold_1.start : (fold_1.start + fold.size), ]
profiles1 <- profiles[fold_1.start : (fold_1.start + fold.size), ]

#fold 2 term.matrix
fold2.term.matrix <- term.matrix[fold_2.start : (fold_2.start + fold.size), ]
profiles2 <- profiles[fold_2.start : (fold_2.start + fold.size), ]

#fold 3 term.matrix
fold3.term.matrix <- term.matrix[fold_3.start : 9500, ]
profiles3 <- profiles[fold_3.start : 9500, ]


#create fold 1,2 model
fold12.term.matrix <- rbind(fold1.term.matrix, fold2.term.matrix)
fold12.profiles <- rbind(profiles1, profiles2)
model12.gender <- createGenderModel(fold12.profiles, fold12.term.matrix, "gender_fold12")
model12.age <- createAgeModel(fold12.profiles, fold12.term.matrix, "age_fold12")

#create fold 2,3 model
fold23.term.matrix <- rbind(fold2.term.matrix, fold3.term.matrix)
fold23.profiles <- rbind(profiles2, profiles3)
model23.gender <- createGenderModel(fold23.profiles, fold23.term.matrix, "gender_fold23")
model23.age <- createAgeModel(fold23.profiles, fold23.term.matrix, "age_fold23")

#create fold 1,3
fold13.term.matrix <- rbind(fold1.term.matrix, fold3.term.matrix)
fold13.profiles <- rbind(profiles1, profiles3)
model13.gender <- createGenderModel(fold13.profiles, fold13.term.matrix, "gender_fold13")
model13.age <- createAgeModel(fold13.profiles, fold13.term.matrix, "age_fold13")


###################### TEST #########################

#test gender fold12 model
gender12.results <- predict(model12.gender, fold3.term.matrix)
corrects12 <- gender12.results[gender12.results == profiles3$gender]
length(corrects12)/length(gender12.results)

#test gender fold23 model
gender23.results <- predict(model23.gender, fold1.term.matrix)
corrects23 <- gender23.results[gender23.results == profiles1$gender]
length(corrects23)/length(gender23.results)

#test gender fold13 model
gender13.results <- predict(model13.gender, fold2.term.matrix)
corrects13 <- gender13.results[gender13.results == profiles2$gender]
length(corrects13)/length(gender13.results)

#----------------------------------------------------------------------------------

#test age fold12 model
age12.results <- predict(model12.age, fold3.term.matrix)
corrects12 <- age12.results[age12.results == profiles3$age]
length(corrects12)/length(age12.results)

#test age fold23 model
age23.results <- predict(model23.age, fold1.term.matrix)
corrects23 <- age23.results[age23.results == profiles1$age]
length(corrects23)/length(age23.results)

#test age fold13 model
age13.results <- predict(model13.age, fold2.term.matrix)
corrects13 <- age13.results[age13.results == profiles2$age]
length(corrects13)/length(age13.results)