library(tm)
library(SnowballC)
library(e1071)
####################_CONSTANTS_####################
postsPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
profPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
modelsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/"
trainingLength <- 7500 #75% of users

####################_METHODS_####################
#reads users' post and changes the class of ID and Gender to character and factor respectively
readUserProfiles <- function() {
  
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profile <- read.csv(profPath, header = TRUE, colClasses = col.class)
  age <- cut(profile$age, 
             breaks = c(-Inf, 24, 34, 49, Inf), 
             labels = c("xx-24", "25-34", "35-49", "50-xx"), 
             right = T)
  profile$age <- age
  return(profile)
}

#read post of a user, filters the post and converts it to lowercase
readUserPost <- function(userId) {
  file.path = paste(postsPath, userId, ".txt", sep="")
  #print(paste(file.path,i))
  file.text <- scan(file.path, what = "character", sep = "\n")
  file.text <- gsub("[[:punct:]]", " ", file.text)
  file.text <- tolower(file.text)
  return(file.text)
}

#couts the frequency of words in a post using bag-of-words
countWordsInVector <- function(bagOfWords, post) {
  counts <- c();
  
  post.tokens <- strsplit(post, " ")[[1]]
  
  filtered <- post.tokens[(post.tokens %in% bagOfWords)]
  
  #to avoid NA values bag-of-words is added and then
  #subtracted from freqTbl
  combined <- c(filtered, bagOfWords)
  freqsTbl <- table(combined) - 1
  
  #count the occurance of the bagOfWord
  tokens <- names(freqsTbl)
  countsTbl <- freqsTbl[match(bagOfWords, tokens)]
  
  return (countsTbl)
}

#generates the naive bayes model to predict gender
createGenderModel <- function(featuresTbl, name) { 
  
  gender <- profiles$gender[1:trainingLength]
  
  gender.training.data <- cbind.data.frame(gender, featuresTbl)
  
  model <- naiveBayes(gender.training.data[,-1], gender.training.data[,1])
  
  saveRDS(model,paste(modelsPath, name, ".rds", sep=""))
  
  return (model)
}

#generates and saves the naive bayes model to predict age
createAgeModel <- function(featuresTbl, name) { 
  
  age <- profiles$age[1:trainingLength]
  
  tdata.age <- cbind.data.frame(age, featuresTbl)
  
  model <- naiveBayes(tdata.age[,-1], tdata.age[,1])
  
  saveRDS(model,paste(modelsPath, name, ".rds", sep = ""))
  
  return (model)
}

classifyByPost <- function(tdv, model) {
  
  tdv <- as.list(tdv)
  return (predict(model, tdv))
}

createDocumentTermMatrix <- function () {
  posts <- c()
  for(i in 1 : 9500) {
    post <- scan(paste(postsPath, profiles$userid[i], ".txt", sep = ""), what = "character", sep = "\n")
    post <- paste(post , collapse = " ")
    post <- iconv(post, "WINDOWS-1252","UTF-8")
    posts <- c(posts, post)
  }
  
  text <- posts
  text.vector <- VectorSource(text)
  text.corpus <- Corpus(text.vector)
  text.corpus <- tm_map(text.corpus, tolower)
  text.corpus <- tm_map(text.corpus, PlainTextDocument)
  text.corpus <- tm_map(text.corpus, removePunctuation)
  text.corpus <- tm_map(text.corpus, removeNumbers)
  text.corpus <- tm_map(text.corpus, removeWords, stopwords(kind= "en"))
  
  
  #text.corpus <- tm_map(text.corpus, stemDocument)
  text.corpus <- tm_map(text.corpus, stripWhitespace)
  doc.term <- DocumentTermMatrix(text.corpus)
  doc.term <- removeSparseTerms(doc.term, sparse = 0.992)
  doc.term.matrix <- as.matrix(doc.term)
  rownames(doc.term.matrix) <- profiles$userid
  return (doc.term.matrix)
}

runTest <- function(from, to) {
  
  ageCorrect = 0
  genderCorrect = 0
  for(i in from:to){#nrow(test.users)) {

    actualGender <- profiles[i, ]$gender
    predictedGender <- classifyByPost(dtm[i, ], gender.model)
    if (actualGender == predictedGender){
      genderCorrect = genderCorrect + 1
    }
    
      actualAge <- profiles[i, ]$age
      predictedAge <- classifyByPost(dtm[i, ], age.model)
      if (actualAge == predictedAge){
        ageCorrect = ageCorrect + 1
      }
      if(i %% 100 == 0) {
        print(i)
      }
  }
  print(ageCorrect)
  print(genderCorrect)
}
########################################

profiles <- readUserProfiles()
profiles.train <- profiles[1:trainingLength, ]

#dtm <- createDocumentTermMatrix();
#write.csv(dtm, paste(modelsPath, "dtm.csv", sep=""))

dtm <- read.csv("/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/dtm.csv")
bow <- colnames(dtm)

gender.model <- createGenderModel(dtm[1:trainingLength, ], "gender_tm_model")
age.model <- createAgeModel(dtm[1:trainingLength, ], "age_tm_model")
runTest(trainingLength+1, 9500)
########################################