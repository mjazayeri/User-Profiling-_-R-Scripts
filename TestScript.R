
####################_CONSTANTS_####################
postsPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
profPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
modelsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/"
bagOfWordsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/bagOfWords.txt"
trainingLength <- 7000 #75% of users

####################_FUNCTIONS_####################

#reads users' post and changes the class of ID and Gender to character and factor respectively
readUserProfiles <- function() {
  
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profile <- read.csv(profPath, header = TRUE, colClasses = col.class)
  
  return(profile)
}

#reads LIWC, sorts it to match user profiles and removes irrelevant columns
readLIWC <- function (path, profiles) {
  data <- read.csv(path, header=T)
  # frequencyTable <- read.csv(paste(modelsPath,"frequencyTable.csv",sep=""))
  
  #order the liwc to match with userProfile 
  data <- data[match(profiles$userid, data$userId), ]
  
  #remove repeatitive and irrelevant columns 
  col.to.remove <- colnames(profiles)
  data <- data[, !(names(data) %in% col.to.remove)]
  
  return (data)
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
  countsTbl <- freqsTbl[match(wordsBag, tokens)]
  
  return (countsTbl)
}

#classifies a user given file path of user's post and a predictive model
classifyByPost <- function(filePath, model) {
  post <- scan(filePath, what = "character", sep = "\n")
  post <- gsub("[[:punct:]]", " ", post)
  post <- tolower(post)
  
  counts <- countWordsInVector(wordsBag, post)
  countLst <- as.list(counts)
  names(countLst) <- wordsBag
  
  return (predict(model, countLst))
}

classifyByLIWC <- function(userId, model) {
  
  liwc <- liwcTable[liwcTable$userId == userId, ]
  return(predict(model, liwc))
}

####################_MAIN PART OF SCRIPT_####################

library(e1071)

#readModels from file
# ageModel <- readRDS("/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/age_mixed_model.rds")
# genderModel <- readRDS("/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/gender_mixed_model.rds")
# wordsBag <- scan(bagOfWordsPath, what = "character", sep="\n")
test.users <- readUserProfiles()[7001:9500, ]

liwc.path <- paste(modelsPath, "LIWC.csv", sep = "")
liwcTable <- readLIWC(test.users)

ageCorrect = 0
genderCorrect = 0
for(i in 1:nrow(test.users)) {
  sample <- test.users[i,]
  
  #use the path where users' posts are located
  path <- paste(postsPath, sample$userid, ".txt", sep = "")
  
  actualAge <- sample$age
  predictedAge <- classifyByPost(path, ageModel)
  print(paste(actualAge, " ", predictedAge))
  if (actualAge == pridictedAge){
    ageCorrect = ageCorrect + 1
  }
  
  actualGender <- sample$gender
  predictedGender <- classifyByPost(path, genderModel)
  if (actualGender == predictedGender){
    genderCorrect = genderCorrect + 1
  }
}
print(ageCorrect)
print(genderCorrect)

#####################################################