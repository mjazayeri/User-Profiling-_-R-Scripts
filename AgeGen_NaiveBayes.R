
####################_CONSTANTS_####################
postsPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/text/"
profPath <- "/Users/Mehdi/Desktop/ML/Project/data/training/profile/profile.csv"
bagOfWordsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/bow.txt"
modelsPath <- "/Users/Mehdi/Desktop/ML/Project/Profiling-R-Script/"
trainingLength <- 7500 #75% of users

####################_FUNCTIONS_####################

#reads users' post and changes the class of ID and Gender to character and factor respectively
readUserProfiles <- function() {
  
  col.class <- c("integer","character", "numeric", "factor", rep("numeric",5))
  profile <- read.csv(profPath, header = TRUE, colClasses = col.class)
  
  return(profile)
}

#reads LIWC, sorts it to match user profiles and removes irrelevant columns
readLIWC <- function (profiles) {
  data <- read.csv(paste(modelsPath, "LIWC.csv", sep = ""), header=T)
  # frequencyTable <- read.csv(paste(modelsPath,"frequencyTable.csv",sep=""))
  
  #order the liwc to match with userProfile 
  data <- data[match(profiles$userid, data$userId), ]
  
  #remove repeatitive and irrelevant columns 
  col.to.remove <- colnames(profiles)
  col.to.remove <- c(col.to.remove, "userId")
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
countWordsInVector_old <- function(post) {
  counts <- c();
  
  post.tokens <- strsplit(post, " ")[[1]]
  
  #changes the token vector of post to frequency table of the post
  tokens.table <- table(post.tokens)
  
  #count the occurance of the bagOfWord
  tokens <- names(tokens.table)
  for(i in 1 : length(bagOfWords)) {
    if( bagOfWords[i] %in% tokens)
      counts[i] <- tokens.table[ names(tokens.table)==bagOfWords[i] ]
    else
      counts[i] <- 0
  }
  return(counts)
}

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

#generates the frequency table of all posts and saves a copy in model's folder
createFrequencyTable <- function (userIds) {

  featuresCount <- c()
  for(i in 1 : length(userIds)) {
    post <- readUserPost(userIds[i])
    counts <- countWordsInVector(wordsBag, post)
    featuresCount <- rbind(featuresCount, counts)
  }
  
  colnames(featuresCount) <- wordsBag
  #rownames(featuresCount) <- profiles[,2]
  
  write.csv(featuresCount, paste(modelsPath, "frequencyTable.csv" ,sep = ""))
  return (featuresCount)
}

#generates the naive bayes model to predict gender
createGenderModel <- function(featuresTbl, name) { 
  
  gender <- user.profiles$gender
  
  gender.training.data <- cbind.data.frame(gender, featuresTbl)
  
  #gender.training.data$gender <- as.factor(gender.training.data$gender)
  model <- naiveBayes(gender.training.data[,-1], gender.training.data[,1])
  
  saveRDS(model,paste(modelsPath, name, ".rds", sep=""))
  
  return (model)
}

#generates and saves the naive bayes model to predict age
createAgeModel <- function(featuresTbl, name) { 
  
  age <- cut(user.profiles$age, 
             breaks = c(-Inf, 24, 34, 49, Inf), 
             labels = c("xx-24", "25-34", "35-49", "50-xx"), 
             right = T)
  tdata.age <- cbind.data.frame(age, featuresTbl)
  model <- naiveBayes(tdata.age[,-1], tdata.age[,1])
  
  saveRDS(model,paste(modelsPath, name, ".rds", sep = ""))
  
  return (model)
}

classifyByPost <- function(filePath, model) {
  post <- scan(filePath, what = "character", sep = "\n")
  post <- gsub("[[:punct:]]", " ", post)
  post <- tolower(post)
  
  counts <- countWordsInVector(wordsBag, post)
  countLst <- as.list(counts)
  names(countLst) <- wordsBag
  
  return (predict(model, countLst))
}
runTest <- function(from, to, gender.model, age.model) {
  
  ageCorrect = 0
  genderCorrect = 0
  for(i in from:to){#nrow(test.users)) {
    sample <- profiles[i,]
    path <- paste(postsPath, sample$userid, ".txt", sep = "")
    actualGender <- profiles[i, ]$gender
    predictedGender <- classifyByPost(path, gender.model)
    if (actualGender == predictedGender){
      genderCorrect = genderCorrect + 1
    }
    
    actualAge <- profiles[i, ]$age
    predictedAge <- classifyByPost(path, age.model)
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
####################_MAIN PART OF SCRIPT_####################


library(e1071)
wordsBag <- scan(bagOfWordsPath, what="character", sep="\n")


user.profiles <- readUserProfiles()[1:trainingLength, ]
frequencyTable <- createFrequencyTable(user.profiles$userid)

#frequencyTable <- read.csv(paste(modelsPath,"frequencyTable.csv",sep=""))


genderModel <- createGenderModel(frequencyTable, "gender_mixed_model")
ageModel <- createAgeModel(frequencyTable, "age_mixed_model")
profiles <- readUserProfiles()
runTest(trainingLength + 1, 8500, gender.model = genderModel, age.model = ageModel)

