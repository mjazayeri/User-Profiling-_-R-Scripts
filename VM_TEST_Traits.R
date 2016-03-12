path.liwc <- "/Users/Mehdi/Desktop/ML/Project/Processed Data/LIWC.csv"
path.nrc <- "/Users/Mehdi/Desktop/ML/Project/Processed Data/nrc.csv"
likes.path <- "/Users/Mehdi/Desktop/ML/Project/data/training/relation/relation.csv"

ope.projection <- c(-83,-84,-85,-86)
neu.projection <- c(-82,-83,-84,-85)
ext.projection <- c(-82,-83,-85,-86)
agr.projection <- c(-82,-83,-84,-86)
con.projection <- c(-82,-84,-85,-86)


read.LIWC_Likes_NRC <- function() {
  col.class <- c("character", rep("numeric",82), "factor", "factor", rep("numeric", 5))
  liwc <- read.csv(path.liwc, header = TRUE, colClasses = col.class)
  liwc$age <- NULL
  liwc$gender <- NULL
  liwc$Seg <- NULL
  
  likes <- read.csv(likes.path, header = T, colClasses = c("numeric","character","numeric"))
  likes.frame <- as.data.frame(table(likes$userid))
  names(likes.frame) <- c("userid", "likesCount")
  likes.frame <- likes.frame[match(liwc$userId, likes.frame$userid), ]
  likesCount <- likes.frame$likesCount
  liwc <- cbind(liwc, likesCount)
  
  col.class <- c("character", rep("numeric", 10))
  nrc <- read.csv(path.nrc, header = T, colClasses = col.class)
  nrc <- nrc[match(liwc$userId, nrc$userId), ]
  nrc$userId <- NULL
  #liwc$userId <- NULL
  liwc_nrc <- cbind(liwc, nrc)
  return(liwc_nrc)
}

liwc <- read.LIWC_Likes_NRC()

forest.ope <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_ope.rds")
forest.neu <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_neu.rds")
forest.ext <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_ext.rds")
forest.agr <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_agr.rds")
forest.con <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_con.rds")

library(randomForest)

getOPEvalues <- function(userId) {
  ope.test <- liwc[liwc$userId == userId, ope.projection]
  ope.value <- predict(forest.ope, ope.test[, -1])
  return(ope.value)
}
getNEUvalues <- function(userId) {
  neu.test <- liwc[liwc$userId == userId, neu.projection]
  neu.value <- predict(forest.neu, ope.test)
  return(neu.value)
}
getEXTvalues <- function(userId) {
  ext.test <- liwc[liwc$userId == userId, ext.projection]
  ext.value <- predict(forest.ext, ope.test)
  return(ext.value)
}
getAGRvalues <- function() {
  agr.test <- liwc[liwc$userId == userId, agr.projection]
  agr.value <- predict(forest.agr, ope.test)
  return(agr.value)
}
getCONvalues <- function() {
  con.test <- liwc[liwc$userId == userId, con.projection]
  con.value <- predict(forest.con, ope.test) 
  return(con.value)
}