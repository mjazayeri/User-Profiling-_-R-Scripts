path.liwc <- "/Users/Mehdi/Desktop/ML/Project/Processed Data/LIWC.csv"
likes.path <- "/Users/Mehdi/Desktop/ML/Project/data/training/relation/relation.csv"
dir.models <- "/Users/Mehdi/Desktop/ML/Project/Models/"
trainSet.size <- 7500
  
ope.projection <- c(-83,-84,-85,-86)
neu.projection <- c(-82,-83,-84,-85)
ext.projection <- c(-82,-83,-85,-86)
agr.projection <- c(-82,-83,-84,-86)
con.projection <- c(-82,-84,-85,-86)

read.liwc <- function() {
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
  
  liwc$userId <- NULL
  return(liwc)
}

liwc <- read.liwc()
ope.train <- liwc[1: trainSet.size, ope.projection]
neu.train <- liwc[1: trainSet.size, neu.projection]
ext.train <- liwc[1: trainSet.size, ext.projection]
agr.train <- liwc[1: trainSet.size, agr.projection]
con.train <- liwc[1: trainSet.size, con.projection]

library(randomForest)
forest.ope <- randomForest(ope.train$ope ~ ., ope.train, replace = T, mtry = 28)
# saveRDS(forest.ope, paste(dir.models,"ope_randomForest.rds", sep = ""))
forest.neu <- randomForest(neu.train$neu ~ ., neu.train, replace = T, mtry = 28)
# saveRDS(forest.neu, paste(dir.models,"neu_randomForest.rds", sep = ""))
forest.ext <- randomForest(ext.train$ext ~ ., ext.train, replace = T, mtry = 28)
# saveRDS(forest.ext, paste(dir.models,"ext_randomForest.rds", sep = ""))
forest.agr <- randomForest(agr.train$agr ~ ., agr.train, replace = T, mtry = 28)
# saveRDS(forest.agr, paste(dir.models,"agr_randomForest.rds", sep = ""))
forest.con <- randomForest(con.train$con ~ ., con.train, replace = T, mtry = 28)
# saveRDS(forest.con, paste(dir.models,"con_randomForest.rds", sep = ""))

#baseline : ope:0.65	neu:0.80	ext:0.79	agr:0.66	con:0.73

test.set <- liwc[(trainSet.size + 1) : 9500, ]

predict.ope <- predict(forest.ope, test.set[ ,ope.projection])
ope.errors <- sqrt(sum((predict.ope - test.set$ope) ^ 2) / nrow(test.set))

predict.neu <- predict(forest.neu, test.set[ ,neu.projection])
neu.errors <- sqrt(sum((predict.neu - test.set$neu) ^ 2) / nrow(test.set))

predict.ext <- predict(forest.ext, test.set[ ,ext.projection])
ext.errors <- sqrt(sum((predict.ext - test.set$ext) ^ 2) / nrow(test.set))

predict.agr <- predict(forest.agr, test.set[ ,agr.projection])
agr.errors <- sqrt(sum((predict.agr - test.set$agr) ^ 2) / nrow(test.set))

predict.con <- predict(forest.con, test.set[ ,con.projection])
con.errors <- sqrt(sum((predict.con - test.set$con) ^ 2) / nrow(test.set))


print(var(liwc$ope))
print(var(liwc$con))
print(var(liwc$ext))
print(var(liwc$agr))
print(var(liwc$neu))
print(sd(liwc$ope))
print(sd(liwc$con))
print(sd(liwc$ext))
print(sd(liwc$agr))
print(sd(liwc$neu))
