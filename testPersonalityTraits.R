col.class <- c("character", rep("numeric",82), "factor", "factor", rep("numeric", 5))
liwc <- read.csv("/Users/Mehdi/Desktop/ML/Project/Processed Data/LIWC.csv", header = TRUE, colClasses = col.class)
liwc$age <- NULL
liwc$gender <- NULL
liwc$Seg <- NULL
likes <- read.csv("/Users/Mehdi/Desktop/ML/Project/data/training/relation/relation.csv", header = T, colClasses = c("numeric","character","numeric"))
likes.frame <- as.data.frame(table(likes$userid))
names(likes.frame) <- c("userid", "likesCount")
likes.frame <- likes.frame[match(liwc$userId, likes.frame$userid), ]
likesCount <- likes.frame$likesCount
liwc <- cbind(liwc, likesCount)
liwc$userId <- NULL
liwc <- liwc[7501:9500, ]

ope.projection <- c(-83,-84,-85,-86)
neu.projection <- c(-82,-83,-84,-85)
ext.projection <- c(-82,-83,-85,-86)
agr.projection <- c(-82,-83,-84,-86)
con.projection <- c(-82,-84,-85,-86)

ope.test <- liwc[, ope.projection]
neu.test <- liwc[, neu.projection]
ext.test <- liwc[, ext.projection]
agr.test <- liwc[, agr.projection]
con.test <- liwc[, con.projection]

tree.ope <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/tree_ope.rds")
tree.neu <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/tree_neu.rds")
tree.ext <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/tree_ext.rds")
tree.agr <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/tree_agr.rds")
tree.con <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/tree_con.rds")

reg.ope <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/regression_ope.rds")
reg.neu <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/regression_neu.rds")
reg.ext <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/regression_ext.rds")
reg.agr <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/regression_agr.rds")
reg.con <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/regression_con.rds")

forest.ope <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_ope.rds")
forest.neu <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_neu.rds")
forest.ext <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_ext.rds")
forest.agr <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_agr.rds")
forest.con <- readRDS("/Users/Mehdi/Desktop/ML/Project/Models/randomForest_con.rds")

library(rpart)
library(randomForest)

ope.tree.error <- sqrt(sum((predict(tree.ope, ope.test) - liwc$ope ) ^ 2) / 2000)
neu.tree.error <- sqrt(sum((predict(tree.neu, ope.test) - liwc$neu ) ^ 2) / 2000)
ext.tree.error <- sqrt(sum((predict(tree.ext, ope.test) - liwc$ext ) ^ 2) / 2000)
agr.tree.error <- sqrt(sum((predict(tree.agr, ope.test) - liwc$agr ) ^ 2) / 2000)
con.tree.error <- sqrt(sum((predict(tree.con, ope.test) - liwc$con ) ^ 2) / 2000)

ope.reg.error <- sqrt(sum((predict(reg.ope, ope.test) - liwc$ope ) ^ 2) / 2000)
neu.reg.error <- sqrt(sum((predict(reg.neu, ope.test) - liwc$neu ) ^ 2) / 2000)
ext.reg.error <- sqrt(sum((predict(reg.ext, ope.test) - liwc$ext ) ^ 2) / 2000)
agr.reg.error <- sqrt(sum((predict(reg.agr, ope.test) - liwc$agr ) ^ 2) / 2000)
con.reg.error <- sqrt(sum((predict(reg.con, ope.test) - liwc$con ) ^ 2) / 2000)

ope.forest.error <- sqrt(sum((predict(forest.ope, ope.test) - liwc$ope ) ^ 2) / 2000)
neu.forest.error <- sqrt(sum((predict(forest.neu, ope.test) - liwc$neu ) ^ 2) / 2000)
ext.forest.error <- sqrt(sum((predict(forest.ext, ope.test) - liwc$ext ) ^ 2) / 2000)
agr.forest.error <- sqrt(sum((predict(forest.agr, ope.test) - liwc$agr ) ^ 2) / 2000)
con.forest.error <- sqrt(sum((predict(forest.con, ope.test) - liwc$con ) ^ 2) / 2000)

result <- cbind(ope.tree.error, neu.tree.error, ext.tree.error, agr.tree.error, con.tree.error)
result <- rbind(result, cbind(ope.reg.error, neu.reg.error, ext.reg.error, agr.reg.error, con.reg.error))
result <- rbind(result, cbind(ope.forest.error, neu.forest.error, ext.forest.error, agr.forest.error, con.forest.error))