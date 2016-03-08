######################## CONSTANTS ########################

liwc.path <- "/Users/Mehdi/Desktop/ML/Project/Processed Data/LIWC.csv"
likes.path <- "/Users/Mehdi/Desktop/ML/Project/data/training/relation/relation.csv"
dir.models <- "/Users/Mehdi/Desktop/ML/Project/Models/"
train.length <- 7500

######################## TRAINING ########################

# read liwc file and adds Number of Likes column to it
# filters irrelevant columns : age, gender, userId, Seg
read.liwc <- function() {
  col.class <- c("character", rep("numeric",82), "factor", "factor", rep("numeric", 5))
  liwc <- read.csv(liwc.path, header = TRUE, colClasses = col.class)
  liwc$age <- NULL
  liwc$gender <- NULL
  liwc$Seg <- NULL
  
  likes <- read.csv(likes.path, header = T, colClasses = c("numeric","character","numeric"))
  likes.frame <- as.data.frame(table(likes$userid))
  names(likes.frame) <- c("userid", "likesCount")
  
  #order the likes to match liwc rows
  likes.frame <- likes.frame[match(liwc$userId, likes.frame$userid), ]
  likesCount <- likes.frame$likesCount
  liwc <- cbind(liwc, likesCount)
  
  liwc$userId <- NULL
  return(liwc)
}

library(rpart)
liwc <- read.liwc()

#a grow control for regression tree
ctrl <- rpart.control(minsplit=2, minbucket=1, cp=0.0001)

# create regression/tree based on ope
liwc.ope <- liwc[1:train.length, c(-83,-84,-85,-86)]
reg.ope <- lm(formula = liwc.ope$ope~., liwc.ope)
fit.ope <- rpart(liwc.ope$ope ~., liwc.ope, method = "anova", control = ctrl)
pfit.ope <- prune(fit.ope, cp = fit.ope$cptable[which.min(fit.ope$cptable[,"xerror"]),"CP"])
saveRDS(reg.ope, paste(dir.models,"regression_ope.rds", sep = ""))
saveRDS(pfit.ope, paste(dir.models,"tree_ope.rds", sep = ""))

# create regression/tree based on neu
liwc.neu <- liwc[1:train.length, c(-82,-83,-84,-85)]
reg.neu <- lm(formula = liwc.neu$neu~., liwc.neu)
fit.neu <- rpart(liwc.neu$neu ~., liwc.neu, control = ctrl)
pfit.neu <- prune(fit.neu, cp = fit.neu$cptable[which.min(fit.neu$cptable[,"xerror"]),"CP"])
saveRDS(reg.neu, paste(dir.models,"regression_neu.rds", sep = ""))
saveRDS(pfit.neu, paste(dir.models,"tree_neu.rds", sep = ""))

# create regression/tree based on ext
liwc.ext <- liwc[1:train.length, c(-82,-83,-85,-86)]
reg.ext <- lm(formula = liwc.ext$ext~., liwc.ext)
fit.ext <- rpart(liwc.ext$ext ~., liwc.ext, control= ctrl)
pfit.ext <- prune(fit.ext, cp = fit.ext$cptable[which.min(fit.ext$cptable[,"xerror"]),"CP"])
saveRDS(reg.ext, paste(dir.models,"regression_ext.rds", sep = ""))
saveRDS(pfit.ext, paste(dir.models,"tree_ext.rds", sep = ""))

# create regression/tree based on agr
liwc.agr <- liwc[1:train.length, c(-82,-83,-84,-86)]
reg.agr <- lm(liwc.agr$agr~., liwc.agr)
fit.agr <- rpart(liwc.agr$agr ~., liwc.agr, control = ctrl)
pfit.agr <- prune(fit.agr, cp = fit.agr$cptable[which.min(fit.agr$cptable[,"xerror"]),"CP"])
saveRDS(reg.agr, paste(dir.models,"regression_agr.rds", sep = ""))
saveRDS(pfit.agr, paste(dir.models,"tree_agr.rds", sep = ""))

# create regression/tree based on con
liwc.con <- liwc[1:train.length, c(-82,-84,-85,-86)]
reg.con <- lm(formula = liwc.con$con~., liwc.con)
fit.con <- rpart(liwc.con$con ~., liwc.con, control = ctrl)
pfit.con <- prune(fit.con, cp = fit.con$cptable[which.min(fit.con$cptable[,"xerror"]),"CP"])
saveRDS(reg.con, paste(dir.models,"regression_con.rds", sep = ""))
saveRDS(pfit.con, paste(dir.models,"tree_con.rds", sep = ""))

####################### TEST #######################
str <- 7001
end <- 9500

# sample.ope <- liwc[str:end, c(-83,-84,-85,-86)]
sample.ope <- liwc[str:end, c(-82,-83,-84,-85,-86)]
reg.errors <- sqrt(sum((predict(reg.ope, sample.ope) - liwc$ope[str:end] ) ^ 2) / (end - str))
fit.errors <- sqrt(sum((predict(pfit.ope, sample.ope) - liwc$ope[str:end] ) ^ 2) / (end - str))
print(paste("REG",reg.errors, "TREE", fit.errors, collapse = " "))

# sample.neu <- liwc[str:end, c(-82,-83,-84,-85)]
sample.neu <- liwc[str:end, c(-82,-83,-84,-85,-86)]
reg.errors <- sqrt(sum((predict(reg.neu, sample.neu) - liwc$neu[str:end] ) ^ 2) / (end - str))
fit.errors <- sqrt(sum((predict(pfit.neu, sample.neu) - liwc$neu[str:end] ) ^ 2) / (end - str))
print(paste("REG",reg.errors, "TREE", fit.errors, collapse = " "))

# sample.ext <- liwc[str:end, c(-82,-83,-85,-86)]
sample.ext <- liwc[str:end, c(-82,-83,-84,-85,-86)]
reg.errors <- sqrt(sum((predict(reg.ext, sample.ext) - liwc$ext[str:end] ) ^ 2) / (end - str))
fit.errors <- sqrt(sum((predict(pfit.ext, sample.ext) - liwc$ext[str:end] ) ^ 2) / (end - str))
print(paste("REG",reg.errors, "TREE", fit.errors, collapse = " "))

# sample.agr <- liwc[str:end, c(-82,-83,-84,-86)]
sample.agr <- liwc[str:end, c(-82,-83,-84,-85,-86)]
reg.errors <- sqrt(sum((predict(reg.agr, sample.agr) - liwc$agr[str:end] ) ^ 2) / (end - str))
fit.errors <- sqrt(sum((predict(pfit.agr, sample.agr) - liwc$agr[str:end] ) ^ 2) / (end - str))
print(paste("REG",reg.errors, "TREE", fit.errors, collapse = " "))

# sample.con <- liwc[str:end, c(-82,-84,-85,-86)]
sample.con <- liwc[str:end, c(-82,-83,-84,-85,-86)]
reg.errors <- sqrt(sum((predict(pfit.con, sample.con) - liwc$con[str:end] ) ^ 2) / (end - str))
fit.errors <- sqrt(sum((predict(pfit.con, sample.con) - liwc$con[str:end] ) ^ 2) / (end - str))
print(paste("REG",reg.errors, "TREE", fit.errors, collapse = " "))