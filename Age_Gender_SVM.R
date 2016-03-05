library(e1071)
dir.models <- "/Users/Mehdi/Desktop/ML/Project/Models/"

user_term <- read.csv("/Users/Mehdi/Desktop/ML/Project/Processed Data/user_term.csv", header = T)
user_term$userid <- as.character(user_term$userid)
user_term$gender <- as.factor(user_term$gender)

train_user_term <- user_term[1:7500, ]

age.svm <- svm(train_user_term[, c(-1,-2,-3,-4)], train_user_term$age)
saveRDS(age.svm, paste(dir.models,"svm_age.rds", sep = ""))


gender.svm <- svm(train_user_term[, c(-1,-2,-3,-4)], train_user_term$gender)
saveRDS(gender.svm, paste(dir.models,"svm_gender.rds", sep = ""))
