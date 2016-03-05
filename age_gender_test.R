library(e1071)
dir.model <- "/Users/Mehdi/Desktop/ML/Project/Models/"
path.user_term <- "/Users/Mehdi/Desktop/ML/Project/Processed Data/user_term.csv"

user_term <- read.csv(path.user_term)
test_user_term <- user_term[7501:9500, ]

model.age <- readRDS(paste(dir.model, "svm_age.rds", sep = ""))

age.result <- predict(model.age, test_user_term[, c(-1,-2,-3,-4)])
age.correct <- age.result[age.result == test_user_term$age]
length(age.correct)/length(age.result)

model.gender <- readRDS(paste(dir.model, "svm_gender.rds", sep = ""))

gender.result <- predict(model.gender, test_user_term[, c(-1,-2,-3,-4)])
gender.correct <- gender.result[gender.result == test_user_term$gender]
length(gender.correct)/length(gender.result)
