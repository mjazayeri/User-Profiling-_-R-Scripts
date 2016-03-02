postsPath <- "/Users/mehdijazayeri3000/Desktop/ML/Project/data/training/text/"
setwd(postsPath)

#read post files
scan("7267f43c71fcf53f4580fd3cd808bd48.txt", what = "character", sep = "\n")

#data <- read.csv("/Users/mehdijazayeri3000/Desktop/ML/Project/data/training/profile/profile.csv", header = TRUE, nrows = 3)
#str(data)

col.class <-sapply(data, class)
col.class[c(2,4)] <- c("character", "factor")
profPath = "/Users/mehdijazayeri3000/Desktop/ML/Project/data/training/profile/profile.csv"

data <- read.csv(profPath, header = TRUE, colClasses = col.class)

#summary of data frame
str(data)

comments <- rep(vector, 9500)
data <- cbind(data, comments)
words <- scan("7267f43c71fcf53f4580fd3cd808bd48.txt", what = "character", sep = " ")
table(words)



profs <- read.csv(profPath)
test <- profs[1:6,4]
p <- c()
for(i in 1:6) { 
  p <- rbind(p,c(1:10))
}
test <- cbind(test,p)
library(e1071)
test
model <- naiveBayes(test[-2],test[2])
model