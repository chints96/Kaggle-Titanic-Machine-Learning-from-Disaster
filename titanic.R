setwd("C:/Users/user/Desktop/Kaggle/titanic")

titanic.train <- read.csv(file = "train.csv", header = TRUE)
titanic.test <- read.csv(file = "test.csv", header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <-NA

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='', "Embarked"] <-'S'

is.na(titanic.full$Age)
table(is.na(titanic.full$Age))

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

# clean missing values of Fare

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), 'Fare'] <- fare.median

#categorical casting

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)


#split dataset backout into train test
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]


titanic.train$Survived <- as.factor(titanic.train$Survived)
Survived.equation <- "Survived ~ Pclass + Sex + Age + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
install.packages("randomForest")
library(randomForest)



titanic.model <- randomForest(formula =Survived.formula, data= titanic.train, ntree =500, mtry= 3, nodesize= 0.01*nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + Parch + Fare + Embarked"
survived <- predict(titanic.model, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- survived


write.csv(output.df,file = "Kaggle_Submission.csv", row.names = FALSE)
