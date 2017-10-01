#Read semester 3 results.
S3 <- read.csv("S3.csv", header = TRUE)
View(S3)
str(S3)
summary(S3)

#Read semester 4 results.
S4 <- read.csv("S4.csv", header = TRUE)
View(S4)
table(S4$Total)
S4$Total <- S4$X10CS42_3 + S4$X10CS43_3 + S4$X10CS44_3 + S4$X10CS45_3 + S4$X10CS46_3 + S4$X10CSL47_3 + S4$X10CSL48_3 + S4$X10CS41_3
View(S4)
table(S4$Total)
str(S4)
summary(S4)

#Form data in the required format with P/F.
sem3 <- S3[-27] 
sem4 <- S4[-27]
View(sem3)
View(sem4)
sem3$Result <- ifelse((sem3$X10CS32_2 > 34 & sem3$X10CS33_2 > 34 & sem3$X10CS34_2 > 34 & sem3$X10CS35_2 > 34 & sem3$X10CS31_2 > 34 & sem3$X10CS36_2 > 34 & sem3$X10CS32_1 > 14 & sem3$X10CS33_1 > 14 & sem3$X10CS34_1 > 14 & sem3$X10CS35_1 > 14 & sem3$X10CS31_1 > 14 & sem3$X10CS36_1 > 14) ,"P", "F")
table(sem3$Result)
sem4$Result <- ifelse((sem4$X10CS42_2 > 34 & sem4$X10CS43_2 > 34 & sem4$X10CS44_2 > 34 & sem4$X10CS45_2 > 34 & sem4$X10CS41_2 > 34 & sem4$X10CS46_2 > 34 & sem4$X10CS42_1 > 14 & sem4$X10CS43_1 > 14 & sem4$X10CS44_1 > 14 & sem4$X10CS45_1 > 14 & sem4$X10CS41_1 > 14 & sem4$X10CS46_1 > 14) ,"P", "F")
table(sem4$Result)

#Factor the Result column.
str(sem3$Result)
sem3$Result <- as.factor(sem3$Result)
sem4$Result <- as.factor(sem4$Result)
str(sem3$Result)

#Form the required training sets.
x_train <- sem3[c("X10CS31_3","X10CS32_3","X10CS33_3","X10CS34_3","X10CS35_3","X10CS36_3")]
y_train <- sem3$Result
data_train <- cbind(x_train,y_train)

#Form the required testing set.
x_test <- sem4[c("X10CS41_1","X10CS42_1","X10CS43_1","X10CS44_1","X10CS45_1","X10CS46_1")]

#Change row names to match the training set.
colnames(x_test)[colnames(x_test)=="X10CS41_1"] <- "X10CS31_1"
colnames(x_test)[colnames(x_test)=="X10CS42_1"] <- "X10CS32_1"
colnames(x_test)[colnames(x_test)=="X10CS43_1"] <- "X10CS33_1"
colnames(x_test)[colnames(x_test)=="X10CS44_1"] <- "X10CS34_1"
colnames(x_test)[colnames(x_test)=="X10CS45_1"] <- "X10CS35_1"
colnames(x_test)[colnames(x_test)=="X10CS46_1"] <- "X10CS36_1"

View(x_test)
View(x_train)

#Prepare a list of actual output.
y_actual <- sem4$Result

#Start training the models.

#Support Vector Machine:
library(e1071)
#fine tuning algorithms.
fit_svm <- svm(y_train ~ ., data = data_train, cost = 2^(2:8), kernel = "linear")
summary(fit_svm)
y_test_svm = predict(fit_svm, x_test)
result_svm = ifelse(y_actual == y_test_svm, 1,0)
table(result_svm)

#Naive Bayes:
library(e1071)
fit_nb <- naiveBayes(y_train ~ ., data = data_train)
summary(fit_nb)
y_test_nb = predict(fit_nb, x_test)
result_nb = ifelse(y_actual == y_test_nb, 1,0)
table(result_nb)

#Random Forest:
library(randomForest)
fit_rf <- randomForest(y_train ~ ., data_train,ntree=500)
summary(fit_rf)
y_test_rf = predict(fit_rf, x_test)
result_rf = ifelse(y_actual == y_test_rf,1,0)
table(result_rf)
#Gradient Boosting:
#library(caret)
#fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)
#fit_gb <- train(y_train ~ ., data = data_train, method = "gbm", trControl = fitControl,verbose = FALSE)
#summary(fit_gb)
#y_test_gb = predict(fit_gb,x_test)
#result_gb = ifelse(y_actual == y_test_gb, 1,0)

#Compute Results:
table(result_svm)
table(result_nb)
table(result_rf)
#table(result_gb)

#Visualizing the results:
#my_data1 <- data.frame(as.integer(y_actual), as.integer(y_test_gb))
#colnames(my_data1) <- c("Actual Results", "Gradient Boosting")

sem3_results <- sem3$Result

my_data2 <- data.frame(as.integer(y_actual), as.integer(y_test_nb))
colnames(my_data2) <- c("Actual Results", "Naive Bayes")

my_data3 <- data.frame(as.integer(y_actual), as.integer(y_test_rf))
colnames(my_data3) <- c("Actual Results", "Random Forest")

my_data4 <- data.frame(as.integer(sem3_results), as.integer(y_actual), as.integer(y_test_svm))
colnames(my_data4) <- c("Sem3 Actual Result", "Sem4 Actual Results", "SVM sem4 Pred")

#Plotting:
#barplot(as.matrix(my_data1), main="Results Comparision", ylab="Result", beside=TRUE)

barplot(as.matrix(my_data2), main="Results Comparision", ylab="Result", beside=TRUE)

barplot(as.matrix(my_data3), main="Results Comparision", ylab="Result", beside=TRUE)

barplot(as.matrix(my_data4), main="Results Comparision", ylab="Result", beside=TRUE)
