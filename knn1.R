#Read the training and test dataset
library(readr)
training <- read_csv("E:/SUBJECTS/569 MATH SL S17--/training.txt", 
                     col_names = FALSE)
View(training)
test <- read_csv("E:/SUBJECTS/569 MATH SL S17--/test.txt", 
                 col_names = FALSE)
View(test)

#Shuffling training dataset
set.seed(1234)
training<-training[sample(nrow(training)),]
View(training)

#Normalizing the first two columns
tr_nor<-as.data.frame(lapply(training[,c(1,2)],normalize))
View(tr_nor)
test<-as.data.frame(lapply(test[,c(1,2)],normalize))
View(test)

#Prepping attributes for feeding the classifier
train<-tr_nor
train$X3<-training$X3

#for k=5
predictions<-NULL
K = 5
predictions <- knn_predict(test, train,train$X3,K) #calling knn_predict()
#Results
test$X3 <- predictions #Adding predictions in test data as 7th column
ggplot(test,aes(X1,X2,colour=X3))+
geom_point()

#for k=10
test<-as.data.frame(lapply(test[,c(1,2)],normalize))
test$X3<-NULL
predictions<-NULL
K = 10
predictions <- knn_predict(test, train,train$X3,K) #calling knn_predict()
#Results
test$X3 <- predictions #Adding predictions in test data as 7th column
ggplot(test,aes(X1,X2,colour=X3))+
  geom_point()

#for k=15
predictions<-NULL
K = 15
predictions <- knn_predict(test, train,train$X3,K) #calling knn_predict()
#Results
test$X3<-NULL
test$X3 <- predictions #Adding predictions in test data as 7th column
ggplot(test,aes(X1,X2,colour=X3))+
  geom_point()
