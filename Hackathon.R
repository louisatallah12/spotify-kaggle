data_train = read.csv("data.csv")
test =  read.csv("test.csv")

str(data_train)
head(data_train)

#on applique pca sur le train set entier
data_train = data_train[,1:14]
pca = princomp(data_train[,-14], cor = T)
plot(pca, type = "l")
biplot(pca)

library(factoextra)
fviz_pca_var(
  pca,
  col.var = "contrib", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE     
)
fviz_contrib(pca, choice = "var", axes = 1)

# loudness - energy - acousticness

library(caTools) 
set.seed(703185)
split = sample.split(data_train, SplitRatio = 0.70)
training_set = subset(data_train, split == TRUE)
test_set = subset(data_train, split = FALSE)

#function about prediction accuracy
prediction_accuracy = function(pred,actual){
  mean(pred==actual)
}

#logistic 
model_log = glm(target~loudness+energy+acousticness, data = training_set)
summary(model_log)

pred.log = predict(model_log, newdata = test_set, type = "response")
pred.log = ifelse(pred.log >=0.5,1,0)
prediction_accuracy(pred.log, test_set$target)

prediction.log = predict(model_log, newdata = test)
prediction.log = ifelse(prediction.log >=0.5,1,0)

to_be_submitted = data.frame(id=rownames(test) , target=prediction.log)
write.csv(to_be_submitted , file = "nouveau.csv", row.names = F)

head(df)

#tree
library(rpart)
model_tree = rpart(target~loudness+energy+acousticness,method = "class" ,data = training_set)
pred.tree = predict(model_tree, newdata = test_set, type= "class")
prediction_accuracy(pred.tree, test_set$target)

prediction.tree = predict(model_tree, newdata = test, type = "class")

to_be_submitted = data.frame(id=rownames(test) , target=prediction.tree)
write.csv(to_be_submitted , file = "nouveau.csv", row.names = F)

#Bagging meilleur 
library(randomForest)
model_bagging = randomForest(target~., data = training_set, mtry = 13 , importance = TRUE, ntrees = 10000, method = "class")
pred.bagging = predict(model_bagging, newdata = test_set)
pred.bagging = ifelse(pred.bagging >=0.5,1,0)
prediction_accuracy(pred.bagging, test_set$target)

prediction.bagging = predict(model_bagging, newdata = test)
prediction.bagging = ifelse(prediction.bagging>=0.5,1,0)

to_be_submitted = data.frame(id=rownames(test) , target=prediction.bagging)
write.csv(to_be_submitted , file = "nouveau.csv", row.names = F)

#Random Forests 
model_rf = randomForest(target~., data = training_set, mtry = 3, importance =TRUE, ntrees = 10000, type = "class")
pred.rf = predict(model_rf, newdata = test_set)
pred.rf = ifelse(pred.rf >=0.5,1,0)
prediction_accuracy(pred.rf, test_set$target)

prediction.rf = predict(model_rf, newdata = test)
prediction.rf = ifelse(prediction.rf >=0.5,1,0)

to_be_submitted = data.frame(id=rownames(test) , target=prediction.rf)
write.csv(to_be_submitted , file = "nouveau.csv", row.names = F)

#Boosting 
library(gbm)
model_boosting = gbm(
  target~., 
  data = training_set, 
  distribution = "bernoulli", 
  n.trees = 5000, 
  interaction.depth = 10, 
  shrinkage = 0.01
)
pred.boosting = predict(model_boosting, newdata = test_set, type = "response")
pred.boosting = ifelse(pred.boosting>=0.5,1,0)
prediction_accuracy(pred.boosting, test_set$target)

prediction.boosting = predict(model_boosting, newdata = test)
prediction.boosting = ifelse(prediction.boosting >=0.5,1,0)

to_be_submitted = data.frame(id=rownames(test) , target=prediction.boosting)
write.csv(to_be_submitted , file = "nouveau.csv", row.names = F)

#lda 
library(MASS)
model_lda = lda(target~., data = training_set)
pred.lda = predict(model_lda, newdata = test_set, type = "response")
prediction_accuracy(pred.lda$class,test_set$target)

#qda 
model_qda = qda(target~., data = training_set)
pred.qda = predict(model_qda, newdata = test_set, type = "response")
prediction_accuracy(pred.qda$class,test_set$target)
