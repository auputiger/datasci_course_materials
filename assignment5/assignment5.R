# Step 1: Read and summarize the data
setwd("D:\\Coursera\\DataScienceUW\\datasci_course_materials\\assignment5")
getwd()
seaflow = read.csv("seaflow_21min.csv")
str(seaflow)
colnames(seaflow)
table(seaflow$pop)
summary(seaflow$fsc_small)

# Step 2: Split the data into test and training sets
library(caret)
inTrain = createDataPartition(y = seaflow$pop,
                              p = 0.5, 
                              list = FALSE)
training = seaflow[inTrain, ]
testing = seaflow[-inTrain, ]
mean(training$time)

# Step 3: Plot the data
library(ggplot2)
summary(seaflow$chl_small)
p = ggplot(data = training, aes(x = chl_small, y = pe, col = pop))
p = p + geom_point(size = 5) + scale_color_discrete(name = "POP Categories")
p

# Step 4: Train a decision tree.
library(rpart)
fol = formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
treeMod = rpart(fol, method = "class", data = training)
print(treeMod)
plot(treeMod, margin = 0.08)
text(treeMod, use.n = TRUE, cex = 0.8)

# Step 5: Evaluate the decision tree on the test data.
predTree = predict(treeMod, newdata = testing, type = "class")
table(predTree, testing$pop)
(0+5040+9509+9023+7318)/nrow(testing)

# Step 6: Build and evaluate a random forest.
library(randomForest)
rfMod = randomForest(fol, data = training)
print(rfMod)
predRF = predict(rfMod, newdata = testing)
table(predRF, testing$pop)
(49+5558+10092+9069+8498)/nrow(testing)

importance(rfMod)

# Step 7: Train a support vector machine model and compare results.
library(e1071)
svmMod = svm(fol, data = training)
print(svmMod)
predSVM = predict(svmMod, newdata = testing)
table(predSVM, testing$pop)
(47+5645+10041+9058+8473)/nrow(testing)

# Step 8: Sanity check the data
trainWo208 = training[training$file_id != 208, ]
testWo208 = testing[testing$file_id != 208, ]
svm208 = svm(fol, data = trainWo208)
pred208 = predict(svm208, newdata = testWo208)
table(pred208, testWo208$pop)
(40+4553+9477+7126+7810)/nrow(testWo208)
(40+4553+9477+7126+7810)/nrow(testWo208)-(47+5645+10041+9058+8473)/nrow(testing)

ggplot(data = training, aes(x = time, y = pe, col = pop)) + geom_point()
ggplot(data = training, aes(x = time, y = fsc_big, col = pop)) + geom_point()
ggplot(data = training, aes(x = time, y = fsc_small, col = pop)) + geom_point()
ggplot(data = training, aes(x = time, y = fsc_perp, col = pop)) + geom_point()
ggplot(data = training, aes(x = time, y = chl_small, col = pop)) + geom_point()
ggplot(data = training, aes(x = time, y = chl_big, col = pop)) + geom_point()
