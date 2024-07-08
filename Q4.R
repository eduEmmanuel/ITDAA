library(fastDummies)
library(rpart)
library(rpart.plot)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(factoextra)
#Question 4
df_orig= read.csv("C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project/forest_mapping_datasets.csv",header =TRUE,sep=',' ) 
head(df,5)

#4.1
df<-df_orig
df$class <- as.factor(df$class) 

target <- df$class
features <- df[,-which(names(df)=="class")]

set.seed(123)  # Set seed 
split <- sample.split(df$class, SplitRatio = 0.8)

# Create training and testing sets
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

nrow(train_data)
nrow(test_data)
#4.2.
# Fit logistic regression model
lmodel <- glm(class ~ ., data = train_data, family = binomial)
# Summary of the model
summary(model)
#Naïve Bayes
nb_model <- naiveBayes(class ~ ., data = train_data)

# Print the model
summary(nb_model)

#decision tree
dt_model <- rpart(class ~ ., data = train_data, method = "class")
#rpart.plot(dt_predictions, main = "Decision Tree ")


# Print the model
#print(dt_model)
#4.3. look at week 3 case study 2 to interpret
lmprediction_test <- predict(lmodel, newdata = test_data, type = "response")
lmprediction_test_class = ifelse(lg_train_cm > 0.5,"Good","Bad")
nb_prediction_test <- predict(nb_model, newdata = test_data)
dt_prediction_test <- predict(dt_model, newdata = test_data, type = "class")

lgprediction_training <- predict(lmodel, newdata = train_data, type = "response")
nb_prediction_training <- predict(nb_model, newdata = train_data)
dt_prediction_training <- predict(dt_model, newdata = train_data, type = "class")

#confusion Matrix training 
lg_train_cm <-  table(Predicted = as.factor(lgprediction_training), Actual = train_data$class)
nb_train_cm <-  confusionMatrix(as.factor(nb_prediction_training), train_data$class)
dt_train_cm <-  confusionMatrix(as.factor(dt_prediction_training), train_data$class)
print("Training set")
print("Logistic Regression:")
print(lg_train_cm)
print("Naive Bayes:")
print(nb_train_cm)
print("Decision Tree:")
print(nb_train_cm)

#confusion Matrix test
lg_test_cm <-  table(Predicted = as.factor(lmprediction_test), Actual = test_data$class)
nb_test_cm <-  confusionMatrix(as.factor(nb_prediction_test), test_data$class)
dt_test_cm <-  confusionMatrix(as.factor(dt_prediction_test), test_data$class)
print("Test set")
print("Logistic Regression:")
print(lg_test_cm)
print("Naive Bayes:")
print(nb_test_cm)
print("Decision Tree:")
print(dt_test_cm)

#classification accuracy training
lg_accuracy <- sum(diag(lg_train_cm)) / sum(lg_train_cm)
nb_accuracy <- sum(nb_prediction_training == train_data$class) / nrow(train_data)
dt_accuracy <- sum(dt_prediction_training == train_data$class) / nrow(train_data)
print("Training set classification accuracy")
print("Logistic Regression:")
print(lg_accuracy)
print("Naive Bayes:")
print(nb_accuracy)
print("Decision Tree:")
print(dt_accuracy)

#classification accuracy test
lg_accuracy_test <- sum(diag(lg_test_cm)) / sum(lg_test_cm)
nb_accuracy_test <- sum(nb_prediction_test == test_data$class) / nrow(test_data)
dt_accuracy_test <- sum(dt_prediction_test == test_data$class) / nrow(test_data)
print("test set classification accuracy")
print("Logistic Regression:")
print(lg_accuracy_test)
print("Naive Bayes:")
print(nb_accuracy_test)
print("Decision Tree:")
print(dt_accuracy_test)

#4.4. 
trDataNoClass<- subset(train_data, select =-class)#remove class
head(trDataNoClass)
trainingData_scaled <- scale(trDataNoClass)
head(trainingData_scaled)

# Function to compute the total within-cluster sum of squares (WSS)
wss <- function(k) {
  kmeans(trainingData_scaled, k, nstart = 10)$tot.withinss
}

# Compute WSS for k = 1 to k = 20
k_values <- 1:20
wss_values <- sapply(k_values, wss)

# Print WSS values
wss_values
# Plot the WSS values to visualize the elbow
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares (WSS)",
     main = "Elbow Method")

#5 clusters
kmeans_model <- kmeans(trainingData_scaled, centers = 5, nstart = 20)
summary(kmeans_model)

# Visualize the clustering
fviz_cluster(kmeans_model, data = trainingData_scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

