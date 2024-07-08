library(caTools)
library(caret)
library(fastDummies)
library(rpart)
library(rpart.plot)
library(car)
#Question 3
df= read.csv("C:/Users/Emmanuel/Documents/Data Science/ITBDA4-14/project/car_pricing_datasets.csv",header =TRUE,sep=',' ) 
#3.1.
#head(df,5)
df <- subset(df, select = -car_ID)
df <- subset(df, select = -symboling)

df$actualCarName <- sub(" .*", "", df$CarName)

#naming in actualCarName column
unique_values <- unique(df$enginesize)
print(unique_values)

#fix inconsistent naming of cars
df[df$actualCarName == 'Nissan', 'actualCarName'] <- 'nissan'
df[df$actualCarName == 'toyouta', 'actualCarName'] <- 'toyota'
df[df$actualCarName == 'vokswagen', 'actualCarName'] <- 'volkswagen'
df[df$actualCarName == 'vw', 'actualCarName'] <- 'volkswagen'

# Extract the rest of the words after the first space using base R
#df$model <- sub("^[^ ]+ ", "", df$CarName)


#remove carName. we have actualCarName and model to replace it.
df <- subset(df, select = -CarName)
print(df)

#3.2.

df_one_hot <- dummy_cols(df_copy, select_columns = c("fueltype", "aspiration","doornumber","carbody", "drivewheel","enginelocation","enginetype", "cylindernumber","fuelsystem","actualCarName"), remove_first_dummy = FALSE, remove_selected_columns = TRUE)
print(df_one_hot)

# Min-Max normalization function
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Function to normalize all numerical columns except 'id'
normalize_data <- function(df, exclude_columns) {
  numerical_columns <- setdiff(names(df), exclude_columns)
  
  for (col in numerical_columns) {
    df[[col]] <- min_max_normalize(df[[col]])
  }
  
  return(df)
}
# Apply Min-Max normalization to each feature
normalized_data <- normalize_data(df_one_hot, exclude_columns = "price")
print(normalized_data$price)

set.seed(123)  # Set seed 
split <- sample.split(normalized_data$price, SplitRatio = 0.8)

# Create training and testing sets
train_data <- subset(normalized_data, split == TRUE)
test_data<- subset(normalized_data, split == FALSE)

#3.3.
#a
#Multiple Linear Regression
model_lr <- lm(price ~ ., data = train_data)
# Print the summary of the model
summary(model_lr)
#b
#Decision tree
tree_model <- rpart( price~., data = train_data, method = "class")
summary(tree_model)
#rpart.plot(tree_model, main = "Decision Tree ")
rpart.plot(tree_model, 
                      type = 3, 
                      extra = 104, 
                     fallen.leaves = TRUE, 
                      main = "Decision Tree",box.palette = "Blues")

#3.4.
#####R2(coefficient of determination)

###Multiple Linear Regression
#run model
lr_training_pred <- predict(model_lr,newdata = train_data, type="response")
lr_test_pred <- predict(model_lr,newdata = test_data, type="response")


#training data
r_squared <- summary(lm(price ~ ., data = train_data))$r.squared
print("Coefficient of determination training data:")
print(r_squared)

#testing data
r_squared <- summary(lm(price ~ ., data = test_data))$r.squared
print("Coefficient of determination test data:")
print(r_squared)

#Decision tree
#run model
dt_training_pred <- predict(tree_model,newdata = train_data, type="class")
dt_test_pred <- predict(tree_model,newdata = test_data, type="class")

#training data
observed_original <- as.numeric(as.character(train_data$price))
predicted <- as.numeric(as.character(dt_training_pred))
r_squared <- 1 - sum((observed_original - predicted)^2) / sum((observed_original - mean(observed_original))^2)
print("Coefficient of determination training data:")
print(r_squared)

#test data
observed_original <- as.numeric(as.character(test_data$price))
predicted <- as.numeric(as.character(dt_test_pred))
r_squared <- 1 - sum((observed_original - predicted)^2) / sum((observed_original - mean(observed_original))^2)
print("Coefficient of determination test data:")
print(r_squared)
####R2 end

##root mean square error
#Multiple Linear Regression
#training
rmse <- sqrt(mean((train_data$price - lr_training_pred)^2))
print("root mean square error training data:")
print(rmse)

#test
rmse <- sqrt(mean((test_data$price - lr_test_pred)^2))
print("root mean square error test data:")
print(rmse)

#Decision tree
#training
rmse <- sqrt(mean((as.numeric(as.character(train_data$price)) - as.numeric(as.character(dt_training_pred)))^2))
print("root mean square error training data:")
print(rmse)

#test
rmse <- sqrt(mean((as.numeric(as.character(test_data$price)) - as.numeric(as.character(dt_test_pred)))^2))
print("root mean square error test data:")
print(rmse)

#3.5.
#Multiple Linear Regression

plot(lr_test_pred,test_data$price,
     main = "Goodness-of-Fit Scatter Plot (Multiple Linear Regression)",
     xlab = "Predicted Price",
     ylab = "Actual Price")    # Point color
abline(a=0, b=1, col='red')

#Decision Tree 
plot(dt_test_pred,test_data$price,
     main = "Goodness-of-Fit Scatter Plot (Decision tree)",
     xlab = "Predicted Price",
     ylab = "Actual Price")    # Point color
abline(a = 0, b = 1, col = "red")  

#3.6.