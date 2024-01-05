#MGT5372_2808473M

# Required library
library(readr)
library(dplyr)

#Section 1

# Question 1.1

# Loading the datasets
df1 <- read_csv("C:/Users/Methil/OneDrive/Documents/Data Science/flight_reviews_01.csv")
df2 <- read_csv("C:/Users/Methil/OneDrive/Documents/Data Science/flight_reviews_02.csv")

# Merging the two datasets
df <- bind_rows(df1, df2)

# Exporting the merged dataset
write.csv(df, "Data.csv", row.names = FALSE)


# Question 1.2 & 1.3

# Checking for missing values
missing_values <- colSums(is.na(df))

# Reporting columns with missing values
print(missing_values[missing_values > 0])

# Dealing with missing values
for(col in names(df)) {
  if(is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  } else {
    levels <- unique(df[[col]])
    df[[col]][is.na(df[[col]])] <- levels[which.max(tabulate(match(df[[col]], levels)))]
  }
}


# Question 1.4

# Getting numeric columns
numeric_cols <- df %>% select_if(is.numeric)

# Description table for numeric columns
description <- sapply(numeric_cols, function(x) c(Mean = mean(x), Median = median(x),
                                                  Min = min(x), Max = max(x),
                                                  Standard_Deviation = sd(x)))
print(description)

# Question 1.5

# Required library
library(lubridate)

# Convert 'review_date' and 'date_flown' to Date type
df$review_date <- dmy(df$review_date) # Using dmy function to convert dates in "day/month/year" format
df$date_flown <- my(df$date_flown) # Using my function to convert dates in "month/year" format

# Decompose 'review_date'
df$review_year <- year(df$review_date)
df$review_month <- month(df$review_date)
df$review_day <- day(df$review_date)

# Decompose 'date_flown'
df$flown_year <- year(df$date_flown)
df$flown_month <- month(df$date_flown)

# Drop the original columns
df$review_date <- NULL
df$date_flown <- NULL


#Section 2

# Question 2.1

# Count and percentages of verified and non-verified trips
verified_counts <- table(df$trip_verified)
verified_percentages <- prop.table(verified_counts) * 100

# Average ratings for verified and non-verified trips
avg_rating <- tapply(df$rating, df$trip_verified, mean)

print(verified_percentages)
print(avg_rating)

# Question 2.2

# Required library
library(ggplot2)

# Count of each type of travellers
traveller_counts <- table(df$type_of_traveller)

# Convert to data frame
traveller_df <- as.data.frame(table(df$type_of_traveller))
names(traveller_df) <- c("type_of_traveller", "count")

# Pie chart
ggplot(traveller_df, aes(x = "", y = count, fill = type_of_traveller)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(count/sum(count)*100), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(fill = "Type of Traveller")

# Question 2.3

# Most common type of traveller
common_traveller <- names(which.max(traveller_counts))

# Most common seat type
seat_counts <- table(df$seat_type)
common_seat <- names(which.max(seat_counts))

print(common_traveller)
print(common_seat)

# Question 2.4

# Required library
library(GGally)

# Selecting numeric columns
numeric_cols <- df %>% select_if(is.numeric)

# ggpairs plot
ggpairs(numeric_cols)


#section 3

# Question 3.1

# Required libraries
install.packages("caTools")
library(caTools)

# Select the response and predictor variables
df_model <- df[c("rating", "seat_comfort", "cabin_staff_service", "food_and_beverages", "ground_service", "value_for_money", "wifi_and_connectivity", "recommend")]

# Split the data into training and testing sets
set.seed(123) # For reproducibility
split <- sample.split(df_model$recommend, SplitRatio = 0.75)
training_set <- subset(df_model, split == TRUE)
testing_set <- subset(df_model, split == FALSE)

#Implementing logistic regression model

# Required library
library(glmnet)

# Build the model
log_model <- glm(recommend ~ ., family = binomial, data = training_set)

# Make predictions
log_pred <- predict(log_model, newdata = testing_set, type = "response")

# Convert probabilities to binary outcome
log_pred <- ifelse(log_pred > 0.5, 1, 0)

#Implementing the decision tree model

# Required library
library(rpart)

# Build the model
tree_model <- rpart(recommend ~ ., data = training_set, method = "class")

# Make predictions
tree_pred <- predict(tree_model, newdata = testing_set, type = "class")

#computing the accuracy of each model's predictions on the testing data to select the best model

# Required library
library(lattice)
library(caret)
library(ggplot2)


# Convert the variables to factors with "FALSE" and "TRUE" levels
log_pred <- as.factor(log_pred)
levels(log_pred) <- c("FALSE", "TRUE")
tree_pred <- as.factor(tree_pred)
testing_set$recommend <- as.factor(testing_set$recommend)
levels(testing_set$recommend) <- c("FALSE", "TRUE")

# Compute accuracy of logistic regression model
log_accuracy <- sum(diag(confusionMatrix(log_pred, testing_set$recommend)$table)) / sum(confusionMatrix(log_pred, testing_set$recommend)$table)

# Compute accuracy of decision tree model
tree_accuracy <- sum(diag(confusionMatrix(tree_pred, testing_set$recommend)$table)) / sum(confusionMatrix(tree_pred, testing_set$recommend)$table)

# Compare accuracies
log_accuracy
tree_accuracy

# Compute importance of features in logistic regression model
log_imp <- abs(coef(log_model))

# Sort in descending order
log_imp <- sort(log_imp, decreasing = TRUE)

print(log_imp)
