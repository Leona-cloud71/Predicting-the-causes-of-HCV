

library(readr)
df<- read_csv("C:/Users/DE LEO/Desktop/STAT DATA/hepatitis.csv")

head(df)

# wants to know the datatype
sapply(df, class)

unique(df$class)

library(dplyr)

df <- df %>%
  mutate(class = recode(class,
                           `1=LIVE` = 1,
                           `2=DIE` = 2))

df<- df   %>%
mutate(sex = recode(sex, `m` = 1, `f` = 2))


# Checking for missing values in the dataset

sapply(df, function(x) sum(is.na(x)))

############# Having a look at the correlation matrix

# first install these packages

install.packages("ggplot2")
install.packages("reshape2")
install.packages("RColorBrewer")

library(ggplot2)
library(reshape2)
library(RColorBrewer)


# Calculate the correlation matrix
cor_matrix <- cor(df, use = "complete.obs")

# Melt the correlation matrix for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Plot the heatmap

ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "grey", size = 0.5) +
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(-1, 1)) +
  geom_text(aes(label = round(value, 1)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed(ratio = 1) +
  theme(legend.position = "none")

# Total number of patients who died and those who lived

# Using base R

class_counts <- table(df$class)
cat('Total No. of Patient who lived :', class_counts[1], '\n')
cat('Total No. of Patients who died :', class_counts[2], '\n')

# Using dplyr

library(dplyr)

class_counts <- df %>% count(class)
cat('Total No. of patients who lived :', class_counts[class_counts$class == 1,]$n, '\n')
cat('Total No. of Patients who died :', class_counts[class_counts$class == 2,]$n, '\n')


# Calculate the counts for each category
class_counts <- df %>% count(class)

# Define the labels and explode values
labels <- c("Patients who lived", "Patients who died")
explode <- c(0, 0.1)

# Add a new column for exploded positions

class_counts <- class_counts %>% 
  mutate(explode = ifelse(class == 1, explode[2], explode[1]))

# Plot the pie chart

ggplot(class_counts, aes(x = "", y = n, fill = factor(class, labels = labels))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_void() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.2)) +
  theme(legend.position = "top") +
  labs(fill = "") +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5))

### To count the number of male and female

table(df$sex)

# Calculate the counts for each sex category

sex_counts <- df %>% count(sex)

# Define the labels and colors

labels <- c("Male", "Female")
colors <- c("skyblue", "pink")

# Create a pie chart

ggplot(sex_counts, aes(x = "", y = n, fill = factor(sex, labels = labels))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  theme_void() +
  geom_text(aes(label = scales::percent(n/sum(n))), position = position_stack(vjust = 0.3)) +
  theme(legend.position = "top") +
  labs(fill = "") +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Sex")) +
  theme(legend.title = element_blank())


#### Install the package
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(gridExtra)





# Define the theme
theme_custom <- theme_classic() +
  theme(axis.title=element_text(size=15))

# Create the plots
plot1 <- ggplot(df, aes(x = age, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_viridis_d() +
  labs(x = "Age", fill = "class") +
  theme_custom

plot2 <- ggplot(df, aes(x = fatigue, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_viridis_d() +
  labs(x = "fatigue", fill = "class") +
  theme_custom

plot3 <- ggplot(df, aes(x = malaise, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  labs(x = "malaise", fill = "class") +
  theme_custom

plot4 <- ggplot(df, aes(x = spiders, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("pink", "green")) +
  labs(x = "spiders", fill = "class") +
  theme_custom

plot5 <- ggplot(df, aes(x = ascites, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("yellow", "grey")) +
  labs(x = "ascites", fill = "class") +
  theme_custom

plot6 <- ggplot(df, aes(x = varices, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("brown", "darkblue")) +
  labs(x = "varices", fill = "class") +
  theme_custom

plot7 <- ggplot(df, aes(x = bilirubin, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("darkgreen", "orange")) +
  labs(x = "BLB", fill = "class") +
  theme_custom

plot8 <- ggplot(df, aes(x = albumin, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("red", "darkblue")) +
  labs(x = "ALB", fill = "class") +
  theme_custom

plot9 <- ggplot(df, aes(x = protime, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("pink", "brown")) +
  labs(x = "protime", fill = "class") +
  theme_custom

plot10 <- ggplot(df, aes(x = histology, fill = factor(class))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  labs(x = "histology", fill = "class") +
  theme_custom

# Arrange the plots in a grid

grid.arrange(plot1, plot2, plot3,plot5, plot6, plot7, plot8, plot9, plot10, nrow = 5, ncol = 2)


#### splitting the data into training and test datasets

## here we are trying to predict whether a patient died of hepatitis or not
## the class will be y label and the rest of the data will be x label.

library(dplyr)

# Drop the "class" column

x <- df %>% select(-class)

# Display the first few rows
head(x)


# Select the "Category" column

y <- df %>% select(class)

# Display the first few rows
head(y)

# to split my data into training set and test set, intsall the package

install.packages("caTools")
library(caTools)
library(caret)
library(e1071)

# Set a seed for reproducibility
#data(x)
set.seed(42)

# Split the data

split <- sample.split(df$class, SplitRatio = 0.8)

# Create training and testing sets
X_train <- subset(x, split == TRUE)
X_test <- subset(x, split == FALSE)
y_train <- subset(y, split == TRUE)
y_test <- subset(y, split == FALSE)


# Get the number of rows in X_train and X_test
nrow(X_train)
nrow(X_test)

cat("Number of rows in X_train:", nrow(X_train), "\n")
cat("Number of rows in X_test:", nrow(X_test), "\n")






# Install the caret package if not already installed

install.packages("caret")
library(caret)



######################################################



# Define a list to store the accuracies and best models
accuracies <- list()
best_models <- list()

# Define the model (e.g., Random Forest)
model <- list(name = "Random Forest", method = "rf")

# Define the grid for hyperparameter tuning
tune_grid <- expand.grid(mtry = c(1, 2, 3))

# Train the model with grid search
train_control <- trainControl(method = "cv", number = 5)
grid_search <- train(x = X_train, y = y_train, 
                     method = model$method,
                     tuneGrid = tune_grid,
                     trControl = train_control)

# Get the best model
best_model <- grid_search$finalModel

# Predict on the test set
y_pred <- predict(best_model, newdata = X_test)

# Calculate accuracy
conf_matrix <- confusionMatrix(y_pred, y_test)
accuracy <- conf_matrix$overall['Accuracy']

# Store the results
accuracies[[model$name]] <- accuracy
best_models[[model$name]] <- best_model

# Print results
cat(sprintf("Best parameters for %s: %s\n", model$name, paste(grid_search$bestTune, collapse = ", ")))
cat(sprintf("Accuracy for %s: %f\n", model$name, accuracy))


###### Logistic regression and Random forest

# Load necessary libraries
library(caret)
library(randomForest)
library(nnet) # For multinom function



# Logistic Regression Model (Multinomial for the iris dataset)
log_reg_model <- train(y ~ ., data = split[x, ],
                       method = "multinom",
                       trControl = trainControl(method = "cv", number = 5))

# Print the logistic regression model
print(log_reg_model)

# Predict with logistic regression model
log_reg_pred <- predict(log_reg_model, newdata = split[-x, ])

# Confusion matrix for logistic regression
log_reg_conf_matrix <- confusionMatrix(log_reg_pred, df$class[-x])
print(log_reg_conf_matrix)



# Random Forest Model
rf_model <- randomForest(y ~ ., data = split[x, ],
                         max_depth = 10,
                         ntree = 300)

# Print the random forest model
print(rf_model)

# Predict with random forest model
rf_pred <- predict(rf_model, newdata = split[-x, ])

# Confusion matrix for random forest
rf_conf_matrix <- confusionMatrix(rf_pred, df$class[-x])
print(rf_conf_matrix)






###### Data Preparation

# Load necessary libraries
library(caret)
library(xgboost)
library(e1071)



# Ensure there are no NA values in the dataset
sum(is.na(X_train)) # Should be 0
sum(is.na(X_test)) # Should be 0

# Convert factor to numeric for xgboost
y_train_numeric <- as.numeric(y_train) - 1 # xgboost requires numeric labels
y_test_numeric <- as.numeric(y_test) - 1

# Convert data to matrix format for xgboost
X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)



















####################

# Train the Gradient Boosting Model
gb_model <- xgboost(data = X_train_matrix, 
                    label = y_train_numeric, 
                    eta = 0.1, 
                    max_depth = 3, 
                    nround = 100,
                    objective = "multi:softprob", 
                    num_class = 3, 
                    verbose = 0)

# Predict with the Gradient Boosting Model
gb_pred_prob <- predict(gb_model, X_test_matrix)
gb_pred <- max.col(matrix(gb_pred_prob, ncol = 3)) - 1
gb_pred_factor <- factor(gb_pred, levels = 0:2, labels = levels(y_train))

# Confusion matrix for Gradient Boosting
gb_conf_matrix <- confusionMatrix(gb_pred_factor, y_test)
print(gb_conf_matrix)


################# Scale the data

X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)

# Train the Support Vector Machine Model
svm_model <- svm(Species ~ ., data = data.frame(X_train_scaled, Species = y_train), 
                 type = "C-classification", 
                 kernel = "linear", 
                 cost = 10)

# Predict with the SVM Model
svm_pred <- predict(svm_model, newdata = data.frame(X_test_scaled))

# Confusion matrix for SVM
svm_conf_matrix <- confusionMatrix(svm_pred, y_test)
print(svm_conf_matrix)

########################################


# Load necessary library for multinom
library(nnet)

# Train the Logistic Regression Model (Multinomial for the iris dataset)
log_reg_model <- train(Species ~ ., data = split[x, ],
                       method = "multinom",
                       trControl = trainControl(method = "cv", number = 5))

# Print the logistic regression model
print(log_reg_model)

# Make predictions on the testing data
y_pred_log_reg <- predict(log_reg_model, newdata = X_test)

# Confusion matrix to evaluate predictions
log_reg_conf_matrix <- confusionMatrix(y_pred_log_reg, y_test)
print(log_reg_conf_matrix)


##########################################

# Load necessary libraries
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(nnet)
library(ggplot2)
library(gridExtra)


# Convert factor to numeric for xgboost
y_train_numeric <- as.numeric(y_train) - 1 # xgboost requires numeric labels
y_test_numeric <- as.numeric(y_test) - 1

# Convert data to matrix format for xgboost
X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)

# Scale the data for SVM
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)


##################################


# Train Logistic Regression Model
log_reg_model <- train(class ~ ., data = split[x, ],
                       method = "multinom",
                       trControl = trainControl(method = "cv", number = 5))

# Train Random Forest Model
rf_model <- randomForest(class ~ ., data = split[x, ], 
                         ntree = 300, maxnodes = 10)

# Train Gradient Boosting Model
gb_model <- xgboost(data = X_train_matrix, 
                    label = y_train_numeric, 
                    eta = 0.1, 
                    max_depth = 3, 
                    nround = 100,
                    objective = "multi:softprob", 
                    num_class = 3, 
                    verbose = 0)

# Train Support Vector Machine Model
svm_model <- svm(class ~ ., data = data.frame(X_train_scaled, class = y_train), 
                 type = "C-classification", 
                 kernel = "linear", 
                 cost = 10)

###################################

# Make predictions

y_pred_log_reg <- predict(log_reg_model, newdata = X_test)
y_pred_rf <- predict(rf_model, newdata = X_test)
gb_pred_prob <- predict(gb_model, X_test_matrix)
gb_pred <- max.col(matrix(gb_pred_prob, ncol = 3)) - 1
y_pred_gb <- factor(gb_pred, levels = 0:2, labels = levels(y_train))
y_pred_svm <- predict(svm_model, newdata = data.frame(X_test_scaled))

# Create confusion matrices
conf_matrix_log_reg <- confusionMatrix(y_pred_log_reg, y_test)
conf_matrix_rf <- confusionMatrix(y_pred_rf, y_test)
conf_matrix_gb <- confusionMatrix(y_pred_gb, y_test)
conf_matrix_svm <- confusionMatrix(y_pred_svm, y_test)
#################################


# Function to plot confusion matrix
plot_confusion_matrix <- function(conf_matrix, title) {
  cm <- as.table(conf_matrix$table)
  ggplot(data = as.data.frame(cm), aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    ggtitle(title) +
    theme_minimal()
}

# Plot all confusion matrices
p1 <- plot_confusion_matrix(conf_matrix_log_reg, "Logistic Regression")
p2 <- plot_confusion_matrix(conf_matrix_rf, "Random Forest")
p3 <- plot_confusion_matrix(conf_matrix_gb, "Gradient Boosting")
p4 <- plot_confusion_matrix(conf_matrix_svm, "Support Vector Machine")

# Arrange plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

##################### Model training 
library(caret)
library(ggplot2)
library(dplyr)

# Train Logistic Regression Model
log_reg_model <- train(y ~ ., data = split[x, ],
                       method = "multinom",
                       trControl = trainControl(method = "cv", number = 5))

# Train Random Forest Model
rf_model <- randomForest(Species ~ ., data = iris[trainIndex, ], 
                         ntree = 300, maxnodes = 10)

# Train Gradient Boosting Model
gb_model <- xgboost(data = X_train_matrix, 
                    label = y_train_numeric, 
                    eta = 0.1, 
                    max_depth = 3, 
                    nround = 100,
                    objective = "multi:softprob", 
                    num_class = 3, 
                    verbose = 0)

# Train Support Vector Machine Model
svm_model <- svm(Species ~ ., data = data.frame(X_train_scaled, Species = y_train), 
                 type = "C-classification", 
                 kernel = "linear", 
                 cost = 10)

# Make predictions
y_pred_log_reg <- predict(log_reg_model, newdata = X_test)
y_pred_rf <- predict(rf_model, newdata = X_test)
gb_pred_prob <- predict(gb_model, X_test_matrix)
gb_pred <- max.col(matrix(gb_pred_prob, ncol = 3)) - 1
y_pred_gb <- factor(gb_pred, levels = 0:2, labels = levels(y_train))
y_pred_svm <- predict(svm_model, newdata = data.frame(X_test_scaled))

# Create confusion matrices
conf_matrix_log_reg <- confusionMatrix(y_pred_log_reg, y_test)
conf_matrix_rf <- confusionMatrix(y_pred_rf, y_test)
conf_matrix_gb <- confusionMatrix(y_pred_gb, y_test)
conf_matrix_svm <- confusionMatrix(y_pred_svm, y_test)

# Calculate accuracies
accuracies <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Gradient Boosting", "Support Vector Machine"),
  Accuracy = c(conf_matrix_log_reg$overall['Accuracy'], 
               conf_matrix_rf$overall['Accuracy'],
               conf_matrix_gb$overall['Accuracy'], 
               conf_matrix_svm$overall['Accuracy'])
)

# Sort accuracies in descending order
accuracies <- accuracies %>% arrange(desc(Accuracy))


##################### Plot accuracies using ggplot2
# Define a color palette

color_palette <- c("Logistic Regression" = "#1f77b4", "Random Forest" = "#ff7f0e", 
                   "Gradient Boosting" = "#2ca02c", "Support Vector Machine" = "#d62728")

# Create the plot
p <- ggplot(accuracies, aes(x = reorder(Model, -Accuracy), y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Model Accuracies", x = "Model", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 2), "cm"))

# Adjust the layout
p <- p + theme(plot.margin = unit(c(1, 1, 1, 2), "cm"))

# Save the plot as a PNG file with a resolution of 300 dpi
ggsave("model_comparison.png", plot = p, dpi = 300, width = 10, height = 5)

# Display the plot
print(p)



#########################

######################

# Make predictions using the Gradient Boosting model
y_pred <- predict(gb_model, newdata = X_test)

# Calculate accuracy using confusionMatrix
cm <- confusionMatrix(y_pred, y_test)

# Extract accuracy from confusionMatrix
accuracy <- cm$overall['Accuracy']

# Print the accuracy
print(paste("Accuracy of gb_model:", round(accuracy, 2)))

































































