 data=read.csv(file.choose(new=F),header=T)
data
attach(data)
data
sum(is.na(data))
credit=na.omit(data)
credit
dim(data)
#1. Data Exploration & Visualization
#Check class distribution (fraud vs. non-fraud).
#Summary statistics and feature distributions.
library(ggplot2)

# Class distribution
ggplot(data=data, aes(x=factor(Class))) + 
  geom_bar() + 
  ggtitle("Class Distribution (Fraud vs Non-Fraud)")

# Summary statistics
summary(data)
##2. Fraud Detection Using Machine Learning
#Train a classification model (e.g., Logistic Regression, Random Forest).
install.packages("caret")

library(caret)

# Prepare data
data$Class=as.factor(data$Class)
set.seed(42)
trainIndex = createDataPartition(data$Class, p=0.7, list=FALSE)
train = data[trainIndex, ]
test = data[-trainIndex, ]

# Train model
model = glm(Class ~ ., data=train, family="binomial")
coef(model)
# Predictions
predictions = predict(model, test, type="response")
pred_class =ifelse(predictions > 0.5, 1, 0)
# Evaluation
confusionMatrix(factor(pred_class), test$Class)


library(randomForest)
# Train Random Forest
rf_model = randomForest(Class ~ ., data=train, ntree=100)
# Predictions
rf_pred = predict(rf_model, test)
# Evaluate
confusionMatrix(rf_pred, test$Class)

## correlation matrix
# Install necessary packages if not already installed
install.packages("corrplot")
install.packages("ggcorrplot")

# Load required libraries
library(corrplot)
library(ggcorrplot)

# Compute correlation matrix (excluding non-numeric columns like ID and Class)
cor_matrix <- cor(data[, -c(1, 31)], use = "complete.obs")

# Plot Correlation Matrix using corrplot
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7)

# Alternative correlation plot using ggcorrplot
ggcorrplot(cor_matrix, lab = TRUE, hc.order = TRUE, outline.color = "yellow")


## histogram
# Install required packages if not already installed
install.packages("ggplot2")
install.packages("reshape2")

# Load libraries
library(ggplot2)
library(reshape2)

# Exclude non-numeric columns (Assuming Class is at column 31)
df_numeric = data[, sapply(data, is.numeric)]  # Select only numeric columns

# Convert dataset to long format for easier visualization
df_long = melt(df_numeric)

# Plot histograms for all features
ggplot(df_long, aes(x = value)) +
  geom_histogram(fill = "yellow", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Features", x = "Value", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






