# Define train_values_url
train_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"

# Import train_values
train_values <- read.csv(train_values_url)

# Define train_labels_url
train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"

# Import train_labels
train_labels <- read.csv(train_labels_url)

# Define test_values_url
test_values_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"

# Import test_values
test_values <- read.csv(test_values_url)

# Merge data frames to create the data frame train
train <- merge(train_labels, train_values)

# Look at the number of pumps in each functional status group
table(train$status_group)

# As proportions
prop.table(table(train$status_group))
prop.table(table(train$payment, train$status_group), margin = 1)

# Load the ggplot package and examine train
library(ggplot2)
str(train)

# Create bar plot for quantity
qplot(quantity, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Create bar plot for quality_group
qplot(quality_group, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top")

# Create bar plot for waterpoint_type
qplot(waterpoint_type, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

library(ggplot2)

# Create a histogram for `construction_year` grouped by `status_group`
ggplot(train, aes(x = construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

# Now subsetting when construction_year is larger than 0
ggplot(subset(train, construction_year > 0), aes(x = construction_year)) +
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

# Load the randomForest library
library(randomForest)

# Set seed and create a random forest classifier
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + extraction_type_group + quality_group + quantity + waterpoint_type + construction_year, data = train, importance = TRUE, ntree = 5, nodesize = 2)

# Use random forest to predict the values in train
pred_forest_train <- predict(model_forest, train)

# Observe the first few rows of your predictions
head(pred_forest_train)

# Variable Importance
varImpPlot(model_forest)
