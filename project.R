library(MLDataR)
library(dplyr)
library(tidyr)
library(tidymodels)
library(data.table)
library(ConfusionTableR)
library(OddsPlotty)
library(randomForest)


heartdisease <- MLDataR::heartdisease %>% 
  dplyr::mutate(
    HeartDisease = as.factor(HeartDisease),
    Sex = as.factor(Sex),
    RestingECG = as.factor(RestingECG),
    Angina = as.factor(Angina),
    HeartPeakReading = as.factor(HeartPeakReading)
  )



heartdisease


train_index <- createDataPartition(heartdisease$HeartDisease, p = 0.8, list = FALSE, times = 1)
train <- heartdisease[train_index, ]
test <- heartdisease[-train_index, ]


fit <- rpart::rpart(HeartDisease ~ ., method = "class", data = train, control = rpart::rpart.control(
                                                                                                     # minbucket = round(minsplit/3), 
                                                                                                     cp = 0.01, 
                                                                                                     # maxcompete = 4, 
                                                                                                     # maxsurrogate = 5, 
                                                                                                     # usesurrogate = 2, xval = 10,
                                                                                                     # surrogatestyle = 0, 
                                                                                                     # maxdepth = 5,
                                                                                                     # xval = 5
                                                                                                     )
                    )



rpart::printcp(fit)
rpart::plotcp(fit)



fit_prune <- rpart::rpart(HeartDisease ~ ., data = train, control = rpart::rpart.control(minsplit = 20,
                                                                                         # minbucket = round(minsplit/3), 
                                                                                         cp = 0.039634, 
                                                                                         # maxcompete = 4, 
                                                                                         # maxsurrogate = 5, 
                                                                                         # usesurrogate = 2, xval = 10,
                                                                                         # surrogatestyle = 0, 
                                                                                         maxdepth = 6,
                                                                                         # xval = 5
)
)

rpart.plot(fit_prune)
prediction_test <- predict(fit_prune, newdata = test, type = "class")
prediction_train <- predict(fit_prune, type = "class")
table(predicted = prediction_test, actual = test$HeartDisease)
table(predicted = prediction_train, actual = train$HeartDisease)

accuracy_test <- mean(test$HeartDisease == prediction_test)
accuracy_test
accuracy_train <- mean(train$HeartDisease == prediction_train)
accuracy_train


# Variable importance
var_imp <- fit_prune$variable.importance
var_imp <- tibble::tibble(
  Feature = names(var_imp), 
  Importance = var_imp) %>%
  dplyr::arrange(desc(Importance))
  

# Plot the variable importance
ggplot(var_imp, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Features", y = "Importance") +
  theme_minimal()

# Additional plots for better understanding
# Plotting ROC Curve for the test set
library(pROC)

# Calculate ROC curve for the test set
prob_test <- predict(fit_prune, newdata = test, type = "prob")[,2]
roc_test <- roc(test$HeartDisease, prob_test, levels = rev(levels(test$HeartDisease)))

# Calculate ROC curve for the training set
prob_train <- predict(fit_prune, type = "prob")[,2]
roc_train <- roc(train$HeartDisease, prob_train, levels = rev(levels(train$HeartDisease)))

# Create a data frame for ggplot
roc_data <- data.frame(
  FPR = c(roc_train$specificities, roc_test$specificities),
  TPR = c(roc_train$sensitivities, roc_test$sensitivities),
  Set = c(rep("Training Set", length(roc_train$specificities)), rep("Test Set", length(roc_test$specificities)))
)

# Plot the ROC curve using ggplot
ggplot(roc_data, aes(x = 1 - FPR, y = TPR, color = Set)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curve for Training and Test Sets",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("Training Set" = "lightblue", "Test Set" = "darkblue"))

# Calculate AUC for both training and test sets
auc_train <- auc(roc_train)
auc_test <- auc(roc_test)

auc_train
auc_test




### random forest