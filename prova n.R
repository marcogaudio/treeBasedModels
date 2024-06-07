library(tidyverse)
library(openxlsx)
library(readr)
library(readxl)
library(rpart)
library(rpart.plot)
library(distRforest)
library(extras)
library(MetricsWeighted)
library(CASdatasets)
library(caret)
data("freMTPL2freq")
set.seed(1984)

dataset <- freMTPL2freq %>% 
  dplyr::sample_frac(0.4)

# divido train e test dataset
# Split the data into training and test sets
train_index <- createDataPartition(dataset$ClaimNb, p = 0.8, list = FALSE, times = 1)
train <- dataset[train_index, ]
test <- dataset[-train_index, ]


# cov <- health %>%
#   dplyr::select(-c(sex, smoker, region)) %>% 
#   cov



# k fold
k <- 5
# re-sample the data
# set a seed in order to obtain the same resample each time
# Creare gli indici dei fold per il solo test set
folds <- createFolds(train$ClaimNb, k = k)

# Inizializza un vettore per memorizzare il numero del fold per ciascuna riga
fold_number <- rep(NA, nrow(train))

# Assegna il numero del fold a ciascuna riga
for (fold in seq_along(folds)) {
  fold_number[folds[[fold]]] <- fold
}

# Aggiungi la variabile fold al dataset
train$fold <- factor(fold_number)

# Aggiungi la variabile fold al dataset
# health_train$fold <- factor(unlist(lapply(folds, function(x) rep(seq_along(folds), lengths(folds)))))

train <- train %>% 
  sample_frac(size = 1) %>% 
  mutate(fold = rep(1:k, length = n())) %>% 
  arrange(fold)

train %>% 
  dplyr::group_by(fold) %>% 
  summarise(sum(ClaimNb))

sev_cp_values_grid <- seq(from = 0,
                          to = 0.001,
                          len = 5)

# vector for storing the error for each complexity
# parameter in cp_values_grid.
sev_error_estimates_rt_validation <- rep(0, times = length(sev_cp_values_grid))
sev_error_estimates_rt_train <- rep(0, times = length(sev_cp_values_grid))

# vector for storing loss for each fold.
sev_error_estimate_per_fold_rt_validation <- rep(0, k)
sev_error_estimate_per_fold_rt_train <- rep(0, k)
# j = 5
# i = 1
results <- list(validation = list(), train = list())
# Load the necessary packages
library(doParallel)
library(doSNOW)
library(foreach)

cl <- makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = k, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

initial_time <- Sys.time() 
for(j in 1:length(sev_cp_values_grid)){
  
  print(j)
  
  sev_current_cp_value = sev_cp_values_grid[j]

  for(i in 1:k) {
  # foreach::foreach(
  #   i = 1:k,
  #   .errorhandling = "pass",
  #   # .export = c("train", "sev_error_estimates_rt_validation", "sev_error_estimates_rt_train"),
  #   # .combine = c,
  #   .packages = c("magrittr", "distRforest", "rpart", "dplyr"),
  #   .options.snow = opts) %dopar% {

    # print(paste0("folder: ", i))
    
    train_dataset = train %>%
      filter(fold != i) %>% 
      dplyr::select(-c(fold, IDpol))
    
    validation_dataset = train %>%
      filter(fold == i) %>%
      dplyr::select(-c(fold, IDpol))

    # fit the regression tree
    sev_train_tree <- distRforest::rpart(cbind(Exposure , ClaimNb) ~.,
                                         data = train_dataset , method = "poisson",
                                         control = rpart.control(xval = 0,
                                                                 # maxcompete = 0,
                                                                 # maxsurrogate = 0,
                                                                 # minsplit = 200,
                                                                 minbucket = 0.01 * nrow(train),
                                                                 cp = sev_current_cp_value))

    # get predictions
    validation_dataset$fit <- predict(sev_train_tree, newdata=validation_dataset, type="vector")
    train_dataset$fit <- predict(sev_train_tree)
    # get loss
    # sev_error_estimate_per_fold_rt_validation[i] <- deviance_gamma(actual = validation_dataset$charges, predicted = validation_dataset$fit)
    sev_error_estimate_per_fold_rt_validation[i] <- deviance_poisson(actual = validation_dataset$ClaimNb,
                                                                     predicted = validation_dataset$fit,
                                                                     w = validation_dataset$Exposure)
    
    # print(paste0("val: ", sev_error_estimate_per_fold_rt_validation[i]))

    # sev_error_estimate_per_fold_rt_train[i] <- deviance_gamma(actual = train_dataset$charges, predicted = train_dataset$fit)
    sev_error_estimate_per_fold_rt_train[i] <- deviance_poisson(actual = train_dataset$ClaimNb,
                                                                predicted = train_dataset$fit,
                                                                w = train_dataset$Exposure)
    # print(paste0("train: ", sev_error_estimate_per_fold_rt_train[i]))
    
  }
  # sev_error_estimates_rt_validation[j] <- mean(sev_error_estimate_per_fold_rt_validation)
  # sev_error_estimates_rt_train[j] <- mean(sev_error_estimate_per_fold_rt_train)
  
  # list(validation = mean(sev_error_estimate_per_fold_rt_validation),
  #      train = mean(sev_error_estimate_per_fold_rt_train))
  
  results[[1]][j] <- mean(sev_error_estimate_per_fold_rt_validation)
  print(paste0("val mean: ", results[[1]][j]))
  results[[2]][j] <- mean(sev_error_estimate_per_fold_rt_train)
  print(paste0("val mean: ", results[[2]][j]))
  
}
close(pb)
final_time <- Sys.time()
final_time - initial_time

# results[[1]]
# results[[2]]

table <- tibble(
  cp_value = sev_cp_values_grid,
  error_estimate_validation = unlist(results[[1]]),
  error_estimate_train = unlist(results[[2]])
)

ggplot(table, aes(x = cp_value)) +
  geom_line(aes(y = error_estimate_validation), color = "darkred") +
  geom_line(aes(y = error_estimate_train), color="steelblue", linetype="twodash") +
  labs(x = "Complexity parameter", y = "Poisson Deviance")

###
# Load the necessary packages
library(doParallel)
library(magrittr)
library(doSNOW)

# Register a parallel backend with the number of cores you want to use
cl = makeCluster(detectCores() - 1)
registerDoSNOW(cl)

# Initialize the error estimates vectors
sev_error_estimates_rt_validation <- rep(NA, length(sev_cp_values_grid))
sev_error_estimates_rt_train <- rep(NA, length(sev_cp_values_grid))
# j = 1
# i = 2




# Use foreach to iterate over the cp values in parallel
for(j in 1:length(sev_cp_values_grid)) {
  print(j)
  sev_current_cp_value = sev_cp_values_grid[j]
  
  kfold <- function(i, t){
    
    print(i)
    train_dataset = t %>% 
      filter(fold != i)
    validation_dataset = t %>% 
      filter(fold == i)
    
    sev_train_tree <- distRforest::rpart(ClaimNb ~., weights = Exposure,
                                         data = train_dataset , method = "poisson",  
                                         control = rpart.control(xval = 0,
                                                                 # maxcompete = 0, 
                                                                 # maxsurrogate = 0,
                                                                 # minsplit = 200,
                                                                 # minbucket = 0.3 * nrow(train_dataset),
                                                                 cp = sev_current_cp_value))
    
    # get predictions
    validation_dataset$fit <- predict(sev_train_tree, newdata=validation_dataset, type="vector") 
    train_dataset$fit <- predict(sev_train_tree)
    # get loss
    sev_error_estimate_per_fold_rt_validation[i] <- deviance_poisson(actual = validation_dataset$ClaimNb,
                                                                     predicted = validation_dataset$fit,
                                                                     w = validation_dataset$Exposure)

    sev_error_estimate_per_fold_rt_train[i] <- deviance_poisson(actual = train_dataset$ClaimNb,
                                                                predicted = train_dataset$fit,
                                                                w = train_dataset$Exposure)
    
    
    
  }
  
  
  foreach::foreach(
    i = 1:k,
          .errorhandling = "pass",
          # .export = c("train", "sev_error_estimates_rt_validation", "sev_error_estimates_rt_train"),
          # .combine = c,
          .packages = c("magrittr", "distRforest", "rpart", "dplyr")) %dopar%{
            
            kfold( i = 1, t = train)
            
    print(i)
    train_dataset = train %>%
      filter(fold != i)
    validation_dataset = train %>%
      filter(fold == i)

    # fit the regression tree
    sev_train_tree <- distRforest::rpart(ClaimNb ~., weights = Exposure,
                                         data = train_dataset , method = "poisson",
                                         control = rpart.control(xval = 0,
                                                                 # maxcompete = 0,
                                                                 # maxsurrogate = 0,
                                                                 # minsplit = 200,
                                                                 # minbucket = 0.3 * nrow(train_dataset),
                                                                 cp = sev_current_cp_value))

    # get predictions
    validation_dataset$fit <- predict(sev_train_tree, newdata=validation_dataset, type="vector")
    train_dataset$fit <- predict(sev_train_tree)
    # get loss
    # sev_error_estimate_per_fold_rt_validation[i] <- deviance_gamma(actual = validation_dataset$charges, predicted = validation_dataset$fit)
    sev_error_estimate_per_fold_rt_validation[i] <- deviance_poisson(actual = validation_dataset$ClaimNb,
                                                                     predicted = validation_dataset$fit,
                                                                     w = validation_dataset$Exposure)
    # sev_error_estimate_per_fold_rt_validation[i] = 1
    # sev_error_estimate_per_fold_rt_train[i] <- deviance_gamma(actual = train_dataset$charges, predicted = train_dataset$fit)
    sev_error_estimate_per_fold_rt_train[i] <- deviance_poisson(actual = train_dataset$ClaimNb,
                                                                predicted = train_dataset$fit,
                                                                w = train_dataset$Exposure)
    sev_error_estimate_per_fold_rt_train[i] = 1
  }
  
  
  # Return the mean error estimates for the current cp value
  list(validation = mean(sev_error_estimate_per_fold_rt_validation),
       train = mean(sev_error_estimate_per_fold_rt_train))
} -> results


# collect the cp values and the related errors, plot them and get the best estimate
table2_sev <- tibble(
  cp_value = sev_cp_values_grid,
  error_estimate_validation = sev_error_estimates_rt_validation,
  error_estimate_train = sev_error_estimates_rt_train
)

# Extract the mean error estimates from the results list
sev_error_estimates_rt_validation <- sapply(results, "[[", "validation")
sev_error_estimates_rt_train <- sapply(results, "[[", "train")



###





table2_sev %>% 
  dplyr::filter(error_estimate_train == min(table2_sev$error_estimate_train))

table2_sev %>% 
  dplyr::filter(error_estimate_validation == min(table2_sev$error_estimate_validation))


ggplot(table2_sev, aes(x = cp_value)) +
  geom_line(aes(y = error_estimate_validation), color = "darkred") +
  geom_line(aes(y = error_estimate_train), color="steelblue", linetype="twodash") +
  labs(x = "Complexity parameter", y = "Gamma Deviance")



best_estimate_cp_sev <- table2_sev %>% slice_min(error_estimate_validation) %>% 
  select(cp_value) %>%
  slice(1) %>% 
  pull() 

best_estimate_cp_sev

