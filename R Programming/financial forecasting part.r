# ans b
# Import the data
library(readxl)
library(caret)
# Load the dataset
dataset <- read_excel("D:/New folder/financial forecasting part.xlsx")
exchange_rates <- dataset$`USD/EUR`

# Define the maximum time delay for the input vector
max_delay <- 4

# Initialize the input/output matrices
input_matrix <- matrix(NA, nrow = length(exchange_rates) - max_delay, ncol = max_delay)
output_vector <- exchange_rates[(max_delay + 1):length(exchange_rates)]

# Construct the input/output matrices
for (i in 1:max_delay) {
  input_matrix[, i] <- exchange_rates[(max_delay - i + 1):(length(exchange_rates) - i)]
}

# Split the data into training and testing sets
train_samples <- 400
train_input <- input_matrix[1:train_samples, ]
train_output <- output_vector[1:train_samples]
test_input <- input_matrix[(train_samples + 1):nrow(input_matrix), ]

test_output <- output_vector[(train_samples + 1):length(output_vector)]

# Perform any necessary preprocessing or normalization of the input and output data

# Print the dimensions of the input/output matrices
cat("Training input matrix dimensions:", dim(train_input), "\n")
cat("Training output vector length:", length(train_output), "\n")
cat("Testing input matrix dimensions:", dim(test_input), "\n")
cat("Testing output vector length:", length(test_output), "\n")

#ans c
train_input_norm <- scale(train_input)
test_input_norm <- scale(test_input)

# Check the distribution of the normalized data
hist(train_input_norm)

# De-normalize the data
train_input_denorm <- train_input_norm * sd(train_input) + mean(train_input)
test_input_denorm <- test_input_norm * sd(test_input) + mean(test_input)

# Truncate the train_input_denorm vector to the same length as the train_output vector
train_input_denorm <- train_input_denorm[1:length(train_output)]

# Plot the de-normalized data
plot(train_input_denorm, train_output, main = "De-Normalized Data")
abline(0, 1)

train_input_norm <- train_input_norm[1:length(train_output)]

plot(train_input_norm, train_output, main = "Normalized Data")
abline(0, 1)

#ANS d


library(caret)
library(neuralnet)

# To calculate sMAPE (symmetric MAPE)
smape <- function(actual, predicted) {
  200 * mean(abs(actual - predicted) / (abs(actual) + abs(predicted)))
}

# Define a list of  input vectors (time delays)
input_vectors <- list(1:1, 1:2, 1:3, 1:4)

# Define a list of  MLP models
mlp_models <- list(
  mlp1 = list(hidden = c(5), linear.output = TRUE, activation = "logistic", learningrate = 0.01),
  mlp2 = list(hidden = c(10, 5), linear.output = TRUE, activation = "tanh", learningrate = 0.001),
  mlp3 = list(hidden = c(15, 10, 5), linear.output = TRUE, activation = "relu", learningrate = 0.0001),
  mlp4 = list(hidden = c(5), linear.output = FALSE, activation = "logistic", learningrate = 0.01),
  mlp5 = list(hidden = c(10, 5), linear.output = FALSE, activation = "tanh", learningrate = 0.001),
  mlp6 = list(hidden = c(15, 10, 5), linear.output = FALSE, activation = "relu", learningrate = 0.0001)
)

results <- list()

for (input_vector in input_vectors) {
  for (i in 1:length(mlp_models)) {
    model <- neuralnet(
      formula = train_output ~ .,
      data = cbind(train_input[, input_vector], train_output),
      hidden = mlp_models[[i]]$hidden,
      linear.output = mlp_models[[i]]$linear.output,
      learningrate = mlp_models[[i]]$learningrate
    
    )
    
    
    predictions <- predict(model, cbind(test_input[, input_vector]))
    
    # Calculate evaluation metrics
    rmse <- RMSE(predictions, test_output)
    mae <- MAE(predictions, test_output)
    mape <- mean(abs(predictions - test_output) / test_output) * 100
    smape_val <- smape(predictions, test_output)
    
    # Store the results
    result <- list(
      input_vector = input_vector,
      model_name = paste0("mlp", i),
      RMSE = rmse,
      MAE = mae,
      MAPE = mape,
      sMAPE = smape_val
    )
    results[[length(results) + 1]] <- result
  }
}
# The results
for (result in results) {
  cat("Input Vector:", paste(result$input_vector, collapse = "-"), "\n")
  cat("Model:", result$model_name, "\n")
  cat("RMSE:", result$RMSE, "\n")
  cat("MAE:", result$MAE, "\n")
  cat("MAPE:", result$MAPE, "\n")
  cat("sMAPE:", result$sMAPE, "\n")
  cat("\n")
}

# ans h

library(neuralnet)
library(caret)


smape <- function(actual, predicted) {
  200 * mean(abs(actual - predicted) / (abs(actual) + abs(predicted)))
}

input_vector <- 1:2
model <- neuralnet(
  formula = train_output ~ .,
  data = cbind(train_input[, input_vector], train_output),
  hidden = c(10, 5),
  linear.output = TRUE,
  learningrate = 0.001
)


predictions <- predict(model, cbind(test_input[, input_vector]))

# Calculate stat. indices
rmse <- RMSE(predictions, test_output)
mae <- MAE(predictions, test_output)
mape <- mean(abs(predictions - test_output) / test_output) * 100
smape_val <- smape(predictions, test_output)

# Create a scatter plot 
plot(predictions, test_output, main = "MLP8 Predictions vs. Desired Output", xlab = "Predictions", ylab = "Desired Output")
abline(0, 1)

# Print the stat. indices
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape_val, "\n")