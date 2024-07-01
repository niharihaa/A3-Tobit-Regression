# Function to install packages only if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c("AER", "readr", "VGAM", "sandwich", "lmtest", "pROC", "nortest", "caTools")

# Install and load necessary packages
install_if_missing(required_packages)

# Load the dataset
dataset_path <- "C:\\Users\\nihar\\OneDrive\\Desktop\\Bootcamp\\SCMA 632\\DataSet\\NSSO68.csv"
nsso_data <- read.csv(dataset_path)

# Display the first few rows of the dataset to inspect column names
head(nsso_data)

# Display the column names
colnames(nsso_data)

# Check for missing values in the relevant columns
sum(is.na(nsso_data$MPCE_URP))
sum(is.na(nsso_data$Age))
sum(is.na(nsso_data$Education))
sum(is.na(nsso_data$Sex))

# Remove rows with missing values in the 'Education' column
nsso_data <- nsso_data[!is.na(nsso_data$Education), ]

# Verify the removal of missing values
sum(is.na(nsso_data$Education))  # Should return 0

# Identify outliers using boxplot
boxplot(nsso_data$MPCE_URP, main = "Boxplot of MPCE_URP before removing outliers")

# Remove outliers based on quantiles
q <- quantile(nsso_data$MPCE_URP, probs = c(0.01, 0.99))
nsso_data <- nsso_data[nsso_data$MPCE_URP >= q[1] & nsso_data$MPCE_URP <= q[2], ]

# Create a boxplot after removing outliers
boxplot(nsso_data$MPCE_URP, main = "Boxplot of MPCE_URP after removing outliers")

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
library(caTools)
split <- sample.split(nsso_data$MPCE_URP, SplitRatio = 0.7)
train_data <- subset(nsso_data, split == TRUE)
test_data <- subset(nsso_data, split == FALSE)

# Fit the Tobit model on the training set
tobit_model_train <- vglm(MPCE_URP ~ Age + Education + Sex, 
                          tobit(Lower = 0), 
                          data = train_data, 
                          control = vglm.control(maxit = 1000, trace = TRUE, epsilon = 1e-8))

# View the summary of the model fitted on the training set
summary(tobit_model_train)

# Predict on the test set
predictions <- predict(tobit_model_train, newdata = test_data, type = "response")

# Evaluate the model performance
actuals <- test_data$MPCE_URP
residuals_test <- actuals - predictions

# Plot residuals for the test set
hist(residuals_test, main = "Histogram of Residuals (Test Set)", xlab = "Residuals")

# Normality check using Q-Q plot for the test set residuals
qqnorm(residuals_test, main = "Q-Q Plot of Residuals (Test Set)")
qqline(residuals_test, col = "red")

# Homoscedasticity test
plot(predictions, residuals_test, 
     main = "Residuals vs Predictions (Test Set)", xlab = "Predictions", ylab = "Residuals")
abline(h = 0, col = "red")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(residuals_test))
print(paste("Mean Absolute Error (MAE):", mae))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals_test^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))