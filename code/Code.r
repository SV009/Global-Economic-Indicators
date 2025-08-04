# Replace 'path/to/yourfile.csv' with the actual file path
data <- read.csv("Global Economy Indicators.csv")
# Show the structure of the data (variables and types)
str(data)

# Show the first few rows
head(data)

# Summary of the data for quick stats on each variable
summary(data)

# Check for missing values in each column
missing_values <- colSums(is.na(data))
missing_values

# Check for missing values in each column
missing_values <- colSums(is.na(data))
print("Missing values in each column:")
print(missing_values)

# Impute missing values with mean for columns with fewer missing values
data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B..[is.na(data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B..)] <- mean(data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B.., na.rm = TRUE)
data$Construction..ISIC.F.[is.na(data$Construction..ISIC.F.)] <- mean(data$Construction..ISIC.F., na.rm = TRUE)
data$Exports.of.goods.and.services[is.na(data$Exports.of.goods.and.services)] <- mean(data$Exports.of.goods.and.services, na.rm = TRUE)
data$Final.consumption.expenditure[is.na(data$Final.consumption.expenditure)] <- mean(data$Final.consumption.expenditure, na.rm = TRUE)
data$Gross.capital.formation[is.na(data$Gross.capital.formation)] <- mean(data$Gross.capital.formation, na.rm = TRUE)
data$Gross.fixed.capital.formation..including.Acquisitions.less.disposals.of.valuables.[is.na(data$Gross.fixed.capital.formation..including.Acquisitions.less.disposals.of.valuables.)] <- mean(data$Gross.fixed.capital.formation..including.Acquisitions.less.disposals.of.valuables., na.rm = TRUE)
data$Household.consumption.expenditure..including.Non.profit.institutions.serving.households.[is.na(data$Household.consumption.expenditure..including.Non.profit.institutions.serving.households.)] <- mean(data$Household.consumption.expenditure..including.Non.profit.institutions.serving.households., na.rm = TRUE)
data$Manufacturing..ISIC.D.[is.na(data$Manufacturing..ISIC.D.)] <- mean(data$Manufacturing..ISIC.D., na.rm = TRUE)
data$X.Transport..storage.and.communication..ISIC.I..[is.na(data$X.Transport..storage.and.communication..ISIC.I..)] <- mean(data$X.Transport..storage.and.communication..ISIC.I.., na.rm = TRUE)
data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..[is.na(data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..)] <- mean(data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H.., na.rm = TRUE)

# Verify missing values after imputation
missing_values_after <- colSums(is.na(data))
print("Missing values in each column after imputation:")
print(missing_values_after)

# Install dplyr if not already installed
if (!require(dplyr)) install.packages("dplyr")

# Load the dplyr package
library(dplyr)
# Load the dplyr package
library(dplyr)

# Now try running the filtering code again with exact column names
filtered_data <- data %>%
  select(
    CountryID,
    Country,
    Year,
    Population,
    X.Agriculture..hunting..forestry..fishing..ISIC.A.B..,
    Manufacturing..ISIC.D.,
    X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..,
    Total.Value.Added,
    Gross.National.Income.GNI..in.USD,
    Gross.Domestic.Product..GDP.,
    Imports.of.goods.and.services,
    Exports.of.goods.and.services
  )

# Display the first few rows of the filtered data
head(filtered_data)

summary(filtered_data)

str(filtered_data)

unique(filtered_data$Country)

library(ggplot2)
install.packages("naniar")

library(naniar)

gg_miss_var(filtered_data)

head(filtered_data)

# Remove rows with any missing values
cleaned_data <- na.omit(filtered_data)

# Check if missing values are removed
summary(cleaned_data)

# Impute missing values with the mean for each column
imputed_data <- cleaned_data

# Apply mean imputation to each column with NA values
imputed_data[] <- lapply(imputed_data, function(x) {
  if (is.numeric(x)) {
    # Replace NA with the mean of the column
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})

# Display summary of the imputed dataset to confirm no missing values
summary(imputed_data)


# Trend and Time Series Analysis

# Display summary statistics for all variables in the imputed data
summary(imputed_data)

# Load ggplot2 for visualization
library(ggplot2)



library(scales)  # Load the scales package for comma formatting
# Zoomed-in example
ggplot(imputed_data, aes(x = Gross.Domestic.Product..GDP.)) +
  geom_histogram(binwidth = 5e10, fill = "blue", color = "black") +
  labs(title = "Distribution of GDP (Zoomed In)", x = "GDP", y = "Frequency") +
  xlim(0, 1e12) +
  theme_minimal()


# Distribution of GNI with zoomed-in range
ggplot(imputed_data, aes(x = Gross.National.Income.GNI..in.USD)) +
  geom_histogram(binwidth = 5e10, fill = "green", color = "black") +
  coord_cartesian(xlim = c(0, 1e12)) +  # Adjust the range to zoom in
  labs(title = "Distribution of GNI (Zoomed In)", x = "GNI (in USD)", y = "Frequency")

# Distribution of Agriculture Contribution with zoomed-in range
ggplot(imputed_data, aes(x = X.Agriculture..hunting..forestry..fishing..ISIC.A.B..)) +
  geom_histogram(binwidth = 1e9, fill = "orange", color = "black") +
  coord_cartesian(xlim = c(0, 5e10)) +  # Adjust the range to zoom in
  labs(title = "Distribution of Agriculture Contribution (Zoomed In)", x = "Agriculture Contribution (in USD)", y = "Frequency")

# Time Series Plot for GDP
ggplot(imputed_data, aes(x = Year, y = Gross.Domestic.Product..GDP., color = Country)) +
  geom_line() +
  labs(title = "GDP Over Time by Country", x = "Year", y = "GDP") +
  theme(legend.position = "none")

# Time Series Plot for GNI
ggplot(imputed_data, aes(x = Year, y = Gross.National.Income.GNI..in.USD, color = Country)) +
  geom_line() +
  labs(title = "GNI Over Time by Country", x = "Year", y = "GNI") +
  theme(legend.position = "none")
#Box plot try out



# Log-transformed Box Plot for Agriculture Contribution
ggplot(imputed_data, aes(x = "Agriculture", y = log1p(X.Agriculture..hunting..forestry..fishing..ISIC.A.B..))) +
  geom_boxplot(fill = "orange", color = "black", outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Box Plot of Agriculture Contribution (Log-Transformed)",
    x = "Agriculture",
    y = "Log(1 + Contribution)"
  ) +
  theme_minimal()


# Log-transformed Box Plot for Manufacturing Contribution
ggplot(imputed_data, aes(x = "Manufacturing", y = log1p(Manufacturing..ISIC.D.))) +
  geom_boxplot(fill = "blue", color = "black", outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Box Plot of Manufacturing Contribution (Log-Transformed)",
    x = "Manufacturing",
    y = "Log(1 + Contribution)"
  ) +
  theme_minimal()

# Log-transformed Box Plot for Services Contribution
ggplot(imputed_data, aes(x = "Services", y = log1p(X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..))) +
  geom_boxplot(fill = "green", color = "black", outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Box Plot of Services Contribution (Log-Transformed)",
    x = "Services",
    y = "Log(1 + Contribution)"
  ) +
  theme_minimal()

# Comparative Analysis (Developed vs. Developing Economies)


# Expanded list of developed countries
developed_countries <- c("United States", "Canada", "United Kingdom", "Germany", "France", 
                         "Italy", "Spain", "Japan", "Australia", "New Zealand", 
                         "Norway", "Sweden", "Finland", "Denmark", "Switzerland", 
                         "Netherlands", "Belgium", "Austria", "Ireland", "Singapore", 
                         "South Korea", "Israel", "Luxembourg", "Iceland", "Portugal", 
                         "Greece", "Czech Republic", "Slovenia", "Estonia", "Slovakia", 
                         "Malta", "Lithuania", "Latvia", "Cyprus", "Hong Kong")
# Add a 'Development_Status' column to classify countries
imputed_data$Development_Status <- ifelse(imputed_data$Country %in% developed_countries, 
                                          "Developed", "Developing")

# Add a 'Development_Status' column to classify countries
imputed_data$Development_Status <- ifelse(imputed_data$Country %in% developed_countries, 
                                          "Developed", "Developing")

# Verify the classification counts
table(imputed_data$Development_Status)

unique(imputed_data$Country)

# Trim white spaces from country names
imputed_data$Country <- trimws(imputed_data$Country)

# Re-run the classification
imputed_data$Development_Status <- ifelse(imputed_data$Country %in% developed_countries, 
                                          "Developed", "Developing")

# Verify the classification counts again
table(imputed_data$Development_Status)

# Subset data for developed and developing countries
developed_data <- imputed_data %>% filter(Development_Status == "Developed")
developing_data <- imputed_data %>% filter(Development_Status == "Developing")

library(ggplot2)

# Plot for Agriculture Contribution over Time
ggplot(imputed_data, aes(x = Year, y = `X.Agriculture..hunting..forestry..fishing..ISIC.A.B..`, color = Development_Status)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Agriculture Contribution Over Time by Development Status", x = "Year", y = "Agriculture Contribution") +
  theme_minimal()

# Plot for Manufacturing Contribution over Time
ggplot(imputed_data, aes(x = Year, y = `Manufacturing..ISIC.D.`, color = Development_Status)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Manufacturing Contribution Over Time by Development Status", x = "Year", y = "Manufacturing Contribution") +
  theme_minimal()

# Plot for Services Contribution over Time
ggplot(imputed_data, aes(x = Year, y = `X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..`, color = Development_Status)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Services Contribution Over Time by Development Status", x = "Year", y = "Services Contribution") +
  theme_minimal()

#Analysis 3: Sector Contribution Analysis

# Calculate sectoral contribution percentages
imputed_data$Agriculture_Percentage <- (imputed_data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B. / imputed_data$Gross.Domestic.Product..GDP.) * 100
imputed_data$Manufacturing_Percentage <- (imputed_data$Manufacturing..ISIC.D. / imputed_data$Gross.Domestic.Product..GDP.) * 100
imputed_data$Services_Percentage <- (imputed_data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H. / imputed_data$Gross.Domestic.Product..GDP.) * 100


library(ggplot2)
library(dplyr)

# Aggregate data by year and development status
aggregated_data <- imputed_data %>%
  group_by(Year, Development_Status) %>%
  summarize(
    Avg_Agriculture_Percentage = mean(Agriculture_Percentage, na.rm = TRUE),
    Avg_Manufacturing_Percentage = mean(Manufacturing_Percentage, na.rm = TRUE),
    Avg_Services_Percentage = mean(Services_Percentage, na.rm = TRUE),
    .groups = "drop"
  )



# Plot Agriculture Percentage over Time by Development Status
ggplot(aggregated_data, aes(x = Year, y = Avg_Agriculture_Percentage, color = Development_Status)) +
  geom_line(size = 1) +
  labs(title = "Agriculture Contribution Over Time by Development Status",
       x = "Year", y = "Average Agriculture Contribution (%)") +
  theme_minimal()

# Plot Manufacturing Percentage over Time by Development Status
ggplot(aggregated_data, aes(x = Year, y = Avg_Manufacturing_Percentage, color = Development_Status)) +
  geom_line(size = 1) +
  labs(title = "Manufacturing Contribution Over Time by Development Status",
       x = "Year", y = "Average Manufacturing Contribution (%)") +
  theme_minimal()

# Plot Services Percentage over Time by Development Status
ggplot(aggregated_data, aes(x = Year, y = Avg_Services_Percentage, color = Development_Status)) +
  geom_line(size = 1) +
  labs(title = "Services Contribution Over Time by Development Status",
       x = "Year", y = "Average Services Contribution (%)") +
  theme_minimal()

#Analysis 4 Correlation Analysis

# Display all column names to verify their spelling
names(imputed_data)


# Select relevant columns for correlation analysis
selected_data <- imputed_data %>%
  select(
    `X.Agriculture..hunting..forestry..fishing..ISIC.A.B..`, 
    `Manufacturing..ISIC.D.`, 
    `X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..`, 
    `Gross.Domestic.Product..GDP.`, 
    `Gross.National.Income.GNI..in.USD`, 
    `Imports.of.goods.and.services`, 
    `Exports.of.goods.and.services`
  )

# Calculate correlation matrix
correlation_matrix <- cor(selected_data, use = "complete.obs")

# Display the correlation matrix
print(correlation_matrix)

# Verify column names first
names(imputed_data)

# Compute correlation matrix
correlation_matrix <- cor(imputed_data[, c(
  "X.Agriculture..hunting..forestry..fishing..ISIC.A.B..", 
  "Manufacturing..ISIC.D.", 
  "X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..", 
  "Gross.Domestic.Product..GDP.", 
  "Gross.National.Income.GNI..in.USD", 
  "Imports.of.goods.and.services", 
  "Exports.of.goods.and.services"
)], use = "complete.obs")

# Melt the correlation matrix for ggplot
library(reshape2)
correlation_melted <- melt(correlation_matrix)

# Plot the heatmap
library(ggplot2)
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Economic Indicators", 
       x = "Variables", 
       y = "Variables")

#Analysis 5 Regression Analysis

regression_model <- lm(
  log1p(Gross.National.Income.GNI..in.USD) ~ 
    log1p(X.Agriculture..hunting..forestry..fishing..ISIC.A.B..) +
    log1p(Manufacturing..ISIC.D.) +
    log1p(X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..) +
    Exports.of.goods.and.services +
    Imports.of.goods.and.services +
    Population,
  data = imputed_data
)
summary(regression_model)

library(car)
vif(regression_model)

par(mfrow = c(2, 2))  # Arrange diagnostic plots in a grid
plot(regression_model)

hist(residuals(regression_model), main = "Histogram of Residuals", xlab = "Residuals")


# Check for negative values
summary(imputed_data$Manufacturing..ISIC.D.)

# Find rows with negative or missing values
imputed_data[imputed_data$Manufacturing..ISIC.D. <= 0 | is.na(imputed_data$Manufacturing..ISIC.D.), ]

imputed_data$Manufacturing..ISIC.D.[imputed_data$Manufacturing..ISIC.D. < 0] <- 0

imputed_data$log_Manufacturing <- log1p(imputed_data$Manufacturing..ISIC.D.)

imputed_data$NetExports <- imputed_data$Exports - imputed_data$Imports


regression_model <- lm(
  log1p(Gross.National.Income.GNI..in.USD) ~ 
    log1p(X.Agriculture..hunting..forestry..fishing..ISIC.A.B..) +
    log_Manufacturing +
    log1p(X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..) +
    NetExports + 
    Population, 
  data = imputed_data
)

summary(regression_model)

library(car)
vif(regression_model)

par(mfrow = c(2, 2))
plot(regression_model)

#Model Validation

# Set seed for reproducibility
set.seed(123)

# Create a training index
train_index <- sample(1:nrow(imputed_data), size = 0.7 * nrow(imputed_data))

# Split the data
train_data <- imputed_data[train_index, ]
test_data <- imputed_data[-train_index, ]

# Fit the model on training data
regression_model_train <- lm(
  log1p(Gross.National.Income.GNI..in.USD) ~ 
    log1p(X.Agriculture..hunting..forestry..fishing..ISIC.A.B..) +
    log_Manufacturing +
    log1p(X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..) +
    NetExports +
    Population,
  data = train_data
)

# Summary of the model
summary(regression_model_train)

# Make predictions on test data
predicted_values <- predict(regression_model_train, newdata = test_data)

# Actual values from the test dataset
actual_values <- log1p(test_data$Gross.National.Income.GNI..in.USD)

# Calculate MSE
mse <- mean((actual_values - predicted_values)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAPE
mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100

# Print results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")

library(ggplot2)

# Create a scatter plot
ggplot(data = NULL, aes(x = actual_values, y = predicted_values)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Predicted vs. Actual GNI (Log-Transformed)",
    x = "Actual GNI (Log-Transformed)",
    y = "Predicted GNI (Log-Transformed)"
  ) +
  theme_minimal()

set.seed(123) # Set a seed for reproducibility
split <- sample(2, nrow(imputed_data), replace = TRUE, prob = c(0.8, 0.2))
train_data <- imputed_data[split == 1, ]
validation_data <- imputed_data[split == 2, ]

# 1. Generate predictions for the validation dataset
validation_data$predicted_GNI <- predict(regression_model, newdata = validation_data)

# 2. Compute metrics for the validation dataset
# Transform log values back to the original scale for better interpretation
actual_GNI <- exp(validation_data$Gross.National.Income.GNI..in.USD) - 1
predicted_GNI <- exp(validation_data$predicted_GNI) - 1

# Mean Squared Error (MSE)
mse_validation <- mean((actual_GNI - predicted_GNI)^2)

# Root Mean Squared Error (RMSE)
rmse_validation <- sqrt(mse_validation)

# Mean Absolute Percentage Error (MAPE)
mape_validation <- mean(abs((actual_GNI - predicted_GNI) / actual_GNI)) * 100


summary(validation_data)

validation_data$log_Manufacturing <- log1p(validation_data$Manufacturing..ISIC.D.)
validation_data$log_Agriculture <- log1p(validation_data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B..)
validation_data$log_Retail <- log1p(validation_data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H..)

validation_data[validation_data$Manufacturing..ISIC.D. < 0, "Manufacturing..ISIC.D."] <- 0
validation_data[validation_data$X.Agriculture..hunting..forestry..fishing..ISIC.A.B.. < 0, "X.Agriculture..hunting..forestry..fishing..ISIC.A.B.."] <- 0
validation_data[validation_data$X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H.. < 0, "X.Wholesale..retail.trade..restaurants.and.hotels..ISIC.G.H.."] <- 0

validation_data$predicted_GNI <- predict(regression_model, newdata = validation_data)

# Calculate Metrics
mse_validation <- mean((validation_data$predicted_GNI - log1p(validation_data$Gross.National.Income.GNI..in.USD))^2)
rmse_validation <- sqrt(mse_validation)
mape_validation <- mean(abs((exp(validation_data$predicted_GNI) - validation_data$Gross.National.Income.GNI..in.USD) /
                              validation_data$Gross.National.Income.GNI..in.USD)) * 100

# Print Metrics
cat("Validation Metrics:\n")
cat("Mean Squared Error (MSE):", mse_validation, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_validation, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape_validation, "%\n")

ggplot(validation_data, aes(x = log1p(Gross.National.Income.GNI..in.USD), y = predicted_GNI)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs. Actual GNI (Log-Transformed)", x = "Actual GNI (Log-Transformed)", y = "Predicted GNI (Log-Transformed)")


#Residual Analysis
# Residual Analysis Plots
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid

# Residuals vs Fitted
plot(regression_model, which = 1, main = "Residuals vs Fitted")

# Normal Q-Q Plot
plot(regression_model, which = 2, main = "Normal Q-Q Plot")

# Scale-Location Plot
plot(regression_model, which = 3, main = "Scale-Location Plot")

# Residuals vs Leverage (Cook's Distance)
plot(regression_model, which = 5, main = "Residuals vs Leverage")

