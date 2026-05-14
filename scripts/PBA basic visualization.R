# checking working directory
getwd()

# importing the dataset
loan <- read.csv("LoanData.csv", header = TRUE, sep = ",")  # Replace with your file path

# Basic Inspection of the Dataset 

cat("Structure of the dataset:\n")
str(loan)  # This displays the column names, types, and a preview of the data

cat("\nDimensions of the dataset:\n")
print(dim(loan))  # It prints the number of rows and columns

cat("\nSummary of the dataset:\n")
summary(loan)  # This generates the summary statistics for numeric and factor columns

cat("\nFirst few rows of the dataset:\n")
head(loan)  # Displays the first six rows to preview the data structure and content

# Looking for any missing values 

cat("\nMissing Values Check:\n")
missing_values <- colSums(is.na(loan))  # This counts NA values for each column
print(missing_values)

# Looking for Duplicates

cat("\nNumber of Duplicate Rows:\n")
duplicates <- sum(duplicated(loan))  # Counts rows with identical values across all columns
print(duplicates)

# Data Type Conversion

cat("\nConverting categorical columns to factors (if any):\n")
dataset <- loan %>%
  mutate_if(is.character, as.factor)  # Automatically converts text columns to factors
str(loan)  # Recheck structure after conversion


# Basic Visualization 

cat("\nVisualizing distributions of numerical columns:\n")
numerical_data <- dataset %>%
  select_if(is.numeric)  # Select only numeric columns

# Creating a histograms dynamically for each numeric variable
numerical_data %>%
  gather(key = "Variable", value = "Value") %>%  # Reshape data into long format
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "purple", alpha = 0.6) +  # Custom aesthetics
  facet_wrap(~ Variable, scales = "free") +  # Separate plots for each variable
  theme_minimal() +
  labs(title = "Numerical Variables Distribution",
       x = "Values",
       y = "Frequency")

# Correlation Heatmap
cat("\nGenerating Correlation Heatmap for Numerical Variables:\n")
correlation_matrix <- loan %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")  # Calculate pairwise correlations

# Convert correlation matrix to long format for ggplot
correlation_melt <- melt(correlation_matrix)

# Plot heatmap
ggplot(correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables",
       fill = "Correlation")


