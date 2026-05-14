# checking working directory
getwd()

# importing the dataset
loan_data <- read.csv("preprocessed_loanData_without_encoding.csv", header = TRUE, sep = ",")

# Displaying first few rows of dataset
head(loan_data)

# Checking the structure dataset
str(loan_data)

# Load necessary library
library(dplyr)

# Separating numerical and categorical (statistical) data
numerical_data <- loan_data %>%
  select_if(is.numeric)

categorical_data <- loan_data %>%
  select_if(~ !is.numeric(.))

# Displaying first few rows of numerical data
print("Numerical Data:")
head(numerical_data)

# Displaying the first few rows of categorical data
print("Categorical Data:")
head(categorical_data)

# Check the structure to confirm separation
print("Structure of Numerical Data:")
str(numerical_data)

print("Structure of Categorical Data:")
str(categorical_data)

# Saving separated data to CSV for reference if needed in future
write_csv(numerical_data, "numerical_data.csv")
write_csv(categorical_data, "categorical_data.csv")

#Visualization Numerical Data

# Age vs Loan Approved

# Creating a histogram for Age vs Loan Approved
ggplot(loan_data, aes(x = Age, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Age vs Loan Approved",
    x = "Age",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Annual income vs Loan Approved

# Creating a histogram for Annual Income vs Loan Approved
ggplot(loan_data, aes(x = AnnualIncome, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 10000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Annual Income vs Loan Approved",
    x = "Annual Income",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )
# Credit score vs Loan approved

# Creating histogram for Credit Score vs Loan Approved
ggplot(loan_data, aes(x = CreditScore, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 20, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Credit Score vs Loan Approved",
    x = "Credit Score",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Experience vs Loan Approved 

# Creating histogram for Experience vs Loan Approved
ggplot(loan_data, aes(x = Experience, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Experience vs Loan Approved",
    x = "Years of Experience",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Loan Amount vs Loan Approved

# Creating histogram for Loan Amount vs Loan Approved
ggplot(loan_data, aes(x = LoanAmount, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Loan Amount vs Loan Approved",
    x = "Loan Amount (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Loan Duration vs loan approved

# Histogram for Loan Duration vs Loan Approved
ggplot(loan_data, aes(x = LoanDuration, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 6, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Loan Duration vs Loan Approved",
    x = "Loan Duration (in months)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Number of dependent vs Loan Approved

#histogram for Number of Dependents vs Loan Approved
ggplot(loan_data, aes(x = NumberOfDependents, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Number of Dependents vs Loan Approved",
    x = "Number of Dependents",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Monthly debt payments vs Loan Approved 

# Creating histogram for Monthly Debt Payment vs Loan Approved
ggplot(loan_data, aes(x = MonthlyDebtPayments, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 100, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Monthly Debt Payment vs Loan Approved",
    x = "Monthly Debt Payment (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Credit card utilization rate vs Loan Approved

# Creating histogram for Credit Card Utilization vs Loan Approved
ggplot(loan_data, aes(x = CreditCardUtilizationRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Credit Card Utilization vs Loan Approved",
    x = "Credit Card Utilization Rate",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Number of open credit lines vs Loan Approved

# Histogram for Number of Open Credit Lines vs Loan Approved
ggplot(loan_data, aes(x = NumberOfOpenCreditLines, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red","green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Number of Open Credit Lines vs Loan Approved",
    x = "Number of Open Credit Lines",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Number of credit inquires vs Loan Approved

# Histogram for Number of Credit Inquiries vs Loan Approved
ggplot(loan_data, aes(x = NumberOfCreditInquiries, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Number of Credit Inquiries vs Loan Approved",
    x = "Number of Credit Inquiries",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Debt total income ration vs Loan approved

# Creating histogram for Debt-to-Income Ratio vs Loan Approved
ggplot(loan_data, aes(x = TotalDebtToIncomeRatio, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.07, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Debt-to-Income Ratio vs Loan Approved",
    x = "Debt-to-Income Ratio",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

#Bankrupt history vs Loan Approved 


# Creating a histogram for Bankruptcy History vs Loan Approved
ggplot(loan_data, aes(x = BankruptcyHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Bankruptcy History vs Loan Approved",
    x = "Bankruptcy History (0 = No, 1 = Yes)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Previous loan defaults vs Loan Approved

# Creating histogram for Previous Loan Defaults vs Loan Approved
ggplot(loan_data, aes(x = PreviousLoanDefaults, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Previous Loan Defaults vs Loan Approved",
    x = "Previous Loan Defaults (0 = No, 1 = Yes)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# payment history vs Loan Approved

# histogram for Payment History vs Loan Approved
ggplot(loan_data, aes(x = PaymentHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Payment History vs Loan Approved",
    x = "Payment History (Number of Late Payments)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Length of credit history vs Loan approved

# Creating histogram for Length of Credit History vs Loan Approved
ggplot(loan_data, aes(x = LengthOfCreditHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Length of Credit History vs Loan Approved",
    x = "Length of Credit History (Years)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# savings account balance vs Loan Approved 

# Creating histogram for Savings Account Balance vs Loan Approved (limited to $50,000)
ggplot(loan_data, aes(x = SavingsAccountBalance, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Savings Account Balance vs Loan Approved (Limited to $50,000)",
    x = "Savings Account Balance (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  xlim(0, 50000) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Checking account balance vs Loan approved

# A histogram for Checking Account Balance vs Loan Approved
ggplot(loan_data, aes(x = CheckingAccountBalance, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Checking Account Balance vs Loan Approved",
    x = "Checking Account Balance (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  xlim(0, 17000) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Total assets vs Loan approved

# Create a histogram for Total Assets vs Loan Approved
ggplot(loan_data, aes(x = TotalAssets, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 50000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Total Assets vs Loan Approved",
    x = "Total Assets (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

#Total Liabilities vs Loan Approved 

# A histogram for Total Liabilities vs Loan Approved
ggplot(loan_data, aes(x = TotalLiabilities, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 30000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Total Liabilities vs Loan Approved",
    x = "Total Liabilities (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Monthly income vs Loan Approved 

# Creating a histogram for Monthly Income vs Loan Approved
ggplot(loan_data, aes(x = MonthlyIncome, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Monthly Income vs Loan Approved",
    x = "Monthly Income (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Utility bills payment history vs Loan approved 

# Create a histogram for Utility Bill Payment History vs Loan Approved
ggplot(loan_data, aes(x = UtilityBillsPaymentHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Utility Bill Payment History vs Loan Approved",
    x = "Utility Bills Payment History (Number of Late Payments)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Job tenure vs Loan Approved 

# A histogram for Job Tenure vs Loan Approved
ggplot(loan_data, aes(x = JobTenure, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Job Tenure vs Loan Approved",
    x = "Job Tenure (Years)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Networth vs Loan approved 

# Create a histogram for Net Worth vs Loan Approved
ggplot(loan_data, aes(x = NetWorth, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 10000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Net Worth vs Loan Approved",
    x = "Net Worth (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Base interest rate vs loan approved 

#Creating a histogram for base interst rate vs Loan Approved
ggplot(loan_data, aes(x = BaseInterestRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.002, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Base Interest Rate vs Loan Approved",
    x = "Base Interest Rate (%)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Interest rate vs loan approved

# Histogram for Interest Rate vs Loan Approved
ggplot(loan_data, aes(x = InterestRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.005, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Interest Rate vs Loan Approved",
    x = "Interest Rate (%)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  ) 
# Monthly loan payment vs Loan Approved

# Create a histogram for Monthly Loan Payment vs Loan Approved
ggplot(loan_data, aes(x = MonthlyLoanPayment, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 100, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Monthly Loan Payment vs Loan Approved",
    x = "Monthly Loan Payment (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Total debt income ratio vs Loan Approved 

# Create a histogram for Total Debt-to-Income Ratio vs Loan Approved
ggplot(loan_data, aes(x = TotalDebtToIncomeRatio, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.05, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Total Debt-to-Income Ratio vs Loan Approved",
    x = "Total Debt-to-Income Ratio (DTI)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )

# Risk score vs Loan Approved 

# Create a histogram for Risk Score vs Loan Approved
ggplot(loan_data, aes(x = RiskScore, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Histogram of Risk Score vs Loan Approved",
    x = "Risk Score",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  )


