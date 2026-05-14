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

# Separating numerical and categorical data
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


######################################################
##                                                  ##
##                Visualization Numerical Data      ##
##                                                  ##
######################################################


####    Age vs Loan Approved


# Creating a histogram for Age vs Loan Approved
agevsloan_p <- ggplot(loan_data, aes(x = Age, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 100, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Age vs Loan Approved",
    x = "Age (Years)", 
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7), 
    axis.text = element_text(size = 7),                
    axis.title = element_text(size = 7),            
    
  )

#######   Understanding from the Graph(Age vs Loan Approved)   ##########

## The density of applicants who were not approved seems to be distributed across a wide range of ages.
## However, there appears to be a higher concentration of rejections among younger applicants (Ex. ages 20–30) compared to older ones.
## Overlapping Regions:There is significant overlap between approved and not approved loans in the 30–40 age range, suggesting age alone may not be the sole determining factor for loan approval in this group.
## Thresholds or Patterns: (a) Younger applicants (under 30) are more likely to face loan rejection, possibly due to factors like shorter credit histories or lower income levels.
## (b) Middle-aged applicants (30–50) have the highest loan approval rates, making this age group favorable for loan applications.




#######  Annual income vs Loan Approved


# Creating a histogram for Annual Income vs Loan Approved
annualincome_p <- ggplot(loan_data, aes(x = AnnualIncome, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 15000, position = "dodge", alpha = 1) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  scale_x_continuous(labels = dollar_format()) +  # Format x-axis as dollars
  labs(
    title = "Annual Income vs Loan Approved",
    x = "Annual Income (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    axis.text = element_text(size = 7),               
    axis.title = element_text(size = 7),              
    
  )

#########   Understanding from the Graph(Annual income vs Loan Approved)   #############

## $50,000 Annual Income Threshold: This appears to be a rough threshold, with higher approval rates for incomes above this range.
## Approved loans increase sharply in the $60,000–$100,000 range, suggesting that higher income can boost the loan approval chances.
## Overlap Region: Incomes between $40,000–$60,000 show significant overlap between approved and rejected loans, indicating that other factors (Ex. credit score or debt-to-income ratio) might influence the decisions in this range.




####  Credit score vs Loan approved


# Creating histogram for Credit Score vs Loan Approved
creditscore_p <- ggplot(loan_data, aes(x = CreditScore, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 20, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Credit Score vs Loan Approved",
    x = "Credit Score",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    
  )

#######   Understanding from the Graph(Credit Score vs Loan Approved)   ##########

## Most approved loans are concentrated in the 650–800 credit score range, indicating this as a favorable range for loan approvals.
## A large proportion of loan rejections belong to applicants with credit scores below 600.
## Critical Threshold: Around 600 credit score, there is a noticeable transition where approvals start increasing, and loan rejections start decreasing.
## Overlapping Range: Scores between 550–650 show significant overlap between approvals and rejections, indicating that other factors (Ex. income, loan amount) may influence decisions in this range.




####   Experience vs Loan Approved 


# Creating histogram for Experience vs Loan Approved
experience_p <- ggplot(loan_data, aes(x = Experience, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Experience vs Loan Approved",
    x = "Years of Experience",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Experience vs Loan Approved)   ##########

## Applicants with fewer years of experience (less than 5 or 3 years) have a noticeably higher frequency of loan denials.
## Most approvals are concentrated among applicants with 10–20 years of experience.
## Very few approvals occur for applicants with less than 5 years of experience, indicating this is considered a high-risk group.
## Critical Threshold: Around 10 years of experience, the likelihood of approval increases significantly, while denials drop.
## Overlap Region: In the 5–10 years range, both approvals and denials are observed, suggesting other factors (e.g., income, credit score) influence decisions in this range.




####    Loan Amount vs Loan Approved


# Creating histogram for Loan Amount vs Loan Approved
loanamount_p <- ggplot(loan_data, aes(x = LoanAmount, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Loan Amount vs Loan Approved",
    x = "Loan Amount (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Loan Amount vs Loan Approved)   ##########

## Higher number of loan rejections are observed for loan amounts above $25,000.
## A significant portion of denied loans is in the $15,000–$30,000 range, indicating that larger loan amounts increase rejection likelihood.
## Approved loans are concentrated in the $5,000–$20,000 range, suggesting this is a favorable range for loan approvals.
## Critical Threshold: Loan amounts exceeding $25,000 are associated with higher loan rejection rates.
## Overlap Region: Loan amounts in the $15,000–$25,000 range show both approvals and rejections, suggesting that other factors (credit score, income) can influence decisions in this range.




#### Loan Duration vs loan approved


# Histogram for Loan Duration vs Loan Approved
loanduration_p <- ggplot(loan_data, aes(x = LoanDuration, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 6, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Loan Duration vs Loan Approved",
    x = "Loan Duration (in months)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Loan Duration vs Loan Approved)   ##########

## High frequencies of loan rejections are occurs for longer duration particularly beyond 35 months. 
## Rejections are relatively evenly spread across short durations but it increase significantly for loans exceeding 60 months.
## Most approved loans have durations in the 24–60 months range, indicating that shorter durations are more favorable for loan approval. 
## Critical Threshold: Loan durations beyond 60 months are associated with higher rejection rates.
## Overlap Region: The 36–60 months range shows both approvals and rejections, suggesting other factors such as creditworthiness or income can influence the decisions here.




#### Number of dependent vs Loan Approved


#histogram for Number of Dependents vs Loan Approved
dependents_p <- ggplot(loan_data, aes(x = NumberOfDependents, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Number of Dependents vs Loan Approved",
    x = "Number of Dependents",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Number of Dependents vs Loan Approved)   ##########

## Approvals are more frequent among applicants with 0–2 dependents, indicating that fewer dependents may positively influence loan decisions.
## The approval rate decreases for applicants with 3 or more dependents, possibly due to perceived financial strain.
## Critical Threshold: Applicants with 3 or more dependents are more likely to face loan rejections.
## Overlap Region: Applicants with 2–3 dependents show both approvals and rejections, suggesting other factors like income or loan amount may play a important role.




#### Monthly debt payments vs Loan Approved 


# Creating histogram for Monthly Debt Payment vs Loan Approved
monthlydebtpay_p <- ggplot(loan_data, aes(x = MonthlyDebtPayments, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 100, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Monthly Debt Payment vs Loan Approved",
    x = "Monthly Debt Payment (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Monthly debt payment vs Loan Approved)   ##########

## Loan rejections are more frequent for applicants with debt payment exceeding $500. 
## A significant portion of rejection is occurs in $300-$800 range, likely due to higher financial obligations.
## Approved loans are concentrated in the $0–$400 range, indicating that lower monthly debt payments positively influence loan decisions.
## Critical Threshold: Applicants with monthly debt payments exceeding $500 are more likely to face loan rejections.
## Overlap Region: The $300-$500 range shows both approvals and rejections, and it also suggest other factors like income, loan amount, or credit score can influence the decision in this range.   




#### Credit card utilization rate vs Loan Approved


# Creating histogram for Credit Card Utilization vs Loan Approved
creditcardutilization_p <- ggplot(loan_data, aes(x = CreditCardUtilizationRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Credit Card Utilization vs Loan Approved",
    x = "Credit Card Utilization Rate",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )


#### Number of open credit lines vs Loan Approved


# Histogram for Number of Open Credit Lines vs Loan Approved
opencredit_p <- ggplot(loan_data, aes(x = NumberOfOpenCreditLines, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange","lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Open Credit Lines vs Loan Approved",
    x = "Number of Open Credit Lines",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### Number of credit inquires vs Loan Approved


# Histogram for Number of Credit Inquiries vs Loan Approved
creditinquiries_p <- ggplot(loan_data, aes(x = NumberOfCreditInquiries, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Number of Credit Inquiries vs Loan Approved",
    x = "Number of Credit Inquiries",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### Debt total income ration vs Loan approved


# Creating histogram for Debt-to-Income Ratio vs Loan Approved
debtincome_p <- ggplot(loan_data, aes(x = TotalDebtToIncomeRatio, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Debt-to-Income Ratio vs Loan Approved",
    x = "Debt-to-Income Ratio",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

####  Bankrupt history vs Loan Approved 


# Creating a histogram for Bankruptcy History vs Loan Approved
bankrupcyhist_p <- ggplot(loan_data, aes(x = BankruptcyHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 500, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Bankruptcy History vs Loan Approved",
    x = "Bankruptcy History (0 = No, 1 = Yes)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### Previous loan defaults vs Loan Approved


# Histogram for Previous Loan Defaults vs Loan Approved
previousloandef_p <- ggplot(loan_data, aes(x = PreviousLoanDefaults, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Previous Loan Defaults vs Loan Approved",
    x = "Previous Loan Defaults (0 = No, 1 = Yes)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### payment history vs Loan Approved


# histogram for Payment History vs Loan Approved
payhistory_p <- ggplot(loan_data, aes(x = PaymentHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 500, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Payment History vs Loan Approved",
    x = "Payment History (Number of Late Payments)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### Length of credit history vs Loan approved


# Creating histogram for Length of Credit History vs Loan Approved
credithistory_p <- ggplot(loan_data, aes(x = LengthOfCreditHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Length of Credit History vs Loan Approved",
    x = "Length of Credit History (Years)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#### savings account balance vs Loan Approved 


# Creating histogram for Savings Account Balance vs Loan Approved (limited to $50,000)
savingaccbal_p <- ggplot(loan_data, aes(x = SavingsAccountBalance, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 5000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Savings Account Balance vs Loan Approved (Limited to $50,000)",
    x = "Savings Account Balance (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  xlim(0, 50000) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

####   Checking account balance vs Loan approved

# A histogram for Checking Account Balance vs Loan Approved

checkaccbal_p <- ggplot(loan_data, aes(x = CheckingAccountBalance, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1000, position = "dodge", alpha = 1) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Checking Account Balance vs Loan Approved",
    x = "Checking Account Balance (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  xlim(0, 17000) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )


#### Total assets vs Loan approved


# Created a histogram for Total Assets vs Loan Approved

totalassets_p <- ggplot(loan_data, aes(x = TotalAssets, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 500000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  scale_x_continuous(labels = dollar_format()) +  # Format x-axis as dollars
  labs(
    title = "Total Assets vs Loan Approved",
    x = "Total Assets (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title alignment and size
    axis.title = element_text(size = 9),               # Adjust axis title size
    axis.text = element_text(size = 8),                # Adjust axis text size
    legend.text = element_text(size = 7),              # Adjust legend text size
    legend.title = element_text(size = 8),             # Adjust legend title size
    legend.position = "top",                           # Place legend at the top
    plot.margin = margin(10, 10, 10, 10)               # Add spacing around the plot to prevent overlapping
  )



####  Total Liabilities vs Loan Approved 


# Creating a histogram for Total Liabilities vs Loan Approved with fixes
totallib_p <- ggplot(loan_data, aes(x = TotalLiabilities, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 50000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  scale_x_continuous(labels = dollar_format()) +  # Format x-axis as dollars
  labs(
    title = "Total Liabilities vs Loan Approved",
    x = "Total Liabilities (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title alignment and size
    axis.title = element_text(size = 9),               # Adjust axis title size
    axis.text = element_text(size = 8),                # Adjust axis text size
    legend.text = element_text(size = 8),              # Adjust legend text size
    legend.position = "top",                           # Move legend to the top
    plot.margin = margin(10, 10, 10, 10)               # Adding spacing around the plot
  )




#### Monthly income vs Loan Approved 


# Creating a histogram for Monthly Income vs Loan Approved
mincome_p <- ggplot(loan_data, aes(x = MonthlyIncome, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1000, position = "dodge", alpha = 1) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Monthly Income vs Loan Approved",
    x = "Monthly Income (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )



#######   Understanding from the Graph(Monthly income vs Loan Approved)   ##########

## Loan rejections are more frequent among applicants with monthly incomes below $5,000.
## The loan rejections are gradually decrease as monthly income exceeds $5,000, indicating that higher income levels positively influence approval likelihood.
## Approved loans are concentrated among applicants with monthly incomes between $4,000 and $8,000, suggesting this range is favorable for loan approvals.
## Few approvals occur for applicants with monthly incomes below $3,000, likely due to insufficient financial capacity to manage loan obligations.
## Critical Threshold: Applicants with monthly incomes below $4,000 are significantly more likely to face loan denials.
## Overlap Region: The $4,000–$6,000 range shows both loan approvals and rejections, indicating other factors like debt obligations or credit scores play an important role.




#### Utility bills payment history vs Loan approved 


# Create a histogram for Utility Bill Payment History vs Loan Approved
utibillpay_p <- ggplot(loan_data, aes(x = UtilityBillsPaymentHistory, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.05, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Utility Bill Payment History vs Loan Approved",
    x = "Utility Bills Payment History (Number of Late Payments)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )


#### Job tenure vs Loan Approved 


# A histogram for Job Tenure vs Loan Approved
jobtenure_p <- ggplot(loan_data, aes(x = JobTenure, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Job Tenure vs Loan Approved",
    x = "Job Tenure (Years)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )



#### Networth vs Loan approved 

# Create a histogram for Net Worth vs Loan Approved with fixes
networth_p <- ggplot(loan_data, aes(x = NetWorth, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 100000, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  scale_x_continuous(labels = dollar_format()) +  # Format x-axis as dollars
  labs(
    title = "Net Worth vs Loan Approved",
    x = "Net Worth (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  # Center align the title and adjust its size
    axis.title = element_text(size = 10),               # Adjust axis title size
    axis.text = element_text(size = 9),                 # Adjust axis text size
    legend.text = element_text(size = 9),               # Adjust legend text size
    legend.title = element_text(size = 10),             # Adjust legend title size
    legend.position = "top",                            # Place legend at the top
    plot.margin = margin(10, 10, 10, 10)                # Add spacing around the plot to avoid overlap
  )





#### Base interest rate vs loan approved 


#Creating a histogram for base interst rate vs Loan Approved
baseintrate_p <- ggplot(loan_data, aes(x = BaseInterestRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.02, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Base Int Rate vs Loan Approved",
    x = "Base Interest Rate (%)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )



#### Interest rate vs loan approved


# Histogram for Interest Rate vs Loan Approved
interestrate_p <- ggplot(loan_data, aes(x = InterestRate, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.05, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Interest Rate vs Loan Approved",
    x = "Interest Rate (%)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  ) 


#######   Understanding from the Graph(Interest Rate vs Loan Approved)   ##########

## Loan denials are more frequent for applicants with interest rates above 7%, likely due to affordability concerns or higher-risk profiles.
## Loan rejections are decrease significantly for loans with interest rates below 5%, suggesting that lower rates positively impact loan approval chances.
## Approved loans are concentrated at interest rates between 3% and 6%, indicating this range is favorable for loan approvals.
## Critical Threshold: Loans with interest rates above 7% are significantly more likely to face denials.
## Overlap Region: The 5%–7% range shows both approvals and denials, suggesting that other factors like income, credit score, or debt-to-income ratio play a role.




####  Monthly loan payment vs Loan Approved


# Create a histogram for Monthly Loan Payment vs Loan Approved
monthlyloanpay_p <- ggplot(loan_data, aes(x = MonthlyLoanPayment, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 200, position = "dodge", alpha = 1) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Monthly Loan Payment vs Loan Approved",
    x = "Monthly Loan Payment (in $)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )


#######   Understanding from the Graph(Monthly loan payment vs Loan Approved)   ##########

## Loan denials are significantly higher for applicants with monthly loan payments exceeding $700.
## Approved loans are concentrated among applicants with monthly loan payments between $300 and $600, suggesting this range is favorable for approvals.
## Critical Threshold: Applicants with monthly loan payments above $700 are more likely to face denials.
## Overlap Region: The $500–$700 range shows both approvals and denials, suggesting other factors like income, savings, or credit score influence decisions.




####    Total debt income ratio vs Loan Approved 


# Create a histogram for Total Debt-to-Income Ratio vs Loan Approved
totaldebt_p <- ggplot(loan_data, aes(x = TotalDebtToIncomeRatio, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Total Debt-to-Income Ratio vs Loan Approved",
    x = "Total Debt-to-Income Ratio (DTI)",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )
print(totaldebt_p)

####  Risk score vs Loan Approved 


# Create a histogram for Risk Score vs Loan Approved
riskscore_p <- ggplot(loan_data, aes(x = RiskScore, fill = factor(LoanApproved))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("orange", "lightblue"), labels = c("Not Approved", "Approved")) +
  labs(
    title = "Risk Score vs Loan Approved",
    x = "Risk Score",
    y = "Count",
    fill = "Loan Approved"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 7),
    legend.position = "top"
  )

#######   Understanding from the Graph(Risk Score vs Loan Approved)   ##########

## Loan rejections are more frequent among applicants with low risk scores (below 50), indicating that lower scores are associated with higher risk profiles.
## Approved loans are concentrated among applicants with risk scores between 60 and 80, suggesting this range is favorable for loan approvals.
## Critical Threshold: Applicants with risk scores below 50 are more likely to face denials.
## Overlap Region: The 50–60 risk score range shows both approvals and denials, suggesting that other factors like income, credit score, or debt obligations may influence decisions in this range.


#########################################################################################################
###########################################################################################################

x = list()
x <- append(x, agevsloan_p)
x <- append(x, annualincome_p)
x <- append(x, creditscore_p)
x <- append(x, experience_p)
x <- append(x, loanamount_p)
x <- append(x, loanduration_p)
x <- append(x, dependents_p)
x <- append(x, monthlydebtpay_p)
x <- append(x, creditcardutilization_p)
x <- append(x, opencredit_p)
x <- append(x, creditinquiries_p)
x <- append(x, debtincome_p)
x <- append(x, bankrupcyhist_p)
x <- append(x, previousloandef_p)
x <- append(x, payhistory_p)
x <- append(x, credithistory_p)
x <- append(x, savingaccbal_p)
x <- append(x, checkaccbal_p)
x <- append(x, totalassets_p)
x <- append(x, totallib_p)
x <- append(x, mincome_p)
x <- append(x, utibillpay_p)
x <- append(x, jobtenure_p)
x <- append(x, networth_p)
x <- append(x, baseintrate_p)
x <- append(x, interestrate_p)
x <- append(x, monthlyloanpay_p)
x <- append(x, totaldebt_p)
x <- append(x, riskscore_p)


print(x)



##############################################################################
# For 1-8 
# Standardizing the plot themes for better appearance

theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title size and alignment
    axis.title = element_text(size = 8),               # Adjust axis title size
    axis.text = element_text(size = 7),                # Adjust axis text size
    legend.text = element_text(size = 7),              # Adjust legend text size
    legend.title = element_text(size = 8),             # Adjust legend title size
    legend.position = "top",                           # Place legend at the top
    plot.margin = margin(5, 5, 5, 5)                   # Adding spacing around plots
  )

# Applying the custom theme to each plot in the list
x_subset <- list(
  agevsloan_p + theme_custom,
  annualincome_p + theme_custom,
  creditscore_p + theme_custom,
  experience_p + theme_custom,
  loanamount_p + theme_custom,
  loanduration_p + theme_custom,
  dependents_p + theme_custom,
  monthlydebtpay_p + theme_custom
)

# Combine the plots with a 2-column layout and add spacing
combined_plot <- wrap_plots(x_subset, ncol = 2, nrow = 4) +
  plot_annotation(
    title = "Loan Approval Analysis - Visualization",
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5),  # Adding title for the combined plot
      plot.margin = margin(10, 10, 10, 10)               # Increasing spacing around combined plot
    )
  )

# Visualizing the plot
print(combined_plot)

####################################################################################################################################################
# For 9-16
# Standardizing the plot themes for better appearance


theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title size and alignment
    axis.title = element_text(size = 8),               # Adjust axis title size
    axis.text = element_text(size = 7),                # Adjust axis text size 
    legend.text = element_text(size = 7),              # Adjust legend text size
    legend.title = element_text(size = 8),             # Adjust legend title size
    legend.position = "top",                           # Place legend at the top
    plot.margin = margin(5, 5, 5, 5)                   # Add spacing around plots
  )

# Applying the custom theme to each plot in the list
x_subset <- list(
  creditcardutilization_p + theme_custom,
  opencredit_p + theme_custom,
  creditinquiries_p + theme_custom,
  debtincome_p + theme_custom,
  bankrupcyhist_p + theme_custom,
  previousloandef_p + theme_custom,
  payhistory_p + theme_custom,
  credithistory_p + theme_custom
)

# Combine the plots with a 2-column layout and add spacing
combined_plot <- wrap_plots(x_subset, ncol = 2, nrow = 4) +
  plot_annotation(
    title = "Loan Approval Analysis - Credit & History Factors",
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5),  # Adding a title for the combined plot
      plot.margin = margin(10, 10, 10, 10)               # Increase spacing around combined plot
    )
  )

# Visualize the adjusted combined plot
print(combined_plot)


##############################################################
# 16-22
# Standardizing the plot themes for better appearance

theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title size and alignment
    axis.title = element_text(size = 8),               # Adjust axis title size
    axis.text = element_text(size = 7),                # Adjust axis text size 
    legend.text = element_text(size = 7),              # Adjust legend text size
    legend.title = element_text(size = 8),             # Adjust legend title size
    legend.position = "top",                           # Place legend at the top
    plot.margin = margin(5, 5, 5, 5)                   # Add spacing around plots
  )

# Applying the custom theme to each plot in the list
x_subset <- list(
  totalassets_p + theme_custom,
  totallib_p + theme_custom,
  mincome_p + theme_custom,
  utibillpay_p + theme_custom,
  jobtenure_p + theme_custom,
  networth_p + theme_custom
)

# Combine the plots with a 2-column layout and add spacing
combined_plot <- wrap_plots(x_subset, ncol = 3, nrow = 8) +
  plot_annotation(
    title = "Loan Approval Analysis - Visualization",
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5),  # Adding a title for the combined plot
      plot.margin = margin(10, 10, 10, 10)               # Increase spacing around combined plot
    )
  )

# Visualize the adjusted combined plot
print(combined_plot)

################################################################################
#22-27
# Standardizing the plot themes for better appearance

theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title size and alignment
    axis.title = element_text(size = 8),               # Adjust axis title size
    axis.text = element_text(size = 7),                # Adjust axis text size 
    legend.text = element_text(size = 7),              # Adjust legend text size
    legend.title = element_text(size = 8),             # Adjust legend title size
    legend.position = "top",                           # Place legend at the top
    plot.margin = margin(5, 5, 5, 5)                   # Add spacing around plots
  )

# Applying the custom theme to each plot in the list
x_subset <- list(
  baseintrate_p + theme_custom,
  interestrate_p + theme_custom,
  monthlyloanpay_p + theme_custom,
  totaldebt_p + theme_custom,
  riskscore_p + theme_custom
)

# Combine the plots with a 2-column layout and added spacing
combined_plot <- wrap_plots(x_subset, ncol = 2, nrow = 3) +
  plot_annotation(
    title = "Loan Approval Analysis - Interest Rates and Scores",
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5),  # Adding a title for the combined plot
      plot.margin = margin(10, 10, 10, 10)               # Increase spacing around combined plot
    )
  )


print(combined_plot)

