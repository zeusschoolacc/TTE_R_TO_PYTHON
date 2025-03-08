# Load necessary libraries
library(TrialEmulation)

# Load the dataset (assuming it comes from the current environment or package)
data("data_censored") # Ensure this dataset is available in memory

# Check the first few rows to confirm the data is loaded
head(data_censored)

# Save the dataset as a CSV file in the current working directory
write_csv(data_censored, "./data/data_censored.csv")

# Confirm the file has been saved successfully
print("Data saved as data_censored.csv")