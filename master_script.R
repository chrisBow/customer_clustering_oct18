# Load package to read in Excel spreadsheet
library(readxl)

# Read in Excel file
raw_data <- read_excel("../input/Online Retail.xlsx")

# Overview of raw data
str(raw_data)