# Load package to read in Excel spreadsheet
library(readxl)


# Read in Excel file
raw_data <- read_excel("Online Retail.xlsx")


# Overview of raw data
str(raw_data)


# Filter to include UK rows only
library(dplyr)

uk_data <-
  raw_data %>%
  filter(Country == "United Kingdom")

dim(uk_data)


# Change some features to factors
uk_data <- 
  uk_data %>%
  mutate(InvoiceNo = factor(InvoiceNo),
         StockCode = factor(StockCode),
         CustomerID = factor(CustomerID),
         Country = factor(Country))

glimpse(uk_data)


# Create additional date and time features

library(lubridate)

uk_data <-
  uk_data %>%
  mutate(date = floor_date(InvoiceDate, "day"),
         hour = hour(InvoiceDate),
         day = wday(InvoiceDate, label = TRUE))

levels(uk_data$day)

weekend <- c("Sat", "Sun")

uk_data <-
  uk_data %>%
  mutate(is_weekend = day %in% weekend)


# 

library(ggplot2)

table(uk_data$day)

uk_data %>%
  group_by(day) %>%
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  ggplot(aes(x = day, y = revenue)) +
  geom_col()


