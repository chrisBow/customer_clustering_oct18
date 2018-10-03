# Load package to read in Excel spreadsheet
library(readxl)


# Read in Excel file
raw_data <- read_excel("Online Retail.xlsx")


# Overview of raw data
library(dplyr)
glimpse(raw_data)


# Filter to include UK rows only
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

glimpse(uk_data)



# Create customer-centric dataset

customer_summary <-
  uk_data %>%
  group_by(CustomerID) %>%
  summarise(orders = n(),
            quantity = sum(Quantity),
            revenue = sum(Quantity * UnitPrice),
            last_order = max(date)) %>%
  mutate(avg_order_val = revenue / orders,
         avg_item_val = revenue / quantity)

head(customer_summary)


# Remove customers with net spend of £0

customer_summary_spend <-
  customer_summary %>%
  filter(revenue > 0)

dim(customer_summary)
dim(customer_summary_spend)


library(ggplot2)


# Plot histogram of average item value by count of customers

ggplot(customer_summary_spend, 
       aes(x = avg_item_val)) + 
  geom_histogram(binwidth = 0.05) +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.1, 100)) +
  labs(title = "Histogram of Customer Average Item Value",
       x = "Average item value (£)",
       y = "Count of customers") +
  theme_minimal()


# Find price range that covers 10% of our customers

quantile(customer_summary_spend$avg_item_val, c(0.45, 0.55))


# Create segment of customerIDs with an average item cost of 
# more than £1.70 and less than £1.87

customer_item_10pc <-
  customer_summary_spend %>%
  filter(avg_item_val > 1.7,
         avg_item_val < 1.87)

dim(customer_item_10pc)


# Look for most recent purchase

customer_summary_spend %>%
  arrange(desc(last_order)) %>%
  head()


# Haven't ordered in last six months

six_months <-
  customer_summary_spend %>%
  filter(last_order < "2011-07-01") %>%
  arrange(desc(last_order))

dim(six_months)


# Customers who ordered lights

library(stringr)

light_on <-
  uk_data %>%
  mutate(light = str_detect(uk_data$Description, "LIGHT")) %>%
  select(CustomerID, light) %>%
  unique()

table(light_on$light)


# Create customer segment

lighting_lovers <-
  light_on
  







