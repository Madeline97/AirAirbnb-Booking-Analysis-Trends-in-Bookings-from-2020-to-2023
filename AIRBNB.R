library(dplyr)
library(ggplot2)
library(lubridate)

data_2023 <- read.csv("AB_US_2023.csv")
data_2020 <- read.csv("AB_US_2020.csv")

data_2023$last_review <- as.Date(data_2023$last_review)
data_2020$last_review <- as.Date(data_2020$last_review)

data_2023$year <- year(data_2023$last_review)
data_2023$month <- month(data_2023$last_review, label = TRUE)
data_2020$year <- year(data_2020$last_review)
data_2020$month <- month(data_2020$last_review, label = TRUE)

monthly_data_2023 <- data_2023 %>%
  group_by(year, month) %>%
  summarise(
    total_bookings = n(),
    average_price = mean(price, na.rm = TRUE)
  ) %>%
  filter(!is.na(total_bookings) & total_bookings != 0) 

monthly_data_2020 <- data_2020 %>%
  group_by(year, month) %>%
  summarise(
    total_bookings = n(),
    average_price = mean(price, na.rm = TRUE)
  ) %>%
  filter(!is.na(total_bookings) & total_bookings != 0)

data <- data.frame(
  month = c('Jan', 'Feb', 'Mar', 'Apr', 'May'),
  bookings = c(120, 140, 180, 150, 200)
)
data$month <- factor(data$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)

data$month <- factor(data$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)

data$bookings <- as.numeric(data$bookings)
data <- data[!is.na(data$bookings) & !is.na(data$month), ]


data$month <- factor(data$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)

data$bookings <- as.numeric(data$bookings)  
data <- na.omit(data)


ggplot(data, aes(x = month, y = bookings)) +
  geom_line(color = "purple", size = 1.5) +  
  geom_point(color = "#8A2BE2") +           
  geom_text(aes(label = bookings), vjust = -0.5, color = "#40E0D0", size = 5) +  
  labs(title = "Bookings by Month", x = "Month", y = "Number of Bookings") +
  annotate("text", x = 1, y = 200, label = "Data source: Airbnb Open Data", hjust = 0, vjust = 1, size = 4, color = "#FF1493") + 
  theme_minimal() 


peak_months_2023 <- monthly_data_2023 %>%
  filter(total_bookings == max(total_bookings)) %>%
  select(month, total_bookings)

print(peak_months_2023)

