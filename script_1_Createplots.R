library(readr)
library(ggplot2)
data <- read_csv("SampleSuperstore.csv")
library(dplyr)

data

data_state = data %>% group_by(State, Segment)  %>%
  summarise(total_sales = sum(Sales),
            total_profits = sum(Profit),
            avg_discount = mean(Discount),
            .groups = 'drop')
data_state
nrow(data_state)


hist(x, breaks = bins, col = 'darkgray', border = 'white',
     xlab = 'Waiting time to next eruption (in mins)',
     main = 'Histogram of waiting times')

library(shiny)
profit <- data$Profit
bins <- seq(min(profit), max(profit), length.out = input$bins + 1)
