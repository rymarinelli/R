#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

# Task One 
# Calculate Profit For Each Month
# Profit is found by subtracting rev by expense

profit <- revenue - expenses


#Task Two Finds Profit After Tax
taxProfit <- round(profit - (profit*.30),2)

#Task Three Finds Profit Margin 
profitMargin <- round((taxProfit/revenue)*100,2)


#Task Four Determines Good months 


averageProfit <- mean(profit)

testMonths <- profit - averageProfit

goodMonths <- paste("January", "June", "July","Aug","Sept", "December")
badMonths <- paste("Feb","March", "April", "May","October", "November")
bestMonth <- "December"
worstMonth <- "March"

revenueRounded <- round (revenue/1000)
expenses <- round (expenses/1000)
profit <- round (profit/1000)
 
