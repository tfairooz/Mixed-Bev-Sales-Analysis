library(readr)
# Uploading the dataset to R from computer folder
Mixed_Beverage_Gross_Receipts <- 
  read_csv("~/Desktop/R_Project/Data Analysis Projects/Mixed_Beverage_Gross_Receipts.csv")
View(Mixed_Beverage_Gross_Receipts)

##Renaming the dataset:
MB <- Mixed_Beverage_Gross_Receipts
## plotting annual montlhy aggregate of the Total Receipts against year
# first step is to get the monthly aggregate sales per year. I created a new dataframe; MB1.
MB1 <- aggregate(MB["Total Receipts"],by=MB["Obligation End Date"],sum)

# changing name of variables
names(MB1)[names(MB1) == "Obligation End Date"] <- "Year"
names(MB1)[names(MB1) == "Total Receipts"] <- "Total"

# to plot the two variables in MB1:

# first create a tsibble object (MB1_ts)
install.packages("fpp3")
library("fpp3")

MB1_ts <- MB1 %>%
  mutate(Month = yearmonth(Year)) %>%
  select(- Year) %>%
  as_tsibble(index = Month) 

MB1_ts %>%
  mutate(Total = Total/1000000) %>%
  autoplot() +
  labs(y = "Total Tax Receipts in Millions of $",
       x = "Month",
       title= "Mixed Beverage Total Tax Receipts in Houston",
       subtitle = "Zip codes 77002 and 77010") +
  scale_x_yearmonth(date_breaks = "1 year")

## To adjust for Inflation
# Load the FRED CPI dataset
CPI_F_B_2007_092023 <- 
  read_excel("~/Desktop/R_Project/Data Analysis Projects/CPI F&B 2007 - 092023.xls")

#change it to a tsibble object

CPI <- CPI_F_B_2007_092023 %>%
  mutate(Month = yearmonth(Date)) %>%
  select(- Date) %>%
  as_tsibble(index = Month)

# To merge the 2 datasets

new <- merge(CPI, MB1_ts, by='Month', all=TRUE)

# to plot only the inflation adjusted numbers 

new %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month) %>%
  mutate(Inf_adjusted = (Total/CPI)/1000000) %>%
  autoplot(Inf_adjusted) +
  labs(title = "Inflation Adjusted Mixed Bevarage Receipts in Houston",
       subtitle = "Zip 77002 & 77010",
       y = "Inflation Adjusted Monthly Aggregate Tax Receipts") +
       scale_x_yearmonth(date_breaks = "1 year")
       
## to plot inflation adjusted and non-adjusted "SALES" data on a single plot; from the tax receipts 
# using 8.25% as base sales tax.

new <- new %>%
  mutate(Sales = (((Total*100)/8.25))/100000) %>%
  mutate(Inf_sales = ((Sales/CPI)*100))

new <- new %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

#install GGplot

install.packages("tidyverse")
library("tidyverse")

ggplot(new, aes(Month)) +
  geom_line(aes(y=Sales), color = "orange") +
  geom_line(aes(y=Inf_sales), color = "blue") +
  scale_x_yearmonth(date_breaks = "1 year") +
  labs(title = "Mixed Beverage Sales in Houston",
       subtitle = "Zip Code 77002 & 77010",
       y = "Sales in $10000",
       x = "Months")







