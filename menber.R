rm(list = ls())
library(readxl)
library(dplyr)
library(lubridate)
member2023 <- read_excel("Member2023new.xlsx")
festival <- read_excel("浅水湾营销活动时间表.xlsx")



# Attempt to convert the date columns to Date objects using multiple formats
member2023$日期 <- parse_date_time(member2023$日期, orders = c("ymd", "mdy", "dmy"))
festival$开始日期 <- parse_date_time(festival$开始日期, orders = c("ymd", "mdy", "dmy"))
festival$结束日期 <- parse_date_time(festival$结束日期, orders = c("ymd", "mdy", "dmy"))
# Create a function to find the festival name for a given sales date
find_festival <- function(sales_date, festivals) {
    for(i in 1:nrow(festivals)) {
        if(sales_date >= festivals$开始日期[i] & sales_date <= festivals$结束日期[i]) {
            return(festivals$营销活动[i])
        }
    }
    return('日常') # Return '日常' if no festival is found for the sales date
}
# Apply the function to each sales date
member2023$营销活动 <- sapply(member2023$日期, find_festival, festivals = festival)
# Create the `isFestival` column based on the `营销活动` column
member2023$isFestival <- ifelse(member2023$营销活动 == '日常', 0, 1)
# aggregate data 
member2023$性别 <- ifelse(member2023$性别 == "男",1,0)
member2023$每单人均消费 = member2023$消费金额/member2023$实际消费人数
member2023$接单 = ifelse(member2023$接单渠道 == "预定台",0,1) 
library(writexl)

# Assuming your data frame is named SmallerDataset
write_xlsx(member2023, path = "/Users/ruofeiwu/Desktop/御湾project/Member2023new.xlsx")