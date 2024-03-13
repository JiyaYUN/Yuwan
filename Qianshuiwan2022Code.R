rm(list = ls())

library(readxl)
library(readr)

setwd("C:/Users/nokia_u5v99od/Desktop/YuwanProject/Qianshuiwan")
QSW2022 <- read_excel("QianShuiWan-QianShanDian-2022.xlsx")
View(QSW2022)

options(max.print = 500)





# Data Checking: unique values of every column
findUniqueValuesByIndex <- function(dataset, columnIndex) {
    uniqueValuesList <- list()
    columnNames <- names(dataset)[columnIndex]
    for (i in seq_along(columnNames)) {
        columnName <- columnNames[i]
        uniqueValuesList[[columnName]] <- unique(dataset[[columnName]])
    }
    return(uniqueValuesList) 
}

columnIndex <- c(1:48)

uniqueValues <- findUniqueValuesByIndex(QSW2022, columnIndex)





# 先删去必定不要的列
SmallerDataset <- QSW2022[,c(2,3,4,5,6,8,9,12,13,16,18,19,20,22,23,24,32)]
# 删去不确定、待决定的三行数据——因为其实这些行都是有消费的，看看怎么处理吧
SmallerDataset <- SmallerDataset[!(SmallerDataset$订单状态=="消费成功/已清台\r\n-----\r\n撤单\r\n"),]
SmallerDataset <- SmallerDataset[!(SmallerDataset$订单状态=="消费成功/已清台\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n"),]






# 加一列“日期”在最后
SmallerDataset$time_posix = strptime(SmallerDataset$就餐时间, format = "%Y年%m月%d日 %H时%M分")
SmallerDataset$日期 = format(SmallerDataset$time_posix, "%Y/%m/%d")
SmallerDataset <- SmallerDataset[, -which(names(SmallerDataset) == "time_posix")]





# 去掉2022年的11、12月
SmallerDataset <- SmallerDataset[SmallerDataset$日期<="2022/10/31",]





# 午餐为0，晚餐为1
SmallerDataset$餐段 <- gsub("午餐", 0, SmallerDataset$餐段)
SmallerDataset$餐段 <- gsub("晚餐", 1, SmallerDataset$餐段)





# ！！！！！！分成三个数据列：Retrieve撤单、Member会员、Individual散客！！！！！！
Retrieve <- SmallerDataset[SmallerDataset$订单状态=="撤单\r\n"
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n"
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n" 
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n",]

SmallerDataset <- SmallerDataset[SmallerDataset$订单状态!="撤单\r\n"
                                 & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n"
                                 & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n" 
                                 & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n",]

Member<-SmallerDataset[SmallerDataset$客户姓名!="散客",]

Individual<-SmallerDataset[SmallerDataset$客户姓名=="散客",]





# 分别对数据框的每一行添加唯一ID

OnlyID <- function(dataset){
    dataset$唯一序列<-seq_len(nrow(dataset))
    return(dataset)
}

Member<-OnlyID(Member)
Individual<-OnlyID(Individual)
Retrieve<-OnlyID(Retrieve)





# 桌台区域不好搞，因为有的数据就是一条内包含多张桌子，很麻烦，再想想办法？
# > uniqueValues[["桌台区域"]]
# [1] "B区\r\n"                                                                                
# [2] "打包区\r\n"                                                                             
# [3] "A区\r\n"                                                                                
# [4] "C区\r\n"                                                                                
# [5] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n"
# [6] "A区\r\n-----\r\nA区\r\n"                                                                
# [7] "A区\r\n-----\r\nA区\r\n-----\r\nA区\r\n"                                                
# [8] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n"                                                
# [9] "B区\r\n-----\r\nB区\r\n"                                                                
# [10] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n"                                
# [11] "B区\r\n-----\r\nA区\r\n-----\r\nB区\r\n"                                                
# [12] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n" 

# 比如陈珊珊的数据
MsCoral <- SmallerDataset[SmallerDataset$客户姓名=="陈珊珊",]
View(MsCoral)





# 不知道把表单独输出成文件总是乱码，所以我这里就不搞了