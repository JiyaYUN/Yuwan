rm(list = ls())

library(readxl)
library(readr)

setwd("C:/Users/nokia_u5v99od/Desktop/YuwanProject/Qianshuiwan")
QSW2023 <- read_excel("QianShuiWan-QianShanDian-2023.xlsx")
View(QSW2023)

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

uniqueValues <- findUniqueValuesByIndex(QSW2023, columnIndex)





# 先删去必定不要的列
SmallerDataset <- QSW2023[,c(2,3,4,5,6,8,9,12,13,16,18,19,20,22,23,24,32)]
# 删去没用的一行数据
SmallerDataset <- SmallerDataset[!(SmallerDataset$订单状态=="消费成功/已清台\r\n-----\r\n撤单\r\n"),]
# 删去不确定、待决定的一行数据——因为其实这一行是有消费的（9个人，好几百块钱，看看怎么处理吧）
SmallerDataset <- SmallerDataset[!(SmallerDataset$订单状态=="消费成功/已清台\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n"),]
# 删去没用的五行数据
SmallerDataset<-SmallerDataset[!is.na(SmallerDataset$客户姓名),]





# 加一列“日期”在最后
SmallerDataset$time_posix = strptime(SmallerDataset$就餐时间, format = "%Y年%m月%d日 %H时%M分")
SmallerDataset$日期 = format(SmallerDataset$time_posix, "%Y/%m/%d")
SmallerDataset <- SmallerDataset[, -which(names(SmallerDataset) == "time_posix")]





# 午餐为0，晚餐为1
SmallerDataset$餐段 <- gsub("午餐", 0, SmallerDataset$餐段)
SmallerDataset$餐段 <- gsub("晚餐", 1, SmallerDataset$餐段)





# ！！！！！！分成三个数据列：Retrieve撤单、Member会员、Individual散客！！！！！！
Retrieve <- SmallerDataset[SmallerDataset$订单状态=="撤单\r\n"
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n"
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n" 
                           | SmallerDataset$订单状态=="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n",]

SmallerDataset <- SmallerDataset[SmallerDataset$订单状态!="撤单\r\n"
                           & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n"
                           & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n" 
                           & SmallerDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n",]

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
# [1] "打包区\r\n"                                                             
# [2] "B区\r\n"                                                                
# [3] "C区\r\n"                                                                
# [4] "A区\r\n"                                                                
# [5] "A区\r\n-----\r\nA区\r\n-----\r\nA区\r\n-----\r\nA区\r\n"                
# [6] "B区\r\n-----\r\nB区\r\n"                                                
# [7] "A区\r\n-----\r\nA区\r\n"                                                
# [8] "A区\r\n-----\r\nA区\r\n-----\r\nA区\r\n"                                
# [9] "A区\r\n-----\r\nB区\r\n"                                                
# [10] "B区\r\n-----\r\nA区\r\n"                                                
# [11] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n-----\r\nB区\r\n"
# [12] "B区\r\n-----\r\nB区\r\n-----\r\nB区\r\n"                                
# [13] "B区\r\n-----\r\nC区\r\n" 

# 比如熊女士的数据
MsBear <- SmallerDataset[SmallerDataset$客户姓名=="熊女士",]
View(MsBear)





# 不知道把表单独输出成文件总是乱码，所以我这里就不搞了





# 以下没啥用

if(FALSE) {
    
    #write.csv(Retrieve,"Retrieve.csv", row.names = FALSE, fileEncoding = "UTF-8")
    #write_csv(Retrieve, "Retrieve.csv")
    #write.csv(mydata, "output.csv", row.names = FALSE)
    
#SmallerDataset <- SmallerDataset[!(SmallerDataset$折前金额==0 & SmallerDataset$消费金额==0),]
    
#zeroo<-SmallerDataset[SmallerDataset$折前金额==0 & SmallerDataset$消费金额==0,]

#删掉所有撤单的订单（有200多笔），总共剩20603笔
TotalDataset1 <- TotalDataset[TotalDataset$订单状态!="撤单\r\n"
                              & TotalDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n"
                              & TotalDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n" 
                              & TotalDataset$订单状态!="撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n-----\r\n撤单\r\n"
                              & TotalDataset$订单状态!="消费成功/已清台\r\n-----\r\n撤单\r\n",]



# 消费金额为0的订单有624笔
ExpenditureZero<-TotalDataset1[TotalDataset1$消费金额==0,]
# 其中有279笔消费金额为0但折前金额不为0（看怎么处理）
print(nrow(ExpenditureZero[ExpenditureZero$折前金额!=0,]))



# 消费金额大于10的订单：19110笔
TotalDataset2 <- TotalDataset1[TotalDataset1$消费金额>=10,]



# 散客订单占比：88%
print(nrow(TotalDataset2[TotalDataset2$客户姓名=="散客",])/nrow(TotalDataset2))
# 会员大概2000笔



# 会员散客分开
Member<-TotalDataset2[TotalDataset2$客户姓名!="散客",]
Individual<-TotalDataset2[TotalDataset2$客户姓名=="散客",]

# 分别加唯一ID

# 
uniqueValuesInd <- findUniqueValuesByIndex(Individual, columnIndex)
uniqueValuesMem <- findUniqueValuesByIndex(Member, columnIndex)

tempppp<-Individual[Individual$桌台区域=="B区\r\n-----\r\nC区\r\n",]

}