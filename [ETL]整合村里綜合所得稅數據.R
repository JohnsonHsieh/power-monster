source("[EDA]用電戶數vs總用電量.R")
source("[ETL]整合村里統計數據.R")

income <- read_excel("data/新北市101年_綜合所得稅所得總額各縣市鄉鎮村里統計分析表.xlsx", skip=1) %>% 
  filter(村里!="其　他", 村里!="合　計") %>%
  select(T_Name=鄉鎮市區, Substitu=村里, 所得中位數=中位數) %>%
  na.omit()

dat3 <- right_join(dat2, income)
# dat3[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]
cID <- which(is.na(dat3$User_) )
income[cID,2] <- c("五[峰]里", "碧山里","石[曹]里","公[館]里",
                   "永[館]里", "[槍]寮里", "西山里", "瓦[瑤]里",
                   "灰[瑤]里", "[峰]廷里", "爪[峰]里", "濂洞里",
                   "濂新里", "崁[腳]里", "新[部]里")
dat3 <- full_join(dat2, income)



# tmp <- read.csv("data/RealPriceGeocodes.csv", fileEncoding = "big5")
# install.packages("data.table")
# library(data.table)
# tmp <- fread("data/RealPriceGeocodes.csv")
# colnames(tmp) <- iconv(colnames(tmp), "big5")
# tmp <- apply(tmp, 2, function(x) iconv(x, "big5"))
# write.csv(tmp, "data/RealPriceGeocodes_utf8.csv", row.names = F)

tmp1 <- fread("data/RealPriceGeocodes_utf8.csv")
head(tmp1)



