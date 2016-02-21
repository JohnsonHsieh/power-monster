source("[EDA]用電戶數vs總用電量.R")

#' 讀取 新北市104Q3戶數依老年人口數區分
filepath <- paste0("data/新北市104Q3戶數依老年人口數區分/", dir("data/新北市104Q3戶數依老年人口數區分/"))

tmp <- lapply(filepath, function(x){
  tmp <- read_excel(x)[1,1]
  df <- read_excel(x, skip = 3)
  df <- data.frame(C_Name="新北市", T_Name=substr(tmp, 11,13), df)
  df
})

#'老人指數 資料
df.oldman <- do.call(rbind, tmp) %>% 
  mutate(無老年人口戶數比例=as.numeric(gsub("%", "", 無老年人口戶數比例...))) %>%
  mutate(C_Name=as.character(C_Name), 
         T_Name=as.character(T_Name),
         Substitu=as.character(地區)) %>%
  select(C_Name, T_Name, Substitu, 無老年人口戶數比例)

#'用電資料
dat0 <- mutate(dat, C_Name=as.character(C_Name_1), 
               T_Name=as.character(T_Name_1),
               Substitu=as.character(Substitu_1)) %>%
  select(C_Name, T_Name, Substitu, User_, PowerSum_)

#' 處理兩邊資料 里 合併不齊的問題

# dat1 <- full_join(dat0, df.oldman)
# dat1[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]

dat1 <- right_join(dat0, df.oldman)
# dat1[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]

cID <- which(is.na(dat1$User_) )
df.oldman[cID,3] <- c("公[館]里","爪[峰]里","濂洞里","濂新里",
                      "[峰]廷里","石[曹]里", "崁[腳]里", "瓦[瑤]里","灰[瑤]里", 
                      "新生里", "新[部]里","五[峰]里", "[槍]寮里","永[館]里")

#' 合併 電力資料(dat0) 與 老人指數(df.oldman)
dat1 <- full_join(dat0, df.oldman)


#' 讀取 新北市104Q3戶數依戶長年齡區分
read_excel("data/新北市104Q3戶數依戶長年齡區分/不動產資訊平台_統計資訊查詢 (1).xls") -> a

filepath <- paste0("data/新北市104Q3戶數依戶長年齡區分/", dir("data/新北市104Q3戶數依老年人口數區分/"))
tmp2 <- lapply(filepath, function(x){
  tmp <- read_excel(x)[1,1]
  df <- read_excel(x, skip = 3)
  df <- data.frame(C_Name="新北市", T_Name=substr(tmp, 11,13), df)
  df
})

df.head <- do.call(rbind, tmp2) %>% 
  mutate(戶長平均年齡=as.numeric(戶長平均年齡.歲.)) %>%
  mutate(C_Name=as.character(C_Name), 
         T_Name=as.character(T_Name),
         Substitu=as.character(地區)) %>%
  select(C_Name, T_Name, Substitu, 戶長平均年齡)

df.head[cID,3] <- c("公[館]里","爪[峰]里","濂洞里","濂新里",
                    "[峰]廷里","石[曹]里", "崁[腳]里", "瓦[瑤]里","灰[瑤]里", 
                    "新生里", "新[部]里","五[峰]里", "[槍]寮里","永[館]里")

#' 合併 電力, 老人, 戶長 資料
dat2 <- full_join(dat1, df.head)



