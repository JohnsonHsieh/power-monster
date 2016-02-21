# open the shapefile
source("https://raw.githubusercontent.com/snexuz/TWD97TM2toWGS84/master/TWD97TM2toWGS84.R")
# install.packages("maptools")
# install.packages("rgeos")
# devtools::install_github("karthik/wesanderson")

library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(dplyr)
library(maptools)
library(reshape2)

dat <- read.csv("data/用電戶數用電量-整理.csv", row.names = 1)
dat <- mutate(dat, class=paste(T_Name_1, Substitu_1, sep="_"))


# Find outlier
dat$group <- ifelse(dat$class %in% c("中和區_中原里","中和區_平河里","新店區_復興里","新店區_寶福里","中和區_碧河里","三重區_中興里"), "B",
                    ifelse(dat$class %in% c("中和區_秀景里","樹林區_南園里","林口區_湖南里","林口區_南勢里","淡水區_竿蓁里"),"C","A"))


library(plotly)

# d <- diamonds[sample(nrow(diamonds), 1000), ]
# plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
#         mode = "markers", color = carat, size = carat)

# plot_ly(dat, x=User_, y=PowerSum_, text=class, mode="markers", color=group) 

p0 <- ggplot(dat, aes(x=User_, y=PowerSum_, colour=group)) + 
  geom_point(aes(text=class)) +
  geom_text(data=filter(dat, group%in%c("B","C")), family="STHeiti", 
            aes(label=class)) +
  labs(x="用電戶數", y="總用電量", title="新北市住宅用電數據(102年8月)") +
  theme_gray(base_family = "STHeiti", base_size = 14)

ggplotly(p0)



# 區分四個象限
dat1 <- filter(dat, group=="A")
library(ellipse)
n <- 100
x <- dat1$User_
y <- dat1$PowerSum_

df_ell <- ellipse(cor(x, y), 
        scale=c(sd(x),sd(y)), 
        centre=c(mean(x),mean(y)), level = 0.7, npoints = n) %>% data.frame()

names(df_ell) <- names(center)

dat1$io <- "inner"
low <- 46:96
sub_df_ell <- df_ell[low,]

for(i in 1:(nrow(sub_df_ell)-1)){
  index <- which(dat1$User_ > sub_df_ell[i,1], dat1$User_ < sub_df_ell[i+1,1])
  index2 <- which(dat1$PowerSum_[index] < sub_df_ell[i,2])
  dat1$io[index[index2]] <- "outter"
}

sub_df_ell <- df_ell[-low,]

for(i in 1:(nrow(sub_df_ell)-1)){
  index <- which(dat1$User_ < sub_df_ell[i,1], dat1$User_ > sub_df_ell[i+1,1])
  index2 <- which(dat1$PowerSum_[index]  > sub_df_ell[i,2])
  dat1$io[index[index2]] <- "outter"
}


dat1$io[which(dat1$User_ > max(df_ell[,1]))] <- "outter"


mytext <- data.frame(User_=c(1000,1000,4000,4000), PowerSum_=c(5*10^5, 4*10^6, 4*10^6, 5*10^5), lab=c("III", "II", "I", "IV"))
center <- colMeans(dat1[,4:5])

p <- ggplot(dat1, aes(x=User_, y=PowerSum_)) + 
  geom_point(aes(colour=io, text=class)) +
  geom_path(data=df_ell, aes(x=User_, y=PowerSum_)) + 
  geom_vline(xintercept=center[1], lty=2) + 
  geom_hline(yintercept=center[2], lty=2) +
  labs(x="用電戶數", y="總用電量", title="新北市住宅用電數據(102年8月)") +
  theme_gray(base_family = "STHeiti", base_size = 14) +
  geom_text(data=mytext, aes(x=User_, y=PowerSum_, label=lab), size=16)


ggplotly(p)

dat1$subgp <- ifelse(dat1$User_ > center[1] & dat1$PowerSum_ > center[2], "I",
                     ifelse(dat1$User_ < center[1] & dat1$PowerSum_ > center[2], "II",
                            ifelse(dat1$User_ < center[1] & dat1$PowerSum_ < center[2], "III","IV"
                                   )))

df <- group_by(dat1, T_Name_1, subgp) %>%
  filter(io=="outter") %>%
  summarise(count=n()) %>% 
  dcast(T_Name_1~subgp, fill = 0) %>% 
  arrange(-II/(I+II+III+IV))


#----------------------
# 合併火災數據
#----------------------
df.fire <- read.csv("data/火災資料_整理.csv")

df <- group_by(dat1, T_Name_1, subgp, io) %>%
  summarise(count=n()) %>% 
  dcast(T_Name_1+io~subgp, fill = 0)

df1 <- merge(df, df.fire, by.x="T_Name_1", by.y="鄉.鎮.市.區")

# install.packages("GGally")
library(GGally)
ggpairs(df.all)
head(df.all)

ggplot(df1, aes(x=I, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限I")
  
ggplot(df1, aes(x=II, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限II")

ggplot(df1, aes(x=III, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限III")


ggplot(df1, aes(x=IV, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限IV")


# 第I, II象限的結果顯示，outter的火災案件數 高於 inner的火災案件數
# 合併I, II象限 (取 用電量高於平均的數據再做比較)

df2 <- data.frame(T_Name_1=df1$T_Name_1, io=df1$io, 
           OverPowerSum=(df1$I + df1$II), 火災案件數量=df1$火災案件數量)

ggplot(df2, aes(x=OverPowerSum, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限I + II")


df3 <- data.frame(T_Name_1=df1$T_Name_1, io=df1$io, 
                  OverPowerSum=(df1$I + df1$IV), 火災案件數量=df1$火災案件數量)

ggplot(df3, aes(x=OverPowerSum, y=火災案件數量, colour=io)) + geom_point() +
  stat_smooth(method = "lm") + 
  theme_gray(base_family = "STHeiti", base_size = 14) +
  labs(x="xx里 in yy區", title="象限I + IV")


#------------------------
#
#------------------------
library(readxl)
filepath <- paste0("data/新北市104Q3戶數依老年人口數區分/", dir("data/新北市104Q3戶數依老年人口數區分/"))
tmp <- lapply(filepath, function(x){
  tmp <- read_excel(x)[1,1]
  df <- read_excel(x, skip = 3)
  df <- data.frame(C_Name="新北市", T_Name=substr(tmp, 11,13), df)
  df
})


df.oldman <- do.call(rbind, tmp) %>% 
  mutate(無老年人口戶數比例=as.numeric(gsub("%", "", 無老年人口戶數比例...))) %>%
  mutate(C_Name=as.character(C_Name), 
         T_Name=as.character(T_Name),
         Substitu=as.character(地區)) %>%
  select(C_Name, T_Name, Substitu, 無老年人口戶數比例)

dat0 <- mutate(dat, C_Name=as.character(C_Name_1), 
               T_Name=as.character(T_Name_1),
               Substitu=as.character(Substitu_1)) %>%
  select(C_Name, T_Name, Substitu, User_, PowerSum_)

# dat1 <- full_join(dat0, df.oldman)
# dat1[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]

dat1 <- right_join(dat0, df.oldman)
dat1[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]

cID <- which(is.na(dat1$User_) )
df.oldman[cID,3] <- c("公[館]里","爪[峰]里","濂洞里","濂新里",
                                      "[峰]廷里","石[曹]里", "崁[腳]里", "瓦[瑤]里","灰[瑤]里", 
                                      "新生里", "新[部]里","五[峰]里", "[槍]寮里","永[館]里")

dat1 <- full_join(dat0, df.oldman)
# dat1[which(is.na(dat1$無老年人口戶數比例) | is.na(dat1$User_) ),]

# df.oldman$Substitu <- gsub("公舘里", "公[館]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("爪峯里", "爪[峰]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("濓洞里", "濂洞里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("濓新里", "濂新里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("峯廷里", "[峰]廷里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("石𥕢里", "石[曹]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("瓦󿾨里", "瓦[瑤]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("灰󿾨里", "灰[瑤]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("新󿾴���里", "新生里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("新廍里", "新[部]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("五峯里", "五[峰]里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("寮里", "[槍]寮里", df.oldman$Substitu)
# df.oldman$Substitu <- gsub("永舘里", "永[館]里", df.oldman$Substitu)


head(dat1)
head(df.oldman)
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

dat2 <- full_join(dat1, df.head)

head(dat2)

plot(dat2$User_, dat2$無老年人口戶數比例)
plot(dat2$User_, dat2$戶長平均年齡)

plot(dat2$PowerSum_, dat2$無老年人口戶數比例)
plot(dat2$PowerSum_, dat2$戶長平均年齡)


write.csv(dat2, "dat2.csv", row.names = F)
