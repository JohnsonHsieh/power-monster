source("[EDA]用電戶數vs總用電量.R")

# 加入火災數據 (by 行政區)
df.fire <- read.csv("data/火災資料_整理.csv")

df <- group_by(dat1, T_Name_1, subgp, io) %>%
  summarise(count=n()) %>% 
  dcast(T_Name_1+io~subgp, fill = 0)

df1 <- merge(df, df.fire, by.x="T_Name_1", by.y="鄉.鎮.市.區")

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

