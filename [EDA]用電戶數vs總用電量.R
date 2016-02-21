library(ggplot2)
library(ggmap)
library(dplyr)
library(magrittr)
library(maptools)
library(reshape2)
library(plotly)
library(ellipse)
library(readxl)

# 讀取用電戶、用電量數據
dat <- read.csv("data/用電戶數用電量-整理.csv", row.names = 1)
dat <- mutate(dat, class=paste(T_Name_1, Substitu_1, sep="_"))

# 肉眼觀察法取出離群值 (class = [A, B, C] 其中 [B, C] 為離群值)
dat$group <- ifelse(dat$class %in% c("中和區_中原里","中和區_平河里","新店區_復興里","新店區_寶福里","中和區_碧河里","三重區_中興里"), "B",
                    ifelse(dat$class %in% c("中和區_秀景里","樹林區_南園里","林口區_湖南里","林口區_南勢里","淡水區_竿蓁里"),"C","A"))

# plotly 輸出互動式視覺化 (用電量 ~ 用電戶數 byeach 里)
p1 <- ggplot(dat, aes(x=User_, y=PowerSum_, colour=group)) + 
  geom_point(aes(text=class)) +
  geom_text(data=filter(dat, group%in%c("B","C")), family="STHeiti", 
            aes(label=class)) +
  labs(x="用電戶數", y="總用電量", title="新北市住宅用電數據(102年8月)") +
  theme_gray(base_family = "STHeiti", base_size = 14)

# ggplotly(p1)


# 針對 class A 做橢圓分群 (io = [inner, outter])
dat1 <- filter(dat, group=="A")

df_ell <- summarise(dat1, x=cor(User_, PowerSum_), 
                    sd.x=sd(User_), sd.y=sd(PowerSum_),
                    ce.x=mean(User_), ce.y=mean(PowerSum_)) %$%
  ellipse(x=x, scale=c(sd.x, sd.y), centre=c(ce.x, ce.y), level=0.7) %>%
  data.frame %>% setNames(c("User_", "PowerSum_"))

dat1$io <- "inner"
low <- 46:96 # 橢圓下方的位置

sub_df_ell_low <- df_ell[low,]
for(i in 1:(nrow(sub_df_ell_low)-1)){
  index <- which(dat1$User_ > sub_df_ell_low[i,1], dat1$User_ < sub_df_ell_low[i+1,1])
  index2 <- which(dat1$PowerSum_[index] < sub_df_ell_low[i,2])
  dat1$io[index[index2]] <- "outter"
}

sub_df_ell_up <- df_ell[-low,]
for(i in 1:(nrow(sub_df_ell_up)-1)){
  index <- which(dat1$User_ < sub_df_ell_up[i,1], dat1$User_ > sub_df_ell_up[i+1,1])
  index2 <- which(dat1$PowerSum_[index]  > sub_df_ell_up[i,2])
  dat1$io[index[index2]] <- "outter"
}

dat1$io[which(dat1$User_ > max(df_ell[,1]))] <- "outter"

# plotly 輸出互動式視覺化 (aes(x=用電量, y=用電戶數, colour=[inner, outter]), byeach 里)
mytext <- data.frame(User_=c(1000,1000,4000,4000), PowerSum_=c(5*10^5, 4*10^6, 4*10^6, 5*10^5), lab=c("III", "II", "I", "IV"))
center <- colMeans(dat1[,4:5])

p2 <- ggplot(dat1, aes(x=User_, y=PowerSum_)) + 
  geom_point(aes(colour=io, text=class)) +
  geom_path(data=df_ell, aes(x=User_, y=PowerSum_)) + 
  geom_vline(xintercept=center[1], lty=2) + 
  geom_hline(yintercept=center[2], lty=2) +
  labs(x="用電戶數", y="總用電量", title="新北市住宅用電數據(102年8月)") +
  theme_gray(base_family = "STHeiti", base_size = 14) +
  geom_text(data=mytext, aes(x=User_, y=PowerSum_, label=lab), size=16)

# ggplotly(p2)

# 加入四個象限的分群
dat1$subgp <- ifelse(dat1$User_ > center[1] & dat1$PowerSum_ > center[2], "I",
                     ifelse(dat1$User_ < center[1] & dat1$PowerSum_ > center[2], "II",
                            ifelse(dat1$User_ < center[1] & dat1$PowerSum_ < center[2], "III","IV"
                            )))

head(dat1)
write.csv(dat1, "data/dat1.csv", row.names = F)
