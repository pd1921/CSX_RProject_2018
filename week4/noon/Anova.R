
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm


# 匯出資料，並合併三個檔案

train <- read.csv('./Data/train.csv', stringsAsFactors = F)
test  <- read.csv('./Data/test.csv', stringsAsFactors = F)
gender <- read.csv('./Data/gender_submission.csv', stringsAsFactors = F)

k1 <- merge(test, gender, by="PassengerId", all = T)
full <- bind_rows(train, k1)

str(full)

full$Survived <- as.factor(full$Survived)
str(full)
head(full)
summary(full)
tail(full)

# 載入ggplot，準備畫圖
library(ggplot2)
#圖設定為黑白配色（theme_bw）
old <- theme_set(theme_bw())

ggplot(data = full, aes(x = Survived, y = Fare)) +
  geom_boxplot() + coord_flip() +
  labs( y = 'Fare', x = 'Survived', 
        title = 'Fare Box')

#以下函式計算95%信賴區間
full <- na.omit(full)
with(full, 
     tapply(Fare, Survived,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))

#此函數會預設進行 Welch 校正，以處理兩樣本變異數不相同的問題
t.test(Fare ~ Survived, data = full)

#可加上參數 var.equal=TRUE 來假設變異數同值(不做Welch校正)
t.test(Fare ~ Survived, data = full, var.equal = TRUE)

