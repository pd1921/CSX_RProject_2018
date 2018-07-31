
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

library(Hmisc)
full$Embarked <- factor(full$Embarked, 
                        levels = c('S',
                                   'C',
                                   'Q'
                        ))

tapply(full$Fare, full$Embarked, mean)

full <- na.omit(full)
ggplot(data = full, 
       aes(x = Embarked, y = Fare)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(full$Fare) , 
             linetype = 'dotted') +
  labs(x = '登入港口', y = '票價') +
  coord_flip()

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

full$Fsize <- full$SibSp + full$Parch
anova(m1 <- lm(Fare ~ Fsize, data = full))

#full$Survived <- as.numeric(full$Survived)-1
ggplot(data = full, 
       aes(group = Embarked, 
           y = Fare, x = Pclass)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = Embarked, 
                  y = Fare, x = Pclass), 
              method = 'lm', se = F) + 
  facet_grid( . ~  Embarked) +
  labs(x = '港口', y = '票價')

