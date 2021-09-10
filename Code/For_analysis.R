library(dplyr)
library(BBmisc)
#install.packages('rmarkdown'); install.packages('knitr')
library(rmarkdown); library(knitr)

data_DY <- read.csv("output.csv", header=T)

# 가설1 테스트
data_DY_average_message_length <- data_DY[, c('average_message_length')]
# average_message_length에 대해 스케일링(min-max 정규화)
data_DY_average_message_length <- normalize(data_DY_average_message_length, method='range',range=c(0,1))
data_DY_chat_type2 <- data_DY[, c('chat_type2')]

data_DY_1 <- data.frame(data_DY_average_message_length,data_DY_chat_type2)
colnames(data_DY_1) = c('average_message_length', 'chat_type2')
head(data_DY_1)

data_shin <- read.csv("output_shin.csv", header=T)

data_shin_average_message_length <- data_shin[, c('average_message_length')]
# average_message_length에 대해 스케일링(min-max 정규화)
data_shin_average_message_length <- normalize(data_shin_average_message_length, method='range',range=c(0,1))
data_shin_chat_type2 <- data_shin[, c('chat_type2')]

data_shin_1 <- data.frame(data_shin_average_message_length,data_shin_chat_type2)
colnames(data_shin_1) = c('average_message_length', 'chat_type2')
head(data_shin_1)

data_HS <- read.csv("output_HS.csv", header=T)

data_HS_average_message_length <- data_HS[, c('average_message_length')]
# average_message_length에 대해 스케일링(min-max 정규화)
data_HS_average_message_length <- normalize(data_HS_average_message_length, method='range',range=c(0,1))
data_HS_chat_type2 <- data_HS[, c('chat_type2')]

data_HS_1 <- data.frame(data_HS_average_message_length,data_HS_chat_type2)
colnames(data_HS_1) = c('average_message_length', 'chat_type2')
head(data_HS_1)

data_JY <- read.csv("output_JY.csv", header=T)

data_JY_average_message_length <- data_JY[, c('average_message_length')]
# average_message_length에 대해 스케일링(min-max 정규화)
data_JY_average_message_length <- normalize(data_JY_average_message_length, method='range',range=c(0,1))
data_JY_chat_type2 <- data_JY[, c('chat_type2')]

data_JY_1 <- data.frame(data_JY_average_message_length,data_JY_chat_type2)
colnames(data_JY_1) = c('average_message_length', 'chat_type2')
head(data_JY_1)

data_sjy <- read.csv("output_sjy.csv", header=T)

data_sjy_average_message_length <- data_sjy[, c('average_message_length')]
# average_message_length에 대해 스케일링(min-max 정규화)
data_sjy_average_message_length <- normalize(data_sjy_average_message_length, method='range',range=c(0,1))
data_sjy_chat_type2 <- data_sjy[, c('chat_type2')]

data_sjy_1 <- data.frame(data_sjy_average_message_length,data_sjy_chat_type2)
colnames(data_sjy_1) = c('average_message_length', 'chat_type2')
head(data_sjy_1)


# 각자 모은 feature 정보들 합치기
data_DY_1 <- bind_rows(data_DY_1, data_shin_1, data_HS_1, data_JY_1, data_sjy_1)
data_DY_1
head(data_DY_1)
#write.csv(data_DY_1,file="H1.csv")

# boxplots of different chat types
boxplot(average_message_length~chat_type2, data=data_DY_1,
        xlab='Chat types (Private/Public)',
        ylab='Average message length', col=2:3)
abline(h=mean(data_DY_1$average_message_length),col='gray')

#Conducting One-Way ANOVA 
aov.out <- aov(average_message_length~chat_type2,data=data_DY_1)
summary(aov.out)
anova(lm(average_message_length~chat_type2,data=data_DY_1))


# 가설 3,4,5 테스트
# data_DY <- 모든 테이블다 여기로 합치기
data_DY <- bind_rows(data_DY, data_shin, data_HS, data_JY, data_sjy)
head(data_DY)

#pairs.panels로 한꺼번에 보여주기
library(psych)
cordat <- data_DY[,c('emoji_ratio','slang_ratio','my_message_stake','intimacy_score')]
#write.csv(cordat,file="H345.csv")
pairs.panels(cordat,method="pearson")
#cor(cordat, method="pearson")
pairs.panels(cordat,method="spearman") # 둘중 하나만 사용해도 될 듯 -> pearson으로 하기

cor.test(cordat$emoji_ratio,cordat$intimacy_score) # H3 p-value = 0.0008415
cor.test(cordat$slang_ratio, cordat$intimacy_score) # H4 p-value = 0.2003
cor.test(cordat$my_message_stake,cordat$intimacy_score) # 5 p-value = 0.01551

# 시각화 부분
library(ggplot2)

#write.csv(data_DY,file="visualization.csv")
data_DY <- read.csv("visualization.csv", header=T)
ggplot(data_DY, aes(x=emoji_ratio,y=intimacy_score,colour=chat_type1, shape=chat_type2)) +
  geom_point(size=2) + xlab('emoji Ratio') + 
  ylab('Intimacy Score') +
  ggtitle('Scatter Plot of Emoji Ratio and Intimacy Based on Chat Types') +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw()

ggplot(data_DY, aes(x=slang_ratio,y=intimacy_score,colour=chat_type1, shape=chat_type2)) +
  geom_point(size=2) + xlab('Slang Ratio') + 
  ylab('Intimacy Score') +
  ggtitle('Scatter Plot of Slang Ratio and Intimacy Based on Chat Types') +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw()

summary(data_DY)
str(data_DY)

# 가설2 테스트

data_DY <- read.csv("output.csv", header=T)

data_DY_2 <- data_DY[, c('emoji_ratio', 'chat_type2')]
data_shin_2 <- data_shin[, c('emoji_ratio', 'chat_type2')]
data_HS_2 <- data_HS[, c('emoji_ratio', 'chat_type2')]
data_JY_2 <- data_JY[, c('emoji_ratio', 'chat_type2')]
data_sjy_2 <- data_sjy[, c('emoji_ratio', 'chat_type2')]

data_DY_2 <- bind_rows(data_DY_2, data_shin_2, data_HS_2, data_JY_2, data_sjy_2)
head(data_DY_2)
#write.csv(data_DY_2,file="H2.csv")

# boxplots of different chat types
boxplot(emoji_ratio~chat_type2, data=data_DY_2,
        xlab='Chat types (Private/Public)',
        ylab='Emoji Ratio', col=2:3)
abline(h=mean(data_DY_2$emoji_ratio),col='gray')

#Conducting One-Way ANOVA 
aov.out <- aov(emoji_ratio~chat_type2,data=data_DY_2)
summary(aov.out)
anova(lm(emoji_ratio~chat_type2,data=data_DY_2))

