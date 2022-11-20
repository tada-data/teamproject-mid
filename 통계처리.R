users <- read.csv("users.csv", header=T)
View(users)

users2 <- read.csv("users2.csv", header=T)
View(users2)

wilcox.test(value~year, data=users2)
#moonBook::acs에서 성별에 따라 나이를 확인하겠다와 같은 느낌
#이산형 데이터라고 생각
#http://www.incodom.kr/R%ED%99%9C%EC%9A%A9/Wilcoxon_Signed-Rank_Test

users1 <- read.csv("users1.csv", header=T)
View(users1)



fit1 <- glm(year~value, data=users2, family=binomial)
summary(fit1)

fit2 <- glm(year~value, data=users2, family=quasibinomial)
summary(fit2)
pchisq(summary(fit2)$dispersion*fit1$df.residual, fit1$df.residual, lower=F)

fit3 <- lm(year~value, data=users2)
summary(fit3)


result <- glm(년도 ~ ., family = binomial, data=users)
summary(result)

reduce.model <- step(result)
summary(reduce.model)

library(moonBook)
View(acs)

chisq.test(users$소계)
chisq.test(users$만60.69세)


moviechange <- read.csv("moviechange.csv", header=T)
View(moviechange)

chisq.test(moviechange)

fisher.test(moviechange)

# 국어성적과 영어성적사이에 관계가 있는지와 같은 느낌
# 이산형 데이터라고 생각
cor(moviechange$OTT영화시청증감률, moviechange$여가영화관증감률, method="pearson")

cor.test(moviechange$OTT영화시청증감률, moviechange$여가영화관증감률, method="pearson")

#https://butter-shower.tistory.com/231


library(dplyr)

moviechange1 <- moviechange %>% filter(!moviechange$index=="70세이상")

plot(moviechange1$OTT영화시청증감률, moviechange1$여가영화관증감률)

cor.test(moviechange1$OTT영화시청증감률, moviechange1$여가영화관증감률, method="spearman")


fit <- lm(OTT영화시청증감률~여가영화관증감률, data=moviechange)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
