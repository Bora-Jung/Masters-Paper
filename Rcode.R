##=========== 1. Data Input
data <- read.csv("D:/00 졸업논문/졸업논문정리/data.csv", header=T)
str(data)
data$CARD_Store_RegisterNumber <- as.factor(data$CARD_Store_RegisterNumber)


##===================== 2. Data View
library(plyr)

# 사용자별 거래빈도수
byUser <- ddply(data, .(CARD_No), summarise, sum=sum(co))
byUser <- arrange(byUser, desc(sum), CARD_No)
summary(byUser)
fivenum(byUser$sum)
boxplot(byUser$sum)
hist(byUser$sum, col="white", xlab="User Sum", ylab="Frequency")
plot(byUser$sum, xlab='User', ylab='User Sum')

# 음식점별 거래빈도수
byItem <- ddply(data, .(CARD_Store_RegisterNumber), summarise, sum=sum(co))
byItem <- arrange(byItem, desc(sum), CARD_Store_RegisterNumber)
summary(byItem)
plot(byItem$sum, xlab='Store', ylab='Store Sum')
boxplot(byItem$sum)
hist(byItem$sum)

# 평균(32)이상 거래된 가게만 추출
byItem_meanup<- subset(byItem, sum>32)
library(sqldf)
data <- sqldf("select b.CARD_No, a.CARD_Store_RegisterNumber, b.co
               from byItem_meanup a inner join data b
               on a.CARD_Store_RegisterNumber=b.CARD_Store_RegisterNumber")
data$CARD_Store_RegisterNumber <- as.character(data$CARD_Store_RegisterNumber)
data$CARD_Store_RegisterNumber <- as.factor(data$CARD_Store_RegisterNumber)
summary(data)


##------------3. Collaboration Filtering
library(recommenderlab)

data.matrix <- as(data, "realRatingMatrix")
data.matrix <- data.matrix[rowCounts(data.matrix)>3] #방문한 가게가 3이상
str(data.matrix)

trainingData <- sample(3337,2336) #3337 중 2336명 랜덤샘플링
trainingSet <- data.matrix[trainingData]
head(as(trainingSet, "matrix"))  #row:user, col:item

# User-based CF - Cosine, Pearson
scheme <- evaluationScheme(data.matrix, method="split", 
                           train=.7, given=3, goodRating=3, k=1)

algorithms_UBCF1 <- list(
  "user-based CF_Cosine_c2" = list(name="UBCF", parameter=list(method="Cosine", nn=2)),
  "user-based CF_Cosine_c4" = list(name="UBCF", parameter=list(method="Cosine", nn=4)),
  "user-based CF_Cosine_c6" = list(name="UBCF", parameter=list(method="Cosine", nn=6)),
  "user-based CF_Cosine_c8" = list(name="UBCF", parameter=list(method="Cosine", nn=8)),
  "user-based CF_Cosine_c10" = list(name="UBCF", parameter=list(method="Cosine", nn=10)),
  "user-based CF_Pearson_c2" = list(name="UBCF", parameter=list(method="Pearson", nn=2)),
  "user-based CF_Pearson_c4" = list(name="UBCF", parameter=list(method="Pearson", nn=4)),
  "user-based CF_Pearson_c6" = list(name="UBCF", parameter=list(method="Pearson", nn=6)),
  "user-based CF_Pearson_c8" = list(name="UBCF", parameter=list(method="Pearson", nn=8)),
  "user-based CF_Pearson_c10" = list(name="UBCF", parameter=list(method="Pearson", nn=10))
)
algorithms_UBCF1

results_UBCF1 <- evaluate(scheme, algorithms_UBCF1, n=c(1, 3, 5, 7, 10)) #n:추천개수
avg(results_UBCF1)
plot(results_UBCF1, annotate=TRUE, legend="topleft")
plot(results_UBCF1, "prec/rec", annotate=TRUE, legend="bottomright")

# User-based CF - Jaccard
data.matrix.bin <- binarize(data.matrix, minRating=1)
trainingSet <- data.matrix.bin[trainingData]

algorithms_UBCF2 <- list(
  "user-based CF_Jaccard_c2" = list(name="UBCF", param=list(nn=2)),
  "user-based CF_Jaccard_c4" = list(name="UBCF", param=list(nn=4)),
  "user-based CF_Jaccard_c6" = list(name="UBCF", param=list(nn=6)),
  "user-based CF_Jaccard_c8" = list(name="UBCF", param=list(nn=8)),
  "user-based CF_Jaccard_c10" = list(name="UBCF", param=list(nn=10))
)

results_UBCF2 <- evaluate(scheme, algorithms_UBCF2, n=c(1, 3, 5, 7, 10)) #n:추천개수
avg(results_UBCF2)
plot(results_UBCF2, annotate=TRUE, legend="topleft")
plot(results_UBCF2, "prec/rec", annotate=TRUE, legend="bottomright")


# Item-based CF
algorithms_IBCF <- list(
  "item-based CF" = list(name="IBCF", param=list(method="Jaccard"))
)

results_IBCF <- evaluate(scheme, algorithms_IBCF, n=c(1, 3, 5, 7, 10))
avg(results_IBCF)
plot(results_IBCF, annotate=TRUE, legend="topright")


##------------4. Association Rules
# data input
dataset <- read.csv("D:/00 졸업논문/졸업논문정리/data_association.csv", header=T)
str(dataset)

# 평균 이상 방문되어진 음식점만 추출
library(sqldf)
dataset <- sqldf("select * from dataset
                  where CARD_Store_RegisterNumber in(
                    select CARD_Store_RegisterNumber from dataset
                    group by CARD_Store_RegisterNumber
                    having count(CARD_Store_RegisterNumber)>32)")
dataset$CARD_Store_RegisterNumber <- as.character(dataset$CARD_Store_RegisterNumber)
dataset$CARD_Store_RegisterNumber <- as.factor(dataset$CARD_Store_RegisterNumber)
str(dataset)

# list 변환
data.list <- split(dataset$CARD_Store_RegisterNumber, dataset$CARD_No)
head(data.list)

listData <- list()
for (i in 1:length(data.list)) {
  listData[[i]] <- as.character(data.list[[i]][!duplicated(data.list[[i]])])
}

# transaction으로 변환
library(arules)
library(recommenderlab)
data.tr <- as(listData, "transactions")
summary(data.tr)
data.tr <- as(data.tr, "matrix")

# train, test 분할
n=nrow(data.tr)
tr = sample(1:n,size=floor(7*n/10),replace=F)
data.train=data.tr[tr,]
data.test=data.tr[-tr,]

# function
scorevec <- function(indi,temp.ts){
  score <- rep(0,ncol(temp.ts))
  rule.ext <- names(indi)[indi!=0]
  nrule<-length(rule.ext)
  qq <- comb(rule.ext)
  for(i in 1:ncol(temp.ts))
  {
    if(sum(rslt[,2] %in% colnames(temp.ts)[i]) > 0)
    {
      rslt.ex <- rslt[rslt[,2] %in% colnames(temp.ts)[i],]
      q<-vector()
      for(j in 1:length(qq))
      {
        w <- which(match(as.cha(rslt.ex[1]), qq[j]) != "NA")
        q <- c(q,w)
      }
      rslt.ex1 <- rslt.ex[q,]
      if(length(q) > 0) score[i] <- max(rslt.ex1[,4])
    }
  }
  score
}

comb <- function(rule.ex){
  ky <- rule.ex
  for(i in 1:(length(rule.ex)-1))
    for(j in (i+1):length(rule.ex))
      ky <- c(ky, paste(rule.ex[i], rule.ex[j], sep=","))
    ky
}

as.cha <- function(cha)
{
  cha.re <- character(length=nrow(cha))
  for(i in 1:nrow(cha))
  {
    cha.re[i] <- as.character(cha[i,1])
  }
  cha.re
}


# Apriori Algorithm
library(stringr)
library(arules)
rules=apriori(data.train, parameter=list(supp=0.001, conf=0.01, target="rules", maxlen=2))
rules=as(rules,"data.frame")

n=nrow(rules)
p=NULL;
for(i in 1:n){
  pp = gsub("\\}|\\{","",str_split(rules[i,]$rules," => ")[[1]])
  p = c(p,pp)
}
tmp <- t(matrix(p,nrow=2))
rslt <- cbind(tmp,rules[,c(2,3)])
colnames(rslt)[c(1,2)] <- c("from","to")
for(i in 1:nrow(data.train))
{
  if(i == 1) result <- t(data.frame(scorevec(data.train[i,],data.train)))
  if(i > 1) result <- rbind(result, scorevec(data.train[i,],data.train))
}
rownames(result)[1] <- ""

# hit ratio
hr <- vector()
for(i in 1:nrow(result))
{
  z <- colnames(data.train)[data.train[i,] == 1]
  x <- colnames(data.train)[sort(result[i,], decreasing=T,index.return=T)[[2]]][1:10]
  hr[i] <- sum(x %in% z)
}
mean(hr)

