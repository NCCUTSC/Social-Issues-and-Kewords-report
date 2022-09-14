library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)

setwd("C:/Users/JOEZ/OneDrive/桌面/R/國發會")
#標準化=========
pro.issue<-read.csv("./excel檔/施政計畫/issue.csv",header=TRUE,fileEncoding = "Big5")
row.names(pro.issue)<-pro.issue[,1]
pro.issue <- pro.issue[,2:ncol(pro.issue)]
colnames(pro.issue)<-c(104:110)

pub.issue<-read.csv("./excel檔/政府出版品/issue.csv",header=TRUE,fileEncoding = "Big5")
row.names(pub.issue)<-pub.issue[,1]
pub.issue <- pub.issue[,2:ncol(pub.issue)]
colnames(pub.issue)<-c(100:110)

jour.issue<-read.csv("./excel檔/學術期刊/issue.csv",header=TRUE,fileEncoding = "Big5")
row.names(jour.issue)<-jour.issue[,1]
jour.issue <- jour.issue[,2:ncol(jour.issue)]
colnames(jour.issue)<-c(99:110)

standardize<-function(matrix){
  m<-max(matrix)
  n<-min(matrix)
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      matrix[i,j] <- (matrix[i,j]-n)/(m-n)
    }
  }
  return(matrix)
}

pro.issue<-standardize(pro.issue)
pub.issue<-standardize(pub.issue)
jour.issue<-standardize(jour.issue)

pro.issue<-as.matrix(pro.issue)
pub.issue<-as.matrix(pub.issue)
jour.issue<-as.matrix(jour.issue)

sortfunction<-function(matrixA,matrixB,matrixC,n){
  matrixA<-matrixA[,which(colnames(matrixA)==n):ncol(matrixA)]
  matrixB<-matrixB[,which(colnames(matrixB)==n):ncol(matrixB)]
  matrixC<-matrixC[,which(colnames(matrixC)==n):ncol(matrixC)]
  a<-matrix(c(matrixA),ncol=1)
  b<-matrix(c(matrixB),ncol=1)
  c<-matrix(c(matrixC),ncol=1)
  result<-cbind(c(rep(104,28),rep(105,28),rep(106,28),rep(107,28),rep(108,28),rep(109,28),rep(110,28)),a,b,c)
  colnames(result)<-c("年度","施政計畫","政府出版品","學術期刊")
  return(result)
}


sort.issue<-sortfunction(pro.issue,pub.issue,jour.issue,104)
cor(sort.issue[,2:4])

write.csv(cor(sort.issue[,2:4]), "./excel檔/加權/sort.issue.correlation.csv", row.names = TRUE,fileEncoding = "Big5")


weight <- as.data.frame(prcomp(sort.issue[,2:4],scale = TRUE)$rotation)[,1]
issue.index <-sort.issue[,2]*weight[1]+sort.issue[,3]*weight[2]+sort.issue[,4]*weight[3]
sort.issue <- cbind(sort.issue,issue.index,議題=rep(c("高齡化社會","長期照護","少子化衝擊","晚婚(不婚)化",
                                                 "外籍移工","白領移民開放","中高齡就業","教育分配不均",
                                                 "青年高失業率","高房價","城鄉資源失衡","貧富差距擴大",
                                                 "薪資成長緩慢","高級人力流失","世代資源不均",
                                                 "數位轉型與自動化發展","電子廢棄物","資(通)訊安全",
                                                 "網路詐騙","假消息","零工經濟/非典型就業",
                                                 "淨零排放的產業轉型壓力","極端氣候對水資源","海平面上升"
                                                 ,"極端氣候對環境影響","環境污染","能源轉型","糧食危機"),7))

#write.csv(sort.issue, "./excel檔/加權/sort.issue.csv", row.names = FALSE,fileEncoding = "Big5")

sort.issue <- as.data.frame(sort.issue)
sort.issue$issue.index <- as.numeric(sort.issue$issue.index)
sort.issue$施政計畫 <- as.numeric(sort.issue$施政計畫)
sort.issue$政府出版品 <- as.numeric(sort.issue$政府出版品)
sort.issue$學術期刊 <- as.numeric(sort.issue$學術期刊)

issue.index.plot<-(ggplot(data=sort.issue,mapping=aes(x=as.numeric(年度),y=issue.index,group=議題))
+geom_line(mapping = aes(x=as.numeric(年度),y=issue.index,color=議題))
+geom_point(size=0.5)
+theme_bw()
+scale_x_continuous(name="年度",limits = c(104,110),breaks=c(104:110))
+scale_y_continuous(name="議題指標",limits = c(0,1.25),breaks=c(0.0,0.25,0.5,0.75,1.0,1.25)))
#+scale_y_continuous(limits = c(0,0.75),breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0,7)))

png("./圖片檔/加權/issue.index.png",width = 850,height = 600,res = 90,bg = "transparent")
print(issue.index.plot)
dev.off()


issue.index.sum<-as.matrix(tapply(sort.issue$issue.index, sort.issue$議題, FUN=sum))
write.csv(issue.index.sum, "./excel檔/加權/issue.index.sum.csv", row.names = TRUE ,fileEncoding = "Big5")

