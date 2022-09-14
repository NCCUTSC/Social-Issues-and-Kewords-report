library("tm")
library("tmcn")
library("Rwordseg")
library("SnowballC")
library("slam")
library("tmcn")
library(igraph)
library(network)
library(jiebaR)
library(Matrix)
library(stringr)
library(statnet)
library(GGally)
library("tm")
library("Rwordseg")
library("slam")
library(pdftools)
#===============================讀檔+整理資料=====
setwd("C:/Users/user/Desktop/R/文字探勘")
cc<-worker(user = "new_words - 第二版.txt", stop_word = "stop_words.txt", bylines = FALSE)

pro104 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/104.txt')
pro105 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/105.txt')
pro106 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/106.txt')
pro107 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/107.txt')
pro108 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/108.txt')
pro109 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/109.txt')
pro110 <- readLines('C:/Users/user/Desktop/R/txt檔/部會資料/110.txt')


pro.doc<-c(pro104,pro105,pro106,pro107,pro108,pro109,pro110)
content<-list()

for (i in 1:7){
  content[[i]]=cc[pro.doc[[i]]]
}
x <- VectorSource(content)
x <- Corpus(x)
x.text <- tm_map(x, stripWhitespace)
#x.text <- tm_map(x,content_transformer(removePunctuation))
#x.text <- tm_map(x,content_transformer(removeNumbers))
#x.text <- tm_map(x,content_transformer(function(word) {gsub("[A-Za-z0-9]", "", word)}))
#x.text <- tm_map(x,content_transformer(segmentCN),returnType="tm")
x.text[["7"]][["content"]]

pro.tdm <-TermDocumentMatrix(x.text,control = list(wordLengths=c(2,Inf)))
pro.tdm #Term Document Matrix
pro.tdm <- as.matrix(pro.tdm)
#str_length(row.names(new.tdm))
#new.tdm[new.tdm>=1] <- 1
#new.tdm<-data.frame(new.tdm)

#which((row.names(pro.tdm)=="\"學前幼童\","))
#length(which((row.names(pro.tdm)=="\"生育率\",")=="TRUE"))==0


mykeyword<-function(new.tdm,keyword){
  if(length(which((row.names(new.tdm)==keyword[1])=="TRUE"))==0){
    keymatrix<-matrix(0,1,ncol(new.tdm))
  }else{
    keymatrix<-new.tdm[which((row.names(new.tdm)==keyword[1])=="TRUE"),]
  }
  for (i in 2:length(keyword)){
    if(length(which((row.names(new.tdm)==keyword[i])=="TRUE"))==0){
      keymatrix<-rbind(keymatrix,0)
    }else{
      keymatrix<-rbind(keymatrix,new.tdm[which((row.names(new.tdm)==keyword[i])=="TRUE"),])
    }
  }
  return(keymatrix)
}

###############################age####
age<-c("\"中高齡勞工\",","\"高齡工作者\",","\"老年經濟安全\",","\"經濟安全保障\",","\"退休\","
       ,"\"退休金\",","\"高齡化社會\",","\"高齡友善\","
       ,"\"勞工退休金\",","\"勞退\",","\" 老人居住安排\",","\"獨居\","
       ,"\"以屋換屋\",","\"以房養老\",","\"全齡宅\",","\"二度就業\",")
pro.finallmatrix<-mykeyword(pro.tdm,17,age)
pro.finallmatrix.size<- mykeyword(pro.tdm,age)
age1<-c("中高齡勞工","高齡工作者"
        ,"老年經濟安全","經濟安全保障","退休"
        ,"退休金","高齡化社會","高齡友善"
        ,"勞工退休金","勞退","老人居住安排","獨居"
        ,"以屋換屋","以房養老","全齡宅","二度就業")
row.names(pro.finallmatrix.size)<- age1
plot.levels<-factor(row.names(pro.finallmatrix),levels = rev(age1))
cplot.levels<-factor(row.names(pro.finallmatrix),levels = age1)

for(i in 1:nrow(pro.finallmatrix)){
  for(j in 1:ncol(pro.finallmatrix)){
    if (pro.finallmatrix[i,j]>0){pro.finallmatrix[i,j] <- j}
  }
}
pro.finallmatrix<-data.frame(pro.finallmatrix)
pro.x<-ggplot(pro.finallmatrix,aes(
      y =pro.finallmatrix, 
      x =colnames(pro.finallmatrix)))+geom_point(pro.finallmatrix,mapping=aes(y = plot.levels,x=pro.finallmatrix[,1],size=pro.finallmatrix.size[,1],color=cplot.levels))+
scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+
xlab("時間")+ylab("關鍵字")+labs(size="次數大小",color="")+
theme(legend.position="right",legend.key.height= unit(0.5, 'cm'),legend.key.width= unit(0.5, 'cm'))
pro.x2<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,2],size=pro.finallmatrix.size[,2],color=rownames(pro.finallmatrix)))
pro.x3<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,3],size=pro.finallmatrix.size[,3],color=rownames(pro.finallmatrix)))
pro.x4<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,4],size=pro.finallmatrix.size[,4],color=rownames(pro.finallmatrix)))
pro.x5<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,5],size=pro.finallmatrix.size[,5],color=rownames(pro.finallmatrix)))
pro.x6<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,6],size=pro.finallmatrix.size[,6],color=rownames(pro.finallmatrix)))
pro.x7<-geom_point(pro.finallmatrix,mapping=aes(y = rownames(pro.finallmatrix),x=pro.finallmatrix[,7],size=pro.finallmatrix.size[,7],color=rownames(pro.finallmatrix)))

pro.age<-pro.x+pro.x2+pro.x3+pro.x4+pro.x5+pro.x6+pro.x7
pro.age<-plotfunction(pro.tdm,c("\"中高齡勞工\",","\"高齡工作者\",","\"老年經濟安全\",","\"經濟安全保障\",","\"退休\","
                                ,"\"退休金\",","\"高齡化社會\",","\"高齡友善\","
                                ,"\"勞工退休金\",","\"勞退\",","\" 老人居住安排\",","\"獨居\","
                                ,"\"以屋換屋\",","\"以房養老\",","\"全齡宅\",","\"二度就業\","),c("中高齡勞工","高齡工作者"
                                                                                   ,"老年經濟安全","經濟安全保障","退休"
                                                                                   ,"退休金","高齡化社會","高齡友善"
                                                                                   ,"勞工退休金","勞退","老人居住安排","獨居"
                                                                                   ,"以屋換屋","以房養老","全齡宅","二度就業"))
###############################kid####
pro.kid.matrix<-mykeyword(pro.tdm,22,c("\"生育率\",","\"育兒負擔\",","\"兒童養育\",","\"幼兒教育\","
                                       ,"\"兒童照顧\",","\"兒童健康\",","\"育兒成本\",","\"兒童\",","\"幼兒\","
                                       ,"\"學前兒童\",","\"職場暴力\",","\"職場霸凌\",","\"職場健康\",","\"職場能力\","
                                       ,"\"就業職能\",","\"就業市場\",","\"就業環境\",","\"就業選擇\",","\"兒童權利公約\","
                                       ,"\"兒童最佳利益\",","\"兒童發展\",","\"兒童安置機構\","))
row.names(pro.kid.matrix.size)<-c("\"生育率\",","\"育兒負擔\",","\"兒童養育\",","\"幼兒教育\","
                             ,"\"兒童照顧\",","\"兒童健康\",","\"育兒成本\",","\"兒童\",","\"幼兒\","
                             ,"\"學前兒童\",","\"職場暴力\",","\"職場霸凌\",","\"職場健康\",","\"職場能力\","
                             ,"\"就業職能\",","\"就業市場\",","\"就業環境\",","\"就業選擇\",","\"兒童權利公約\","
                             ,"\"兒童最佳利益\",","\"兒童發展\",","\"兒童安置機構\",")
pro.kid.matrix.size<-mykeyword(pro.tdm,c("\"生育率\",","\"育兒負擔\",","\"兒童養育\",","\"幼兒教育\","
                                         ,"\"兒童照顧\",","\"兒童健康\",","\"育兒成本\",","\"職場健康\","
                                         ,"\"職場能力\",","\"就業職能\",","\"就業選擇\",","\"兒童權利公約\","
                                         ,"\"兒童發展\",","\"托育資源可近性\",","\"育嬰假\",","\"兒童津貼\","
                                         ,"\"準公共托育\","))
for(i in 1:nrow(pro.kid.matrix)){
  for(j in 1:ncol(pro.kid.matrix)){
    if (pro.kid.matrix[i,j]>0){pro.kid.matrix[i,j] <- j}
  }
}
pro.kid.matrix<-data.frame(pro.kid.matrix)
kpro.x<-ggplot(pro.kid.matrix,aes(
  y =pro.kid.matrix, 
  x =colnames(pro.kid.matrix)))+geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,1],size=pro.kid.matrix.size[,1],color=rownames(pro.kid.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
kpro.x2<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,2],size=pro.kid.matrix.size[,2],color=rownames(pro.kid.matrix)))
kpro.x3<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,3],size=pro.kid.matrix.size[,3],color=rownames(pro.kid.matrix)))
kpro.x4<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,4],size=pro.kid.matrix.size[,4],color=rownames(pro.kid.matrix)))
kpro.x5<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,5],size=pro.kid.matrix.size[,5],color=rownames(pro.kid.matrix)))
kpro.x6<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,6],size=pro.kid.matrix.size[,6],color=rownames(pro.kid.matrix)))
kpro.x7<-geom_point(pro.kid.matrix,mapping=aes(y = rownames(pro.kid.matrix),x=pro.kid.matrix[,7],size=pro.kid.matrix.size[,7],color=rownames(pro.kid.matrix)))

pro.kid<-kpro.x+kpro.x2+kpro.x3+kpro.x4+kpro.x5+kpro.x6+kpro.x7
pro.kid<-plotfunction(pro.tdm,c("\"生育率\",","\"育兒負擔\",","\"兒童養育\",","\"幼兒教育\","
                                ,"\"兒童照顧\",","\"兒童健康\",","\"育兒成本\",","\"職場健康\","
                                ,"\"職場能力\",","\"就業職能\",","\"就業選擇\",","\"兒童權利公約\","
                                ,"\"兒童發展\",","\"托育資源可近性\",","\"育嬰假\",","\"兒童津貼\","
                                ,"\"準公共托育\","),c("生育率","育兒負擔","兒童養育","幼兒教育"
                                                      ,"兒童照顧","兒童健康","育兒成本","職場健康"
                                                      ,"職場能力","就業職能","就業市場","兒童權利公約"
                                                      ,"兒童發展","托育資源可近性","育嬰假","兒童津貼"
                                                      ,"準公共托育"))

###############################marry####
pro.marry.matrix<-mykeyword(pro.tdm,15,c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                         ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\",","\"婚姻因應\","
                                         ,"\"婚姻適應\",","\"受暴婦女\",","\"職業婦女\",","\"婦女健康\",","\"婚姻暴力\","
                                         ,"\"平等就業機會\","))
pro.marry.matrix.size<- mykeyword(pro.tdm,c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                            ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\","
                                            ,"\"婦女健康\",","\"婚姻暴力\",","\"平等就業機會\",","\"重返職場\","))
row.names(pro.marry.matrix.size)<- c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\",","\"婚姻因應\","
                                ,"\"婚姻適應\",","\"受暴婦女\",","\"職業婦女\",","\"婦女健康\",","\"婚姻暴力\","
                                ,"\"平等就業機會\",")

for(i in 1:nrow(pro.marry.matrix)){
  for(j in 1:ncol(pro.marry.matrix)){
    if (pro.marry.matrix[i,j]>0){pro.marry.matrix[i,j] <- j}
  }
}
pro.marry.matrix<-data.frame(pro.marry.matrix)
mpro.x<-ggplot(pro.marry.matrix,aes(
  y =pro.marry.matrix, 
  x =colnames(pro.marry.matrix)))+geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,1],size=pro.marry.matrix.size[,1],color=rownames(pro.marry.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
mpro.x2<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,2],size=pro.marry.matrix.size[,2],color=rownames(pro.marry.matrix)))
mpro.x3<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,3],size=pro.marry.matrix.size[,3],color=rownames(pro.marry.matrix)))
mpro.x4<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,4],size=pro.marry.matrix.size[,4],color=rownames(pro.marry.matrix)))
mpro.x5<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,5],size=pro.marry.matrix.size[,5],color=rownames(pro.marry.matrix)))
mpro.x6<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,6],size=pro.marry.matrix.size[,6],color=rownames(pro.marry.matrix)))
mpro.x7<-geom_point(pro.marry.matrix,mapping=aes(y = rownames(pro.marry.matrix),x=pro.marry.matrix[,7],size=pro.marry.matrix.size[,7],color=rownames(pro.marry.matrix)))

pro.marry<-mpro.x+mpro.x2+mpro.x3+mpro.x4+mpro.x5+mpro.x6+mpro.x7

pro.marry<-plotfunction(pro.tdm,c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                  ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\","
                                  ,"\"婦女健康\",","\"婚姻暴力\",","\"平等就業機會\",","\"重返職場\",")
                                ,c("婚姻滿意度","婚姻市場","婚姻移民","新移民"
                                 ,"跨國婚姻","婚姻平權","婚姻衝突","婚姻與家庭"
                                 ,"婦女健康","婚姻暴力","平等就業機會","重返職場"))
###############################education####
pro.edu.matrix<-mykeyword(pro.tdm,6,c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"升學就業技能\","
                                         ,"\"核心就業力\",","\"就業力\","))
pro.edu.matrix.size<- mykeyword(pro.tdm,c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"勞動力發展\",","\"升學就業技能\","
                                          ,"\"核心就業力\",","\"就業力\",","\"終身學習\",","\"數位學習\","))
row.names(pro.edu.matrix.size)<- c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"升學就業技能\","
                                     ,"\"核心就業力\",","\"就業力\",")
for(i in 1:nrow(pro.edu.matrix)){
  for(j in 1:ncol(pro.edu.matrix)){
    if (pro.edu.matrix[i,j]>0){pro.edu.matrix[i,j] <- j}
  }
}
pro.edu.matrix<-data.frame(pro.edu.matrix)
epro.x<-ggplot(pro.edu.matrix,aes(
  y =pro.edu.matrix, 
  x =colnames(pro.edu.matrix)))+geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,1],size=pro.edu.matrix.size[,1],color=rownames(pro.edu.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
epro.x2<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,2],size=pro.edu.matrix.size[,2],color=rownames(pro.edu.matrix)))
epro.x3<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,3],size=pro.edu.matrix.size[,3],color=rownames(pro.edu.matrix)))
epro.x4<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,4],size=pro.edu.matrix.size[,4],color=rownames(pro.edu.matrix)))
epro.x5<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,5],size=pro.edu.matrix.size[,5],color=rownames(pro.edu.matrix)))
epro.x6<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,6],size=pro.edu.matrix.size[,6],color=rownames(pro.edu.matrix)))
epro.x7<-geom_point(pro.edu.matrix,mapping=aes(y = rownames(pro.edu.matrix),x=pro.edu.matrix[,7],size=pro.edu.matrix.size[,7],color=rownames(pro.edu.matrix)))

pro.edu<-epro.x+epro.x2+epro.x3+epro.x4+epro.x5+epro.x6+epro.x7

pro.edu<-plotfunction(pro.tdm,c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"勞動力發展\",","\"升學就業技能\","
                                ,"\"核心就業力\",","\"就業力\",","\"終身學習\",","\"數位學習\",")
                              ,c("一般就業技能","人才培訓","職涯發展","勞動力發展","升學就業技能"
                                 ,"核心就業力","就業力","終身學習","數位學習"))



###############################care####
pro.care.matrix<-mykeyword(pro.tdm,19,c("\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                      ,"\"長期照護機構\",","\"長照安養機構\",","\"長照All-In-One\",","\"長照保險\","
                                      ,"\"長照2.0\",","\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                      ,"\"二代健保\",","\"健保\",","\"慢性病\",","\"兒童死亡率\","
                                      ,"\"兒童癌症\",","\"發展遲緩\","))
pro.care.matrix.size<- mykeyword(pro.tdm,c("\"照護\",","\"照顧\",","\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                           ,"\"長期照護機構\",","\"長照安養機構\",","\"長照保險\","
                                           ,"\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                           ,"\"二代健保\",","\"健保\",","\"發展遲緩\",","\"長照離職\","
                                           ,"\"照護喘息\","))
row.names(pro.care.matrix)<- c("\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                    ,"\"長期照護機構\",","\"長照安養機構\",","\"長照All-In-One\",","\"長照保險\","
                                    ,"\"長照2.0\",","\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                    ,"\"二代健保\",","\"健保\",","\"慢性病\",","\"兒童死亡率\","
                                    ,"\"兒童癌症\",","\"發展遲緩\",")
for(i in 1:nrow(pro.care.matrix)){
  for(j in 1:ncol(pro.care.matrix)){
    if (pro.care.matrix[i,j]>0){pro.care.matrix[i,j] <- j}
  }
}
pro.care.matrix<-data.frame(pro.care.matrix)
cpro.x<-ggplot(pro.care.matrix,aes(
  y =pro.care.matrix, 
  x =colnames(pro.care.matrix)))+geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,1],size=pro.care.matrix.size[,1],color=rownames(pro.care.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
cpro.x2<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,2],size=pro.care.matrix.size[,2],color=rownames(pro.care.matrix)))
cpro.x3<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,3],size=pro.care.matrix.size[,3],color=rownames(pro.care.matrix)))
cpro.x4<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,4],size=pro.care.matrix.size[,4],color=rownames(pro.care.matrix)))
cpro.x5<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,5],size=pro.care.matrix.size[,5],color=rownames(pro.care.matrix)))
cpro.x6<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,6],size=pro.care.matrix.size[,6],color=rownames(pro.care.matrix)))
cpro.x7<-geom_point(pro.care.matrix,mapping=aes(y = rownames(pro.care.matrix),x=pro.care.matrix[,7],size=pro.care.matrix.size[,7],color=rownames(pro.care.matrix)))

pro.care<-cpro.x+cpro.x2+cpro.x3+cpro.x4+cpro.x5+cpro.x6+cpro.x7

pro.care<-plotfunction(pro.tdm,c("\"照護\",","\"照顧\",","\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                 ,"\"長期照護機構\",","\"長照安養機構\",","\"長照保險\","
                                 ,"\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                 ,"\"二代健保\",","\"健保\",","\"發展遲緩\",","\"長照離職\","
                                 ,"\"照護喘息\","),c("照護","照顧","長照","長期照護","長期照護服務","社區式長期照護"
                                                             ,"長期照護機構","長照安養機構","長照保險"
                                                             ,"健康產業","移工","外籍看護","全民健保"
                                                             ,"二代健保","健保","發展遲緩","長照離職","照護喘息"))
###############################teen####
pro.teen.matrix<-mykeyword(pro.tdm,11,c("\"青年失業\",","\"青年就業\",","\"零工經濟\",","\"非正規就業\","
                                        ,"\"非典型就業\",","\"再就業\",","\"風險承擔\",","\"充分就業\","
                                        ,"\"就業輔導\",","\"勞工保險\",","\"失業救濟金\","))
pro.teen.matrix.size<- mykeyword(pro.tdm,c("\"青年就業\",","\"青年住宅\",","\"零工經濟\",","\"非正規就業\","
                                           ,"\"非典型就業\",","\"風險承擔\",","\"充分就業\",","\"就業輔導\","
                                           ,"\"職業訓練\",","\"就業保險\",","\"失業給付\",","\"失業救濟金\","))
row.names(pro.teen.matrix.size)<- c("\"青年失業\",","\"青年就業\",","\"零工經濟\",","\"非正規就業\","
                               ,"\"非典型就業\",","\"再就業\",","\"風險承擔\",","\"充分就業\","
                               ,"\"就業輔導\",","\"勞工保險\",","\"失業救濟金\",")
for(i in 1:nrow(pro.teen.matrix)){
  for(j in 1:ncol(pro.teen.matrix)){
    if (pro.teen.matrix[i,j]>0){pro.teen.matrix[i,j] <- j}
  }
}
pro.teen.matrix<-data.frame(pro.teen.matrix)
tpro.x<-ggplot(pro.teen.matrix,aes(
  y =pro.teen.matrix, 
  x =colnames(pro.teen.matrix)))+geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,1],size=pro.teen.matrix.size[,1],color=rownames(pro.teen.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
tpro.x2<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,2],size=pro.teen.matrix.size[,2],color=rownames(pro.teen.matrix)))
tpro.x3<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,3],size=pro.teen.matrix.size[,3],color=rownames(pro.teen.matrix)))
tpro.x4<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,4],size=pro.teen.matrix.size[,4],color=rownames(pro.teen.matrix)))
tpro.x5<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,5],size=pro.teen.matrix.size[,5],color=rownames(pro.teen.matrix)))
tpro.x6<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,6],size=pro.teen.matrix.size[,6],color=rownames(pro.teen.matrix)))
tpro.x7<-geom_point(pro.teen.matrix,mapping=aes(y = rownames(pro.teen.matrix),x=pro.teen.matrix[,7],size=pro.teen.matrix.size[,7],color=rownames(pro.teen.matrix)))

pro.teen<-tpro.x+tpro.x2+tpro.x3+tpro.x4+tpro.x5+tpro.x6+tpro.x7

pro.teen<-plotfunction(pro.tdm,c("\"青年就業\",","\"青年住宅\",","\"零工經濟\",","\"非正規就業\","
                                 ,"\"非典型就業\",","\"風險承擔\",","\"充分就業\",","\"就業輔導\","
                                 ,"\"職業訓練\",","\"就業保險\",","\"失業給付\",","\"失業救濟金\",")
                              ,c("青年就業","青年住宅","零工經濟","非正規就業"
                                 ,"非典型就業","風險承擔","充分就業","就業輔導"
                                 ,"職業訓練","就業保險","失業給付","失業救濟金"))
###############################live####
pro.live.matrix<-mykeyword(pro.tdm,27,c("\"居住正義\",","\"住宅宣言\",","\"都市更新\",","\"都更\","
                                        ,"\"房價\",","\"住宅費用\",","\"租屋\",","\"房租\","
                                        ,"\"租金\",","\"屋主\",","\"住宅擁有\",","\"住宅自有\",","\"住宅市場\","
                                        ,"\"住宅交易\",","\"空屋率\",","\"住宅階級\",","\"住宅投資\","
                                        ,"\"公共住宅\",","\"混合住宅\",","\"社會住宅\",","\"社宅\","
                                        ,"\"可負擔住宅\",","\"居住安排\",","\"居住區位\",","\"住宅負擔\","
                                        ,"\"住宅負擔力\",","\"社區居住\","))
pro.live.matrix.size<- mykeyword(pro.tdm,c("\"居住正義\",","\"住宅宣言\",","\"都市更新\",","\"都更\","
                                           ,"\"房價\",","\"住宅費用\",","\"租屋\",","\"房租\","
                                           ,"\"租金\",","\"屋主\",","\"住宅擁有\",","\"住宅自有\","
                                           ,"\"住宅市場\",","\"住宅交易\",","\"空屋率\",","\"住宅階級\","
                                           ,"\"住宅投資\",","\"公共住宅\",","\"混合住宅\",","\"社會住宅\","
                                           ,"\"社宅\",","\"居住安排\",","\"住宅負擔\",","\"住宅負擔力\","
                                           ,"\"危老\",","\"都市危險\",","\"老舊建築物\",","\"租金補貼\","
                                           ,"\"包租代管\",","\"城鄉差距\","))
row.names(pro.live.matrix.size)<- c("\"居住正義\",","\"住宅宣言\",","\"都市更新\",","\"都更\","
                               ,"\"房價\",","\"住宅費用\",","\"租屋\",","\"房租\","
                               ,"\"租金\",","\"屋主\",","\"住宅擁有\",","\"住宅自有\",","\"住宅市場\","
                               ,"\"住宅交易\",","\"空屋率\",","\"住宅階級\",","\"住宅投資\","
                               ,"\"公共住宅\",","\"混合住宅\",","\"社會住宅\",","\"社宅\","
                               ,"\"可負擔住宅\",","\"居住安排\",","\"居住區位\",","\"住宅負擔\","
                               ,"\"住宅負擔力\",","\"社區居住\",")
for(i in 1:nrow(pro.live.matrix)){
  for(j in 1:ncol(pro.live.matrix)){
    if (pro.live.matrix[i,j]>0){pro.live.matrix[i,j] <- j}
  }
}
pro.live.matrix<-data.frame(pro.live.matrix)
lpro.x<-ggplot(pro.live.matrix,aes(
  y =pro.live.matrix, 
  x =colnames(pro.live.matrix)))+geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,1],size=pro.live.matrix.size[,1],color=rownames(pro.live.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
lpro.x2<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,2],size=pro.live.matrix.size[,2],color=rownames(pro.live.matrix)))
lpro.x3<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,3],size=pro.live.matrix.size[,3],color=rownames(pro.live.matrix)))
lpro.x4<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,4],size=pro.live.matrix.size[,4],color=rownames(pro.live.matrix)))
lpro.x5<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,5],size=pro.live.matrix.size[,5],color=rownames(pro.live.matrix)))
lpro.x6<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,6],size=pro.live.matrix.size[,6],color=rownames(pro.live.matrix)))
lpro.x7<-geom_point(pro.live.matrix,mapping=aes(y = rownames(pro.live.matrix),x=pro.live.matrix[,7],size=pro.live.matrix.size[,7],color=rownames(pro.live.matrix)))

pro.live<-lpro.x+lpro.x2+lpro.x3+lpro.x4+lpro.x5+lpro.x6+lpro.x7

pro.live<-plotfunction(pro.tdm,c("\"居住正義\",","\"住宅宣言\",","\"都市更新\",","\"都更\","
                                 ,"\"房價\",","\"住宅費用\",","\"租屋\",","\"房租\","
                                 ,"\"租金\",","\"屋主\",","\"住宅擁有\",","\"住宅自有\","
                                 ,"\"住宅市場\",","\"住宅交易\",","\"空屋率\",","\"住宅階級\","
                                 ,"\"住宅投資\",","\"公共住宅\",","\"混合住宅\",","\"社會住宅\","
                                 ,"\"社宅\",","\"居住安排\",","\"住宅負擔\",","\"住宅負擔力\","
                                 ,"\"危老\",","\"都市危險\",","\"老舊建築物\",","\"租金補貼\","
                                 ,"\"包租代管\",","\"城鄉差距\",")
                              ,c("居住正義","住宅宣言","都市更新","都更"
                                 ,"房價","住宅費用","租屋","房租"
                                 ,"租金","屋主","住宅擁有","住宅自有","住宅市場"
                                 ,"住宅交易","空屋率","住宅階級","住宅投資"
                                 ,"公共住宅","混合住宅","社會住宅","社宅"
                                 ,"居住安排","住宅負擔","住宅負擔力"
                                 ,"危老","都市危險","老舊建築物","租金補貼","包租代管"
                                 ,"城鄉差距"))

###############################salary####
pro.sal.matrix<-mykeyword(pro.tdm,5,c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                      ,"\"健康不平等\","))
pro.sal.matrix.size<- mykeyword(pro.tdm,c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                         ,"\"社會安全\",","\"最低薪資\",","\"世代不公平\","
                                         ,"\"中產階級消失\",","\"階級僵固\","))
row.names(pro.sal.matrix)<- c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                   ,"\"健康不平等\",")
for(i in 1:nrow(pro.sal.matrix)){
  for(j in 1:ncol(pro.sal.matrix)){
    if (pro.sal.matrix[i,j]>0){pro.sal.matrix[i,j] <- j}
  }
}
pro.sal.matrix<-data.frame(pro.sal.matrix)
spro.x<-ggplot(pro.sal.matrix,aes(
  y =pro.sal.matrix, 
  x =colnames(pro.sal.matrix)))+geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,1],size=pro.sal.matrix.size[,1],color=rownames(pro.sal.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
spro.x2<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,2],size=pro.sal.matrix.size[,2],color=rownames(pro.sal.matrix)))
spro.x3<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,3],size=pro.sal.matrix.size[,3],color=rownames(pro.sal.matrix)))
spro.x4<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,4],size=pro.sal.matrix.size[,4],color=rownames(pro.sal.matrix)))
spro.x5<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,5],size=pro.sal.matrix.size[,5],color=rownames(pro.sal.matrix)))
spro.x6<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,6],size=pro.sal.matrix.size[,6],color=rownames(pro.sal.matrix)))
spro.x7<-geom_point(pro.sal.matrix,mapping=aes(y = rownames(pro.sal.matrix),x=pro.sal.matrix[,7],size=pro.sal.matrix.size[,7],color=rownames(pro.sal.matrix)))

pro.sal<-spro.x+spro.x2+spro.x3+spro.x4+spro.x5+spro.x6+spro.x7

pro.sal<-plotfunction(pro.tdm,c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                ,"\"社會安全\",","\"最低薪資\",","\"世代不公平\","
                                ,"\"中產階級消失\",","\"階級僵固\","),
                              c("所得分配不均","住宅負擔","住宅負擔力","相對剝奪感"
                                ,"社會安全","最低薪資","世代不公平",
                                "中產階級消失","階級僵固"))



###############################digital####
pro.dig.matrix<-mykeyword(pro.tdm,7,c("\"產業創新\",","\"數位轉型\",","\"數位環境\",","\"中小企業\","
                                      ,"\"自動化\",","\"ai\",","\"智慧化\","))
pro.dig.matrix.size<- mykeyword(pro.tdm,c("\"產業創新\",","\"數位轉型\",","\"中小企業\","
                                          ,"\"自動化\",","\"ai\",","\"智慧化\",","\"共享經濟\",","\"數位經濟\","))
row.names(pro.dig.matrix)<- c("\"產業創新\",","\"數位轉型\",","\"數位環境\",","\"中小企業\","
                              ,"\"自動化\",","\"AI\",","\"智慧化\",")
for(i in 1:nrow(pro.dig.matrix)){
  for(j in 1:ncol(pro.dig.matrix)){
    if (pro.dig.matrix[i,j]>0){pro.dig.matrix[i,j] <- j}
  }
}
pro.dig.matrix<-data.frame(pro.dig.matrix)
dpro.x<-ggplot(pro.dig.matrix,aes(
  y =pro.dig.matrix, 
  x =colnames(pro.dig.matrix)))+geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,1],size=pro.dig.matrix.size[,1],color=rownames(pro.dig.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
dpro.x2<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,2],size=pro.dig.matrix.size[,2],color=rownames(pro.dig.matrix)))
dpro.x3<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,3],size=pro.dig.matrix.size[,3],color=rownames(pro.dig.matrix)))
dpro.x4<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,4],size=pro.dig.matrix.size[,4],color=rownames(pro.dig.matrix)))
dpro.x5<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,5],size=pro.dig.matrix.size[,5],color=rownames(pro.dig.matrix)))
dpro.x6<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,6],size=pro.dig.matrix.size[,6],color=rownames(pro.dig.matrix)))
dpro.x7<-geom_point(pro.dig.matrix,mapping=aes(y = rownames(pro.dig.matrix),x=pro.dig.matrix[,7],size=pro.dig.matrix.size[,7],color=rownames(pro.dig.matrix)))

pro.dig<-dpro.x+dpro.x2+dpro.x3+dpro.x4+dpro.x5+dpro.x6+dpro.x7

pro.dig<-plotfunction(pro.tdm,c("\"產業創新\",","\"數位轉型\",","\"中小企業\","
                                ,"\"自動化\",","\"ai\",","\"智慧化\",","\"共享經濟\",","\"數位經濟\",")
                              ,c("產業創新","數位轉型","中小企業"
                                ,"自動化","ai","智慧化","共享經濟","數位經濟"))
###############################blackmail####
pro.black.matrix<-mykeyword(pro.tdm,3,c("\"詐騙\",","\"假消息\",","\"風險溝通\","))
pro.black.matrix.size<- mykeyword(pro.tdm,c("\"詐騙\",","\"假消息\",","\"風險溝通\","
                                              ,"\"假新聞\",","\"網路霸凌\",","\"網際網路治理\","))
row.names(pro.black.matrix.size)<- c("\"詐騙\",","\"假消息\",","\"風險溝通\",")
for(i in 1:nrow(pro.black.matrix)){
  for(j in 1:ncol(pro.black.matrix)){
    if (pro.black.matrix[i,j]>0){pro.black.matrix[i,j] <- j}
  }
}
pro.black.matrix<-data.frame(pro.black.matrix)
bpro.x<-ggplot(pro.black.matrix,aes(
  y =pro.black.matrix, 
  x =colnames(pro.black.matrix)))+geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,1],size=pro.black.matrix.size[,1],color=rownames(pro.black.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
bpro.x2<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,2],size=pro.black.matrix.size[,2],color=rownames(pro.black.matrix)))
bpro.x3<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,3],size=pro.black.matrix.size[,3],color=rownames(pro.black.matrix)))
bpro.x4<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,4],size=pro.black.matrix.size[,4],color=rownames(pro.black.matrix)))
bpro.x5<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,5],size=pro.black.matrix.size[,5],color=rownames(pro.black.matrix)))
bpro.x6<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,6],size=pro.black.matrix.size[,6],color=rownames(pro.black.matrix)))
bpro.x7<-geom_point(pro.black.matrix,mapping=aes(y = rownames(pro.black.matrix),x=pro.black.matrix[,7],size=pro.black.matrix.size[,7],color=rownames(pro.black.matrix)))

pro.black<-bpro.x+bpro.x2+bpro.x3+bpro.x4+bpro.x5+bpro.x6+bpro.x7

pro.black<-plotfunction(pro.tdm,c("\"詐騙\",","\"假消息\",","\"風險溝通\","
                                  ,"\"假新聞\",","\"網路霸凌\",","\"網際網路治理\","),
                        c("詐騙","假消息","風險溝通","假新聞","網路霸凌","網際網路治理"))
###############################transform####
pro.trans.matrix<-mykeyword(pro.tdm,13,c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                        ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                        ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\","))
pro.trans.matrix.size<- mykeyword(pro.tdm,c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                            ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                            ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\","))
row.names(pro.trans.matrix.size)<- c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                     ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                     ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\",")
for(i in 1:nrow(pro.trans.matrix)){
  for(j in 1:ncol(pro.trans.matrix)){
    if (pro.trans.matrix[i,j]>0){pro.trans.matrix[i,j] <- j}
  }
}
pro.trans.matrix<-data.frame(pro.trans.matrix)
trpro.x<-ggplot(pro.trans.matrix,aes(
  y =pro.trans.matrix, 
  x =colnames(pro.trans.matrix)))+geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,1],size=pro.trans.matrix.size[,1],color=rownames(pro.trans.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
trpro.x2<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,2],size=pro.trans.matrix.size[,2],color=rownames(pro.trans.matrix)))
trpro.x3<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,3],size=pro.trans.matrix.size[,3],color=rownames(pro.trans.matrix)))
trpro.x4<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,4],size=pro.trans.matrix.size[,4],color=rownames(pro.trans.matrix)))
trpro.x5<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,5],size=pro.trans.matrix.size[,5],color=rownames(pro.trans.matrix)))
trpro.x6<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,6],size=pro.trans.matrix.size[,6],color=rownames(pro.trans.matrix)))
trpro.x7<-geom_point(pro.trans.matrix,mapping=aes(y = rownames(pro.trans.matrix),x=pro.trans.matrix[,7],size=pro.trans.matrix.size[,7],color=rownames(pro.trans.matrix)))

pro.trans<-trpro.x+trpro.x2+trpro.x3+trpro.x4+trpro.x5+trpro.x6+trpro.x7

pro.trans<-plotfunction(pro.tdm,c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                  ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                  ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\","),c("循環經濟","普惠金融","綠色金融","永續金融"
                                                                                               ,"能源轉型","能源價格","綠能","再生能源"
                                                                                               ,"低碳能源","太陽能","太陽光電","風能","風力發電"))
###############################climate####
pro.cli.matrix<-mykeyword(pro.tdm,27,c("\"氣候變遷\",","\"水資源\",","\"水資源安全\",","\"水資源再利用\","
                                       ,"\"空氣污染\",","\"土壤污染\",","\"災害\",","\"天然災害\",","\"水災\","
                                       ,"\"風災\",","\"自然災害\",","\"災後防救\",","\"災害治理\",","\"災害風險\","
                                       ,"\"災害管理\",","\"災害潛勢\",","\"災害應變\",","\"巴黎氣候峰會\",","\"企業環境友善行為\","
                                       ,"\"地方環境治理\",","\"環境治理\",","\"能源貧窮\",","\"綠建築\",","\"氣候難民\","
                                       ,"\"環境政治\",","\"環境教育\",","\"聯合國\","))
row.names(pro.cli.matrix.size)<-c("\"氣候變遷\",","\"水資源\",","\"水資源安全\",","\"水資源再利用\","
                                  ,"\"空氣污染\",","\"土壤污染\",","\"災害\",","\"天然災害\",","\"水災\","
                                  ,"\"風災\",","\"自然災害\",","\"災後防救\",","\"災害治理\",","\"災害風險\","
                                  ,"\"災害管理\",","\"災害潛勢\",","\"災害應變\",","\"巴黎氣候峰會\",","\"企業環境友善行為\","
                                  ,"\"地方環境治理\",","\"環境治理\",","\"能源貧窮\",","\"綠建築\",","\"氣候難民\","
                                  ,"\"環境政治\",","\"環境教育\",","\"聯合國\",")
pro.cli.matrix.size<-mykeyword(pro.tdm,c("\"氣候變遷\",","\"水資源\",","\"水資源安全\",","\"水資源再利用\","
                                         ,"\"空氣污染\",","\"天然災害\",","\"水災\",","\"旱災\","
                                         ,"\"災害風險\",","\"災害管理\",","\"災害應變\",","\"環境治理\","
                                         ,"\"能源貧窮\",","\"綠建築\",","\"氣候難民\","))
for(i in 1:nrow(pro.cli.matrix)){
  for(j in 1:ncol(pro.cli.matrix)){
    if (pro.cli.matrix[i,j]>0){pro.cli.matrix[i,j] <- j}
  }
}
pro.cli.matrix<-data.frame(pro.cli.matrix)
clpro.x<-ggplot(pro.cli.matrix,aes(
  y =pro.cli.matrix, 
  x =colnames(pro.cli.matrix)))+geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,1],size=pro.cli.matrix.size[,1],color=rownames(pro.cli.matrix)))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")  
clpro.x2<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,2],size=pro.cli.matrix.size[,2],color=rownames(pro.cli.matrix)))
clpro.x3<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,3],size=pro.cli.matrix.size[,3],color=rownames(pro.cli.matrix)))
clpro.x4<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,4],size=pro.cli.matrix.size[,4],color=rownames(pro.cli.matrix)))
clpro.x5<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,5],size=pro.cli.matrix.size[,5],color=rownames(pro.cli.matrix)))
clpro.x6<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,6],size=pro.cli.matrix.size[,6],color=rownames(pro.cli.matrix)))
clpro.x7<-geom_point(pro.cli.matrix,mapping=aes(y = rownames(pro.cli.matrix),x=pro.cli.matrix[,7],size=pro.cli.matrix.size[,7],color=rownames(pro.cli.matrix)))

pro.cli<-clpro.x+clpro.x2+clpro.x3+clpro.x4+clpro.x5+clpro.x6+clpro.x7

pro.cli<-plotfunction(pro.tdm,c("\"氣候變遷\",","\"水資源\",","\"水資源安全\",","\"水資源再利用\","
                                ,"\"空氣污染\",","\"天然災害\",","\"水災\",","\"旱災\","
                                ,"\"災害風險\",","\"災害管理\",","\"災害應變\",","\"環境治理\","
                                ,"\"能源貧窮\",","\"綠建築\",","\"氣候難民\",")
                              ,c("氣候變遷","水資源","水資源安全","水資源再利用"
                                 ,"空氣污染","天然災害","水災","旱災"
                                 ,"災害風險","災害管理","災害應變","環境治理"
                                 ,"能源貧窮","綠建築","氣候難民"))
###############################
bindmatrix <- function(matrix){
  x<-sum(matrix[[1]][,1])
    for (j in 2:ncol(matrix[[1]])){
      x<-cbind(x,sum(matrix[[1]][,j]))
    }
  y<-sum(matrix[[2]][,1])
    for (j in 2:ncol(matrix[[2]])){
      y<-cbind(y,sum(matrix[[2]][,j]))
    }
    x<-rbind(x,y)
  for (i in 3:length(matrix)){
  z<-sum(matrix[[i]][,1])
    for (j in 2:ncol(matrix[[i]])){
      z<-cbind(z,sum(matrix[[i]][,j]))
    }
  x<-rbind(x,z)
  }
  return(x)
}

issuematrix <- bindmatrix(list(pro.finallmatrix.size,pro.care.matrix.size,pro.kid.matrix.size
                               ,pro.marry.matrix.size,pro.edu.matrix.size,pro.teen.matrix.size
                               ,pro.live.matrix.size,pro.sal.matrix.size,pro.dig.matrix.size
                               ,pro.black.matrix.size,pro.trans.matrix.size,pro.cli.matrix.size))
rownames(issuematrix) <- c("超高齡化社會","中間世代的照護壓力與負擔","少子女化及托育需求","晚婚、不婚化","教育時間長與教育資源分配",
              "青年失業與零工經濟","高房價及城鄉差距擴大",
              "貧富差距擴大與薪資成長緩慢","產業數位轉型與自動化發展","網路詐騙與假消息",
              "氣候驅動的產業轉型壓力","極端氣候的環境影響")
colnames(issuematrix) <-c("104","105","106","107","108","109","110")
iplot.levels<-factor(row.names(issuematrix),levels = rev(rownames(issuematrix)))
icplot.levels<-factor(row.names(issuematrix),levels = rownames(issuematrix))

issuematrix.size<-issuematrix
for(i in 1:nrow(issuematrix)){
  for(j in 1:ncol(issuematrix)){
    if (issuematrix[i,j]>0){issuematrix[i,j] <- j}
  }
}
issuematrix<-data.frame(issuematrix)
ispro.x<-ggplot(issuematrix,aes(
  y =issuematrix, 
  x =colnames(issuematrix)))+geom_point(issuematrix,mapping=aes(y = iplot.levels,x=issuematrix[,1],size=issuematrix.size[,1],color=icplot.levels))+scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+xlab("時間")+ylab("關鍵字")+
  labs(size="次數大小",color="")+
  theme(legend.position="right",legend.key.height= unit(0.5, 'cm'),legend.key.width= unit(0.5, 'cm'))
ispro.x2<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,2],size=issuematrix.size[,2],color=rownames(issuematrix)))
ispro.x3<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,3],size=issuematrix.size[,3],color=rownames(issuematrix)))
ispro.x4<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,4],size=issuematrix.size[,4],color=rownames(issuematrix)))
ispro.x5<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,5],size=issuematrix.size[,5],color=rownames(issuematrix)))
ispro.x6<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,6],size=issuematrix.size[,6],color=rownames(issuematrix)))
ispro.x7<-geom_point(issuematrix,mapping=aes(y = rownames(issuematrix),x=issuematrix[,7],size=issuematrix.size[,7],color=rownames(issuematrix)))

pro.issue<-ispro.x+ispro.x2+ispro.x3+ispro.x4+ispro.x5+ispro.x6+ispro.x7




#############################function####
plotfunction <- function(bigmatrix,keyword,rowname){
Finallmatrix<-mykeyword(bigmatrix,keyword)
Finallmatrix.size<-mykeyword(bigmatrix,keyword)
rownames(Finallmatrix)<-rowname
rownames(Finallmatrix.size)<-rowname
plot.levels<-factor(row.names(Finallmatrix),levels = rev(rowname))
cplot.levels<-factor(row.names(Finallmatrix),levels = rowname)
for(i in 1:nrow(Finallmatrix)){
  for(j in 1:ncol(Finallmatrix)){
    if (Finallmatrix[i,j]>0){Finallmatrix[i,j] <- j}
  }
}
Finallmatrix<-data.frame(Finallmatrix)
pro.x<-ggplot(Finallmatrix,aes(
  y =Finallmatrix, 
  x =colnames(Finallmatrix)))+geom_point(Finallmatrix,mapping=aes(y = plot.levels,x=Finallmatrix[,1],size=Finallmatrix.size[,1],color=cplot.levels))+
  scale_x_continuous(limits = c(1,7),breaks=c(1:7),labels = c("104","105","106","107","108","109","110"))+
  xlab("時間")+ylab("關鍵字")+labs(size="次數大小",color="")+
  theme(legend.position="right",legend.key.height= unit(0.5, 'cm'),legend.key.width= unit(0.5, 'cm'))
pro.x2<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,2],size=Finallmatrix.size[,2],color=rownames(Finallmatrix)))
pro.x3<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,3],size=Finallmatrix.size[,3],color=rownames(Finallmatrix)))
pro.x4<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,4],size=Finallmatrix.size[,4],color=rownames(Finallmatrix)))
pro.x5<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,5],size=Finallmatrix.size[,5],color=rownames(Finallmatrix)))
pro.x6<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,6],size=Finallmatrix.size[,6],color=rownames(Finallmatrix)))
pro.x7<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,7],size=Finallmatrix.size[,7],color=rownames(Finallmatrix)))

pro.age<-pro.x+pro.x2+pro.x3+pro.x4+pro.x5+pro.x6+pro.x7
return(pro.age)
}

colnames(pro.cli.matrix.size)<-c(104:110)
row.names(pro.cli.matrix.size)<-c("氣候變遷","水資源","水資源安全","水資源再利用"
                                  ,"空氣污染","天然災害","水災","旱災"
                                  ,"災害風險","災害管理","災害應變","環境治理"
                                  ,"能源貧窮","綠建築","氣候難民")

pro.age
pro.care
pro.kid
pro.marry
pro.edu
pro.teen
pro.live
pro.sal
pro.dig
pro.black
pro.trans
pro.cli
pro.issue



#####################################test讀檔####
text<- pdf_text("C:/Users/user/Desktop/R/txt檔/104部會/01行政院104施政方針.pdf")

setwd("C:/Users/user/Desktop/R/txt檔")
file_name<-list.files("部會資料")
dir<-paste("./部會資料/",file_name,sep="")
n<-length(dir)


b<-list.files(dir[7])
new_data<-rep(0,length(b))
for(j in 1:length(b)){
  file<-paste(dir[7],"/",b[j],sep="")
  for (i in 1:length(pdf_text(file)))
    new_data[j]<-str_c(new_data[j],pdf_text(file)[i])
}


#file<-paste(dir[2],"/",b[1],sep="")
#pdf_text(file)[1]


year_110data<-new_data[1]
for (k in 2:length(new_data)){
  year_110data<-str_c(year_110data,new_data[k])
}

writeLines(year_110data,"110")

