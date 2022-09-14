library("tm")
library("tmcn")
library("rJava")
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
cc<-worker(user = "new_words - 第二版.txt", bylines = FALSE)

obj101 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/101.txt')
obj102 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/102.txt',warn=FALSE)
obj103 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/103.txt',warn=FALSE)
obj104 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/104.txt',warn=FALSE)
obj105 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/105.txt',warn=FALSE)
obj106 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/106.txt',warn=FALSE)
obj107 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/107.txt',warn=FALSE)
obj108 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/108.txt',warn=FALSE)
obj109 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/109.txt',warn=FALSE)
obj110 <- readLines('C:/Users/user/Desktop/R/txt檔/政府刊物2/110.txt',warn=FALSE)

obj.doc<-c(obj101,obj102,obj103,obj104,obj105,obj106,obj107,obj108,obj109,obj110)
content<-list()

for (i in 1:10){
  content[[i]]=cc[obj.doc[[i]]]
}

x <- VectorSource(content)
x <- Corpus(x)
x.text <- tm_map(x, stripWhitespace)
x.text <- tm_map(x,content_transformer(removePunctuation))
x.text <- tm_map(x,content_transformer(removeNumbers))
#x.text <- tm_map(x,content_transformer(function(word) {gsub("[A-Za-z0-9]", "", word)}))
#x.text <- tm_map(x,content_transformer(segmentCN),returnType="tm")
x.text[["7"]][["content"]]

obj.tdm101.1 <-TermDocumentMatrix(x.text,control = list(wordLengths=c(2,Inf)))
obj.tdm #Term Document Matrix
obj.tdm <- as.matrix(obj.tdm)
#str_length(row.names(new.tdm))
#new.tdm[new.tdm>=1] <- 1
#new.tdm<-data.frame(new.tdm)

which((row.names(obj.tdm)=="\"照顧\","))
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
pro.finallmatrix.size<- mykeyword(obj.tdm,c("\"中高齡勞工\",","\"高齡工作者\",","\"老年經濟安全\",","\"經濟安全保障\",","\"退休\","
                                            ,"\"退休金\",","\"高齡化社會\",","\"高齡友善\","
                                            ,"\"勞工退休金\",","\"勞退\",","\" 老人居住安排\",","\"獨居\","
                                            ,"\"以屋換屋\",","\"以房養老\",","\"全齡宅\",","\"二度就業\","))
pro.age<-plotfunction(pro.tdm,c("\"中高齡勞工\",","\"高齡工作者\",","\"老年經濟安全\",","\"經濟安全保障\",","\"退休\","
                                ,"\"退休金\",","\"高齡化社會\",","\"高齡友善\","
                                ,"\"勞工退休金\",","\"勞退\",","\" 老人居住安排\",","\"獨居\","
                                ,"\"以屋換屋\",","\"以房養老\",","\"全齡宅\",","\"二度就業\","),c("中高齡勞工","高齡工作者"
                                                                                   ,"老年經濟安全","經濟安全保障","退休"
                                                                                   ,"退休金","高齡化社會","高齡友善"
                                                                                   ,"勞工退休金","勞退","老人居住安排","獨居"
                                                                                   ,"以屋換屋","以房養老","全齡宅","二度就業"))
###############################kid####
pro.kid.matrix.size<-mykeyword(obj.tdm,c("\"生育率\",","\"育兒負擔\",","\"兒童養育\",","\"幼兒教育\","
                                         ,"\"兒童照顧\",","\"兒童健康\",","\"育兒成本\",","\"職場健康\","
                                         ,"\"職場能力\",","\"就業職能\",","\"就業選擇\",","\"兒童權利公約\","
                                         ,"\"兒童發展\",","\"托育資源可近性\",","\"育嬰假\",","\"兒童津貼\","
                                         ,"\"準公共托育\","))
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
pro.marry.matrix.size<- mykeyword(obj.tdm,c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                            ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\","
                                            ,"\"婦女健康\",","\"婚姻暴力\",","\"平等就業機會\",","\"重返職場\","))
pro.marry<-plotfunction(pro.tdm,c("\"婚姻滿意度\",","\"婚姻市場\",","\"婚姻移民\",","\"新移民\","
                                  ,"\"跨國婚姻\",","\"婚姻平權\",","\"婚姻衝突\",","\"婚姻與家庭\","
                                  ,"\"婦女健康\",","\"婚姻暴力\",","\"平等就業機會\",","\"重返職場\",")
                        ,c("婚姻滿意度","婚姻市場","婚姻移民","新移民"
                           ,"跨國婚姻","婚姻平權","婚姻衝突","婚姻與家庭"
                           ,"婦女健康","婚姻暴力","平等就業機會","重返職場"))
###############################education####
pro.edu.matrix.size<- mykeyword(obj.tdm,c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"勞動力發展\",","\"升學就業技能\","
                                          ,"\"核心就業力\",","\"就業力\",","\"終身學習\",","\"數位學習\","))
pro.edu<-plotfunction(pro.tdm,c("\"一般就業技能\",","\"人才培訓\",","\"職涯發展\",","\"勞動力發展\",","\"升學就業技能\","
                                ,"\"核心就業力\",","\"就業力\",","\"終身學習\",","\"數位學習\",")
                      ,c("一般就業技能","人才培訓","職涯發展","勞動力發展","升學就業技能"
                         ,"核心就業力","就業力","終身學習","數位學習"))
###############################care####
pro.care.matrix.size<- mykeyword(obj.tdm,c("\"照護\",","\"照顧\",","\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                           ,"\"長期照護機構\",","\"長照安養機構\",","\"長照保險\","
                                           ,"\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                           ,"\"二代健保\",","\"健保\",","\"發展遲緩\",","\"長照離職\","
                                           ,"\"照護喘息\","))
pro.care<-plotfunction(pro.tdm,c("\"照護\",","\"照顧\",","\"長照\",","\"長期照護\",","\"長期照護服務\",","\"社區式長期照護\","
                                 ,"\"長期照護機構\",","\"長照安養機構\",","\"長照保險\","
                                 ,"\"健康產業\",","\"移工\",","\"外籍看護\",","\"全民健保\","
                                 ,"\"二代健保\",","\"健保\",","\"發展遲緩\",","\"長照離職\","
                                 ,"\"照護喘息\","),c("照護","照顧","長照","長期照護","長期照護服務","社區式長期照護"
                                                 ,"長期照護機構","長照安養機構","長照保險"
                                                 ,"健康產業","移工","外籍看護","全民健保"
                                                 ,"二代健保","健保","發展遲緩","長照離職","照護喘息"))
###############################teen####
pro.teen.matrix.size<- mykeyword(pro.tdm,c("\"青年就業\",","\"青年住宅\",","\"零工經濟\",","\"非正規就業\","
                                           ,"\"非典型就業\",","\"風險承擔\",","\"充分就業\",","\"就業輔導\","
                                           ,"\"職業訓練\",","\"就業保險\",","\"失業給付\",","\"失業救濟金\","))
pro.teen<-plotfunction(pro.tdm,c("\"青年就業\",","\"青年住宅\",","\"零工經濟\",","\"非正規就業\","
                                 ,"\"非典型就業\",","\"風險承擔\",","\"充分就業\",","\"就業輔導\","
                                 ,"\"職業訓練\",","\"就業保險\",","\"失業給付\",","\"失業救濟金\",")
                       ,c("青年就業","青年住宅","零工經濟","非正規就業"
                          ,"非典型就業","風險承擔","充分就業","就業輔導"
                          ,"職業訓練","就業保險","失業給付","失業救濟金"))
###############################live####
pro.live.matrix.size<- mykeyword(pro.tdm,c("\"居住正義\",","\"住宅宣言\",","\"都市更新\",","\"都更\","
                                           ,"\"房價\",","\"住宅費用\",","\"租屋\",","\"房租\","
                                           ,"\"租金\",","\"屋主\",","\"住宅擁有\",","\"住宅自有\","
                                           ,"\"住宅市場\",","\"住宅交易\",","\"空屋率\",","\"住宅階級\","
                                           ,"\"住宅投資\",","\"公共住宅\",","\"混合住宅\",","\"社會住宅\","
                                           ,"\"社宅\",","\"居住安排\",","\"住宅負擔\",","\"住宅負擔力\","
                                           ,"\"危老\",","\"都市危險\",","\"老舊建築物\",","\"租金補貼\","
                                           ,"\"包租代管\",","\"城鄉差距\","))
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
pro.sal.matrix.size<- mykeyword(pro.tdm,c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                          ,"\"社會安全\",","\"最低薪資\",","\"世代不公平\","
                                          ,"\"中產階級消失\",","\"階級僵固\","))
pro.sal<-plotfunction(pro.tdm,c("\"所得分配不均\",","\"住宅負擔\",","\"住宅負擔力\",","\"相對剝奪感\","
                                ,"\"社會安全\",","\"最低薪資\",","\"世代不公平\","
                                ,"\"中產階級消失\",","\"階級僵固\","),c("所得分配不均","住宅負擔","住宅負擔力","相對剝奪感"
                                                              ,"社會安全","最低薪資","世代不公平",
                                                              "中產階級消失","階級僵固"))



###############################digital####
pro.dig.matrix.size<- mykeyword(pro.tdm,c("\"產業創新\",","\"數位轉型\",","\"中小企業\","
                                          ,"\"自動化\",","\"ai\",","\"智慧化\",","\"共享經濟\",","\"數位經濟\","))
pro.dig<-plotfunction(pro.tdm,c("\"產業創新\",","\"數位轉型\",","\"中小企業\","
                                ,"\"自動化\",","\"ai\",","\"智慧化\",","\"共享經濟\",","\"數位經濟\",")
                      ,c("產業創新","數位轉型","中小企業"
                         ,"自動化","ai","智慧化","共享經濟","數位經濟"))
###############################blackmail####
pro.black.matrix.size<- mykeyword(pro.tdm,c("\"詐騙\",","\"假消息\",","\"風險溝通\","
                                            ,"\"假新聞\",","\"網路霸凌\",","\"網際網路治理\","))
pro.black<-plotfunction(pro.tdm,c("\"詐騙\",","\"假消息\",","\"風險溝通\","
                                  ,"\"假新聞\",","\"網路霸凌\",","\"網際網路治理\","),
                        c("詐騙","假消息","風險溝通","假新聞","網路霸凌","網際網路治理"))
###############################transform####
pro.trans.matrix.size<- mykeyword(pro.tdm,c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                            ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                            ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\","))
pro.trans<-plotfunction(pro.tdm,c("\"循環經濟\",","\"普惠金融\",","\"綠色金融\",","\"永續金融\","
                                  ,"\"能源轉型\",","\"能源價格\",","\"綠能\",","\"再生能源\","
                                  ,"\"低碳能源\",","\"太陽能\",","\"太陽光電\",","\"風能\",","\"風力發電\","),c("循環經濟","普惠金融","綠色金融","永續金融"
                                                                                               ,"能源轉型","能源價格","綠能","再生能源"
                                                                                               ,"低碳能源","太陽能","太陽光電","風能","風力發電"))
###############################climate####
pro.cli.matrix.size<-mykeyword(pro.tdm,c("\"氣候變遷\",","\"水資源\",","\"水資源安全\",","\"水資源再利用\","
                                         ,"\"空氣污染\",","\"天然災害\",","\"水災\",","\"旱災\","
                                         ,"\"災害風險\",","\"災害管理\",","\"災害應變\",","\"環境治理\","
                                         ,"\"能源貧窮\",","\"綠建築\",","\"氣候難民\","))
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
text<-pdf_text("C:/Users/user/Desktop/R/txt檔/政府刊物2/內政部移民署雙月刊-第44期.pdf")

setwd("C:/Users/user/Desktop/R/txt檔")
file_name<-list.files("政府刊物2")
dir<-paste("./政府刊物2/",file_name,sep="")
n<-length(dir)


b<-list.files(dir[1])
new_data<-rep(0,40)
for(j in 1:40){
  file<-paste(dir[1],"/",b[j],sep="")
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


