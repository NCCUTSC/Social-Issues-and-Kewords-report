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
library(ggplot2)
library(pdftools)
#===========================
setwd("C:/Users/JOEZ/OneDrive/桌面/R/國發會")
cc<-worker(user = "new_words - 第二版.txt", bylines = FALSE)
#==============================讀取檔案
file_name<-list.files("社會期刊")
dir<-paste("./社會期刊/",file_name,sep="")
n<-length(dir)

test <- readLines("社會期刊/99/2010_1中國大陸研究.txt",encoding="UTF-8")
b<-list.files(dir[9])
new_data<-rep(0,length(b))
for(j in 1:length(b)){
  file<-paste(dir[9],"/",b[j],sep="")
  for (i in 1:length(readLines(file,encoding="UTF-8")))
    new_data[j]<-str_c(new_data[j],readLines(file,encoding="UTF-8")[i])
}

b[82]
#file<-paste(dir[2],"/",b[1],sep="")
#pdf_text(file)[1]

year_108data<-new_data[1]
for (k in 2:length(new_data)){
  year_108data<-str_c(year_108data,new_data[k])
}

writeLines(year_108data,"108")
#=================================
year_99data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/99.txt")
year_100data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/100.txt")
year_101data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/101.txt")
year_102data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/102.txt")
year_103data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/103.txt")
year_104data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/104.txt")
year_105data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/105.txt")
year_106data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/106.txt")
year_107data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/107.txt")
year_108data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/108.txt")
year_109data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/109.txt")
year_110data<-readLines("C:/Users/user/Desktop/R/txt檔/社會期刊/110.txt")

sobj.doc<-c(year_99data,year_100data,year_101data,year_102data
            ,year_103data,year_104data,year_105data,year_106data
            ,year_107data,year_108data,year_109data,year_110data)
sobj.doc<-str_replace_all(sobj.doc, "[\r\n]" , "")

content<-list()
for (i in 1:12){
  content[[i]]=cc[sobj.doc[[i]]]
}




#===========================
Mykeywordmatrix<-function(content,keyword){
  result <- matrix(0,length(keyword),1)
  for (k in 1:length(content)){
    keymatrix <- length(which(content[[k]]==keyword[1]))
    for (i in 2:length(keyword)){
      keymatrix <- rbind(keymatrix,length(which(content[[k]]==keyword[i])))
    }
    result <- cbind(result,keymatrix)
  }
  row.names(result)<-keyword
  colnames(result)<-c(98:110)
  return(result[,2:13])
}
###############################age####
issue1<-Mykeywordmatrix(content,c("高齡化社會","高齡友善","老人居住安排","無障礙空間",
                                  "老人共食","送餐服務","老人送餐","以屋換屋","以房養老",
                                  "全齡宅","人口老化","老人教育","樂齡學習"))
issue2<-Mykeywordmatrix(content,c("長照","長期照顧","長期照護","長期照護機構",
                                  "長期照顧機構","長照安養機構","安養中心",
                                  "長照保險","長期照顧保險","健康產業","外籍看護",
                                  "全民健保","二代健保","健保","長照離職","照護喘息",
                                  "喘息服務","家庭托顧"))
issue3<-Mykeywordmatrix(content,c("少子化","少子女化","生育率","育兒負擔","兒童養育",
                                  "幼兒教育","兒童照顧","兒童健康","育兒成本",
                                  "兒童權利公約","兒童發展","托育","幼托","育嬰假",
                                  "產假","兒童津貼","育兒津貼","公共托育","公共托嬰",
                                  "公共保母","產檢"))
issue4<-Mykeywordmatrix(content,c("晚婚","不婚","婚姻滿意度","婚姻市場","婚姻平權",
                                  "婚姻衝突","家庭","婦女健康","婚姻暴力",
                                  "平等就業","擇偶","聯誼","結婚補助","結婚津貼",
                                  "婚假"))
issue5<-Mykeywordmatrix(content,c("移工","外勞","外籍勞工","東南亞移工","產業移工",
                                  "社福移工","失聯移工","行蹤不明移工","非法工作",
                                  "非法滯留","自由轉換雇主","仲介費","人力仲介",
                                  "跨國勞動力","勞動力短缺"))
issue6<-Mykeywordmatrix(content,c("移民","新移民","新住民","外籍配偶","婚姻移民",
                                  "跨國婚姻","國籍歸化","入籍","長久居留",
                                  "永久居留","就業金卡","外國特定專業人才",
                                  "港澳特定專業人才","經濟移民","投資移民",
                                  "技術移民","移民政策","白領移民","移民法規"))
issue7<-Mykeywordmatrix(content,c("中高齡勞工","中高齡者就業","中高齡勞動參與",
                                  "高齡工作者","二度就業","老年經濟安全",
                                  "經濟安全保障","退休","退休金","勞工退休金",
                                  "勞退"))
issue8<-Mykeywordmatrix(content,c("城鄉教育","教育補助","教育經費","教育機會均等",
                                  "教育資源","偏鄉教育","原鄉教育","教育優先區",
                                  "廢校","學習成就","國民教育","十二年國教","義務教育"))
issue9<-Mykeywordmatrix(content,c("青年就業","青年低薪","青年失業","青年住宅",
                                  "高學歷高失業","學歷貶值","學歷通膨","學非所用",
                                  "學用落差","學用配合度","升學就業技能","高等教育",
                                  "充分就業"))
issue10<-Mykeywordmatrix(content,c("住宅負擔","居住正義","世代正義","住宅宣言","都市更新",
                                   "都更","房價","住宅費用","租屋","房租","租金","屋主",
                                   "住宅擁有","住宅自有","住宅市場","住宅交易","空屋率",
                                   "住宅階級","住宅投資","公共住宅","混合住宅","社會住宅",
                                   "社宅","居住安排","危老建築","老舊建築物","租金補貼",
                                   "包租代管","房價抑制","打房","房市炒作","實價登錄",
                                   "土地稅","不動產交易稅"))
issue11<-Mykeywordmatrix(content,c("產業分布","就業機會","城鄉差距","交通擁塞","停車位",
                                   "都市綠地","電線纜地下化","都市化","城鄉人口失衡",
                                   "鄉村人口流失","農村人口流失","農村人口老化","農村勞動力",
                                   "人口過度集中","人口密度","垃圾處理","廢棄物處理","焚化爐",
                                   "汽機車廢氣","熱島效應","汙水","汙水處理","污水下水道"))
issue12<-Mykeywordmatrix(content,c("貧富差距","所得分配不均","住宅負擔","住宅負擔力",
                                   "中產階級","中產階級消失","階級世襲","階級僵固",
                                   "可支配所得","負儲蓄","低收入","中低收入","富人稅",
                                   "M型化社會","社會流動僵固"))
issue13<-Mykeywordmatrix(content,c("低薪","薪資凍漲","薪資成長緩慢","薪資停滯","薪資縮水",
                                   "財產縮水","最低薪資","最低基本工資","轉職率","失業率",
                                   "經常性薪資","實質薪資","勞動市場","勞資關係"))
issue14<-Mykeywordmatrix(content,c("人才流失","流失人才","人才留任","人才短缺",
                                   "人力資源外流","勞動力外流","人才流動","跨國人才",
                                   "人口外移","留學","海外工作","台商外移","台商西進",
                                   "挖角"))
issue15<-Mykeywordmatrix(content,c("世代分配不均","世代正義","世代公平","世代不公平",
                                   "財富世襲","世代戰爭","機會不均","分配不公","Z世代",
                                   "相對剝奪感","債留子孫","社會正義","世代包容","福利國家"))
issue16<-Mykeywordmatrix(content,c("產業機器人","智慧機器人","產業創新","產業轉型",
                                   "數位轉型","中小企業","自動化","自動化設備","產業自動化",
                                   "AI","人工智慧","智慧化","共享經濟","數位經濟","數位化",
                                   "行動支付","物聯網","雲端","雲端運算"))
issue17<-Mykeywordmatrix(content,c("電子垃圾","電子廢棄物","貴金屬回收","電池回收","金屬粉塵",
                                   "重金屬汙染","有毒化學汙染"))
issue18<-Mykeywordmatrix(content,c("資料外洩","資料竊取","資訊安全","通訊安全","資訊通訊安全",
                                   "資通安全","個資保護","個資外洩","網路駭客","網路攻擊",
                                   "惡意軟體","木馬程式","電腦病毒","釣魚網站","釣魚攻擊"))
issue19<-Mykeywordmatrix(content,c("網際網路治理","跨國電信犯罪","網路犯罪","資訊犯罪",
                                   "電腦犯罪","網路詐欺","網路詐騙","解除分期付款","網路購物",
                                   "網路拍賣","網路交易犯罪","妨礙電腦使用"))
issue20<-Mykeywordmatrix(content,c("假消息","假訊息","假新聞","網路謠言","內容農場","查證",
                                   "訊息查證","查證平台","事實查核","數位思辨","媒體識讀",
                                   "網路言論自由","網軍","闢謠專區"))
issue21<-Mykeywordmatrix(content,c("人力派遣","外送員","Uber","零工","零工經濟","非正規就業",
                                   "非典型就業","風險承擔","就業保險","一般就業技能","人才培訓",
                                   "職涯發展","勞動力發展","職業教育","職業訓練","就業力",
                                   "終身學習","數位學習"))
issue22<-Mykeywordmatrix(content,c("漂綠","氣候尋租","碳排放途徑","碳移除途徑","碳預算",
                                   "碳邊境調整機制","碳關稅","碳捕捉","碳封存","綠色拆遷",
                                   "綠色循環生產","碳金融化","碳定價","能源稅","綠色消費",
                                   "公民電廠","躉購費率","綠色衝突","循環經濟","普惠金融",
                                   "綠色金融","永續金融","環境永續","淨零排放","碳排","碳稅",
                                   "碳交易","節能減碳","減碳","工業用電效率"))
issue23<-Mykeywordmatrix(content,c("風險溝通","氣候災害","天然災害","強降雨","水災","水患",
                                   "暴雨","乾旱","旱災","缺水","水資源","水資源安全",
                                   "水資源再利用","灌溉用水","工業用水","家庭用水","需水量",
                                   "水庫","地下水","超抽地下水"))
issue24<-Mykeywordmatrix(content,c("海水倒灌","海平面上升","溫室氣體","甲烷","地球暖化",
                                   "沿海居民","融冰","海冰融化","氣候難民","鹽害","地層下陷",
                                   "低窪"))
issue25<-Mykeywordmatrix(content,c("氣候變遷減緩知識","氣候正效益","淨負排放","碳匯","碳權",
                                   "碳抵銷","土地利用","國土規劃","氣候危機","氣候變遷","聖嬰",
                                   "反聖嬰","災害風險","平時減災","災害管理","災害應變",
                                   "救災應變","災後復原","環境治理","綠建築","極端氣候",
                                   "氣候異常","熱浪"))
issue26<-Mykeywordmatrix(content,c("空氣汙染","水汙染","地下水汙染","地表水系汙染","優養化",
                                   "海洋汙染","海水酸化","海洋暖化","紅潮","廢棄物","垃圾",
                                   "土壤鹽鹼化","沙漠化","資源回收","越境汙染","棲地保育",
                                   "野生動植物保育","生物多樣性","物種滅絕","環境保護","環保"))
issue27<-Mykeywordmatrix(content,c("能源危機","能源安全","能源貧窮","能源價格","能源進口",
                                   "替代能源","再生能源","綠能","低碳能源","太陽能","太陽光電",
                                   "風能","風力發電","水力發電","氫能","地熱能","地熱發電",
                                   "核能","核安","核能安全","石油","燃煤","天然氣","可燃冰",
                                   "生質能"))
issue28<-Mykeywordmatrix(content,c("乾淨用水","糧食危機","糧食安全","糧食進口","糧食自給率",
                                   "飼料自給率","糧食短缺","漁業資源","魚場枯竭","過度捕撈",
                                   "生產過剩","歉收","青農","青年農民","農作物","農村生態保育",
                                   "可耕地","休耕","農業永續經營","食農教育","食物浪費",
                                   "糧食浪費","惜食"))
#輸出excel====================================
issue.doc <- list(issue1,issue2,issue3,issue4,issue5,issue6,issue7,issue8,
                  issue9,issue10,issue11,issue12,issue13,issue14,issue15,issue16,
                  issue17,issue18,issue19,issue20,issue21,issue22,issue23,issue24,
                  issue25,issue26,issue27,issue28)
dir<-paste("./excel檔/學術期刊/",1:28,".csv",sep="")
for (i in 1:28){
  matrix <- as.data.frame(issue.doc[[i]])
  write.csv(matrix,file=dir[i],row.names = TRUE,fileEncoding = "Big5")
}

#輸出圖片====================================
plotfunc<-function(Finallmatrix){
  Finallmatrix.size<-Finallmatrix
  plot.levels<-factor(row.names(Finallmatrix),levels = rev(row.names(Finallmatrix)))
  cplot.levels<-factor(row.names(Finallmatrix),levels = row.names(Finallmatrix))
  for(i in 1:nrow(Finallmatrix)){
    for(j in 1:ncol(Finallmatrix)){
      if (Finallmatrix[i,j]>0){Finallmatrix[i,j] <- j}
    }
  }
  Finallmatrix<-data.frame(Finallmatrix)
  pro.x<-ggplot(Finallmatrix,aes(
    y =Finallmatrix, 
    x =colnames(Finallmatrix)))+geom_point(Finallmatrix,mapping=aes(y = plot.levels,x=Finallmatrix[,1],size=Finallmatrix.size[,1],color=cplot.levels))+
    scale_x_continuous(limits = c(1,12),breaks=c(1:12),labels = c("99","100","101","102","103","104","105","106","107","108","109","110"))+
    xlab("時間")+ylab("關鍵字")+labs(size="次數大小",color="")+
    theme(legend.position="right",legend.key.height= unit(0.5, 'cm'),legend.key.width= unit(0.5, 'cm'))
  pro.x2<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,2],size=Finallmatrix.size[,2],color=rownames(Finallmatrix)))
  pro.x3<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,3],size=Finallmatrix.size[,3],color=rownames(Finallmatrix)))
  pro.x4<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,4],size=Finallmatrix.size[,4],color=rownames(Finallmatrix)))
  pro.x5<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,5],size=Finallmatrix.size[,5],color=rownames(Finallmatrix)))
  pro.x6<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,6],size=Finallmatrix.size[,6],color=rownames(Finallmatrix)))
  pro.x7<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,7],size=Finallmatrix.size[,7],color=rownames(Finallmatrix)))
  pro.x8<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,8],size=Finallmatrix.size[,8],color=rownames(Finallmatrix)))
  pro.x9<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,9],size=Finallmatrix.size[,9],color=rownames(Finallmatrix)))
  pro.x10<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,10],size=Finallmatrix.size[,10],color=rownames(Finallmatrix)))
  pro.x11<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,11],size=Finallmatrix.size[,11],color=rownames(Finallmatrix)))
  pro.x12<-geom_point(Finallmatrix,mapping=aes(y = rownames(Finallmatrix),x=Finallmatrix[,12],size=Finallmatrix.size[,12],color=rownames(Finallmatrix)))
  
  pro.age<-pro.x+pro.x2+pro.x3+pro.x4+pro.x5+pro.x6+pro.x7+pro.x8+pro.x9+pro.x10+pro.x11+pro.x12
  return(pro.age)
}

dir<-paste("./圖片檔/學術期刊/",1:28,".png",sep="")
for (i in 1:28){
  test <- plotfunc(issue.doc[[i]])
  png(dir[i],width = 850,height = 600,res = 90,bg = "transparent")
  print(test)
  dev.off()
}

#重大領域範疇======================
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

issuematrix <- bindmatrix(issue.doc)

row.names(issuematrix) <- c("高齡化社會","長期照護","少子化衝擊","晚婚(不婚)化",
                            "外籍移工","白領移民開放","中高齡就業","教育分配不均",
                            "青年高失業率","高房價","城鄉資源失衡","貧富差距擴大",
                            "薪資成長緩慢","高級人力流失","世代資源不均",
                            "數位轉型與自動化發展","電子廢棄物","資(通)訊安全",
                            "網路詐騙","假消息","零工經濟/非典型就業",
                            "淨零排放的產業轉型壓力","極端氣候對水資源","海平面上升"
                            ,"極端氣候對環境影響","環境污染","能源轉型","糧食危機")
colnames(issuematrix) <- c(99:110)
write.csv(issuematrix,file="C:/Users/JOEZ/OneDrive/桌面/R/國發會/excel檔/學術期刊/issue.csv",row.names = TRUE,fileEncoding = "Big5")



x1 <- sum(issuematrix[,1][1:7])
x2 <- sum(issuematrix[,1][8:15])
x3 <- sum(issuematrix[,1][16:21])
x4 <- sum(issuematrix[,1][22:28])
for (i in 2:12){
  x1 <- cbind(x1,sum(issuematrix[,i][1:7]))
  x2 <- cbind(x2,sum(issuematrix[,i][8:15]))
  x3 <- cbind(x3,sum(issuematrix[,i][16:21]))
  x4 <- cbind(x4,sum(issuematrix[,i][22:28]))
}
fieldmatrix <- rbind(x1,x2,x3,x4)
row.names(fieldmatrix) <- c("人口結構與生養需求","經濟就業與居住資源",
                            "數位轉型與科技影響","氣候變遷與環境挑戰")
colnames(fieldmatrix) <- c(99:110)
write.csv(fieldmatrix,file="C:/Users/JOEZ/OneDrive/桌面/R/國發會/excel檔/學術期刊/field.csv",row.names = TRUE,fileEncoding = "Big5")

issuematrix.plot <- plotfunc(issuematrix)
fieldmatrix.plot <- plotfunc(fieldmatrix)
png("./圖片檔/學術期刊/issue.png",width = 850,height = 600,res = 90,bg = "transparent")
print(issuematrix.plot)
dev.off()

png("./圖片檔/學術期刊/field.png",width = 850,height = 600,res = 90,bg = "transparent")
print(fieldmatrix.plot)
dev.off()

#=============================standardize

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
stadar.age<-standardize(soc.age)

