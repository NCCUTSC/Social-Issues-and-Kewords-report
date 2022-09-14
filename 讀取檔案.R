library(pdftools)
library(stringr)
setwd("C:/Users/JOEZ/OneDrive/桌面/R/國發會")

file_name<-list.files("./pdf黨/施政計畫部會資料")
dir<-paste("./pdf黨/施政計畫部會資料/",file_name,sep="")
n<-length(dir)


b<-list.files(dir[7])
b[1339]
for(j in 1:length(b)){
  file<-paste(dir[7],"/",b[j],sep="")
  data <-pdf_text(file)
  print(file)
  print(j)
  for (i in 1:length(data))
#    new_data[j]<-str_c(new_data[j],pdf_text(file)[i])
    cat(data[i], file="./txt黨第二次/政府施政計畫/110.txt", append=TRUE)
}


data100 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/100.txt",encoding = "utf-8")
data101 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/101.txt",encoding = "utf-8")
data102 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/102.txt",encoding = "utf-8")
data103 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/103.txt",encoding = "utf-8")
data104 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/104去空白.txt",encoding = "utf-8")
data105 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/105去空白.txt",encoding = "utf-8")
data106 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/106去空白.txt",encoding = "utf-8")
data107 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/107去空白.txt",encoding = "utf-8")
data108 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/108去空白.txt",encoding = "utf-8")
data109 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/109去空白.txt",encoding = "utf-8")
data110 <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt黨第二次/政府施政計畫/110去空白.txt",encoding = "utf-8")

length(data110)
nchar(data100[424902])
substr(x = data100[934],start = 0,stop = 73)

test<-pdf_text("C:/Users/JOEZ/OneDrive/桌面/R/國發會/pdf黨/110/臺灣期貨雙月刊202012_all.pdf")



test <- readLines("C:/Users/JOEZ/OneDrive/桌面/R/國發會/txt檔/100 - 複製 (2).txt",encoding = "utf-8")
test[554737]


