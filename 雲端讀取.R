library("googledrive")

#C:\Users\JOEZ\AppData\Local\Temp\Rtmp6vV9wC
temp <- tempfile(fileext = ".zip")
#需要下載，請耐心等待
dl <- drive_download(as_id("16g87tYCzCvnswLYNHIOirfH_mB-QCPxK"), 
                     path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())

#學術期刊
data99 <- readLines(out[12],encoding = 'utf-8')
data100 <- readLines(out[1],encoding = 'utf-8')
data101 <- readLines(out[2],encoding = "utf-8")
data102 <- readLines(out[3],encoding = "utf-8")
data103 <- readLines(out[4],encoding = "utf-8")
data104 <- readLines(out[5],encoding = "utf-8")
data105 <- readLines(out[6],encoding = "utf-8")
data106 <- readLines(out[7],encoding = "utf-8")
data107 <- readLines(out[8],encoding = "utf-8")
data108 <- readLines(out[9],encoding = "utf-8")
data109 <- readLines(out[10],encoding = "utf-8")
data110 <- readLines(out[11],encoding = "utf-8")


#施政計畫
data104 <- readLines(out[13],encoding = "utf-8")
data105 <- readLines(out[14],encoding = "utf-8")
data106 <- readLines(out[15],encoding = "utf-8")
data107 <- readLines(out[16],encoding = "utf-8")
data108 <- readLines(out[17],encoding = "utf-8")
data109 <- readLines(out[18],encoding = "utf-8")
data110 <- readLines(out[19],encoding = "utf-8")


#政府出版品
data100 <- readLines(out[20],encoding = 'utf-8')
data101 <- readLines(out[21],encoding = "utf-8")
data102 <- readLines(out[22],encoding = "utf-8")
data103 <- readLines(out[23],encoding = "utf-8")
data104 <- readLines(out[24],encoding = "utf-8")
data105 <- readLines(out[25],encoding = "utf-8")
data106 <- readLines(out[26],encoding = "utf-8")
data107 <- readLines(out[27],encoding = "utf-8")
data108 <- readLines(out[28],encoding = "utf-8")
data109 <- readLines(out[29],encoding = "utf-8")
data110 <- readLines(out[30],encoding = "utf-8")
