# 3.1 การกำหนด working directory
setwd("C:\Users\Documents")
getwd()

# 3.2 การนําเข้าและส่งออกไฟล์ข้อมูลแบบ flat file
# link download ข้อมูล score.csv : https://goo.gl/mXZT5B 
dat.csv<-read.csv("score.csv",header=TRUE)
dat.csv

dat.csv<-read.csv("/Users/Desktop/score.csv",header=TRUE)
str(dat.csv)

dat.csv<-read.csv("score.csv",header=T, stringsAsFactors = TRUE)
str(dat)

write.csv(dat.csv, file="save_csv_data.csv")

# link download ข้อมูล score.txt : https://goo.gl/qqpUxf 
dat.tsv<-read.table("score.txt",header=T) 
dat.tsv


# link download ข้อมูล score2.txt : https://goo.gl/xTh6ZY
dat.slash<-read.table("score2.txt", header=T, sep = "/")
dat.slash
class(dat.slash)
write.table(dat.tsv, file="save_tsv_data.txt", sep=" ")

#3.3 การนำเข้าและส่งออกไฟล์ข้อมูล .xlsx
install.packages("readxl")
library(readxl)

# link download ข้อมูล score2.xlsx : http://bit.ly/2EjFOnS 
excel_sheets("Score.xlsx")

dat.xlsx1<-read_excel("Score.xlsx", worksheet="Score")
dat.xlsx1


dat.xlsx2<-read_excel("Score.xlsx", worksheet="Score3",col_names=FALSE)

# link download ข้อมูล KAS.xlsx : https://bit.ly/2Au0B61
dat.xlsx3<-read_excel("KAS.xlsx", skip=3, col_names=T)
dat.xlsx3

# 3.4 การนำเข้าข้อมูลจาก clipboard
### for Mac OS
dat.clipboard1<-read.table(file="clipboard", header=T, sep="/")
dat.clipboard1

### for windows
dat.clipboard2<-read.table(pipe("pbpaste"), header=T, sep="/")
dat.clipboard2


# 3.5 การนําเข้าไฟล์ข้อมูลนามสกุล .sav
## link download ข้อมูล AchSolve.sav : https://goo.gl/89GJ9A

# import .sav using foreign package
install.packages("foreign")
library(foreign)
dat.sav1<-read.spss("AchSolve.sav",
                    use.value.labels = T,
                    to.data.frame = T)
dat.sav1

# import .sav using haven package
install.packages("haven")
library(haven)
dat.sav2<-read_sav("AchSolve.sav")
dat.sav2

