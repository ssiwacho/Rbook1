# 4.1 การสำรวจโครงสร้างของชุดข้อมูล
## ดาวน์โหลดข้อมูล messydata.xlsx: https://bit.ly/2SIFmot
library(readxl)
dat<-read_excel("messydata.xlsx", na="-", col_names=TRUE)

### สำรวจภาพรวมของชุดข้อมูล
library(dplyr)
glimpse(dat)
head(dat)
tail(dat)

names(dat)
names(dat)[1]
names(dat)[2:4]
names(dat)[c(1,3,5)]
names(dat)[1]<-"student.name"
names(dat)[c(2,4)]<-c("pretest.LEC","posttest.LEC")

names(dat)[2:5]<-c("pretest.LEC","pretest.PBL","posttest.LEC","posttest.PBL")
head(dat)

# 4.2 การรวมชุดข้อมูลตามแถว (row combined)

## ดาวน์โหลดไฟล​์ข้อมูล 4 ไฟล์ : https://bit.ly/2AuQgqr

#### นำเข้าทีละไฟล์
dat1<-read_excel("file1.xlsx",na="-")
dat2<-read_excel("file2.xlsx",na="-")
dat3<-read_excel("file3.xlsx",na="-")
dat4<-read_excel("file4.xlsx",na="-")


#### นำเข้าทั้ง 4 ไฟล์ด้วยการวนลูป
file.name<-list.files(pattern = "*.xlsx")
file.name
list<-lapply(file.name,read_excel,na="-")
list

###### วนลูปด้วยฟังก์ชัน lappy()
dat.rbind<-rbind(list[[1]],list[[2]],list[[3]],list[[4]])
glimpse(dat.rbind)

###### วนลูปด้วยฟังก์ชัน for()
dat.for<-list[[1]]
for (i in 2:4)
 {
  dat.for<-rbind(dat.for,dat.for[[i]])
}

###### วนลูปด้วยฟังก์ชัน do.call()
rbind.docall<-do.call(what="rbind", args=list)
glimpse(rbind.docall)

#### นำเข้าทั้ง 4 ไฟล์ด้วย rbindlist()
install.packages("data.table")
library(data.table)
rbindlist.dat<-rbindlist(list)
glimpse(rbind.docall)


# 4.3 การรวมชุดข้อมูลตามคอลัมน์ (column combined)

# link ดาวน์โหลดไฟล์ preTest.csv และ postTest.csv : http://bit.ly/2sfzCqA
x<-read.csv("preTest.csv")
y<-read.csv("postTest.csv")
dat.cbind<-cbind(x, y[,-1])


dat.merge<-merge(x, y, by="student.name")
glimpse(dat.merge)


X<-data.frame(id=c(1,2,3),
              x1=c(10,20,30),
              x2=c(5,7,9))
Y<-data.frame(id=c(1,2,5),
              y1=c(20,40,60),
X
inner_join(X,Y,by="id")
full_join(X,Y,by="id")
left_join(X,Y,by="id")
right_join(X,Y,by="id")

# 4.4 การปรับเปล่ียนรูปแบบชุดข้อมูล
install.packages("tidyr")
library(tidyr)

evel<-c("ตํา่ กวา่ ปริญญาตรี", "ตํา่ กวา่ ปริญญาตรี","ปริญญาตรี", "ปริญญาตรี", "ประกาศนียบัตรบัณฑิต", "ประกาศนียบัตรบัณฑิต", "ปริญญาโท", "ปริญญาโท", "ประกาศนียบัตรขั้นสูง", "ประกาศนียบัตรขั้นสูง", "ปริญญาเอก", “ปริญญาเอก")
gender<-c("ชาย", "หญิง", "ชาย", "หญิง", "ชาย", "หญิง", "ชาย", "หญิง", "ชาย", "หญิง", "ชาย", “หญิง")
n<-c(198086, 151587, 572497,889112, 3410, 7133, 39849, 54215,
       680, 1081, 11375, 12027)
long.dat<-data.frame(level,gender,n)
long.dat

wide.dat<-spread(long.dat,key ="gender", value = "n")
wide.dat

long.dat2<-gather(wide.dat, ชาย, หญิง, key = "gender", value = "n.student")
long.dat2

# 4.5 การแยกคอลัมน์
# link ดาวน์โหลดข้อมูลในตาราง 4.5 : https://bit.ly/38T1ENa
iq<-read_excel("IQ.xlsx", col_name=TRUE)
iq.sep<-separate(iq,
                 col="gender.age",
                 into=c("gender","age"),
                 sep="/")
head(iq.sep)


id<-c(1,2,3)
time<-c("2018Jan","2018Feb","2018Mar")
year.month1<-data.frame(id,time)
year.month1

year.month2<-separate(year.month1, col="time",
year.month2

iq2<-read_excel("IQ2.xlsx", col_name=TRUE)
iq2

iq2.sep<-separate(iq2, col="gender.age", into=c("gender","age"), sep="(?<=[ก-ฮ])(?=[0-9])")
iq2.sep


# 4.6 การยุบรวมคอลัมน์
year<-c(rep(2018,6),2019,2019)
month<-c("July","Aug","Sep","Oct","Nov","Dec","Jan","Feb")
book<-c(89,283,159,245,312,74,252,321)
dat.separated<-data.frame(year,month,book)
dat.separated

dat.united<-unite(dat.separated,col=time,year,month,sep="-")
dat.united

# 4.7 การจัดการกับหน่วยข้อมูลที่ซํ้าซ้อน
library(dplyr)
prov.id<-c(1,1,1,1,2,2,2,3,3) 
province<-c(rep("กรุงเทพ",4),rep("เชียงใหม"่ ,3),rep("ขอนแกน่ ",2)) 
income<-c(rep(240804,4),rep(73729,3),rep(29978,2)) 
dat.dup<-data.frame(prov.id, province, income)
dat.dup

duplicated(dat.dup)

duplicate<-duplicated(dat.dup)
dat.uniq<-dat.dup[duplicate==FALSE,]
dat.uniq

dat.uniq2<-unique(dat.dup)
dat.uniq2

distinct(dat.dup)
distinct(dat.dup, province)
distinct(dat.dup, income)
distinct(dat.dup, income, .keep_all = TRUE)

# 4.8 การยุบรวมหน่วยข้อมูล
aggregate(multilevel.dat$rank, by=list(prov.id, province, income),FUN=mean)
dat.aggregated<-aggregate(multilevel.dat$rank ,by=list(prov.id, province, income), FUN=mean)
names(dat.aggregated)<-c("province","prov.id","income","mean.rank")
dat.aggregated

