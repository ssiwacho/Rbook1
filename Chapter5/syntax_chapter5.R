# 5.1 มโนทัศน์พื้นฐานของทัศนภาพข้อมูล
### สี
library(RColorBrewer)
display.brewer.all(type="qual")
brewer.pal(n=8,name="Set3")
display.brewer.all(type="seq")

# 5.2 ฟังก์ชันกราฟิกพื้นฐาน
## link ดาวน์โหลดข้อมูล onlearning.csv : https://bit.ly/3ofgx3t
par(family="ChulaCharasNew")
hist(dat$outcome.online4)

head(colors(),20)
tail(colors(),20)

par(mfrow=c(2,2))
hist(dat$outcome.online4, col=rgb(0.1,0.5,0.3), main="")
hist(dat$outcome.onsite4, col=rgb(0.8,0.5,0.3), main="")
hist(dat$outcome.online4, col=rgb(250,128,114, maxColorValue=255), main="")
hist(dat$outcome.onsite4, col=rgb(112,128,144, maxColorValue=255), main="")

# 5.3 การสํารวจการแจกแจงของตัวแปร

dat<-read.csv("onlinelearning.csv", stringsAsFactor = TRUE)
glimpse(dat)

table(dat$ระดับชั้น)

dat$ระดับชั้น<-factor(dat$ระดับชั้น, levels=c(1,2,3,4,5), labels=c("ม.ปลาย","ปี 1", "ปี 2", "ปี 3", "ปี 4"))
table(dat$ระดับชั้น)
table(dat$ระดับชั้น)*100/nrow(dat)

class.interval<-cut(dat$outcome.online4, breaks=c(0,25,50,75,100))
head(class.interval)
tail(class.interval)
table(class.interval)

dat[,32]<-factor(dat[,32], labels=c("ไม่คล่องแคล่ว","คล่องแคล่วปานกลาง", "คล่องแคล่วมาก"))
table(dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี)
table(dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี, dat$ระดับชั้น)
table(dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี, dat$ระดับชั้น, dat$เพศผู้เรียน)

tab<-table(dat$ระดับชั้น)
prop.table(tab)*100

tab<-table(dat$ความสามารถผเู้ รียนในการใชเ้ ทคโนโลยี, dat$ระดับชั้น)
prop.table(tab, margin=1)*100 # row percentage
prop.table(tab, margin=2)*100 # column percentage
prop.table(tab)*100 # total percentage

install.packages("DescTools")
library(DescTools)
PercTable(dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี, dat$ระดับชั้น), rfrq="101")

par(family="ChulaCharasNew") #กำหนดรูปแบบตัวอักษรในแผนภาพ
barplot(table(dat$ระดับชั้น))

par(family="ChulaCharasNew", mfrow=c(2,2)) 
tab.LearnStyle<-table(dat$สไตล์การเรียนที่ชอบ)
barplot(tab.LearnStyle, names.arg=c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"))
barplot(tab.LearnStyle,names.arg=c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), col="cornflowerblue", horiz=TRUE)
barplot(tab.LearnStyle, names.arg=c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), col=c("#003f5c","#bc5090","#ffa600"), cex.names=2)
barplot(tab.LearnStyle, names.arg=c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), col=brewer.pal(n=3,name="Set3"), cex.names=2,cex.axis=2)


tab.LearnStyleBylevel<-table(dat$สไตล์การเรียนที่ชอบ, dat$ระดับชั้น)
par(family="ChulaCharasNew", mfrow=c(1,2))
barplot(tab.LearnStyleBylevel, legend = c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), beside=TRUE)
barplot(tab.LearnStyleBylevel, legend = c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), beside=TRUE, 
        col=c("#679b9b","#ffeadb","#ff9a76"), cex.names=1.5, cex.axis=1.5)

par(mfrow=c(1,2), family="ChulaCharasNew")
hist(dat$outcome.online4)
hist(dat$outcome.onsite4)

par(mfrow=c(1,3))
break3<-seq(from=0,to=100, length=6)
break5<-seq(from=0,to=100, length=11)
break7<-seq(from=0,to=100, length=31)
hist(dat$outcome.online4, breaks=break3)
hist(dat$outcome.online4, breaks=break5)
hist(dat$outcome.online4, breaks=break7)

par(family="ChulaCharasNew", mfrow=c(2,2))
hist(dat$outcome.onsite4,breaks=30, main="ผลสัมฤทธิ์โดยรวมของนักเรียน (onsite)") 
hist(dat$outcome.onsite4, main="",xlab="ผลสัมฤทธิ์โดยรวมของนักเรียน (onsite)", ylab="ความถี่",cex.lab=1.5, col="maroon")
hist(dat$outcome.onsite4, breaks=seq(from=0,to=100,by=10), main="", xlab="ผลสัมฤทธิ์โดยรวมของนักเรียน (onsite)", 
    ylab="ความถี่",col="peachpuff")
hist(dat$outcome.onsite4,breaks=30, main="", cex.lab=1.5, cex.axis=1.5, col="#625261")


library(scales)
par(family="ChulaCharasNew")
hist(dat$outcome.online4, xlab="ผลสัมฤทธิ์โดยรวมของนักเรียน (onsite)", 
    ylab="ความถี่", cex.lab=1, col=alpha("mediumslateblue",0.6), freq=FALSE, xlim=c(0,100), ylim=c(0,0.05), main="")
lines(density(dat$outcome.online4),col="tomato", lwd=2)
rug(dat$outcome.online4)


par(family="ChulaCharasNew") 
hist(dat$outcome.online4[dat$ความคิดเห็นต่อการเรียน==1], main="", 
    xlab="ผลสัมฤทธิ์โดยรวมของผู้เรียน (online)", ylab="ความถี่", col=alpha("#64958f",0.7), ylim=c(0,70),xlim=c(0,100))
hist(dat$outcome.online4[dat$ความคิดเห็นต่อการเรียน==2], main="", 
    xlab="ผลสัมฤทธิ์โดยรวมของผเู้ รียน (online)", ylab="ความถี่", col=alpha("#ffbb91",0.5), add=TRUE)
legend(70,50,legend=c("ชอบ online มากกว่า","ชอบ onsite มากกว่า"), fill=c(alpha("#64958f",0.7),alpha("#ffbb91",0.5)))

par(family="ChulaCharasNew") 
pie(table(dat$ภูมิลําเนา), labels=c("ภาคกลาง","ภาคเหนือ","อิสาน","ใต้"), main="ภูมิลําเนาของผู้เรียน")


par(family="ChulaCharasNew", mfrow=c(2,2)) 
pie(table(dat$ภูมิลําเนา), labels=c("ภาคกลาง","ภาคเหนือ","อิสาน","ใต้"), radius=1, clockwise=TRUE, cex=1) 
pie(table(dat$ภูมิลําเนา), labels=c("ภาคกลาง","ภาคเหนือ","อิสาน","ใต้"), radius=0.5, clockwise=TRUE, cex=2) 
pie(table(dat$ภูมิลําเนา), labels=c("ภาคกลาง","ภาคเหนือ","อิสาน","ใต้"), radius=-1, clockwise=TRUE, cex=0.8) 
pie(table(dat$ภูมิลําเนา), labels=c("ภาคกลาง","ภาคเหนือ","อิสาน","ใต้"), radius=1, clockwise=FALSE, cex=3,
    col=c("#ff9a76","#ffeadb","#679b9b","#637373"))

tab.LearnStyleBylevel<-table(dat$สไตล์การเรียนที่ชอบ, dat$ระดับชั้น) 
barplot(tab.LearnStyleBylevel, names.arg = c("ม.ปลาย","ปี 1", "ปี 2", "ปี 3", "ปี 4"), beside=FALSE)

barplot(tab.LearnStyleBylevel, legend = c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), beside=FALSE)
barplot(tab.LearnStyleBylevel, legend = c("ฟังบรรยาย","ลงมือปฏิบัติ","ทั้งสอง"), 
        col=c("#132743","#d7385e","#edc988"), cex.names=1.5, cex.axis=1.5, beside=FALSE)

# ดาวน์โหลดชุดข้อมูล statscore.txt : https://goo.gl/GXNyvv
score<-read.table("statscore.txt", sep="", header=T)
glimpse(score)


par(family="ChulaCharasNew") 
class<-factor(score$เพศผเู้ รียน, labels=c("ชาย","หญิง"))
gender<-factor(dat$สไตล์การเรียนที่ชอบ, labels=c("ฟงั บรรยาย","ลงมือปฏิบัติ","ทั้งสอง")) 
plot(class,gender, xlab="ชั้นเรียนของนักเรียน", ylab="เพศ", col=c("#cbaf87","#7e8a97"))

# 5.4 การสำรวจสภาพตัวแปรด้วยสถิติพื้นฐาน (basic statistics)
mean(dat$outcome.online4)
sd(dat$outcome.online4)
quantile(dat$outcome.online4, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))

library(e1071)
skewness(dat$outcome.online4)
kurtosis(dat$outcome.online4)

apply(dat[,4:6],MARGIN=2, FUN=mean)
apply(dat[,4:6],MARGIN=2, FUN=sd)
apply(dat[,4:6],MARGIN=2, FUN=mean, trim=0.1, na.rm=TRUE)

climate.online<-apply(dat[,17:19],MARGIN=1, FUN=mean)
head(climate.online)
tail(climate.online)

summary(dat[,1:8])

# link ดาวน์โหลดชุดข้อมูล cleaning.csv : https://goo.gl/26XvoH
clean.dat<-read.csv("cleaning.csv", header=T)
summary(clean.dat)


install.packages("psych")
library(psych)
head(describe(dat))
describeBy(dat[,4:6],group=dat$ที่ตั้ง)
describeBy(dat[,4:6],group=dat$ที่ตั้ง)

install.packages("pastecs")
library(pastecs)
format(round(stat.desc(dat[,4:6]),3), scientific=FALSE)

par(family="ChulaCharasNew", mfrow=c(1,2))
boxplot(dat$outcome.online4, ylab="ผลสัมฤทธิ์โดยรวมของผู้เรียนในรายวิชา online", col="#28abb9", cex.axis=1.5)
boxplot(dat$outcome.online4, range=0.5, ylab="ผลสัมฤทธิ์โดยรวมของผู้เรียนในรายวิชา online", col="#f39233", 
        horizontal=TRUE, cex.axis=1.5)

par(family="ChulaCharasNew") 
boxplot(dat$outcome.online4~dat$ความคิดเห็นต่อการเรียน, 
        names=c("ชอบเรียน online มากกว่า" ,"ชอบเรียน onsite มากกว่า", "ชอบทั้งสองเท่า ๆ กัน"), 
        col=c("#065c6f","#64958f","#ffbb91"),
        xlab = "", ylab = "ผลสัมฤทธิ์โดยรวมของผู้เรียน (online)",
        cex.axis = 1.5, cex.lab = 1.5, varwidth = FALSE)

# 5.5 การสํารวจความสัมพันธ์ระหว่างตัวแปร
## link ดาวน์โหลดชุดข้อมูล statscore.txt : https://goo.gl/GXNyvv 
par(family="ChulaCharasNew")
plot(x=score$math,y=score$score.final, xlab="พื้นฐานความรู้ทางคณิตศาสตร์", ylab="คะแนนสอบปลายภาควิชาสถิติ")

plot(x=score$math,y=score$score.final, xlab="พื้นฐานความรู้ทางคณิตศาสตร์" , ylab="คะแนนสอบปลายภาควิชาสถิติ", 
    pch=16, col=alpha("darkblue",alpha=0.7), cex=2, family="ChulaCharasNew")

score.week<-score[,5:8]
cor(score.week)
cor(score$score.final, score$math)

pairs(score.week, pch=16, col=alpha("darkblue",0.6), cex=2, cex.labels = 3)


score$gender<-factor(score$gender)
par(family="ChulaCharasNew")
lower<-function(x,y)
{
 r<-round(cor(x,y),digits=2)
 par(usr = c(0, 1, 0, 1))
 text(0.5,0.5, sprintf("%3.2f",r), cex=r*10)
}
mycol<- c("#16697a","#db6400")
upper<-function(x,y)
{
 points(x,y,pch=16, col=alpha(mycol[score$gender],0.6), cex=3)
}
pairs(score.week, cex.labels = 4, lower.panel = lower, upper.panel = upper)

library(scales)
par(family="ChulaCharasNew", mar=c(5,5,1,1))
plot(dat$interaction.online1, dat$outcome.online4,
    xlab="ระดับความจดจ่อกับเนื้อหาและกิจกรรมในชั้นเรียน", ylab="ผลสัมฤทธิ์โดยรวมของนักเรียนในรายวิชา online", 
    pch=16, col=alpha("black",0.6), cex.lab=1.5)


par(family="ChulaCharasNew", mar=c(5,5,1,1))
plot(jitter(dat$interaction.online1), dat$outcome.online4, 
    xlab="ระดับความจดจ่อกับเนื้อหาและกิจกรรมในชั้นเรียน", ylab="ผลสัมฤทธิ์โดยรวมของนักเรียนในรายวิชา online", 
    pch=16, col=alpha("black",0.6), cex.lab=1.5)

par(family="ChulaCharasNew", mar=c(5,5,1,1))
interaction<-factor(dat$interaction.online1, labels=c("น้อยมาก","น้อย","ปานกลาง","มาก","มากที่สุด")) 
boxplot(dat$outcome.online4~interaction, 
        xlab="ระดับความจดจ่อกับเนื้อหาและกิจกรรมในชั้นเรียน",  ylab="ผลสัมฤทธิ์โดยรวมของนักเรียนในรายวิชา online", cex.lab=1.5)

points(jitter(dat$interaction.online1), dat$outcome.online4,
        pch=16, col=alpha("black",0.6))


plot(jitter(dat$interaction.online1), jitter(dat$outcome.online1),
    xlab="ความจดจ่อกับเนื้อหาและกิจกรรมในชั้นเรียน", ylab="ความเข้าใจในเนื้อหาของการเรียนแต่ละครั้ง", 
    col=alpha("black",0.6), pch=16, cex.lab=1.5, xaxt=“n", yaxt=“n")
axis(1, at=c(1,2,3,4,5), labels=c("น้อยมาก","น้อย","ปานกลาง","มาก","มากที่สุด")) 
axis(2, at=c(1,2,3,4,5), labels=c("น้อยมาก","น้อย","ปานกลาง","มาก","มากที่สุด"))

interaction<-factor(dat$interaction.online1, labels=c("น้อยมาก","น้อย","ปานกลาง","มาก","มากที่สุด"))

outcome1<-factor(dat$outcome.online1,
          levels=c(5,4,3,2,1), labels=rev(c("น้อยมาก","น้อย","ปานกลาง","มาก","มากที่สุด")))
plot(interaction, outcome1, xlab="ความจดจ่อกับเนื้อหาและกิจกรรมในชั้นเรียน", ylab="ความเข้าใจในเนื้อหาของการเรียนแต่ละครั้ง", 
    pch=16, cex.lab=2, col=c("#A57548","#FCD7AD","#F6C28B","#5296A5","#82DDF0"))


cor(dat$outcome.online4, dat$interaction.online1,
method="spearman")
cor(dat$outcome.online1,dat$interaction.online1,
method="spearman")

x1<-jitter(dat[,17])
x2<-jitter(dat[,18])
x3<-jitter(dat[,19])
climate<-data.frame(x1,x2,x3)
cor(climate, method="spearman")


par(family = "ChulaCharasNew")
lower<-function(x,y)
{
 r<-round(cor(x,y,method="spearman"),digits=2)
 par(usr = c(0, 1, 0, 1))
 text(0.5,0.5, sprintf("%3.2f",r), cex=r*10)
}
mycol<- c("#16697a","#db6400")
upper<-function(x,y)
{
points(x,y,pch=16, col=mycol[dat$ระดับชั้น], cex=1.5)
}

pairs(climate, 
    labels=c("การเปิดโอกาสให้ผู้เรียนแสดงความคิดเห็น",
                 "ความสนใจและการให้ความสำคัญต่อผู้เรียน", 
                 "การให้คำแนะนำ/ช่วยเหลือผู้เรียน"), 
    lower.panel=lower, upper.panel = upper)

library(psych)
t<-c(14,10,6,13)
phi(t)

mat<-matrix(t,nrow=2, ncol=2, byrow = T)
mat
phi(mat, digits = 4)


dat[,32]<-factor(dat[,32], labels=c("ไม่คล่องแคล่ว", "คล่องแคล่วปานกลาง", "คล่องแคล่ว"))
table(dat$ที่ตั้งสถานศึกษา, dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี)
CramerV(dat$ที่ตั้งสถานศึกษา, dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี)
CramerV(dat$ที่ตั้งสถานศึกษา, dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี , correct=T)

chisq<-chisq.test(dat$ที่ตั้งสถานศึกษา, dat$ความสามารถผู้เรียนในการใช้เทคโนโลยี)
(chisq$observed-chisq$expected)^2/chisq$expected
chisq$residuals