install.packages("dplyr")
library(dplyr)

# link ดาวน์โหลดชุดข้อมูล cleaning.csv : https://goo.gl/26XvoH
# link ดาวน์โหลดชุดข้อมูล item.csv : https://bit.ly/2Vm9xEw
# link ดาวน์โหลดชุดข้อมูล onlinelearning_miss.csv : https://bit.ly/2JoKcqX

# 6.1 การคัดเลือกตัวแปรและการคัดกรองหน่วยข้อมูล

dat.online<-read.csv("onlinelearning_miss.csv")
dat.online<-as_tibble(dat.online) #convert data.frame to nibble
climate<-dat.online[,17:22]
str(climate)

dplyr::select(dat.online, outcome.online4)

dat.subset<-select(dat.online,  interaction.online1,
                                interaction.online2,
                                interaction.online3, 42:45)

paste("interaction.online",1:3, sep="")
subset<-select(dat.online,paste("interaction.online",1:3,sep=""),
                          paste("outcome.online",1:4,sep=""))
str(subset)

outcome<-select(dat.online, starts_with("outcome"))
str(outcome)

dat.online%>%
    select(contains("online") | contains("onsite"))%>%
        glimpse()

female<-dat.online[dat.online$เพศผู้เรียน=="2",]
table(female$เพศผู้เรียน)
table(dat.online$ระดับชั้น)
level.123<-dat.online[dat.online$ระดับชั้น%in%c(1,2,3),]
table(level.123$ระดับชั้น)

ach50<-dat.online[dat.online$outcome.online4>50, ]
summary(ach50$outcome.online4)

filter<-dat.online[dat.online$ระดับชั้น==1 & dat.online$บรรยากาศที่พักอาศัย==2 &
                    dat.online$ความเครียดขณะเรียนonline>=4 & 
                      dat.online$outcome.online4>30 &
                        dat.online$outcome.online4<60, ]
dim(filter)

dat.filtered%>%
    select(ระดับชั้น,
            บรรยากาศที่พักอาศัย, 
            ความเครียดขณะเรียนonline, 
            outcome.online4)%>%
            summary()

dat.online%>%
        filter(outcome.onsite4<=30 | outcome.onsite4>50)%>%
             select(outcome.onsite4)%>%summary()

x<-c(4,5,2,NA,9,3,1,3,NA)
is.na(x)
is.na(x)%>%table()

dat.online%>%dplyr::select(outcome.online4)%>%summary()
nomiss.ach<-dat.online%>%filter(is.na(outcome.online4)==FALSE)
nomiss.ach%>%dplyr::select(outcome.online4)%>%summary()

# 6.2 การแปลงข้อมูล
dat.online<-read.csv("onlinelearning.csv")
table(dat.online$สไตล์การเรียนที่ชอบ)

style<-factor(dat.online$สไตล์การเรียนที่ชอบ), levels=c(1,2,3), 
                labels=c("ชอบเรียนแบบ online","ชอบเรียนแบบ onsite", "ชอบเรียนทั้งสองแบบ"))
table(style)

dat.cleaning<-read.csv("cleaning.csv", header=T)
par(family="ChulaCharasNew")
t<-table(dat.cleaning$Gender)
barplot(t, col="#C97064", cex.axis=2)

dat.cleaning$Gender<-factor(dat.cleaning$Gender)
levels(dat.cleaning$Gender)

dat.cleaning$Gender<-factor(dat.cleaning$Gender,
                   labels=c("Male","Male","Male","Male",
table(dat.cleaning$Gender)

recode.vec<-c("1"="Male","Boy"="Male","M"="Male",
                   "boy"="Male","m"="Male","male"="Male",
                   "Male"="Male","0"="Female","F"="Female",
                   "Girl"="Female","female"="Female",
                   "Female"="Female")
Gender.recoded<-recode.vec[dat.cleaning$Gender]

t<-table(Gender.recoded)
barplot(t, col=c("#C97064","#68A357"), cex.axis=2,cex.names=2)

table(dat.cleaning$Location)

Location.recoded<-recode(dat.cleaning$Location, BK="BKK", CMI="CMI")
table(Location.recoded)

dat.cleaning$Gender<-Gender.recoded
dat.cleaning$Location<-Location.recoded
head(dat.cleaning)

dat.cleaning$Gender.re<-Gender.recoded
dat.cleaning$Location.re<-Location.recoded
head(dat.cleaning)

dat.online$ach10.online<-dat.online$outcome.online4/10
par(mfrow=c(1,2), family="ChulaCharasNew", mar=c(5,5,1,1))
hist(dat.online$outcome.online4, col="orange", xlab="ผลสัมฤทธิ์โดยรวมของผู้เรียนในรายวิชา online", cex.lab=1.5, main="")
hist(dat.online$ach10.online, col="skyblue", xlab="ผลสัมฤทธิ์โดยรวมของผู้เรียนในรายวิชา online", cex.lab=1.5, main="")

it.device<-dat.online$smartphone+dat.online$ipad+
           dat.online$computer
summary(it.device)

outcome.onsite<-(20*dat.online$outcome.onsite1+
                 20*dat.online$outcome.onsite2+
                 20*dat.online$outcome.onsite3+
                 dat.online$outcome.onsite4)/4
boxplot(outcome.onsite, col="#59886b", horizontal = T)

dat.online%>%mutate(outcome.onsite=(20*outcome.onsite1+
                 20*outcome.onsite2+20*outcome.onsite3+
                 outcome.onsite4)/4)%>%
                       select(outcome.onsite)%>%summary()


dat.item<-read.csv("/Users/siwachoat/Desktop/book/item.csv")
head(dat.item)

dat.item.re<-mutate(dat.item, X5.re=6-X5)
head(dat.item.re)


# calculate attitude score
dat.item.re<-dat.item.re%>%
                mutate(att1=(X1+X2+X3+X4+X5)/5,
                       att2=(X1+X2+X3+X4+X5.re)/5)
# draw boxplots
par(family="ChulaCharasNew", mfrow=c(1,2), mar=c(5,5,1,1))
boxplot(dat.item.re$att1, col="#fe8a71", ylim=c(1,5), ylab="เจตคติต่อการเรียนคณิตศาสตร์" , xlab="ไม่ได้กลับสเกล item5", cex.lab=1.5)
boxplot(dat.item.re$att2, col="#3da4ab", ylim=c(1,5),ylab="เจตคติต่อการเรียนคณิตศาสตร์", xlab="มีการกลับสเกล item5", cex.lab=1.5)

dat.item.re%>%select(att1, att2)%>%describe()

# 6.3 การสรุปข้อมูลด้วยค่าสถิติพื้นฐาน

summarise(dat.online, median.level=median(ระดับชั้น), 
        mean.online.subject=mean(จํานวนรายวิชาonline),
        sd.online.subject=sd(จํานวนรายวิชาonline), 
        mean.ach.online=mean(outcome.online4), 
        sd.ach.online=sd(outcome.online4))

dat.group<-group_by(dat.online, ที่ตั้งสถานศึกษา) 
summarise(dat.group,mean.ach.online=mean(outcome.online4),
                    sd.ach.online=sd(outcome.online4))

dat.group<-group_by(dat.online, ที่ตั้งสถานศึกษา, เพศผู้เรียน)
summarise(dat.group,mean.ach.online=mean(outcome.online4),
                    sd.ach.online=sd(outcome.online4))      

dat.online%>%filter_at(vars("smartphone","ipad","computer",
                            contains("interaction.onsite"),
                            contains("climate.onsite"),
                            contains("outcome.onsite")),
                            all_vars(!is.na(.)))%>%
            mutate(it.device=smartphone+ipad+computer,
                     interaction.onsite=(interaction.onsite1+
                                      interaction.onsite2+
                                      interaction.onsite3)/3,
                     climate.onsite=(climate.onsite1+
                                      climate.onsite2+
                                       climate.onsite3)/3,
                     outcome.onsite=(20*outcome.onsite1+
                                       20*outcome.onsite2+
                                       20*outcome.onsite3+
                                            outcome.onsite4)/4)%>% 
            group_by(ประเภทสถานศึกษา)%>%
            summarise_at(vars("it.device",
                              "interaction.onsite",
                              "climate.onsite",
                              "outcome.onsite"),
                         list(Mean=mean, SD=sd,
                              Min=min, Max=max))%>%
            gather(key="Stat", value="Value", 2:17)%>%
            spread(ประเภทสถานศึกษา, value="Value")



