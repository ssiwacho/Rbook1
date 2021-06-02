## syntax สำหรับบทที่ 2

# 2.3 การคํานวณทางคณิตศาสตร์พื้นฐาน

1+1; 3-2; 4*5; 10/2
3^3; sqrt(625); 81^(1/3)
5%%3; (3^3+5-1)
log(10); exp(5)

# 2.4 ฟังก์ชัน (function)

x<-(-10)
if(x<0){
    -(x)
} else {
x }

abs(-10)

log(x=10, base=exp(1))
log(10)

 ?log()
 help(log)

# 2.4 ตัวแปร (Variable) 

## ตัวแปรตัวอักษร
gender1<-"Male"
gender2<-"Female"
gender1
gender2

a<-"1"
b<-"3"
a+b # จะหาผลลัพธ์ไม่ได้

## ตัวแปรตรรกะ
x<-TRUE
x

y<-F
y

student1<-65 # assign 65 to student 
student1 > 50 # Is student1 greater than 50?
student1 == 70 # Is student1 equal to 50?

# 2.5 เวกเตอร์ (vectors)

score<-c(53,69,52,62,57,54,55,67,53,58) #create "score" vector
score #print score

score[3]
score[5:9]
score[c(2,5,7,10)]

score[6]<-60
score

gender<-c("M","F","M","M","M","F","M","F","F","M")
gender

result<-score>=60
result


# การดำเนินการของเวกเตอร์

score+2

# ตาราง 2.1
midterm<-c(70,62,56,64,56)
final<-c(70,64,47,60,62)
total<-0.4*midterm + 0.6*final
total

A<-matrix(x,nrow=4,ncol=2,byrow=F)
A

B<-matrix(x,nrow=4,ncol=2,byrow=T)
B

midterm<-c(70,62,56,64,56)
final<-c(70,64,47,60,62)
total<-0.4*midterm + 0.6*final
mat1<-cbind(midterm,final,total)
mat1

mat2<-rbind(midterm,final,total)
mat2

A[2,]
A[,2]
A[3,2]
A[2:4,]

# ตาราง 2.2
gender<-c("Male","Female","Male",
          "Male","Female","Male",
          "Male","Female")
age<-c(10,10,11,2,9,4,10,14)
weight<-c(59,35,75,20,63,23,47,59)
height<-c(142,135,150,95,141,108,142,155)
mat3<-cbind(gender,age,weight,height)
mat3

