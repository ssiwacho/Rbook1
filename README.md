# สถิติและวิทยาการข้อมูลทางการศึกษา : R สำหรับการจัดระเบียบและจัดกระทำข้อมูล

Repository นี้บันทึกชุดคำสั่งรวมทั้งข้อมูลตัวอย่างที่ใช้ประกอบในหนังสือโดยจำแนกไว้ตามบทเรียน ผู้อ่านสามารถดาวน์โหลดไฟล์ดังกล่าวเพื่อใช้ฝึกปฏิบัติระหว่างการศึกษาได้โดยง่าย


**ผู้ที่สนใจสั่งซื้อหนังสือสามารถสั่งซื้อได้ 2 รูปแบบ**

1. **รูปแบบ ebook** ราคาเล่มละ 279 บาท สามารถสั่งซื้อโดยตรงได้จาก [website ของศูนย์หนังสือจุฬา](https://www.chulabook.com/th/home) <--- คลิกที่นี่เลย
2. **รูปแบบหนังสือ** ราคาเล่มละ 800 บาท (พิมพ์ขาวดำ และสี) ติดต่อสอบถามได้ทาง ดร.สิวะโชติ ศรีสุทธิยากร email: choat.cu@gmail.com (**ของมีจำนวนจำกัด!!**)


R เป็นโปรแกรมทางสถิติที่ถูกพัฒนาขึ้นและได้รับความนิยมอย่างแพร่หลายจากทั้งนักวิจัย นักสถิติ และนักวิทยาการข้อมูล ทั้งนี้เพราะ R มีความสามารถครอบคลุมการดำเนินงานทั้งหมดทางด้านสถิติและวิทยาการข้อมูล ตั้งแต่การนำเข้าข้อมูล การจัดระเบียบและจัดกระทำข้อมูล การสำรวจข้อมูล การสร้างทัศนภาพข้อมูล ไปจนถึงการวิเคราะห์ข้อมูลด้วยโมเดลทางสถิติที่หลากหลาย จุดเด่นสำคัญของ R คือการมีชุมชนทางวิชาการที่เข้มแข็งมีการแลกเปลี่ยนความรู้และช่วยกันพัฒนาโปรแกรม R อย่างสม่ำเสมอ จนทำให้ในปัจจุบันมี package เสริมมากกว่า 10,000 ตัวที่ผู้ใช้สามารถดาวน์โหลดและติดตั้งเพื่อเพิ่มประสิทธิภาพและขอบเขตการทำงานได้โดยแทบไม่มีข้อจำกัด 

หนังสือเล่มนี้เป็นเล่มที่หนึ่งในชุดหนังสือสถิติและวิทยาการข้อมูลทางการศึกษา เป็นการถ่ายทอดความรู้จากประสบการณ์ตรงจากการศึกษาและทำงานที่เกี่ยวข้องกับสถิติและวิทยาการข้อมูลมาเป็นระยะเวลากว่า 15 ปี ของผู้เขียนเอง โดยออกแบบการดำเนินเรื่องให้เป็นลำดับจากง่ายไปยาก มีการอธิบายมโนทัศน์หรือทฤษฎีทางสถิติที่จำเป็นควบคู่กับการแสดงตัวอย่าง เหมาะสำหรับทั้งผู้อ่านที่มีและไม่มีประสบการณ์ในการใช้โปรแกรม R สามารถใช้เพื่อพัฒนาความรู้และทักษะปฏิบัติได้โดยง่าย หนังสือเล่มนี้ประกอบด้วยบทเรียนจำนวน 7 บท ได้แก่

**บทที่ 1:** บทนำ กล่าวถึงภาพรวม วิธีการใช้หนังสือ การดาวน์โหลดและติดตั้งโปรแกรม R 

**บทที่ 2:** กล่าวถึงการใช้โปรแกรม R พื้นฐาน โดยเริ่มตั้งแต่แนะนำสภาพแวดล้อมของโปรแกรม การดำเนินการพื้นฐาน รวมทั้งตัวแปรสำหรับเก็บข้อมูลประเภทต่าง ๆ ในโปรแกรม R ซึ่งเป็นพื้นฐานสำหรับการดำเนินงานทางด้านสถิติและวิทยาการข้อมูลต่อไป

**บทที่ 3:** กล่าวถึงการนำข้อมูลลักษณะต่าง ๆ เข้าสู่โปรแกรม R ซึ่งเป็นขั้นแรกของการดำเนินงานทางสถิติและวิทยาการข้อมูล โปรแกรม R เป็นโปรแกรมที่มีความยืดหยุ่นสูงสามารถนำเข้าข้อมูลได้หลากหลายลักษณะ ทั้งไฟล์ข้อมูลแบบ flat file เช่น .txt หรือ .csv ไฟล์ข้อมูลจากโปรแกรม MS Excel ไฟล์ข้อมูลจากโปรแกรมสำเร็จรูปทางสถิติ เช่น SPSS, Minitab, SAS, STATA เป็นต้น ผู้อ่านจะพบว่าการทำงานบนโปรแกรม R สามารถเชื่อมต่อกับโปรแกรมอื่น ๆ ได้โดยง่ายโดยแทบไม่มีข้อจำกัด

**บทที่ 4:** โดยปกติข้อมูลที่เก็บรวบรวมมาไม่ว่าจะจากแหล่งข้อมูลรูปแบบไหนก็ตาม มักยังไม่พร้อมที่จะนำไปวิเคราะห์ข้อมูลตามวัตถุประสงค์ของผู้วิเคราะห์ในทันที ทั้งนี้เป็นเพราะรูปแบบของชุดข้อมูลยังไม่เหมาะสมหรือไม่ได้อยู่ในรูปแบบชุดข้อมูลจัดระเบียบ (tidy data) เนื้อหาในบทเรียนนี้จะจะกล่าวถึมโนทัศน์และเทคนิคในการการดำเนินการจัดระเบียบข้อมูลที่นำเข้ามาเพื่อให้ได้ชุดข้อมูลที่เรียกว่าชุดข้อมูลจัดระเบียบดังกล่าว 

**บทที่ 5:** กล่าวถึงมโนทัศน์และเทคนิควิธีการสำหรับการสำรวจข้อมูลด้วยโปรแกรม R โดยจำแนกออกเป็นสองส่วนได้แก่ การสำรวจข้อมูลด้วยทัศนภาพข้อมูลพื้นฐาน และการสำรวจข้อมูลด้วยสถิติ เนื้อหาในบทเรียนนี้ครอบคลุมการสำรวจข้อมูลในมิติต่าง ๆ อย่างครบถ้วน ตั้งแต่การสำรวจข้อมูลตัวแปรเดียวไปจนถึงการสำรวจความสัมพันธ์ระหว่างตัวแปรประเภทต่าง ๆ 

**บทที่ 6:** ถึงแม้ชุดข้อมูลจะอยู่ในรูปแบบชุดข้อมูลจัดระเบียบแล้ว แต่ในสถานการณ์จริงเมื่อทำการสำรวจข้อมูลด้วยวิธีการต่าง ๆ ในบทที่ 5 แล้ว ผู้วิเคราะห์มักจะพบว่ายังมีหน่วยข้อมูลบางหน่วย หรือบางตัวแปรที่มีคุณสมบัติไม่สอดคล้องกับข้อตกลงเบื้องต้นของการวิเคราะห์ เช่น มีหน่วยข้อมูลที่เป็นค่าผิดปกติ มีค่าสูญหาย หน่วยของตัวแปรยังไม่ใช่หน่วยที่เหมาะสมสำหรับการดำเนินการวิเคราะห์ หรือจำเป็นต้องมีการรวมคะแนนหรือสร้างข้อมูลของตัวแปรใหม่จากข้อมูลที่มีอยู่เดิม เพื่อให้ได้ตัวแปรที่มีความหมายและเหมาะสมสำหรับการวิเคราะห์ บทเรียนนี้จึงจะกล่าวถึงมโนทัศน์และเทคนิคสำหรับการจัดกระทำข้อมูลอย่างมีประสิทธิภาพในโปรแกรม R 

**บทที่ 7:** ปัญหาหนึ่งที่เป็นปัญหาใหญ่และมักพบอย่างสม่ำเสมอในการดำเนินงานทางด้านสถิติและวิทยาการข้อมูลคือปัญหาค่าสูญหาย ปัญหานี้มีปัจจัยที่เป็นสาเหตุของปัญหาหลายตัว ซึ่งทำให้การสูญหายของข้อมูลสามารถจำแนกได้เป็นหลายประเภท ส่งผลให้การแก้ปัญหาที่เหมาะสมนั้นขึ้นอยู่กับลักษณะการสูญหายของข้อมูลด้วย บทเรียนนี้จะกล่าวถึงมโนทัศน์ของค่าสูญหาย ซึ่งจะช่วยให้ผู้อ่านเข้าใจลักษณะของการเกิดค่าสูญหายได้ดีขึ้น จากนั้นจะกล่าวถึงวิธีการสำรวจเพื่อวินิจฉัยค่าสูญหายในชุดข้อมูล และวิธีการแก้ปัญหาค่าสูญหายด้วยวิธีการต่าง ๆ โดยเน้นไปที่วิธีการทดแทนค่าสูญหาย (missing values imputation) ซึ่งเป็นกลุ่มวิธีการที่ได้รับการยอมรับว่าสามารถแก้ปัญหาค่าสูญหายได้ดีที่สุดในปัจจุบัน อย่างไรก็ตามมีมโนทัศน์ที่คลาดเคลื่อนบางประการเกี่ยวกับการทดแทนค่าสูญหาย ซึ่งในบทเรียนนี้จะกล่าวถึงทั้งหมด รวมทั้งนำเสนอวิธีการที่เหมาะสมสำหรับทดแทนค่าสูญหายในแต่ละสถานการณ์ของการวิเคราะห์



