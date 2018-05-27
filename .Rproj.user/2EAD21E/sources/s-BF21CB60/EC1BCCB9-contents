library(readr)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(choroplethrMaps)
library(choroplethr)

school103 <- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
school104 <- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
school105 <- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
school106 <- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")

Data1<-rbind(school103,school104)
Data2<-rbind(school105,school106)
Data2<-Data2%>%
        rename("學位生-正式修讀學位外國生"="學位生_正式修讀學位外國生",
               "學位生-僑生(含港澳)"="學位生_僑生(含港澳)",
               "學位生-正式修讀學位陸生"="學位生_正式修讀學位陸生",
               "非學位生-外國交換生" ="非學位生_外國交換生",
               "非學位生-外國短期研習及個人選讀"="非學位生_外國短期研習及個人選讀",
               "非學位生-大專附設華語文中心學生"="非學位生_大專附設華語文中心學生",
               "非學位生-大陸研修生"="非學位生_大陸研修生",
               "非學位生-海青班"="非學位生_海青班")
DataFinal<-rbind(Data1,Data2)


total<-NULL
for(i in 1:nrow(DataFinal))
{
  total<-c(total,rowSums(DataFinal[i,3:11]))
}

college103 <- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
college104 <- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
college105 <- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
college106 <- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")

Data1_1<-rbind(college103,college104)
Data1_2<-rbind(college105,college106)
Data1_2<-Data1_2%>%
            rename("學位生-正式修讀學位外國生"="學位生_正式修讀學位外國生",
                   "學位生-僑生(含港澳)"="學位生_僑生(含港澳)",
                   "學位生-正式修讀學位陸生"="學位生_正式修讀學位陸生",
                   "非學位生-外國交換生" ="非學位生_外國交換生",
                   "非學位生-外國短期研習及個人選讀"="非學位生_外國短期研習及個人選讀",
                   "非學位生-大專附設華語文中心學生"="非學位生_大專附設華語文中心學生",
                   "非學位生-大陸研修生"="非學位生_大陸研修生",
                   "非學位生-海青班"="非學位生_海青班")
DataFinal1<-rbind(Data1_1,Data1_2)
DataFinal1$`非學位生-大陸研修生`<-0

Total<-NULL
for(k in 1:nrow(DataFinal1))
{
  Total<-c(Total,rowSums(DataFinal1[k,4:11]))
}
#匯入國家中英文對照表
Country <- read_csv("C:/Users/user/Downloads/CountriesComparisionTable.csv")
Country <-Country[,-1]
Country<-Country%>%rename(國別=Taiwan)
#.................................................................................

#1請問哪些國家來台灣唸書的學生最多呢？
#請取出前十名的國家與總人數，由大到小排序(5分)。
Result1<-DataFinal%>%
        mutate(人數=total)%>%
        group_by(洲別,國別)%>%
        summarise(總人數=sum(人數))%>%
        arrange(desc(總人數))
Result1%>%head(10)


#1又哪間大學的境外生最多呢？
Result2<-DataFinal1%>%
  mutate(人數=Total)%>%
  group_by(學校名稱)%>%
  summarise(總人數=sum(人數))%>%
  arrange(desc(總人數))
Result2%>%head(10)

#承1，請用bar chart呈現各個國家(全部)來台灣唸書的學生人數
chart1<-ggplot()+
        geom_bar(data=Result1%>%head(30),
                 aes(x=國別,y=總人數),
                 stat="identity")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
chart1
#承1，請用面量圖呈現各個國家來台灣唸書的學生人數，人數越多顏色越深(10分)。
Result3<-left_join(Result1,Country,by="國別")#join來台國家與國家中英文對照表
Result3<-Result3[complete.cases(Result3),]#去除遺漏值
#新建一個資料框，裡面放region(地區)與value(總人數)值
Result3_1<-data.frame(region=Result3$English,
                      value=Result3$總人數)
Result3_1<-Result3_1%>%
           group_by(region)%>%
           summarise(value=sum(value))
chart3<-country_choropleth(Result3_1,title = "各個國家來台灣唸書的學生人數分布圖")+ 
        scale_fill_brewer(name="學生人數", palette=9)
chart3

student <- read.csv("C:/Users/user/Desktop/student.csv", header=FALSE,stringsAsFactors = F)
colnames(student)<-c(student[1,])
student<-student[-1,]
student2<-student%>%
  filter(學年度=="103"|學年度=="104")
student2$出國學生人數<-as.numeric(student2$出國學生人數)

#.................................................................................
Q4_1<-student2%>%
  rename(國別=對方學校國別)%>%
  group_by(國別)%>%
  summarise(學生人數=sum(出國學生人數))%>%
  arrange(desc(學生人數))
Q4_1%>%head(10)

Q4_2<-student2%>%
  group_by(學校名稱)%>%
  summarise(學生人數=sum(出國學生人數))%>%
  arrange(desc(學生人數))
Q4_2%>%head(10)
#承4，請用bar chart呈現台灣大專院校(全部)的學生去各國家進修交流人數(10分)。
chart5<-ggplot()+
  geom_bar(data=Q4_1%>%head(30),
           aes(x=國別,y=學生人數),
           stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
chart5


#承4，請用面量圖呈現台灣大專院校的學生去各國家進修交流人數，人數越多顏色越深(10分)。
Q6<-left_join(Q4_1,Country,by="國別")
Q6<-Q6[complete.cases(Q6),]
Q6_1<-data.frame(region=Q6$English,
                 value=Q6$學生人數)
Q6_1<-Q6_1%>%
  group_by(region)%>%
  summarise(value=sum(value))
chart6<-country_choropleth(Q6_1,title = "台灣大專院校(全部)的學生去各國家進修交流人數分布圖")+ 
  scale_fill_brewer(name="學生人數", palette=3)
chart6

#7.台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序(5分)。
X105 <- read_csv("C:/Users/user/Downloads/105.csv")
X105<-X105[,-(4:6)]
Data7<-X105%>%arrange(desc(總人數))
Data7%>%head(10)

#8.承7，請用面量圖呈現台灣學生去各國家留學人數，人數越多顏色越深(10分)。
Data8_1<-left_join(Data7,Country,by="國別")
Data8_1<-Data8_1[complete.cases(Data8_1),]
Data8_2<-data.frame(region=Data8_1$English,
                    value=Data8_1$總人數)
Data8_2<-Data8_2%>%
  group_by(region)%>%
  summarise(value=sum(value))
chart8<-country_choropleth(Data8_2,title = "台灣學生去各國家留學人數分布圖")+ 
  scale_fill_brewer(name="學生人數", palette=2)
chart8
#9請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？
compare<-inner_join(Result1,Q4_1,by="國別")
compare<-compare%>%
  rename("來台學生人數"=總人數,
         "留學人數"=學生人數)
#9_2............................................
compare2<-gather(compare,key=種類,value=人數,來台學生人數,留學人數)
compare2<-compare2%>%arrange(desc(人數))
#9_2圖.............................................
compare2$國家 <- reorder(compare2$國別, compare2$人數)
chart9<-ggplot()+geom_bar(data=compare2%>%head(50),
                          aes(x=國家,y=人數),
                          stat="identity")+
  facet_grid(.~種類)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))
chart9

