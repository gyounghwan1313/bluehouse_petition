### 라이브러리 import
library(rvest)
library(XML)
library(RSelenium)
library(KoNLP)
library(stringr)
library(lubridate)
library(ggplot2)
library(plyr)
library(reshape)
library(xlsx)
library(wordcloud2)
library(reshape2)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(moments)

### 데이터 불러오기
tab = read.csv("c:/data/tab.csv",stringsAsFactors=F,header=T)

### 데이터 정제

# 국민청원 내용 정제
tab$contents=str_replace_all(tab$contents,'[[:punct:]]{1,10}'," ")
tab$contents=str_replace_all(tab$contents,'[0-9]{1,10}'," ")
tab$contents=str_replace_all(tab$contents,'부탁드립니다'," ")
tab$contents=str_replace_all(tab$contents,'요청합니다'," ")
tab$contents=str_replace_all(tab$contents,'요구합니다'," ")
tab$contents=str_replace_all(tab$contents,'바랍니다'," ")
tab$contents=str_replace_all(tab$contents,'원합니다'," ")
tab$contents=str_replace_all(tab$contents,'합니다'," ")
tab$contents=str_replace_all(tab$contents,'하게'," ")
tab$contents=str_replace_all(tab$contents,'있습니다'," ")
tab$contents=str_replace_all(tab$contents,'드립니다'," ")
tab$contents=str_replace_all(tab$contents,'주십시오'," ")
tab$contents=str_replace_all(tab$contents,'n번방',"N번방")
tab$contents=str_replace_all(tab$contents,'[<]'," ")
tab$contents=str_replace_all(tab$contents,'[>]'," ")
tab$contents=str_replace_all(tab$contents,'[+]'," ")
tab$contents=str_replace_all(tab$contents,'[■]'," ")
tab$contents=str_replace_all(tab$contents,'[★]'," ")
tab$contents=str_replace_all(tab$contents,'[☆]'," ")
tab$contents=str_replace_all(tab$contents,'[●]'," ")
tab$contents=str_replace_all(tab$contents,'[○]'," ")
tab$contents=str_replace_all(tab$contents,'[◆]'," ")
tab$contents=str_replace_all(tab$contents,'[^]'," ")
tab$contents=str_replace_all(tab$contents,'[`]'," ")
tab$contents=str_replace_all(tab$contents,'문재인',"대통령")

 
## 데이터 형식 맞추기
tab$numbers=as.integer(tab$numbers)
tab$categories=as.vector(tab$categories)
tab$dates=as.Date(tab$dates)
tab$peoples=as.character(tab$peoples)
tab$peoples=str_replace_all(tab$peoples,'[[:punct:]]',"")
tab$peoples=as.numeric(tab$peoples)
tab$dates=tab$dates-30


## 중복데이터 제거
length(unique(tab$numbers))
tab=tab[!duplicated.data.frame(tab),]
NROW(tab)


### 작년 8월부터 올해 6월까지 데이터만 선별


tabc = tab
tabc=tabc[!(year(tabc$dates)==2020 & month(tabc$dates)==7),]
tabc=tabc[!(year(tabc$dates)==2019 & month(tabc$dates)==7),]
table(month(tabc$dates))


### 워드클라우드 생성

#불용어 사전
notext = c("관련","대한","민국","우리","저희","주십시","드립니")


useSejongDic()

text = extractNoun(tabc$contents) #명사만 추출
text = unlist(text)
text = text[nchar(text)>1] # 1글자 이상만 추출

text = text[!text %in% notext] # 불용어 제거

df = data.frame(table(text))
df$text=as.vector(df$text)
wordcloud2(df[df$Freq>20,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)

# 상위 빈도 20개
head(df[order(df$Freq,decreasing = T),],20)
 

## 분석1 : 월별 청원건수
tabc$month = month(tabc$dates)
tabc$month = factor(tabc$month,levels = c(8,9,10,11,12,1,2,3,4,5,6),ordered=T)

data1 = data.frame(table(tabc$month))

ggplot(data1, aes(x=Var1,y=Freq))+
  geom_bar(stat = 'identity',fill=heat.colors(11))+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=-0.2,size=5)+
  labs(title ="11개월의 월별 청원건수",x="month",y="count")+
  theme(plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=20))+
  theme(axis.title.x=element_text(face="bold",size=20))+
  theme(axis.title.y=element_text(face="bold",
                                  size=20,
                                  angle=90,
                                  vjust=0.5))+
  theme(axis.text.x = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 20))+
  theme(axis.text.y = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 11))
# 결과 :코로나 창궐 이후 2~3배이상의 청원이 늘었다


## 분석2 : 어느 카테고리의 청원이 많은가
data2 = data.frame(table(tabc$categories))

ggplot(data2, aes(x=Var1,y=Freq))+
  geom_bar(stat = 'identity',fill=heat.colors(17))+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=-0.3,size=4)+
  labs(title ="분야별 청원건수",x="Category",y="Count")+
  theme(plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=20))+
  theme(axis.title.x=element_text(face="bold",size=20))+
  theme(axis.title.y=element_text(face="bold",
                                  size=20,
                                  angle=90,
                                  vjust=0.5))+
  theme(axis.text.x = element_text (angle=80,hjust=1,vjust=1,colour="black",size = 13))+
  theme(axis.text.y = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 11))


#해석 : 코로나때문인지 보건복지,안전,환경에 대한 빈도가 높다. 정부 예산 소비와 출산율 대비 저출산 청원은 적다


## 분석3 : 작년과 올해로 나눠서 분석
df2 = tabc[year(tabc$dates) ==2020,] #코로나 이후
df1 = tabc[year(tabc$dates) ==2019,] #코로나 이전

data3 = data.frame(table(df1$categories))
data4 = data.frame(table(df2$categories))

ggplot(data3, aes(x=Var1,y=Freq))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=0.5)+
  labs(title ="11개월동안 분야별 청원건수",x="분야",y="건수")

ggplot(data4, aes(x=Var1,y=Freq))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=0.5)+
  labs(title ="11개월동안 분야별 청원건수",x="분야",y="건수")

data3$year = 2019
data4$year = 2020

data5 = rbind(data3,data4)
data5$year = factor(data5$year,levels=c(2020,2019),ordered = T)

ggplot(data5, aes(x=Var1,y=Freq))+
  geom_col(aes(fill=year))+
  scale_fill_manual(values = c("Green","tomato"))+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=0.5,position="stack")+
  labs(title ="11개월동안 분야별 청원건수",x="분야",y="건수")



ggplot(data5, aes(x=Var1,y=Freq,fill=year))+
  geom_bar(stat='identity')+
  scale_fill_manual(values = c("Green","tomato"))+
  geom_text(aes(y=Freq,label=paste0(Freq,'건')),vjust=1,position="stack",size=4)+
  labs(title ="분야별 청원건수",x="Category",y="Count")+
  theme(plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=20))+
  theme(axis.title.x=element_text(face="bold",size=20))+
  theme(axis.title.y=element_text(face="bold",
                                  size=20,
                                  angle=90,
                                  vjust=0.5))+
  theme(axis.text.x = element_text (angle=90,hjust=1,vjust=1,colour="black",size = 11))+
  theme(axis.text.y = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 11))+
  theme(legend.title.align=0.5,
        legend.box.background=element_rect(),
        legend.box.margin=margin(t=0.1,r=0.1,
                                 b=0.1,l=0.1,unit='cm'))+ 
  guides(fill=guide_legend(title="Year"))+
  theme(legend.title=element_text(face="italic", family="Times",size=14))

# 결과 : 실제로도 보건복지 분야와 안전 분야는 대부분 코로나와 관련성이 있음


# 비율로 변환해서 그래프 그리기
data3 = 
  data3 %>% 
  mutate(ratio=round((Freq/sum(Freq))*100,1))

data4 = 
  data4 %>% 
  mutate(ratio=round((Freq/sum(Freq))*100,1))

data5 = rbind(data3,data4)
data5$year = factor(data5$year,levels=c(2020,2019),ordered = T)


ggplot(data5, aes(x=Var1,y=ratio,fill=year))+
  geom_bar(stat='identity')+
  scale_fill_manual(values = c("Green","tomato"))+
  geom_text(aes(y=ratio,label=paste0(ratio,'%')),vjust=1,position="stack",size=3)+
  labs(title ="분야별 청원건수",x="Category",y="Percent(%)")+
  theme(plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=20))+
  theme(axis.title.x=element_text(face="bold",size=20))+
  theme(axis.title.y=element_text(face="bold",
                                  size=20,
                                  angle=90,
                                  vjust=0.5))+
  theme(axis.text.x = element_text (angle=90,hjust=1,vjust=1,colour="black",size = 11))+
  theme(axis.text.y = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 11))+
  theme(legend.title.align=0.5,
        legend.box.background=element_rect(),
        legend.box.margin=margin(t=0.1,r=0.1,
                                 b=0.1,l=0.1,unit='cm'))+ 
  guides(fill=guide_legend(title="년도"))+
  theme(legend.title=element_text(face="italic", family="Times",size=14))


## 분석4 : 2019년과 20202년의 워드클라우드 비교
df1 #2019
df2 #2020


#2019
text = extractNoun(df1$contents)
text = unlist(text)
text = text[nchar(text)>1]

text = text[!text %in% notext]

df = data.frame(table(text))
wordcloud2(df[df$Freq>10,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)
head(df[order(df$Freq,decreasing = T),],20)

# 긍정/부정 사전 불러오기
pos=readLines('c:/data/pos_pol_word.txt',encoding='UTF-8')
neg=readLines('c:/data/neg_pol_word.txt',encoding='UTF-8')

length(text[text %in% pos]);length(text[text %in% pos])/length(text)*100 #긍정단어 비율
length(text[text %in% neg]); length(text[text %in% neg])/length(text)*100 #부정단어 비율


#2020
text = extractNoun(df2$contents)
text = unlist(text)
text = text[nchar(text)>1]

text = text[!text %in% notext]

df = data.frame(table(text))
wordcloud2(df[df$Freq>15,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)

head(df[order(df$Freq,decreasing = T),],20)


length(text[text %in% pos]);length(text[text %in% pos])/length(text)*100 #긍정단어 비율
length(text[text %in% neg]); length(text[text %in% neg])/length(text)*100 #부정단어 비율

# 결과 : 작년에 윤석열과 조국 관련된 키워드가 많았고 올해는 코로나 키워드가 많음


## 분석5 : 상위 10%의 추천수를 얻은 것들의 분야와 키워드

# 참여자수(좋아요)의 히스토그램
hist(round(tabc$peoples,-1))
quantile(tabc$peoples)
quantile(tabc$peoples,0.9)
quantile(tabc$peoples,0.1)


skewness(tabc$peoples) #왜도

df3=tabc[tabc$peoples>7113,] # 참여자수 많이 받은 상위집단
df4= tabc[tabc$peoples<201,] # 하위집단
 

#상위집단 워드클라우드
text = extractNoun(df3$contents)
text = unlist(text)
text = text[nchar(text)>1] #글자수 2이상

text = text[!text %in% notext] #불용어 처리

df = data.frame(table(text))
wordcloud2(df[df$Freq>10,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)

head(df[order(df$Freq,decreasing = T),],20)


length(text[text %in% pos]);length(text[text %in% pos])/length(text)*100 #긍정어 비율
length(text[text %in% neg]); length(text[text %in% neg])/length(text)*100 #부정어 비율


#하위집단 워드클라우드
text = extractNoun(df4$contents)
text = unlist(text)
text = text[nchar(text)>1] #글자수 2이상

text = text[!text %in% notext] #불용어 처리

df = data.frame(table(text))
wordcloud2(df[df$Freq>10,],rotateRatio= 0,size=0.5)

head(df[order(df$Freq,decreasing = T),],20)


length(text[text %in% pos]);length(text[text %in% pos])/length(text)*100 #긍정어 비율
length(text[text %in% neg]); length(text[text %in% neg])/length(text)*100 #부정어 비율
NROW(text)


#결과 : 국민청원이 다소 정치적인 요소로 사용되고 있다. 상위집단은 정치적 이슈, 하위집단은 사회적약자에 관한 단어


## 분석7 : 카테고리별 워드클라우드

notext = c("관련","대한","민국","우리","저희","주십시","드립니","마련") #불용어

#기타
others = tabc[tabc$categories=="기타",]

text = extractNoun(others$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>5,],size=0.7)
head(df[order(df$Freq,decreasing = T),],10)

#저출산
baby = tabc[tabc$categories=="저출산/고령화대책",]

text = extractNoun(baby$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>1,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)
head(df[order(df$Freq,decreasing = T),],10)

##육아/교육
edu = tabc[tabc$categories=="육아/교육",]
edu = edu[year(edu$dates) ==2019,]
text = extractNoun(edu$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>3,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)
head(df[order(df$Freq,decreasing = T),],10)


##교통/건축/국토
traffic = tabc[tabc$categories=="교통/건축/국토",]

text = extractNoun(traffic$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>5,],size=0.6,shape = 'circle',  rotateRatio = 1,
           minRotation = -pi/3, maxRotation = pi/4)
head(df[order(df$Freq,decreasing = T),],10)

##농산어촌
country = tabc[tabc$categories=="농산어촌",]

text = extractNoun(country$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>1,],minRotation=0, maxRotation=0)
head(df[order(df$Freq,decreasing = T),],10)

##일자리
job = tabc[tabc$categories=="일자리",]

text = extractNoun(job$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>1,],minRotation=0, maxRotation=0,size=0.5)
head(df[order(df$Freq,decreasing = T),],10)

##문화
job = tabc[tabc$categories=="문화/예술/체육/언론",]

text = extractNoun(job$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>2,],minRotation=0, maxRotation=0,size=0.5)
head(df[order(df$Freq,decreasing = T),],10)




##인권/성평등
right = tabc[tabc$categories=="인권/성평등",]

text = extractNoun(right$contents)
text = unlist(text)
text = text[nchar(text)>1]
text = text[!text %in% notext]
df = data.frame(table(text))
wordcloud2(df[df$Freq>2,],minRotation=0, maxRotation=0,size=0.7)
head(df[order(df$Freq,decreasing = T),],10)


### 부동산 카테고리의 의미연결망 그리기

mp=SimplePos09(traffic$contents)


m_df = 
  mp %>% 
  melt %>% 
  as_tibble

head(m_df,40)
m_df=m_df[,c(3,1)]

# 빈도수 테이블
m_count = 
  m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% 
  na.omit %>% 
  filter(str_length(noun)>=2) %>% 
  count(noun,sort = T) %>% 
  head(20)
m_count 

# 빈도수 구하기
m_df2  = 
  m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>% 
  na.omit %>% 
  filter(str_length(noun)>=2) %>% 
  select(3,1)
head(m_df2)


# 불용어 제거
m_df2 = 
  m_df2 %>% 
  filter(!noun %in% notext)

# 의미연결망 그릴 단어 선택
m_df3 = 
  m_df2 %>% 
  filter(noun %in% m_count$noun)


mg = graph_from_data_frame(m_df3) # 화살표 

V(mg)$type <- bipartite_mapping(mg)$type
mm <- as_incidence_matrix(mg) %*% t(as_incidence_matrix(mg))
diag(mm) <- 0
mg <- graph_from_adjacency_matrix(mm)

plot(mg)

## 최종 의미연결망
mg %>% as_tbl_graph() %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))



## 분야별로 좋아요순 분포

# ANOVA분석
aggregate(tabc$peoples~tabc$categories,tabc,mean)
aov(aggregate(tabc$peoples~tabc$categories,tabc,mean))
result = aov(tabc$peoples~tabc$categories,data=tabc)
result
summary(result)
head(tabc)

# error bar를 그리기 위해 평균,표준편차,표준오차를 구함
df1=ddply(tabc,'categories',summarise ,mean=mean(peoples),sd=sd(peoples),n=length(peoples),se=sd/sqrt(n))

ggplot(df1,aes(x=categories,y=mean))+
  geom_bar(stat="identity",fill=heat.colors(17))+
  geom_errorbar(aes(ymin=mean/2,ymax=mean+se),width=0.2)+
  geom_text(aes(y=mean,label=paste0(round(mean,1),'건')),vjust=1.5,size=4)+
  labs(title ="분야별 참여수(좋아요)",x="Category",y="Mean")+
  theme(plot.title=element_text(face="bold",
                                hjust=0.5,
                                size=20))+
  theme(axis.title.x=element_text(face="bold",size=20))+
  theme(axis.title.y=element_text(face="bold",
                                  size=20,
                                  angle=90,
                                  vjust=0.5))+
  theme(axis.text.x = element_text (angle=90,hjust=1,vjust=1,colour="black",size = 16))+
  theme(axis.text.y = element_text (angle=0,hjust=1,vjust=1,colour="black",size = 11))+
  ylim(0,20000)
