library(car)
library(readxl)
library(tidyverse)
library(tm)

setwd("c:/Users/LG1/Desktop/work/90. 의료데이터 분석/2차/")

# read xlsx file
df = read_xlsx("001. 데이터/full_data_220907 (Ver1.1).xlsx",
               col_types = c("text", "text", "text", "text", "text",
                             "text", "text", "text", "numeric", "numeric",
                             "numeric", "text", "numeric", "numeric", "numeric",
                             "text", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric",
                             "text", "text", "text"))

colnames(df)
head(df)
tail(df)

# 데이터 수 : 총 185,430건
# 컬럼 수 : asa, second outcome 포함 33개
nrow(df) == length(unique(df$caseid))
length(colnames(df))

# 추가 변수 기초통계
df %>%
  subset.data.frame(select = c(andur, expd)) %>%
  summary()

# 상자 그림
df %>%
  ggplot(aes(y = andur)) +
  geom_boxplot()

df %>%
  ggplot(aes(y = expd)) +
  geom_boxplot()


# andur의 널값 존재
df %>%
  subset.data.frame(is.na(andur)==T) %>%
  mutate(total = n()) %>%
  group_by(first_outcome) %>%
  summarise(num = n(),
            rate = n()/total) %>%
  unique()


# 상관관계
df %>%
  subset.data.frame(select = c(-caseid, -first_outcome, -second_outcome, -yr, -em,
                               -dm, -htn, -asa,
                               -sex, -atype,
                               -dept, -name, -diagnosis)) %>%
  na.omit() %>%
  cor() %>%
  corrplot::corrplot("number")

# 노출시간 * 예상 노출 시간
df %>%
  ggplot(aes(x = andur, y = expd)) +
  geom_point()

df %>%
  subset.data.frame(select = c(first_outcome, dm)) %>%
  table()

# 기저질환1(dm)
df %>% 
  group_by(dm) %>%
  summarise(num = n(),
            rate = n()/nrow(df)*100)

# 기저질환2(htn)
df %>% 
  group_by(htn) %>%
  summarise(num = n(),
            rate = n()/nrow(df)*100)

# 기저질환1과 2 교차표
df %>%
  subset.data.frame(select = c(htn, dm)) %>%
  table()

# 기저질환1과 합병증 교차표
df %>%
  subset.data.frame(select = c(first_outcome, dm)) %>%
  table()

df %>%
  subset.data.frame(select = c(first_outcome, htn)) %>%
  table()

df %>%
  subset.data.frame(select = c(first_outcome, dm, htn)) %>%
  table() %>%
  write.csv("003. 분석결과 데이터/dm_htn.csv",
            row.names = F)



# 응급여부와 합병증 교차표
df %>%
  subset.data.frame(select = c(first_outcome, em)) %>%
  table()

# 년도와 합병증 교차표
df %>%
  group_by(yr)%>%
  mutate(yr_tot = n()) %>%
  group_by(yr, first_outcome) %>%
  summarise(n = n(),
            rate = n()/yr_tot*100) %>%
  unique() %>%
  write.csv("003. 분석결과 데이터/yr_foutcome.csv", row.names = F)

# 텍스트 분석
# 합병증 발생이 없는 환자의 수술명
name_0 = df %>%
  subset.data.frame(subset = c(first_outcome==0), select = (name)) %>%
  MC_tokenizer() %>%
  tolower() %>%
  table() %>%
  data.frame()

name_0 = name_0[order(name_0$Freq, decreasing = T),]
name_0$nlen = str_count(name_0$.)
head(name_0)

# 합병증 발생한 환자의 수술명
name_1 = df %>%
  subset.data.frame(subset = c(first_outcome==1), select = (name)) %>%
  MC_tokenizer() %>%
  tolower() %>%
  table() %>%
  data.frame()

name_1 = name_1[order(name_1$Freq, decreasing = T),]
name_1$nlen = str_count(name_1$.)
head(name_1)

write.csv(name_0,
          "003. 분석결과 데이터/name_0.csv",
          row.names = F)

write.csv(name_1,
          "003. 분석결과 데이터/name_1.csv",
          row.names = F)

rm(name_0, name_1)

# 수술명 글자수 분포도
df_name = df %>%
  subset.data.frame(select = (name)) %>%
  MC_tokenizer() %>%
  tolower() %>%
  table() %>%
  data.frame()

df_name = df_name[order(df_name$Freq, decreasing = T),]
df_name$nlen = str_count(df_name$.)
head(df_name)

ggplot(df_name, aes(nlen)) +
  geom_histogram(bins = 15)

summary(df_name$nlen)

write.csv(df_name, "003. 분석결과 데이터/name_word.csv", row.names = F)

rm(df_name)

# 수술명별 현황
target = df %>%
  subset.data.frame(select = c(name)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame()

# 수술명별 환자수 요약
head(target)
summary(target$Freq)
sd(target$Freq)

# 1시그마 구간 진단명
target %>%
  subset.data.frame(subset = c(Freq < mean(target$Freq)+2*sd(target$Freq) & Freq > mean(target$Freq)+sd(target$Freq))) %>%
  nrow()

# 2시그마 구간 진단명
target %>%
  subset.data.frame(subset = c(Freq < mean(target$Freq)+3*sd(target$Freq) & Freq > mean(target$Freq)+2*sd(target$Freq))) %>%
  head()

# 3시그마
target %>%
  subset.data.frame(subset = c(Freq > mean(target$Freq)+3*sd(target$Freq))) %>%
  head()


ggplot(target, aes(x = Freq))+
  geom_histogram(bins = 10)

result = data.frame()
for(i in 1:204){
before = df %>%
  subset.data.frame(subset = c(name == target$.[i])) %>%
  mutate(tot = n()) %>%
  group_by(first_outcome, name) %>%
  summarise(n = n(),
            rate = n()/tot) %>%
  unique()
  print(before)
result = rbind(result, before)
print(i)
}

rate = result %>%
  subset.data.frame(subset = c(first_outcome == 1))

summary(rate$rate)
rate = rate[order(rate$rate, decreasing = TRUE),]

write.csv(rate,
          "003. 분석결과 데이터/name_outcome.csv",
          row.names = F)

# 합병증 발생률 0%
result %>%
  subset.data.frame(subset = c(first_outcome == 0 & rate==1)) %>%
  write.csv("003. 분석결과 데이터/zero.csv",
            row.names = FALSE)

rm(target, result)

# 특정 키워드의 합병증 발생률
df[grep("liver", df$name),] %>%
  mutate(tot = n()) %>%
  group_by(first_outcome) %>%
  summarise(n = n(),
            rate = n()/tot) %>%
  unique()


# 수술명 수정
df$adj_name = gsub(" & ",
                   ", ",
                   df$name)

df$adj_name = gsub(paste0(", Lt.|, Lt|Lt.|,lt.|,lt|, lt",
                          ", Rt.|, Rt|Rt.|,rt.|,rt|, rt",
                          "\\( left \\)|\\( right \\)"),
                   "",
                   df$adj_name)

df$adj_name = gsub(paste0(" Lt| lt",
                          " Rt| rt",
                          "\\( left \\)|\\( right \\)"),
                   "",
                   df$adj_name)

df$adj_name = gsub("$recon|recon.", "reconstruction", df$adj_name)

df$adj_name = tolower(df$adj_name)

df %>%
  subset.data.frame(select = c(name)) %>%
  table() %>%
  nrow()

df %>%
  subset.data.frame(select = c(adj_name)) %>%
  table() %>%
  nrow()

df %>%
  subset.data.frame(select = c(name, adj_name)) %>%
  write.csv("003. 분석결과 데이터/adj_name.csv",
          fileEncoding = "UTF-8")


# 조정 후 수술명별 현황
target = df %>%
  subset.data.frame(select = c(adj_name)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame()

head(target)

result = data.frame()
for(i in 1:50){
  before = df %>%
    subset.data.frame(subset = c(adj_name == target$.[i])) %>%
    mutate(tot = n()) %>%
    group_by(first_outcome, adj_name) %>%
    summarise(n = n(),
              rate = n()/tot) %>%
    unique()
  
  result = rbind(result, before)
  print(i)
}


write.csv(result,
          "003. 분석결과 데이터/adj_name_outcome.csv",
          row.names = F,
          fileEncoding = "UTF-8")

rm(target, result, before, i )



# 진단명별 현황
diag = df %>%
  subset.data.frame(select = c(diagnosis)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame(stringsAsFactors = FALSE)

diagnosis$adj_diagnosis = tolower(diagnosis$diagnosis)

adj_diag = diagnosis %>%
  subset.data.frame(select = c(adj_diagnosis)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame()

head(adj_diag)
merge(diagnosis, adj_diag)

write.csv(adj_diag,
          "003. 분석결과 데이터/diagnosis.csv",
          row.names = F,
          fileEncoding = "UTF-8")

# 진단명에 따른 수술 현황
first_diag = df %>%
  subset.data.frame(subset = c(diagnosis == diag[1,]$.),
                    select = c(diagnosis, name)) %>%
  table() %>%
  data.frame()

write.csv(first_diag, "003. 분석결과 데이터/diag_name.csv", row.names = FALSE, fileEncoding = "UTF-8")


# 진단명별 현황
target = df %>%
  subset.data.frame(select = c(diagnosis)) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  data.frame()

# 진단명 환자 수 통계
summary(target$Freq)

# 1시그마 구간 진단명
target %>%
  subset.data.frame(subset = c(Freq < mean(target$Freq)+2*sd(target$Freq) & Freq > mean(target$Freq)+sd(target$Freq))) %>%
  tail()

# 2시그마
target %>%
  subset.data.frame(subset = c(Freq > mean(target$Freq)+2*sd(target$Freq))) %>%
  head()

ggplot(target, aes(x = Freq))+
  geom_histogram(bins = 10)

result = data.frame()
for(i in 1:243){
  before = df %>%
    subset.data.frame(subset = c(diagnosis == target$.[i])) %>%
    mutate(tot = n()) %>%
    group_by(first_outcome, diagnosis) %>%
    summarise(n = n(),
              rate = n()/tot) %>%
    unique()
  
  result = rbind(result, before)
  print(i)
}

rate = result %>%
  subset.data.frame(subset = c(first_outcome ==1))

summary(rate$rate)

write.csv(rate[order(rate$rate, decreasing = TRUE),],
          "003. 분석결과 데이터/diag_result.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


# 합병증에 따른 노출시간(andur) 차이
summary(df$andur)

df %>%
  ggplot(aes(x = andur, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(df, aes(x = first_outcome)) +
  geom_boxplot(aes(y = andur))

# 정규성 검정
target_a = df %>%
  subset.data.frame(first_outcome == 1, andur)
target_b = df %>%
  subset.data.frame(first_outcome == 0, andur)

ks.test(target_a$andur, target_b$andur)

# 등분산성 검정
leveneTest(andur ~ first_outcome, df)

# t검정
t.test(andur ~ first_outcome,
       data = df,
       conf.level = 0.99)

# 합병증에 따른 예상노출시간(expd) 차이
summary(df$expd)

df %>%
  ggplot(aes(x = expd, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(df, aes(x = first_outcome)) +
  geom_boxplot(aes(y = expd))

# 정규성 검정
target_a = df %>%
  subset.data.frame(first_outcome == 1, expd)
target_b = df %>%
  subset.data.frame(first_outcome == 0, expd)

ks.test(target_a$expd, target_b$expd)

# 등분산성 검정
leveneTest(expd ~ first_outcome, df)

# t검정
t.test(expd ~ first_outcome,
       data = df,
       conf.level = 0.99)
