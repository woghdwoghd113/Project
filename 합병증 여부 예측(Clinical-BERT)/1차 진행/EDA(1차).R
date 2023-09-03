library(car)
library(readxl)
library(tidyverse)

# read xlsx file
df = read_xlsx("C:/Users/LG1/Desktop/work/90. 의료데이터 분석/001. 데이터/full_data_220812.xlsx",
               col_types = c("text", "numeric", "text", "text", "numeric", "text",
                             "numeric", "numeric", "numeric", "text", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                             "numeric", "text", "text", "text"))
head(df)
tail(df)

# Do not use asa, second outcome
# 데이터 수 : 총 186,296건
# 컬럼 수 : asa, second outcome 포함 28개
nrow(df) == length(unique(df$caseid))
length(colnames(df))

# 미활용 칼럼 제거
target = df %>% subset.data.frame(select = c(-asa, -second_outcome, -al))
head(target)



# 연속형 변수 요약

target %>%
  subset.data.frame(select = c(-caseid, -first_outcome, -sex, -atype, -dept, -name, -diagnosis)) %>%
  summary()

target %>%
  subset.data.frame(is.na(gl)==T) %>%
  mutate(total = n()) %>%
  group_by(first_outcome) %>%
  summarise(num = n(),
            rate = n()/total) %>%
  unique()

# 상관관계
target %>%
  subset.data.frame(select = c(-caseid, -first_outcome, -sex, -atype, -dept, -name, -diagnosis, -age_grp, -wt_grp, -bmi_grp)) %>%
  na.omit() %>%
  cor() %>%
  corrplot::corrplot("number")

# 색소, 용적률 상관관계
target %>%
  ggplot(aes(x = hem, y = hc)) +
  geom_point()

# 기능검사1, 2 상관관계
target %>%
  ggplot(aes(x = got, y = gpt)) +
  geom_point()

# 질소, 혈장 상관관계
target %>%
  ggplot(aes(x = bu, y = cr)) +
  geom_point()

# 응고인자1, 기능검사3 상관관계
target %>%
  ggplot(aes(x = `in`, y = tb)) +
  geom_point()


# 성별 현황
target %>% 
  group_by(sex) %>%
  summarise(num = n(),
            rate = n()/nrow(target)*100)
# 남성 : 99,819명(53.6%)
# 여성 : 86,477명(46.4%)


# 연령대별 현황
target = target %>%
  mutate(
    age_grp = case_when(
      age < 30 ~ "20대 이하",
      age >= 30 & age < 40 ~ "30대",
      age >= 40 & age < 50 ~ "40대",
      age >= 50 & age < 60 ~ "50대",
      age >= 60 & age < 70 ~ "60대",
      age >= 70 & age < 80 ~ "70대",
      age >= 80 ~ "80대 이상"
      )
    )

target %>%
  group_by(age_grp) %>%
  summarise(num = n(),
            round(n()/nrow(target)*100, 2))
# 20대 이하 :      12,581명
# 30대      :      14,600명
# 40대      :      24,134명
# 50대      :      41,680명
# 60대      :      50,427명
# 70대      :      35,283명
# 80대 이상 :       7,591명

# 시술 현황
target %>% 
  group_by(sex) %>%
  mutate(total = n()) %>%
  group_by(sex, atype) %>%
  summarise(num = n(),
            rate = n()/total) %>% unique()

# 일반 시술 : 163,901명
# 특수 시술 :  22,385명
# 남성 : 99,819명(53.6%)
# 여성 : 86,477명(46.4%)

target %>% 
  group_by(age_grp) %>%
  mutate(total = n()) %>%
  group_by(age_grp, atype) %>%
  summarise(num = n(),
            rate = n()/total) %>%
  unique() %>%
  filter(atype == 1) %>%
  write.csv("c:/Users/LG1/Desktop/work/90. 의료데이터 분석/result_3.csv")


# 진료과 현황
target %>%
  group_by(dept) %>%
  summarise(num = n(),
            rate = round(n()/nrow(target)*100, 2)) %>%
  arrange(desc(num)) %>%
  write.csv("c:/Users/LG1/Desktop/work/90. 의료데이터 분석/result_2.csv")

# 진료과
# GS    : 60,105명
# OS    : 42,551명
# TS    : 26,720명
# NS    : 23,819명
# UR    : 18,670명
# OG    :  6,048명
# OL    :  4,760명
# PS    :  2,925명
# OT    :    461명
# Others:    233명
# GY    :      4명


# 합병증 발생 현황
# y value = first_outcome(합병증)
target %>%
  group_by(first_outcome) %>%
  summarise(num = n(),
            rate = n()/nrow(target)*100)
# 합병증이 있는 환자 :  12,778명(6.9%)
# 합병증이 없는 환자 : 173,518명(93.1%)

# 성별 연령대별 합병증 발생 현황 
target %>%
  group_by(first_outcome, sex, age_grp) %>%
  summarise(num = n()) %>%
  write.csv("c:/Users/LG1/Desktop/work/90. 의료데이터 분석/result_1.csv")

# 진료과별 합병증 발생 현황
target %>%
  group_by(dept) %>%
  mutate(total = n()) %>%
  group_by(dept, first_outcome) %>%
  summarise(total = total,
            num = n(),
            rate = round(n()/total*100, 2)) %>%
  unique() %>%
  arrange(desc(total)) %>%
  filter(first_outcome == 1)


# 체중별 현황
target = target %>%
  mutate(
    wt_grp = case_when(
      wt < 40 ~ "40 미만",
      wt >= 40 & wt < 50 ~ "40 ~ 49",
      wt >= 50 & wt < 60 ~ "50 ~ 59",
      wt >= 60 & wt < 70 ~ "60 ~ 69",
      wt >= 70 & wt < 80 ~ "70 ~ 79",
      wt >= 80 & wt < 90 ~ "80 ~ 89",
      wt >= 90 ~ "90 이상",
    )
  )

target %>%
  group_by(wt_grp, sex) %>%
  mutate(total = n()) %>%
  group_by(wt_grp, sex, first_outcome) %>%
  summarise(num = n(),
            round(n()/total*100, 2)) %>%
  unique() %>%
  arrange() %>%
  subset.data.frame(first_outcome == 1) %>%
  write.csv("c:/Users/LG1/Desktop/work/90. 의료데이터 분석/003. 분석결과 데이터/result_4.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 합병증에 따른 연령 차이
summary(target$age)

target %>%
  ggplot(aes(x = age, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())


ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = age)) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, age)
target_b = target %>%
  subset.data.frame(first_outcome == 0, age)

ks.test(target_a$age, target_b$age)

# 등분산성 검정
leveneTest(age ~ first_outcome, target)

# t검정
t.test(age ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 몸무게 차이
summary(target$wt)

target %>%
  ggplot(aes(x = wt, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = wt))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, wt)
target_b = target %>%
  subset.data.frame(first_outcome == 0, wt)

ks.test(target_a$wt, target_b$wt)

# 등분산성 검정
leveneTest(wt ~ first_outcome, target)

# t검정
t.test(wt ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 키 차이
summary(target$ht)

target %>%
  ggplot(aes(x = ht, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = ht))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, ht)
target_b = target %>%
  subset.data.frame(first_outcome == 0, ht)

ks.test(target_a$ht, target_b$ht)

# 등분산성 검정
leveneTest(ht ~ first_outcome, target)

# t검정
t.test(ht ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 BMI 차이
summary(target$bm)

target %>%
  ggplot(aes(x = bm, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = bm))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, bm)
target_b = target %>%
  subset.data.frame(first_outcome == 0, bm)

ks.test(target_a$bm, target_b$bm)

# 등분산성 검정
leveneTest(bm ~ first_outcome, target)

# t검정
t.test(bm ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 색소 차이
summary(target$hem)

target %>%
  ggplot(aes(x = hem, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = hem))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, hem)
target_b = target %>%
  subset.data.frame(first_outcome == 0, hem)

ks.test(target_a$hem, target_b$hem)

# 등분산성 검정
leveneTest(hem ~ first_outcome, target)

# t검정
t.test(hem ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 혈구 수치 차이
summary(target$wb)

target %>%
  ggplot(aes(x = wb, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = wb))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, wb)
target_b = target %>%
  subset.data.frame(first_outcome == 0, wb)

ks.test(target_a$wb, target_b$wb)

# 등분산성 검정
leveneTest(wb ~ first_outcome, target)

# t검정
t.test(wb ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 용적률 수치 차이
summary(target$hc)

target %>%
  ggplot(aes(x = hc, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = hc))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, hc)
target_b = target %>%
  subset.data.frame(first_outcome == 0, hc)

ks.test(target_a$hc, target_b$hc)

# 등분산성 검정
leveneTest(hc ~ first_outcome, target)

# t검정
t.test(hc ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 혈소판 수치 차이
summary(target$pl)

target %>%
  ggplot(aes(x = pl, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = pl))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, pl)
target_b = target %>%
  subset.data.frame(first_outcome == 0, pl)

ks.test(target_a$pl, target_b$pl)

# 등분산성 검정
leveneTest(pl ~ first_outcome, target)

# t검정
t.test(pl ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 질소 수치 차이
summary(target$bu)

target %>%
  ggplot(aes(x = bu, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = bu))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, bu)
target_b = target %>%
  subset.data.frame(first_outcome == 0, bu)

ks.test(target_a$bu, target_b$bu)

# 등분산성 검정
leveneTest(bu ~ first_outcome, target)

# t검정
t.test(bu ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 혈장 수치 차이
summary(target$cr)

target %>%
  ggplot(aes(x = cr, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = cr))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, cr)
target_b = target %>%
  subset.data.frame(first_outcome == 0, cr)

ks.test(target_a$cr, target_b$cr)

# 등분산성 검정
leveneTest(cr ~ first_outcome, target)

# t검정
t.test(cr ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 응고인자1 차이
summary(target$`in`)

target %>%
  ggplot(aes(x = `in`, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = `in`))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, `in`)
target_b = target %>%
  subset.data.frame(first_outcome == 0, `in`)

ks.test(target_a$`in`, target_b$`in`)

# 등분산성 검정
leveneTest(`in` ~ first_outcome, target)

# t검정
t.test(`in` ~ first_outcome,
       data = target,
       conf.level = 0.99)


# 합병증에 따른 응고인자2 차이
summary(target$ap)

target %>%
  ggplot(aes(x = ap, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = ap))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, ap)
target_b = target %>%
  subset.data.frame(first_outcome == 0, ap)

ks.test(target_a$ap, target_b$ap)

# 등분산성 검정
leveneTest(ap ~ first_outcome, target)

# t검정
t.test(ap ~ first_outcome,
       data = target,
       conf.level = 0.99)


# 합병증에 따른 응고인자3 차이
summary(target$tb)

target %>%
  ggplot(aes(x = tb, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = tb))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, tb)
target_b = target %>%
  subset.data.frame(first_outcome == 0, tb)

ks.test(target_a$tb, target_b$tb)

# 등분산성 검정
leveneTest(tb ~ first_outcome, target)

# t검정
t.test(tb ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 기능검사1 차이
summary(target$got)

target %>%
  ggplot(aes(x = got, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = got))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, got)
target_b = target %>%
  subset.data.frame(first_outcome == 0, got)

ks.test(target_a$got, target_b$got)

# 등분산성 검정
leveneTest(got ~ first_outcome, target)

# t검정
t.test(got ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 기능검사2 차이
summary(target$gpt)

target %>%
  ggplot(aes(x = gpt, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = gpt))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, gpt)
target_b = target %>%
  subset.data.frame(first_outcome == 0, gpt)

ks.test(target_a$gpt, target_b$gpt)

# 등분산성 검정
leveneTest(gpt ~ first_outcome, target)

# t검정
t.test(gpt ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 전해질1 차이
summary(target$sod)

target %>%
  ggplot(aes(x = sod, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = sod))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, sod)
target_b = target %>%
  subset.data.frame(first_outcome == 0, sod)

ks.test(target_a$sod, target_b$sod)

# 등분산성 검정
leveneTest(sod ~ first_outcome, target)

# t검정
t.test(sod ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 전해질2 차이
summary(target$pot)

target %>%
  ggplot(aes(x = pot, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = pot))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, pot)
target_b = target %>%
  subset.data.frame(first_outcome == 0, pot)

ks.test(target_a$pot, target_b$pot)

# 등분산성 검정
leveneTest(pot ~ first_outcome, target)

# t검정
t.test(pot ~ first_outcome,
       data = target,
       conf.level = 0.99)

# 합병증에 따른 당 수치 차이
summary(target$gl)

target %>%
  ggplot(aes(x = gl, color = first_outcome)) +
  geom_density() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line("black"),
        panel.grid.major.y = element_line("gray"),
        panel.grid.minor.y = element_line("gray"),
        panel.grid.major.x = element_blank())

ggplot(target, aes(x = first_outcome)) +
  geom_boxplot(aes(y = gl))

# 정규성 검정
target_a = target %>%
  subset.data.frame(first_outcome == 1, gl)
target_b = target %>%
  subset.data.frame(first_outcome == 0, gl)

ks.test(target_a$gl, target_b$gl)

# 등분산성 검정
leveneTest(gl ~ first_outcome, target)

# t검정
t.test(gl ~ first_outcome,
       data = target,
       conf.level = 0.99)
