library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "c:/R/Data/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

welfare <- raw_welfare

welfare <- rename(welfare,                 # 성별
                  sex = h10_g3,            # 태어난 연도
                  birth = h10_g4,          # 혼인 상태
                  marriage = h10_g10,      # 종교
                  religion = h10_g11,      # 월급
                  income = p1002_8aq1,     # 직업 코드
                  code_job = h10_eco9,     # 직업 코드
                  code_region = h10_reg7)  # 지역 코드

# 성별 변수의 전처리 작업

class(welfare$sex)

table(welfare$sex)

# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA,
                      welfare$sex)

# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)


# (성별에 따른 월급 차이 - "성별에 따라 월급이 다를까?")

# [월급 변수의 전처리 작업]

# numeric type
class(welfare$income)

# 0 ~ 2400만원 사이의 값, 122~316만원 사이에서 가장 많이 분포
summary(welfare$income)


# 그래프상 x축이 2500까지 있어서
# 대다수를 차지하는 0~1000 사이의 데이터가 잘표현되지 않는다.
# 따라서 xlim()을 이용해 0~1000까지만 표현되게 설정
qplot(welfare$income) + xlim(0, 1000)

# 이상치 확인
summary(welfare$income)

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA,
welfare$income)

# 결측치 확인
table(is.na(welfare$income))

# [성별 월급 평균표 만들기]

# 성별에 따른 평균 임금값 도출
sex_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(sex) %>% summarise(mean_income = mean(income))

#결과값 출력
sex_income

# 결과값 막대그래프 출력
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# (나이와 월급의 관계 - "몇 살 때 월급을 가장 많이 받을까?")

# type 확인
class(welfare$birth)

# 이상치 확인
# 정상 범위 1900~2014, 모름/무응답 9999
summary(welfare$birth)

# 결측치 확인
table(is.na(welfare$birth))

# 이상치 결측처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)

# 파생변수 만들기 - 나이
# 태어난 연도 변수를 이용해 나이 변수를 생성
# 2015년도에 조사가 진행됐으므로, 나이 = 2015 - 태어난 연도 + 1
welfare$age <- 2015 - welfare$birth + 1

# 특징 파악
summary(welfare$age)
qplot(welfare$age)

# 나이에 따른 월급표
age_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(age) %>% summarise(mean_income = mean(income))

head(age_income)

# 나이에 따른 월급의 변화
ggplot(data = age_income, aes(x = age, y= mean_income)) + geom_line()


# (연령대에 따른 월급 차이 - "어떤 연령대의 월급이 가장 많을까?")


# 앞에서 만든 나이 변수를 이용하여 연령대 변수 생성
welfare <- welfare %>% mutate(ageg = ifelse(welfare$age < 30, "young",
                                     ifelse(welfare$age < 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)

# 나이대별 평균 월급
ageg_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(ageg) %>% summarise(mean_income = mean(income))

ageg_income

# 그래프로 나타낸 결과
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))


# (연령대 및 성별 월급 차이 - "성별 월급 차이는 연령대별로 다를까?")

# 연령대 및 성별 월급 평균표
sex_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(ageg, sex) %>% summarise(mean_income = mean(income))

sex_income

# 그래프로 나타낸 결과
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +      # "dodge -> 성별 막대 분리
  scale_x_discrete(limits = c("young", "middle", "old")) # 연령대 순 정렬

# 결과 : 성별로 나눈 이번 분석 결과를 보면 남성의 경우 노년과 초년 간 월급
#       차이가 크지 않으며 노년이 초년보다 적은 월급을 받는 현상은 여성에게만
#       나타나고 있음을 알 수 있다. 또한 초년보다 중년이 더 많은 월급을 받는
#       현상 또한 주로 남성에게 나타나고 여성은 큰 차이가 없음을 알 수 있음.


# 나이 및 성별 월급 차이 분석하기

sex_age <- welfare %>% filter(!is.na(income)) %>%
  group_by(age, sex) %>% summarise(mean_income = mean(income))

sex_age

# 그래프로 나타낸 결과
ggplot(data = sex_age, aes(x = age, y = mean_income, fill = sex)) +
  geom_line()

# 결과 : 성별 월급 격차는 30세부터 지속적으로 벌어져 50대 초반에 가장 크게 벌어지고,
#       이후로 점차 줄어들어 70대 후반이 되면 비슷한 수준이 되는걸 알 수 있다.


# (직업별 월급 차이 - "어떤 직업이 월급을 가장 많이 받을까?")

class(welfare$code_job)

table(welfare$code_job)

# 엑셀 파일을 불러오기 위한 패키지
library(readxl)

# 첫 행을 변수명으로 가져오고, 엑셀 파일의 두 번째 시트에 있는
# 직업분류코드 목록을 불러온다.
list_job <- read_excel("c:/R/Data/Koweps_Codebook.xlsx",
                       col_names = T, sheet = 2)
head(list_job)

dim(list_job) # 149 * 2

# welfare와 list_job에 공통으로 들어있는
# code_job 변수를 기준으로 결합 -> left_join() 사용
welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>% filter(!is.na(code_job)) %>%
  select(code_job, job) %>% head(10)

# 직업별 월급 평균표
job_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(job) %>% summarise(mean_income = mean(income))

head(job_income)

# 월급이 높은 상위 10개 직군
top10 <- job_income %>% arrange(desc(mean_income)) %>% head(10)

top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

# 월급이 낮은 하위 10개 직군
bottom10 <- job_income %>% arrange(mean_income) %>% head(10)

bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income) , y = mean_income)) +
  geom_col() +
  coord_flip()

# (성별 직업 빈도 - "성별로 어떤 직업이 가장 많을까?")

# 남성 직업 빈도 상위 10개 추출
job_male <- welfare %>% filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(10)

job_male

# 남성 직업 빈도 상위 10개 직업 그래프
ggplot(data = job_male, aes(x = reorder(job, n), y = n) ) +
  geom_col() +
  coord_flip()



# 여성 직업 빈도 상위 10개 추출
job_female <- welfare %>% filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(10)

job_female

# 여성 직업 빈도 상위 10개 직업 그래프
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()



# (종교 유무에 따른 이혼율 - "종교가 있는 사람들이 이혼을 덜 할까?")

# 종교 변수 검토
class(welfare$religion)

table(welfare$religion)

qplot(welfare$religion)

# 혼인 상태 변수 검토
class(welfare$marriage)

table(welfare$marriage)

# 이혼 여부 변수 만들기
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))

table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)


# 종교 유무에 따른 이혼율 표
religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))

# 이혼 추출
divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)

divorce