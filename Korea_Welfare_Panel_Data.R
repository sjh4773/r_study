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

class(welfare$sex)

table(welfare$sex)


welfare$sex <- ifelse(welfare$sex == 9, NA,
                      welfare$sex) #이상치 결측 처리

table(is.na(welfare$sex))
