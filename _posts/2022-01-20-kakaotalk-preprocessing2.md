---
title: "카카오톡 대화 전처리(2)"
last_modified_at: 2022-01-21
categories: TextMining
tag: [카카오톡, 전처리, tidyverse, lubridate]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 대화 전처리(2)"
---
<div class="notice--success">
1차로 전처리된 데이터에서 날짜와 관련된 각종 변수를 뽑아내는 방법을 적용합니다.
</div>

## 미션 이해

년도, 월, 일, 요일, 오전 오후, 시간 등 데이터 분석에 필요한 각종 변수를 만들고자 합니다. 이런 변수가 있으면 년도별, 혹은 월별 발언(coment)수를 비교해서 카톡방이 얼마나 활성화 되고 있는지를 알 수 있습니다. 요일별 발언수를 살펴서 어떤 요일에 대화를 많이 주고 받고 어떤 요일에는 대화가 거의 없는지 알 수도 있습니다. 시간별 대화량을 비교할 수도 있고요. 텍스트 마이닝을 할 때 오전과 오후의 토픽이 통계적으로 유의하게 차이가 있는지의 여부도 알 수 있습니다. 이런 다양한 변수를 기준으로 데이터를 분석할 수 있기 때문에 날짜로부터 다양한 변수를 뽑아내는 것입니다.

## 최종 결과 확인

다음과 같이 데이터를 전처리하면 됩니다.

``` r
library(tidyverse)
library(lubridate)
(data <- rdata %>% 
    # mutate(date=gsub("년 ", "-", gsub("월 ", "-", gsub("일 ", " ", date)))) %>% 
    # mutate(date=gsub("오전", "AM", gsub("오후", "PM", date))) %>% 
    mutate(date=parse_date_time(date, c("%Y-%m-%d %p %H:%M"))) %>%      # 날짜 형식으로
    mutate(year=year(date), quarter=quarter(date), month=month(date),   # 년, 분기, 월 변수 만들기
           wday=weekdays(date), yday=yday(date), hour=hour(date),       # 요일, 일수, 시간 변수 만들기
           ampm=ifelse(hour(date)<12, "AM", "PM")) %>%                  # 오전 오후 변수 만들기
    select(year:ampm, name, coment))
```

    ## # A tibble: 12 x 9
    ##     year quarter month wday    yday  hour ampm  name  coment                    
    ##    <dbl>   <int> <dbl> <chr>  <dbl> <int> <chr> <chr> <chr>                     
    ##  1  2019       1     2 수요일    58    11 AM    ◇◇◇   오늘 강연 고맙게 잘~ 들었~
    ##  2  2019       1     2 수요일    58    11 AM    □□□   (굿)                      
    ##  3  2019       1     2 목요일    59     7 AM    □□□   사진                      
    ##  4  2019       1     2 목요일    59     7 AM    □□□   저희도 동참합니다 ^^      
    ##  5  2019       1     2 목요일    59     8 AM    ▣▣▣   (최고)                    
    ##  6  2019       1     3 금요일    60     9 AM    ◁◁◁   사진                      
    ##  7  2019       1     3 금요일    60     9 AM    ◁◁◁   Deepfake와 포토샵으로 합~ 
    ##  8  2019       1     3 금요일    60     1 AM    □□□   이모티콘                  
    ##  9  2019       1     3 금요일    60     1 AM    □□□   사진                      
    ## 10  2019       1     3 금요일    60     2 AM    ◁◁◁   와!                       
    ## 11  2019       1     3 토요일    61     8 AM    △△△   https://www.youtube.com/w~
    ## 12  2019       1     3 토요일    61     8 AM    △△△   R에서 10줄로 구현한 keras~

## 코드 설명

### 날짜 형식으로 만들기

문자(character)로 되어 있는 date를 년, 월, 요일 등을 쉽게 뽑아낼 수 있는 날짜 형식으로 바꾸어야 합니다. lubridate 패키지에서 지원하는 parse\_date\_time() 함수를 사용하여 원하는 날짜 형식으로 바꿀 수 있습니다. Y는 년도, m은 월, d는 일, p는 오전 오후, H는 시간, M은 분을 의미합니다. 만약 초까지 포함하고 있는 데이터가 있다면 S를 추가해 주어야 합니다. 주석 처리된 2줄이 있습니다. 데스크탑에 R과 Rstudio를 설치하여 분석하는 경우 없어도 되는 코드인데, Rstudio Cloud에서 분석할 때는 필요한 코드입니다. 데이터에 있는 한글은 제대로 인식하는데 날짜에 있는 한글은 인식을 못하는 것 같아요. 강제로 한글을 영문으로 바꾸어 주어야 날짜 형식으로 변환이 되네요. 추후에 이 부분도 해결이 될 것 같긴 합니다만 임시로 코드를 추가했습니다.

### 날짜 관련 변수 만들기

날짜 형식으로 만들어진 date에 year() 함수를 적용하면 년도를 얻어낼 수 있습니다. 같은 방법으로 다양한 변수들을 만들 수 있는데, quarter는 분기, month는 월, weekdays는 요일, yday는 1월 1일을 1로 해서 12월 31일을 365로하는 1년 중 몇 번째 날인지의 정보를 출력합니다. hour는 시간입니다. ampm은 ifelse() 함수를 사용해서 시간이 12시보다 작으면 오전(AM), 그렇지 않으면 오후(PM)으로 표시하도록 하였습니다.

### 변수 순서 정렬

select() 함수를 이용해서 year부터 ampm까지, 그리고 그 뒤를 이어 name과 coment가 오도록 순서를 정렬했습니다.

### 예고

다음 글에서는 발언수가 많은 상위 10명, 요일별 발언수, 오전 오후의 발언수 차이 등 카톡 방에서 주고 받은 대화들과 관련된 각종 시각화 자료를 얻어 보겠습니다. 정식 텍스트 마이닝은 아직 아닙니다. DEA(Exploratory Data Analysis, 탐색적 데이터 분석)라고 해서 데이터를 이해하는 과정에 해당됩니다. 이 분석이 끝나고난 후 본격적으로 단어 빈도수나 상관관계, 토픽 분석 등이 진행될 예정입니다.
