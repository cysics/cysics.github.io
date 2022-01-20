---
title: "카카오톡 대화 전처리(2)"
last_modified_at: 2022-01-20
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
1차로 전처리된 데이터에서 분석에 필요한 각종 변수를 뽑아내는 방법을 적용합니다.
</div>
## 미션과 문제

년도, 월, 일, 요일, 오전 오후, 시간 등 데이터 분석에 필요한 각종 변수를 만들고자 합니다. 문제는 카톡에서 출력된 일시(2019년 2월 27일 오후 11:02) 데이터는 R에서 사용하는 일반적인 형태가 아닙니다. 따라서 전처리 과정을 통해 일반적인 형태의 날짜로 바꾸고 이를 바탕으로 년, 월, 일, 요일 등 다양한 변수를 만들어 내야 합니다.


## 최종 결과 확인

다음과 같이 데이터를 전처리하면 된다:

``` r
library(tidyverse)
rdata <- read_file("../data/KakaoTalkChatsSample.txt") %>%                # txt 파일 읽어오기
    strsplit("\r") %>% unlist() %>%                                       # 같은 사람의 글은 한 줄로
    gsub("\n", "", .) %>% as_tibble() %>%                                 # 줄바꿈 없애기
    filter(grepl("^\\d.*,.*:", value)) %>%                                # 숫자시작 , : 있는 것만
    separate(value, into=c("date", "text"), sep=", ", extra="merge") %>%  # 날짜와 글 분리
    separate(text, into=c("name", "coment"), sep=" : ", extra="merge")    # 이름과 글 내용 분리

library(lubridate)
(data <- rdata %>% 
    mutate(date=gsub("년 ", "-", gsub("월 ", "-", gsub("일 ", " ", date)))) %>%  # 년월일 대체 및 삭제
    mutate(ampm=ifelse(grepl("오전", date), "AM", "PM")) %>%                     # 오전 오후 변수 만들기
    mutate(date=as.Date(date)) %>%                                               # 날짜 형태로 만들기
    mutate(year=year(date)) %>%                                                  # 년도 변수 만들기
    mutate(month=month(date)) %>%                                                # 달 변수 만들기
    mutate(wday=weekdays(date)) %>%                                              # 요일 변수 만들기
    mutate(day=ifelse(year=="2020", 365+yday(date), 
                      ifelse(year=="2021", 730+yday(date), yday(date)))))        # 연속된 날 수
```

    ## # A tibble: 12 x 8
    ##    date       name  coment                         ampm   year month wday    day
    ##    <date>     <chr> <chr>                          <chr> <dbl> <dbl> <chr> <dbl>
    ##  1 2019-02-27 ◇◇◇   오늘 강연 고맙게 잘~ 들었습니  PM     2019     2 수요일~    58
    ##  2 2019-02-27 □□□   (굿)                            PM     2019     2 수요일~    58
    ##  3 2019-02-28 □□□   사진                            PM     2019     2 목요일~    59
    ##  4 2019-02-28 □□□   저희도 동참합니다 ^^             PM     2019     2 목요일~    59
    ##  5 2019-02-28 ▣▣▣   (최고)                        PM     2019     2 목요일~    59
    ##  6 2019-03-01 ◁◁◁   사진                          AM     2019     3 금요일~    60
    ##  7 2019-03-01 ◁◁◁   Deepfake와 포토샵으로 합성된   AM     2019     3 금요일~    60
    ##  8 2019-03-01 □□□   이모티콘                         PM     2019     3 금요일~    60
    ##  9 2019-03-01 □□□   사진                             PM     2019     3 금요일~    60
    ## 10 2019-03-01 ◁◁◁   와!                           PM     2019     3 금요일~    60
    ## 11 2019-03-02 △△△   https://www.youtube.com/watch PM     2019     3 토요일~    61
    ## 12 2019-03-02 △△△   R에서 10줄로 구현한 keras입니  PM     2019     3 토요일~    61

## 코드 설명

### 예고

다음 글에서는 발언수가 많은 상위 10명, 요일별 발언수, 오전 오후의 발언수
차이 등 카톡 방에서 주고 받은 대화들과 관련된 각종 시각화 자료를 얻어
보겠습니다.
