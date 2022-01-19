---
title: "카카오톡 대화 전처리(1)"
---
김종헌
2022-01-18

## 데이터 다운로드

데스크탑 컴퓨터에서도 카카오톡 대화를 다운 받을 수 있다. 하지만 데이터 전처리를 쉽게 하려면 스마트폰에서 텍스트를 내보내는 것이 좋다.  
![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-18-kakaotalk-preprocessing_files/figure-gfm/kakaotalk.jpg){:style="display:block; margin-left:auto; margin-right:auto"}
스마트폰에서 카톡방의 맨 오른쪽 위(빨간색)을 터치한 후 오른쪽 맨 아래쪽에 설정(초록색)을 터치한다. 중간정도 보면 대화내용 내보내기(노란색)를 터치한 후 텍스트만 보내기(보라색)을 터치하면 위 그림의 오른쪽과 같이 어떤 형태로 텍스트를 내보낼지 선택할 수 있다. 본인이 원하는 방법(예, 갈색)를 이용하여 텍스트 파일을 받을 수 있다.

## 날짜, 유저, 내용 형태로 만들기

다음과 같은 형태의 데이터를 만들어야 한다:

``` r
library(tidyverse)
(rdata <- read_file("../KakaoTalkChatsSample.txt") %>%                    # txt 파일 읽어오기
    strsplit("\r") %>% unlist() %>%                                       # 같은 사람의 글은 한 줄로
    gsub("\n", "", .) %>% as_tibble() %>%                                 # 줄바꿈 없애기
    filter(grepl("^\\d.*,.*:", value)) %>%                                # 숫자시작 , : 있는 것만
    separate(value, into=c("date", "text"), sep=", ", extra="merge") %>%  # 날짜와 글 분리
    separate(text, into=c("name", "coment"), sep=" : ", extra="merge"))   # 이름과 글 내용 분리
```

    ## # A tibble: 15 x 3
    ##    date                       name  coment                                      
    ##    <chr>                      <chr> <chr>                                       
    ##  1 2019년 2월 27일 오후 11:02 ◇◇◇   오늘 강연 고맙게 잘~ 들었습니다 김OO 교수님~
    ##  2 2019년 2월 27일 오후 11:02 □□□   (굿)                                        
    ##  3 2019년 2월 28일 오후 7:08  □□□   사진                                        
    ##  4 2019년 2월 28일 오후 7:08  □□□   저희도 동참합니다 ^^                        
    ##  5 2019년 2월 28일 오후 8:57  ▣▣▣   (최고)                                      
    ##  6 2019년 3월 1일 오전 9:49   ◁◁◁   사진                                        
    ##  7 2019년 3월 1일 오전 9:50   ◁◁◁   Deepfake와 포토샵으로 합성된 사진을 인공지~ 
    ##  8 2019년 3월 1일 오후 1:26   □□□   이모티콘                                    
    ##  9 2019년 3월 1일 오후 1:26   □□□   사진                                        
    ## 10 2019년 3월 1일 오후 2:06   ◁◁◁   와!                                         
    ## 11 2019년 3월 2일 오후 8:22   △△△   https://www.youtube.com/watch?v=Iua-Z36J80A 
    ## 12 2019년 3월 2일 오후 8:22   △△△   R에서 10줄로 구현한 keras입니다. ^^         
    ## 13 2019년 3월 2일 오후 8:24   □□□   오 신기하네요 R에서 keras 라이브러리를 불러~
    ## 14 2019년 3월 2일 오후 8:26   △△△   넵.. 몇 년 전부터 가능했던 것 같은데 저는 ~ 
    ## 15 2019년 3월 2일 오후 9:15   □□□   학술세미나 안내주제 1 Physics Induced Graph~
