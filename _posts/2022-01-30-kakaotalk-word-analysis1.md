---
title: "카카오톡 단어 분석1 (단어 구름, 상관관계)"
last_modified_at: 2022-01-30
categories: [텍스트 마이닝, 단어 분석]
tag: [카카오톡, 전처리, 단어 분석, 단어 구름, 단어 상관관계, tidyverse, tm, RHINO]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 단어 분석1"
---
<div class="notice--success">
카카오톡으로 오고간 대화의 단어를 분석합니다. 빈도수와 단어간 상관관계를 살펴봅니다.
</div>

## 미션 이해

사람들이 가장 많이 사용한 단어는 무엇인지 알아보고 그 단어들과 함께
사용한 단어는 무엇인지 단어간 상관관계를 통해 살펴봅니다. tm 패키지를
이용하여 dtm(document term matrix)를 만들어 단어 빈도수와 단어간
상관관계를 살펴봅시다.

## 최종 결과 확인

### 형태소 분석하기

``` r
pacman::p_load(scales, ggthemes, ggridges,                      # 시각화 관련 패키지
               forecast,                                        # 시계열 예측 관련 패키지
               RHINO, tm, RWeka, tidytext, tidystm,             # 텍스트 마이닝
               igraph, ggraph, tidygraph, wordcloud2,           # 텍스트 마이닝 시각화
               tidymodels, textrecipes, LiblineaR, themis,      # 머신러닝
               lubridate, magrittr, tidyverse)                  # 데이터 전처리 관련 패키지

initRhino()

rdata <- read_file("../data/KakaoTalkChats.txt") %>%                         # txt 파일 읽어오기
    strsplit("\r") %>% unlist() %>%                                          # 같은 사람의 글은 한 줄로
    gsub("\n", "", .) %>% as_tibble() %>%                                    # 줄바꿈 없애기
    filter(grepl("^\\d.*,.*:", value)) %>%                                   # 숫자시작 , : 있는 것만
    separate(value, into=c("date", "text"), sep=", ", extra="merge") %>%     # 날짜와 글 분리
    separate(text, into=c("name", "comment"), sep=" : ", extra="merge")      # 이름과 글 내용 분리

data <- rdata %>% 
    mutate(date=gsub("년 ", "-", gsub("월 ", "-", gsub("일 ", " ", date)))) %>%
    mutate(date=gsub("오전", "AM", gsub("오후", "PM", date))) %>%
    mutate(date=parse_date_time(date, c("%Y-%m-%d %p %H:%M"))) %>%      # 날짜 형식으로
    mutate(year=year(date), quarter=quarter(date), month=month(date),   # 년, 분기, 월 변수 만들기
           wday=weekdays(date), yday=yday(date), hour=hour(date),       # 요일, 일수, 시간 변수 만들기
           ampm=ifelse(hour(date)<12, "AM", "PM")) %>%                  # 오전 오후 변수 만들기
    select(year:ampm, name, comment) %>%                                # 주요 변수 선택
    mutate(형태소=comment %>% sapply(getMorph, "NV") %>%                # 명사, 동사, 형용사만 선택
                  sapply(paste, collapse=" "))                          # 형태소 분석 결과 합치기
```

rdata는 전처리가 거의 없는 데이터이고 data는 EDA나 머신러닝, 텍스트
마이닝 등에 사용될 범용적인 데이터입니다. data 파일의 comment의 글을
형태소 분석하여 “형태소”라는 변수를 추가로 만듭니다. 형태소는 명사,
동사, 형용사에 해당하는 것만 수집하며 나중에 토픽 분석을 쉽게 할 수
있도록 tibble 형태로 유지합니다.

getMorph() 함수는 RHINO 패키지에서 지원하는 형태소 분석 함수입니다.
“NV”는 명사와 동사, 형용사를 의미합니다. 분석결과는 list 형태로 출력이
되는데 이를 paste() 함수를 사용해서 공백을 추가하여 하나의 vector로
만들어서 “형태소”라는 변수로 저장합니다.

### dtm 만들기

``` r
inspect(word_dtm <- data$형태소 %>% na.omit() %>%                       # 결측치 제거
          VectorSource() %>% VCorpus() %>%                              # 말뭉치(corpus) 만들기
          DocumentTermMatrix(control=list(wordLengths=c(2,Inf))))       # 2음절 이상만 선택
```

    ## <<DocumentTermMatrix (documents: 28840, terms: 15273)>>
    ## Non-/sparse entries: 155190/440318130
    ## Sparsity           : 100%
    ## Maximal term length: 10
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##        Terms
    ## Docs    ai 감사 데이터 드리 만들 발표 사람 사진 생각 프렌즈
    ##   12402  0    0      0    0    3    0    3    0    0      0
    ##   13660  1    2      0    0    0    3    5    0    0      0
    ##   13825  0    0      0    0    0    1    0    2    0      0
    ##   13920  0    0      0    0    0    3    0    0    0      0
    ##   14354  1    0      0    0    0    2    1    0    0      0
    ##   14372  1    0      0    0    0    0    1    0    0      0
    ##   14401  2    0      0    0    0    0    1    0    0      0
    ##   14753  0    0      0    0    0    1    2    1    0      0
    ##   14781  0    0      0    0    0    2    1    0    0      0
    ##   6778   8    0      1    1    0    1    0    0    0      1

단어 빈도수를 뽑아내는 방법은 많지만 단어간 상관관계까지 구하려면 dtm을
만들어야 합니다. 변수명은 word\_dtm으로 정하였습니다.

VectorSource() 함수와 VCorpus() 함수는 tm 패키지에서 지원하는 함수로
한글 말뭉치를 만들 때 사용합니다. DocumentTermMatrix() 함수는 말 그대로
단어와 문서를 표현하는 행렬로 만들어줍니다. wordLengths 파라미터를
이용하여 2음절 이상을 선택하였습니다. c(2, Inf)가 2음절 이상 선택한다는
뜻입니다.

### 단어 빈도수 시각화하기

``` r
t(word_dtm) %>% tidy() %>% group_by(term) %>% summarise(Freq=sum(count)) %>% 
    #filter(term!="관하", term!="대하") %>%        
    arrange(desc(Freq)) %>% head(200) %T>% print() %>% # write.csv("단어 빈도수.csv", row.names=F)
    wordcloud2(size=0.4, color="random-dark") 
```

    ## # A tibble: 200 x 2
    ##    term    Freq
    ##    <chr>  <dbl>
    ##  1 ai      2467
    ##  2 사진    1913
    ##  3 데이터  1733
    ##  4 감사    1723
    ##  5 드리    1249
    ##  6 만들    1024
    ##  7 생각     831
    ##  8 사람     774
    ##  9 발표     765
    ## 10 프렌즈   755
    ## # ... with 190 more rows

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-30-kakaotalk-word-analysis1_files/figure-gfm/train_and_test-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

빈도수가 높은 단어 200개를 선택해서 단어 구름(word cloud)를 만듭니다.
빈도수가 많은 단어는 큰 글자로 빈도수가 적은 단어는 작은 글자로 표현하는
것이 단어 구름입니다.

t() 함수는 dtm(document term matrix)을 tdm(term document matrix)로
만들어 줍니다. tidytext 패키지에서 지원하는 tidy() 함수는 문서별 단어의
빈도수를 끄집어 냅니다. summarise() 함수를 써서 문서를 무시하고 전체
단어의 빈도수를 더합니다.

“\#”으로 주석처리한 부분은 특정 단어를 제외할 때 사용할 수 있습니다.

arrange() 함수를 써서 정렬하는데 desc()를 사용하면 빈도수가 가장 높은
것이 첫 번째 행에 놓이게 되고 그 아래로 빈도수가 낮은 단어들이 정렬하게
됩니다. head() 함수를 사용하여 상위 200개 단어만 남깁니다. 대략 200개
정도 될 때 그럴듯하게 보입니다.

“%T&gt;%”는 T 연산자라고 부르는데 magrittr 패키지에서 지원해줍니다. T
연산자 앞까지의 데이터를 기반으로 2가지 결과를 출력할 때 사용하면
편리합니다. 일반적으로 “%&gt;%” 연산자의 경우 계속해서 실행결과가 다음
함수로 연계가 되는데 T 연산자는 T 연산자 앞까지의 결과만 그 뒤로
넘깁니다. 그러니까 200개의 단어를 print() 하고 wordcloud2() 함수를
실행하라는 뜻입니다. 결국 선정된 200개의 데이터를 화면으로 출력하고
동시에 단어 구름(word cloud)를 만들 수 있습니다. print() 함수 뒤에
“\#”으로 주석처리한 부분도 있습니다. 간혹 한글이나 파워포인트에 단어
빈도수를 표기해야할 때도 있는데 이 경우 csv 파일로 만들어서 사용하면
편할 수 있습니다. T 연산자는 단 2개의 함수만 처리할 수 있기 때문에
화면에 출력하고 csv 파일로 만들거나 화면에 출력하고 글자 구름을 만들 수
있습니다. 물론 csv 파일로 저장하면서 글자 구름을 만들 수도 있습니다.

wordcloud2() 함수는 wordcloud2 패키지에서 지원하는 글자 구름 만드는
함수입니다. size는 적당히 수정하여 보기 편한 크기로 만들 때 사용합니다.
color를 “random-dark”로 지정하여 글자 색이 무작위적으로 정해지도록
만들면 꽤 그럴듯하게 보입니다.

### 단어간 상관관계 시각화

``` r
word_tdm <- as.matrix(t(word_dtm))[order(rowSums(as.matrix(t(word_dtm))), decreasing=T)[c(1:20)],] 
word_group <- graph.adjacency(word_tdm %*% t(word_tdm), weighted=T, mode="undirected") %>% 
        simplify() %>%  cluster_louvain() %>% membership()
word_tdm %*% t(word_tdm) %>% 
    qgraph::qgraph(labels=rownames(.), diag=F, layout='spring', 
                   color=rainbow(max(word_group), alpha=0.7)[word_group],
                   vsize=log(diag(.))*1.5, label.cex=1.4) 
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-30-kakaotalk-word-analysis1_files/figure-gfm/post_hoc-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

빈도수가 높은 상위 20개 단어만 선택해서 이들 단어들 사이의 상관관계를
그룹화하여 시각화하였습니다.

첫번째 줄은 빈도수가 높은 상위 20개 단어만 선택해서 tdm 문서로 만드는
내용입니다.

graph.adjacency() 함수를 써서 단어간 상관관계를 인접행렬로 만들어서
simplify(), cluster\_louvain(), membership() 함수를 통해 최대한 간단한
구조의 단어별 군집을 결정합니다.

동시출현행렬을 만들어 qgraph 패키지에서 지원하는 qgraph() 함수로
네트워크 형태로 시각화합니다. qgraph() 함수는 다른 패키지에서도 사용하기
때문에 반드시 qgraph 패키지에서 지원하는 함수를 적용하라는 뜻으로
qgraph::qgrap()로 표현하였습니다. 파라미터 labels는 단어를 가리키며
diag를 FALSE로 지정해서 시각화를 최대한 단순화시킵니다. layout은
‘spring’ 형태로 만들어서 적절하게 단어들이 퍼져있게 만듭니다. color는
앞에서 지정한 군집별로 무지개 색으로 표현합니다. 군집이 몇 개인지 모르기
때문에 max() 함수를 써서 최대값을 알아낸 후 군집 수에 해당 되는 색을
선택하게 됩니다. alpha를 지정하여 색이 투명한 정도를 지정할 수 있습니다.
vsize에 있는 숫자값을 바꾸면 단어를 표현하는 원의 크기를 조정할 수 있고
label.cex 값을 바꾸어 글자 크기를 조정할 수 있습니다. 적절한 크기로 보일
수 있도록 숫자를 조정하는 것이 좋습니다.

## 예고

다음 글에서는 PCA 분석을 통해 단어들 사이의 거리를 비교하고 연속된 두
단어를 이용해서 네트워크 분석을 실시하도록 하겠습니다.
