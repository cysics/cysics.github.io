---
title: "카카오톡 토픽 분석1 (토픽수 결정 및 토픽 분석)"
last_modified_at: 2022-02-05
categories: [텍스트 마이닝, 토픽 분석]
tag: [카카오톡, 토픽 분석, tidyverse, stm]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 토픽 분석1"
---
<div class="notice--success">
카카오톡으로 오고간 대화의 토픽 분석합니다. 구조적 토픽모델(STM, Structural Topic Model)로 적절한 수의 토픽을 결정해서 토픽을 분석해 봅시다.
</div>

## 미션 이해

문서의 내용을 어떤 주제에 대한 것이라고 특징지울 수 있습니다. 이러한 주제를 토픽(Topic)이라고 부르며 다양한 글들에는 다양한 토픽이 있을 수 있습니다. 토픽을 분석하는 다양한 알고리즘이 있는데 LDA(Latent Dirichlet Allocation)라는 방식이 유명합니다. 이는 초기에 개발된 토픽 모델로 토픽들 간의 상관관계를 인정하지 않기 때문에 현실적이지 않습니다. 따라서 토픽들 간의 상관관계를 인정한 STM(Structural Topic Model)이라는 알고리즘으로 토픽 분석하는 것이 좋습니다. 가장 최근에 개발된만큼 매우 많은 정보를 얻을 수 있습니다.

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

names_top3 <- data %>% group_by(name) %>% summarise(n=n()) %>% 
    arrange(desc(n)) %>% slice(1, 2, 3) %>% pull(name)

data <- data %>% mutate(date=ym(paste0(year, "-", month))) %>%                  # 월별 분석을 위해
  mutate(date=as.integer(round((date-as.Date("2019-02-01"))/(365.25/12)))) %>%  # 개월수로 변환
  mutate(group=as.factor(ifelse(name %in% names_top3, "Top3", "Others")))       # Top3와의 비교를 위해
```

rdata는 전처리가 거의 없는 데이터이고 data는 EDA나 머신러닝, 텍스트 마이닝 등에 사용될 범용적인 데이터입니다. data 파일의 comment의 글을 형태소 분석하여 “형태소”라는 변수를 추가로 만듭니다. 형태소는 명사, 동사, 형용사에 해당하는 것만 수집하며 나중에 토픽 분석을 쉽게 할 수 있도록 tibble 형태로 유지합니다.

맨 마지막 부분에 있는 코드는 토픽 분석을 위해 새로운 변수를 만드는 내용입니다. 월별 토픽 주제의 변화를 살펴보고 발언수가 많은 3명을 선정하여 해당 3명과 다른 사람들 사이의 대화 주제는 어떻게 다른지 분석하기 위해 date와 group이라는 변수를 새로 만들었습니다.

우선 ym() 함수를 이용하여 월 단위의 날짜를 만들어 줍니다. 이렇게 만들어진 date에서 시작날짜를 빼주면 일단위로 차이가 구해집니다. 이를 월로 바꿔주기 위해 265.25/12의 값으로 나누어 준 후 round() 함수를 적용하면 28일, 30일, 31일에 의한 차이가 정수로 구해집니다. 하지만 이 값은 여전히 날짜 포맷이기 때문에 숫자로 바꿔주기 위해 as.integer() 함수를 적용했습니다. name에 top3에 해당하는 이름이 있는 경우 “Top3”, 그렇지 않은 경우 “Others”로 바꾸도록 했습니다. %in%는 %in% 왼쪽에 있는 값이 %in% 오른쪽에 있는지의 여부를 판단할 때 사용합니다.

### 데이터 전처리

``` r
stm_pre <- textProcessor(data$형태소, data, wordLengths = c(2,Inf), customstopwords=c("사진", "이모티콘"))
```

    ## Building corpus... 
    ## Converting to Lower Case... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Removing numbers... 
    ## Stemming... 
    ## Creating Output...

``` r
stm_out <- prepDocuments(stm_pre$documents, stm_pre$vocab, stm_pre$meta, lower.thresh=3)
```

    ## Removing 10172 of 15265 terms (14987 of 155173 tokens) due to frequency 
    ## Removing 395 Documents with No Words 
    ## Your corpus now has 25339 documents, 5093 terms and 140186 tokens.

구조적 토픽모델을 적용하기 위한 전처리 과정입니다. 구조적 토픽모델은 다른 변수들과의 관계를 살펴볼 수 있기 때문에 텍스트 이외의 변수에 대한 정보까지 포함하는 메타 데이터 형태로 관리해야 합니다.

textProcessor() 함수는 stm 패키지에서 지원하는 함수입니다. tidystm 패키지를 실행시키면 자동으로 stm 패키지가 실행되고 tidystm 패키지에서 더 많은 정보를 얻을 수 있기 때문에 p\_load() 에서는 tidystm 패키지를 실행시켰습니다. tm 패키지에서 지원하는 형태소 분석와 불용어 처리 등 다양한 기능을 제공하지만 아쉽게도 한국어 형태소 분석을 지원하지 않기 때문에 한국어 형태소 분석한 결과를 적용시켜야 합니다. 영어나 스페인어 등을 형태소 분석한 경우 3음절 이상만 유효한 단어로 취급하는 경우가 많기 때문에 wordLengths 파라미터의 디폴트 값은 3입니다. 이를 2음절 이상에 해당하는 단어를 선택하도록 설정하였습니다. customstopwords에 불용어 처리할 단어들을 지정할 수 있습니다. 사진이나 이모티콘만 있는 경우 해당 단어는 제외합니다.

prepDocuments() 함수를 이용해서 메타데이터를 만듭니다. lower.thresh를 3으로 설정해 놓으면 3단어 미만, 즉 두 단어 이하로 구성된 글의 경우 토픽분석에서 제외하라는 의미입니다. 어차피 3단어 미만은 토픽분석에서 제외되기 때문에 미리 제외시키는 것도 나쁘지 않습니다. 경우에 따라서는 5단어나 7단어 미만일 때도 토픽 분석에서 제외될 때도 있습니다.

### 적절한 토픽수 결정

``` r
old_time <- Sys.time()
searchK(stm_pre$documents, stm_pre$vocab, K=c(10:16), prevalence=~ group + s(date), 
        data=stm_pre$meta, set.seed(3000), verbose=F) %>% plot() 
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-02-kakaotalk-topic-analysis1_files/figure-gfm/train_and_test-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
Sys.time() - old_time
```

    ## Time difference of 1.562655 hours

토픽수는 분석하는 사람이 나름대로 정해도 됩니다. 하지만 우리나라의 경우 객관적인 것을 너무 좋아하다보니 논문을 보면 연구자 개인이 아니라 어떤 지표를 바탕으로 토픽수를 결정하는 경향이 강합니다.

위 코드의 실행결과를 보면 4개의 그래프가 그려지는데 가능도(Held-Out Likelihood)와 의미론적 일관성(Semantic Coherence)은 그 값이 클수록 좋고 잔차(Residuals)는 작을 수록 좋습니다. 이러한 조건을 모두 만족하는 경우는 거의 없기 때문에 분석하는 사람이 적절하게 선택해야 합니다. 예를 들어 가능도를 최우선 기준으로해서 토픽수를 정할 수도 있고 적절하게 잔차가 작으면서 가능도도 어느 정도 큰 토픽수를 정할 수도 있습니다. 참고로 두 번째 방법이 제가 선호하는 방법입니다. 위 그래프는 가능도와 잔차를 기준으로 볼 때 적절한 토픽수는 13개 임을 알 수 있습니다. 약간 아쉬운 것이 의미론적 일관성이 상대적으로 높지 않다는 것입니다. 의미론적 일관성이 클수록 토픽명에 대해 사람들이 수긍할 가능성이 크기 때문에 중요하다고 하는데 한글의 경우 잘 적용되지는 않는 것 같습니다. 

searchK() 함수를 이용해서 적절한 토픽수를 결정할 수 있습니다. 중요한 파라미터가 2개 있는데 K는 비교할 적절한 토픽 수의 범위를 지정합니다. 위 코드에서는 10개에서부터 16개의 토픽수를 비교하라는 뜻입니다. 그 다음 중요한 것은 prevalence입니다. \~표시 뒤에 있는 변수들이 독립변수에 해당됩니다. group과 date에 따른 토픽 발현확률을 회귀분석으로 분석할 것임을 알려주면 토픽분석할 때 랜덤으로 분석하는 과정이 있는데 이 때 최대한 두 변수와 관련성이 높게 토픽을 분석해줍니다.

10:16은 10에서부터 16까지 1씩 증가합니다. 참고로 28,840개의 대화를 10개부터 16개까지 총 7개의 토픽수를 비교하는 위 코드를 실행시키면 1시간 반 넘게 걸립니다. 참고로 컴퓨터 성능에 따라 시간이 다를 수 있는데 제 집에 있는 컴퓨터로는 3시간 가량 걸렸습니다. 보통 처음에는 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 등과 같이 10단위로 넓게 살펴보고 그 후에 2~3 단위로 대략 살펴 본 후 지금처럼 1 단위로 세밀하게 분석해서 적절한 토픽수를 결정하는 것이 좋습니다. 

system.time() 함수를 이용해서 총 걸린 시간을 출력할 수도 있지만 초 단위로 결과가 나와서 시간 단위로 환산하려면 한 번 더 계산해야 하는 문제가 있고 무엇보다 분석 결과를 확인하면서 시간을 체크할 수 없기 때문에 Sys.time() 함수를 이용해서 코드를 searchK() 함수를 실행하기 전의 시간을 체크한 후 해당 함수가 끝난 후의 시간을 체크해서 빼주었습니다. 이렇게 하면 분석 결과를 확인할 수 있으면서 코드를 실행하는데 걸리는 시간도 알 수 있고, 무엇보다 시간이 오래 걸릴 경우 시간 단위로 환산해서 알려주기 때문에 매우 직관적으로 걸린 시간을 알 수 있습니다.

### 토픽분석

``` r
k <- 13
summary(stm_topics <- stm(stm_out$documents, stm_out$vocab, K=k, prevalence=~ group + s(date), 
                          data=stm_out$meta, seed=1000, init.type="Spectral"))
```

    ## Beginning Spectral Initialization 
    ##   Calculating the gram matrix...
    ##   Finding anchor words...
    ##     (중략) .............
    ## Completed E-Step (3 seconds). 
    ## Completed M-Step. 
    ## Model Converged 
    ## A topic model with 13 topics, 23047 documents and a 5091 word dictionary.

    ## Topic 1 Top Words:
    ##       Highest Prob: 감사, 박사, 연구원, 러닝, 머신, 설명, 그림
    ##       FREX: 박사, 러닝, 머신, 정리, 느낌, 원자력, 응원
    ##       Lift: 강당, 건이, 검지, 결과적, 고석진, 골프장, 굉장 
    ##       Score: 감사, 박사, 머신, 러닝, 연구원, 그림, 원자력 
    ## Topic 2 Top Words:
    ##       Highest Prob: ai, 드리, 프렌즈, 세미나, 인공지능, 발표, 안녕 
    ##       FREX: 프렌즈, 세미나, 안녕, 대회, 모시, 제목, 공지
    ##       Lift: 가칭, 간식, 고민중, 관련하, 구경하, 동아리, 락스
    ##       Score: 프렌즈, ai, 세미나, 드리, 안녕, 부탁, 발표
    ## Topic 3 Top Words:
    ##       Highest Prob: 시간, 필요, 강의, 올리, 안되, 메시지, 삭제
    ##       FREX: 강의, 메시지, 삭제, 환영, 이름, 홍보, 대전시
    ##       Lift: 가늘, 감사원, 고수, 교과, 교대, 극단적, 꾸미
    ##       Score: 삭제, 메시지, 강의, 시간, 올리, 필요, 환영
    ## Topic 4 Top Words:
    ##       Highest Prob: 코로나, 투자, 정부, 기업, 미국, 상장, 시장
    ##       FREX: 투자, 미국, 상장, 바이러스, 생산, 일본, 금융
    ##       Lift: new, 가닥, 가대, 가운데, 가전, 간담회, 갈아타 
    ##       Score: 상장, 투자, 금융, 지난해, 대우, 미래에셋, 코로나
    ## Topic 5 Top Words:
    ##       Highest Prob: 데이터, 사람, 분석, 보이, 다르, 결과, 가지
    ##       FREX: 데이터, 요즘, 코딩, 아쉽, 파일, 테스트, 수집
    ##       Lift: 가수, 각도, 개척, 걸치, 검사자, 격하, 고찰 
    ##       Score: 데이터, 사람, 분석, 다르, 결과, 테스트, 코딩 
    ## Topic 6 Top Words:
    ##       Highest Prob: 모르, 학습, 정도, 모델, 재미있, 어렵, 경우 
    ##       FREX: 모르, 재미있, 반갑, 머리, 분류, 언어, 최적 
    ##       Lift: 물리, 가리, 가정, 감각, 개리, 개별적, 겟다
    ##       Score: 학습, 모델, 모르, 정도, 경우, 재미있, 어렵
    ## Topic 7 Top Words:
    ##       Highest Prob: 나오, 말씀, 대전, 분야, 위하, 전문가, 축하
    ##       FREX: 말씀, 대전, 축하, 모두, 장님, 개인적, 설정 
    ##       Lift: 존경, 개인적, 결재, 광도, 균형, 깔끔, 놓치 
    ##       Score: 대전, 나오, 말씀, 축하, 분야, 장님, 모두 
    ## Topic 8 Top Words:
    ##       Highest Prob: 이야기, 저희, 여기, 계시, 어떻하, 교육, 로봇 
    ##       FREX: 이야기, 저희, 계시, 교육, 괜찮, 대단, 스토리 
    ##       Lift: 발주자, 번갈, 번개, 번역기, usb, 가방, 가을 
    ##       Score: 이야기, 저희, 계시, 교육, 로봇, 어떻하, 여기 
    ## Topic 9 Top Words:
    ##       Highest Prob: 생각, 이것, 그렇, 해보, 연구, 논문, 멋지 
    ##       FREX: 생각, 이것, 그렇, 논문, 멋지, 예전, 그리 
    ##       Lift: 개념적, 거지, 거짓말, 경도, 공구, 구경, 구름 
    ##       Score: 생각, 이것, 그렇, 논문, 멋지, 해보, 의미 
    ## Topic 10 Top Words:
    ##       Highest Prob: 문제, 사용, 그것, 무엇, 우리, 부분, 공부 
    ##       FREX: 그것, 코드, 나중, 능력, 거기, 저것, 와우 
    ##       Lift: 강아지, 거구, 걷기, 계산하, 고치, 그대, 기웃거리 
    ##       Score: 문제, 그것, 사용, 무엇, 코드, 부분, 생각하 
    ## Topic 11 Top Words:
    ##       Highest Prob: 기술, 대하, 정보, 도움, 이해, 질문, 지금 
    ##       FREX: 도움, 질문, 학교, 추천, 메타, 수업, 개발자 
    ##       Lift: 강요, 고교, 고대, 고자, 교수자, 낚시꾼, 독립 
    ##       Score: 메타, 버스, 도움, 질문, 수업, 서비스, 정보 
    ## Topic 12 Top Words:
    ##       Highest Prob: 오늘, 공유, 참여, 관심, 진행, 신청, 시작 
    ##       FREX: 링크, 과학, 들어오, 오픈, 대덕, 죄송, 검색
    ##       Lift: 검색엔진, 고고, 김밥, 김승일, 김휘경, 날짜, 마인
    ##       Score: 링크, 참여, 오늘, 신청, 유튜브, 관심, 행사 
    ## Topic 13 Top Words:
    ##       Highest Prob: 만들, 학생, 못하, 회사, 기업, 중요, 사업
    ##       FREX: 만들, 학생, 선생, 대학, 가르치, 공감, 블록체인
    ##       Lift: 과학고, 김종헌, 보드, 인건비, 파라미터, 가르치, 감별 
    ##       Score: 만들, 학생, 못하, 선생, 중요, 사업, 기업 

``` r
stm_removed <- setdiff(c(1:nrow(data)), stm_topics$mu$mu %>% as.data.frame() %>% names() %>% as.numeric())
```

토픽수를 k라는 변수에 입력합니다. prevalence 파라미터로 group과 date도 지정합니다.

각 토픽별로 빈도수가 높은 단어(Highest Prob), 배타적인 단어(FREX) 등이 나타납니다. 개인적으로 빈도수가 높은 단어(Highest Prob)와 해당 토픽에서는 빈도수가 높지만 다른 토픽에서는 빈도수가 낮은 배타적인 단어(FREX)를 기준으로 토픽명을 짐작하고 해당 토픽에 대한 대표적인 문서를 살펴보면서 토픽명을 확정합니다. 이 부분은 토픽 분석 결과를 시각화하는 다음 글에서 본격적으로 살펴볼 예정입니다.

나름대로의 알고리즘으로 토픽분석을 하다보면 단어 수가 작은 문서들은 토픽분석에서 제외되는 경우가 있습니다. 나중에 토픽 발현 확률과 다른 변수들과의 관계를 살펴볼 때나 특정 문서의 내용을 살펴볼 때 토픽분석에서 제외된 문서 정보를 알아야 하기 때문에 setdiff() 함수로 원래 데이터에서 제외된 행의 정보를 stm\_removed에 저장합니다.

## 예고

다음 글에서 본격적으로 토픽 분석 결과를 시각화 해보도록 하겠습니다.
