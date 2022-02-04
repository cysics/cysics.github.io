---
title: "카카오톡 토픽 분석1 (토픽수 결정 및 토픽 분석)"
last_modified_at: 2022-02-02
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

names_top10 <- data %>% group_by(name) %>% summarise(n=n()) %>% 
    arrange(desc(n)) %>% slice(1, 2, 3) %>% pull(name)

data <- data %>% mutate(date=ym(paste0(year, "-", month))) %>%             # 월별 데이터 분석을 위해
  mutate(group=as.factor(ifelse(name %in% names_top10, "Top3", "Others"))) # Top3와의 비교를 위해
```

rdata는 전처리가 거의 없는 데이터이고 data는 EDA나 머신러닝, 텍스트 마이닝 등에 사용될 범용적인 데이터입니다. data 파일의 comment의 글을 형태소 분석하여 “형태소”라는 변수를 추가로 만듭니다. 형태소는 명사, 동사, 형용사에 해당하는 것만 수집하며 나중에 토픽 분석을 쉽게 할 수 있도록 tibble 형태로 유지합니다.

맨 마지막 줄에 있는 코드는 토픽 분석을 위해 새로운 변수를 만드는 내용입니다. 월별 토픽 주제의 변화를 살펴보고 발언수가 많은 3명을 선정하여 해당 3명과 다른 사람들 사이의 대화 주제는 어떻게 다른지 분석하기 위해 date와 group이라는 변수를 새로 만들었습니다.

### 데이터 전처리

``` r
stm_pre <- textProcessor(data$형태소, data, wordLengths = c(2,Inf))
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

textProcessor() 함수는 stm 패키지에서 지원하는 함수입니다. tidystm 패키지를 실행시키면 자동으로 stm 패키지가 실행되고 tidystm 패키지에서 더 많은 정보를 얻을 수 있기 때문에 p\_load() 에서는 tidystm 패키지를 실행시켰습니다. tm 패키지에서 지원하는 형태소 분석와 불용어 처리 등 다양한 기능을 제공하지만 아쉽게도 한국어 형태소 분석을 지원하지 않기 때문에 한국어 형태소 분석한 결과를 적용시켜야 합니다. 영어나 스페인어 등을 형태소 분석한 경우 3음절 이상만 유효한 단어로 취급하는 경우가 많기 때문에 wordLengths 파라미터의 디폴트 값은 3입니다. 이를 2음절 이상에 해당하는 단어를 선택하도록 설정하였습니다.

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
    ## A topic model with 13 topics, 25339 documents and a 5093 word dictionary.

    ## Topic 1 Top Words:
    ##       Highest Prob: 이모티콘, 이야기, 필요, 그것, 재미있, 기업, 학교 
    ##       FREX: 이모티콘, 이야기, 그것, 재미있, 학교, 괜찮, 머리 
    ##       Lift: 강추, 굉장하, 교대, 까맣, 도와주, 마인, 무사 
    ##       Score: 이모티콘, 재미있, 그것, 필요, 이야기, 과학, 기업 
    ## Topic 2 Top Words:
    ##       Highest Prob: 데이터, 문제, 분석, 말씀, 여기, 모델, 해보 
    ##       FREX: 데이터, 예전, 고민, 예측, 테스트, 성능, 수집 
    ##       Lift: 각도, 개리, 겟다, 고정, 구한, 국한, 극단적 
    ##       Score: 데이터, 분석, 모델, 문제, 테스트, 경우, 보이 
    ## Topic 3 Top Words:
    ##       Highest Prob: 박사, 저희, 메시지, 삭제, 못하, 관련, 회사 
    ##       FREX: 박사, 저희, 메시지, 삭제, 도움, 메타, 기획 
    ##       Lift: mb, 가을, 고기, 공개적, 과학관, 관계사, 교체 
    ##       Score: 박사, 삭제, 메시지, 저희, 메타, 버스, 도움 
    ## Topic 4 Top Words:
    ##       Highest Prob: 나오, 기대, 활용, 코드, 우리, 들어오, 오픈 
    ##       FREX: 코드, 들어오, 오픈, 수업, 설계, 소스, 채팅 
    ##       Lift: 감사원, 갑작스럽, 경도, 고민중, 교수자, 구내식당, 길수 
    ##       Score: 나오, 기대, 들어오, 코드, 오픈, 수업, 채팅 
    ## Topic 5 Top Words:
    ##       Highest Prob: 코로나, 투자, 정부, 기업, 미국, 시장, 서울 
    ##       FREX: 투자, 미국, 서울, 상장, 국내, 바이러스, 경제 
    ##       Lift: 가대, 가전, 갈아타, 강세, 거론, 공약, 광풍 
    ##       Score: 상장, 투자, 금융, 지난해, 대우, 미래에셋, 청약 
    ## Topic 6 Top Words:
    ##       Highest Prob: 만들, 생각, 기술, 정도, 시스템, 새롭, 인식 
    ##       FREX: 생각, 이러, 잘하, 얼굴, 귀엽, 궁금하, 실행 
    ##       Lift: 개별적, 검색엔진, 고대, 관리하, 균형, 글자, 까라 
    ##       Score: 생각, 만들, 기술, 정도, 인식, 가치, 환경 
    ## Topic 7 Top Words:
    ##       Highest Prob: 사진, 무엇, 교수, 공부, 사업, 로봇, 딥러닝 
    ##       FREX: 사진, 코딩, 홍보, 가르치, 수학, 그러하, 공감 
    ##       Lift: 가득, 견적, 기웃거리, 꼬이, 내보, 네이밍, 들르 
    ##       Score: 사진, 교수, 공부, 로봇, 무엇, 코딩, 그러하 
    ## Topic 8 Top Words:
    ##       Highest Prob: 사람, 어렵, 안되, 중요, 부분, 지금, 질문 
    ##       FREX: 부분, 추천, 아이디어, 그리, 능력, 동영상, 버전 
    ##       Lift: 감각, 개척, 거구, 거로, 검지, 계절, 고수 
    ##       Score: 사람, 어렵, 중요, 부분, 질문, 지금, 능력 
    ## Topic 9 Top Words:
    ##       Highest Prob: ai, 프렌즈, 세미나, 인공지능, 발표, 내용, 안녕 
    ##       FREX: ai, 프렌즈, 세미나, 인공지능, 소개, 영상, 러닝 
    ##       Lift: 의료인, 가칭, 감동적, 강국, 거창, 경진, 고난 
    ##       Score: ai, 프렌즈, 세미나, 인공지능, 발표, 안녕, 이번 
    ## Topic 10 Top Words:
    ##       Highest Prob: 학습, 그렇, 강화, 이해, 파이, 들어가, 프로젝트 
    ##       FREX: 학습, 그렇, 정리, 최적화, 알고리즘, 나중, 개발자 
    ##       Lift: 개념적, 과금, 김장, 낚시, 둘러보, 따라서, 바꿔주 
    ##       Score: 학습, 그렇, 강화, 최적화, 알고리즘, 파이, 대단 
    ## Topic 11 Top Words:
    ##       Highest Prob: 감사, 드리, 오늘, 공유, 참여, 진행, 관심 
    ##       FREX: 강의, 링크, 주제, 멋지, 축하, 연락, 대덕 
    ##       Lift: 김종헌, 날짜, 비즈, 선착순, 재미나, 컨퍼런스, 클릭 
    ##       Score: 감사, 드리, 오늘, 부탁, 참여, 링크, 유튜브 
    ## Topic 12 Top Words:
    ##       Highest Prob: 이것, 모르, 대전, 계시, 위하, 배우, 논문 
    ##       FREX: 이것, 모르, 대전, 배우, 논문, 요즘, 문화 
    ##       Lift: 간식, 고고, 교정, 그놈, 꽂히, 농업, 달이 
    ##       Score: 이것, 대전, 모르, 논문, 계시, 배우, 누구 
    ## Topic 13 Top Words:
    ##       Highest Prob: 가능, 대하, 사용, 연구, 보내, 이상, 과제 
    ##       FREX: 과제, 힘들, 그러, 엄청나, 기계학, 서버, 인가 
    ##       Lift: 감별, 검사자, 고찰, 금전, 뜨끔, 마이크로, 시국 
    ##       Score: 사용, 가능, 연구, 대하, 과제, 이상, 생기

``` r
stm_removed <- setdiff(c(1:nrow(data)), stm_topics$mu$mu %>% as.data.frame() %>% names() %>% as.numeric())
```

토픽수를 k라는 변수에 입력합니다. prevalence 파라미터로 group과 date도 지정합니다.

각 토픽별로 빈도수가 높은 단어(Highest Prob), 배타적인 단어(FREX) 등이 나타납니다. 개인적으로 빈도수가 높은 단어(Highest Prob)와 해당 토픽에서는 빈도수가 높지만 다른 토픽에서는 빈도수가 낮은 배타적인 단어(FREX)를 기준으로 토픽명을 짐작하고 해당 토픽에 대한 대표적인 문서를 살펴보면서 토픽명을 확정합니다. 이 부분은 토픽 분석 결과를 시각화하는 다음 글에서 본격적으로 살펴볼 예정입니다.

나름대로의 알고리즘으로 토픽분석을 하다보면 단어 수가 작은 문서들은 토픽분석에서 제외되는 경우가 있습니다. 나중에 토픽 발현 확률과 다른 변수들과의 관계를 살펴볼 때나 특정 문서의 내용을 살펴볼 때 토픽분석에서 제외된 문서 정보를 알아야 하기 때문에 setdiff() 함수로 원래 데이터에서 제외된 행의 정보를 stm\_removed에 저장합니다.

## 예고

다음 글에서 본격적으로 토픽 분석 결과를 시각화 해보도록 하겠습니다.
