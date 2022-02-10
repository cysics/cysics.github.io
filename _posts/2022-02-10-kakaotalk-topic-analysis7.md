---
title: "카카오톡 토픽 분석7 (회귀분석 상호작용)"
last_modified_at: 2022-02-10
categories: [텍스트 마이닝, 토픽 분석]
tag: [카카오톡, 토픽 분석, 회귀분석, 상호작용, tidyverse, stm]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 토픽 분석7"
---
<div class="notice--success">
카카오톡으로 오고간 대화의 토픽 분석합니다. 이번 글에서는 연속형 변수에 따른 토픽 발현확률을 회귀분석으로 분석해 보겠습니다.
</div>

## 미션 이해

구조적 토픽모델은 자체 함수를 이용해서 메타 데이터에 있는 다른 변수와
회귀분석을 할 수 있습니다. 다양한 변수들이 독립변수가 되고 토픽
발현확률은 종속변수가 됩니다. 이번 포스트에서는 연속형 변수에 따른 토픽
발현 확률을 어떻게 시각화하는지 살펴보겠습니다.

## 최종 결과 확인

### 데이터 전처리 및 토픽분석

``` r
#### 1. 분석 준비 ####
pacman::p_load(scales, ggthemes, ggridges,                      # 시각화 관련 패키지
               PerformanceAnalytics, pheatmap,                  # 상관관계 시각화
               forecast,                                        # 시계열 예측 관련 패키지
               RHINO, tm, RWeka, tidytext, tidystm,             # 텍스트 마이닝
               igraph, ggraph, tidygraph, wordcloud2,           # 텍스트 마이닝 시각화
               factoextra,                                      # 군집분석 시각화
               tidymodels, textrecipes, LiblineaR, themis,      # 머신러닝
               lubridate, magrittr, tidyverse)                  # 데이터 전처리 관련 패키지


#### 2. 데이터 전처리 ####
rdata <- read_file("../data/KakaoTalkChats.txt") %>%                         # txt 파일 읽어오기
    strsplit("\r") %>% unlist() %>%                                          # 같은 사람의 글은 한 줄로
    gsub("\n", "", .) %>% as_tibble() %>%                                    # 줄바꿈 없애기
    filter(grepl("^\\d.*,.*:", value)) %>%                                   # 숫자시작 , : 있는 것만
    separate(value, into=c("date", "text"), sep=", ", extra="merge") %>%     # 날짜와 글 분리
    separate(text, into=c("name", "comment"), sep=" : ", extra="merge")      # 이름과 글 내용 분리
data <- rdata %>% 
    rownames_to_column("id") %>%                                        # 문서 id
    mutate(date=gsub("년 ", "-", gsub("월 ", "-", gsub("일 ", " ", date)))) %>%
    mutate(date=gsub("오전", "AM", gsub("오후", "PM", date))) %>%       # 오전 오후 구분
    mutate(date=parse_date_time(date, c("%Y-%m-%d %p %H:%M"))) %>%      # 날짜 형식으로
    mutate(year=year(date), quarter=quarter(date), month=month(date),   # 년, 분기, 월 변수 만들기
           wday=weekdays(date), yday=yday(date), hour=hour(date),       # 요일, 일수, 시간 변수 만들기
           ampm=ifelse(hour(date)<12, "AM", "PM")) %>%                  # 오전 오후 변수 만들기
    select(id, year:ampm, name, comment) %>%                                # 주요 변수 선택
    mutate(형태소=comment %>% sapply(getMorph, "NV") %>%                # 명사, 동사, 형용사만 선택
                  sapply(paste, collapse=" "))                          # 형태소 분석 결과 합치기
  
names_top3 <- data %>% group_by(name) %>% summarise(n=n()) %>%          # 발언량이 많은 
    arrange(desc(n)) %>% slice(1, 2, 3) %>% pull(name)                  # 상위 3명 이름 저장

data <- data %>% 
    mutate(group=as.factor(ifelse(name %in% names_top3, "Top3", "Others"))) %>%  # 그룹 지정
    mutate(date=ym(paste0(year, "-", month))) %>%                                 # 년월 지정
    mutate(date=as.integer(round((date-as.Date("2019-02-01"))/(365.25/12))))      # 누적 월 계산


#### 3. 구조적 토픽모델 ####
stm_pre <- textProcessor(data$형태소, data, wordLengths = c(2,Inf), customstopwords=c("사진", "이모티콘"))
stm_out <- prepDocuments(stm_pre$documents, stm_pre$vocab, stm_pre$meta, lower.thresh=3)

k <- 13
stm_topics <- stm(stm_out$documents, stm_out$vocab, K=k, prevalence=~group+s(date), 
                  data=stm_out$meta, seed=1000, init.type="Spectral")

stm_removed <- setdiff(c(1:nrow(data)), stm_topics$mu$mu %>% as.data.frame() %>% names() %>% as.numeric())
```

데이터 전처리, 토픽분석 과정입니다. 이전 글에서 설명한 내용 그대로입니다.

### 회귀분석(상호작용) ####
```
#### 4. 회귀분석(상호작용) ####
summary(stm_fit2 <- estimateEffect(1:k ~ group*date, stmobj=stm_topics, metadata=stm_out$meta, uncertainty="Global"))
```

    ## 
    ## Call:
    ## estimateEffect(formula = 1:k ~ group * date, stmobj = stm_topics, 
    ##     metadata = stm_out$meta, uncertainty = "Global")
    ## 
    ## 
    ## Topic 1:
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.142e-02  1.505e-03  20.880  < 2e-16 ***
    ## groupTop3      8.017e-03  2.604e-03   3.079  0.00208 ** 
    ## date           8.447e-04  6.298e-05  13.413  < 2e-16 ***
    ## groupTop3:date 4.017e-04  1.012e-04   3.968 7.28e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 2:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     0.3045146  0.0050283  60.560  < 2e-16 ***
    ## groupTop3      -0.0623051  0.0082785  -7.526 5.42e-14 ***
    ## date           -0.0048681  0.0002024 -24.051  < 2e-16 ***
    ## groupTop3:date  0.0002801  0.0003169   0.884    0.377    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 3:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     7.688e-02  1.328e-03  57.898   <2e-16 ***
    ## groupTop3      -5.297e-03  2.342e-03  -2.262   0.0237 *  
    ## date           -5.220e-04  5.578e-05  -9.359   <2e-16 ***
    ## groupTop3:date -5.951e-07  8.996e-05  -0.007   0.9947    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 4:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     1.185e-01  3.609e-03  32.835  < 2e-16 ***
    ## groupTop3      -2.168e-02  6.169e-03  -3.514 0.000442 ***
    ## date           -6.805e-05  1.368e-04  -0.497 0.618894    
    ## groupTop3:date -3.969e-04  2.378e-04  -1.669 0.095120 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 5:
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    4.118e-02  1.861e-03  22.127  < 2e-16 ***
    ## groupTop3      1.505e-02  3.585e-03   4.199 2.69e-05 ***
    ## date           1.163e-03  7.478e-05  15.553  < 2e-16 ***
    ## groupTop3:date 4.256e-04  1.379e-04   3.086  0.00203 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 6:
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.506e-02  1.366e-03  25.664  < 2e-16 ***
    ## groupTop3      1.036e-02  2.258e-03   4.588 4.49e-06 ***
    ## date           8.323e-04  5.529e-05  15.055  < 2e-16 ***
    ## groupTop3:date 1.471e-04  9.692e-05   1.517    0.129    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 7:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     6.213e-02  1.847e-03  33.633  < 2e-16 ***
    ## groupTop3      -5.733e-03  3.030e-03  -1.892   0.0585 .  
    ## date            5.990e-04  7.999e-05   7.490 7.16e-14 ***
    ## groupTop3:date -2.716e-04  1.255e-04  -2.164   0.0305 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 8:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5.808e-02  1.609e-03  36.095  < 2e-16 ***
    ## groupTop3       1.563e-02  2.742e-03   5.700 1.21e-08 ***
    ## date            3.415e-04  6.569e-05   5.198 2.03e-07 ***
    ## groupTop3:date -1.517e-04  1.118e-04  -1.356    0.175    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 9:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5.802e-02  1.492e-03  38.880  < 2e-16 ***
    ## groupTop3       7.441e-03  2.527e-03   2.945  0.00323 ** 
    ## date            3.312e-04  6.017e-05   5.504 3.76e-08 ***
    ## groupTop3:date -2.521e-04  9.601e-05  -2.626  0.00866 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 10:
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    4.591e-02  1.697e-03  27.054  < 2e-16 ***
    ## groupTop3      9.946e-03  2.915e-03   3.412 0.000646 ***
    ## date           4.244e-04  7.665e-05   5.536 3.12e-08 ***
    ## groupTop3:date 6.754e-05  1.237e-04   0.546 0.585053    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 11:
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    6.278e-02  2.013e-03  31.192  < 2e-16 ***
    ## groupTop3      1.468e-02  3.557e-03   4.127 3.69e-05 ***
    ## date           1.390e-04  8.007e-05   1.737   0.0825 .  
    ## groupTop3:date 2.143e-04  1.398e-04   1.533   0.1252    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 12:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5.519e-02  1.383e-03  39.901  < 2e-16 ***
    ## groupTop3       1.498e-02  2.400e-03   6.242  4.4e-10 ***
    ## date            1.145e-04  5.787e-05   1.978 0.047949 *  
    ## groupTop3:date -3.336e-04  9.331e-05  -3.575 0.000351 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 13:
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     0.0502890  0.0028984  17.351  < 2e-16 ***
    ## groupTop3      -0.0010839  0.0052318  -0.207    0.836    
    ## date            0.0006707  0.0001185   5.658 1.55e-08 ***
    ## groupTop3:date -0.0001332  0.0002079  -0.640    0.522    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

상호작용 효과는 연속형 독립변수에 따른 연속형 종속변인의 변화에서 범주형에 해당하는 독립변수에 의해 어떻게 달라지는지를 살펴봅니다. 여기에서는 Top3와 나머지 다른 사람들 사이의 차이를 살펴보았습니다. 

상호작용효과를 보려면 범주형 독립변수와 연속형 독립변수를 \*로 묶어주면 됩니다. 그러면 위와 같이 독립변인, 종속변인과 함께 groupT03:date라는 항이 하나 더 보입니다. 이 항이 상호작용 효과를 나타냅니다. p값이 0.05보다 작으면 상호작용 효과가 있는 것으로 판단할 수 있습니다. 토픽 1, 5, 7, 9, 12가 상호작용 효과가 있네요. 그 중 연구와 논문에 해당하는 토픽 9에서의 상호작용 효과를 시각화해보도록 하겠습니다.


### 상호작용 시각화
``` r
plot(stm_fit2, covariate="date", model=stm_topics, method="continuous", xlab="date", topics = 9, 
     moderator="group", moderator.value="Top3", linecol="blue", ylim=c(0, 0.1),
     printlegend=F)
plot(stm_fit2, covariate="date", model=stm_topics, method="continuous", xlab="date", topics = 9, 
     moderator="group", moderator.value="Others", linecol="red", add=T,
     printlegend=F)
legend(0,0.1, "Topic 9", lwd=2)
legend(20,0.03, c("Top3", "Others"), lwd=2, col=c("blue", "red"))
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-10-kakaotalk-topic-analysis7_files/figure-gfm/stm_regression1-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

stm 패키지에서 지원하는 기본 함수로 표현한 시각화입니다. covariate로 연속형 변수를 입력합니다. method를 continuous로 지정하고 시각화할 토픽을 topics에 입력합니다. moderator에는 범주형 변수를 입력합니다. 여기서 moderator하면 조절자라는 의미가 있는데, 상호작용 효과가 있는 경우 group에 의한 조절효과가 있다고 표현하기도 합니다. 그런 맥락에서 이해하면 될 것 같습니다. moderator.value는 그래프로 표현할 범주를 입력하고 linecol로 파란색을 지정했습니다. ylimdm로 적절한 범위를 설정합니다. 밑에 있는 group의 Others도 그래프를 그렸을 때 두 그래프가 다 보일 수 있도록 적절한 범위를 설정해야 합니다. printlegend는 일단 표기하지 않도록 하고 나중에 한꺼번에 표현하였습니다.

같은 방식으로 group의 Others를 붉은색으로 표현합니다. add 파라미터를 통해서 선을 추가하면 됩니다. 그 후에 Topic 9라는 걸 표현해주는 legend를 추가하고 group의 범주를 시각화합니다. Top3는 파란색, Others는 붉은색임을 표현하였습니다.

### 상호작용 이쁘게 시각화하기

``` r
extract.estimateEffect(stm_fit2, "date", moderator="group", 
                       moderator.value=c("Top3", "Others"), topics = c(9)) %>% 
    mutate(covariate.value=seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=36)) %>% 
    ggplot(aes(group=moderator.value, color=moderator.value)) +
    geom_line(aes(covariate.value, estimate), size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(legend.position=c(0.25, 0.85)) +
    scale_color_manual(values=c("tomato", "royalblue")) +
    labs(x="time", y="Expected Topic Proportion", title="group에 대한 조절 효과")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-10-kakaotalk-topic-analysis7_files/figure-gfm/stm_regression2-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

이전 포스트와 마찬가지로 extract.estimateEffect() 함수로 데이터를 뽑아냅니다. moderator로 group을 지정하고 moderator.value로는 두 범주 모두 표현해줍니다. topics에는 9를 입력해줍니다. 이전 포스트를 참고하여 2개 이상의 토픽을 한꺼번에 시각화할 수도 있습니다. 숫자로 되어 있는 covariate.value를 날짜 형식으로 바꿔주고 그래프를 그려주면 됩니다.

### 평활법으로 상호작용 효과 분석하기

``` r
summary(stm_fit3 <- estimateEffect(1:k ~ group*s(date), stmobj=stm_topics, metadata=stm_out$meta, uncertainty="Global"))
```

    ## 
    ## Call:
    ## estimateEffect(formula = 1:k ~ group * s(date), stmobj = stm_topics, 
    ##     metadata = stm_out$meta, uncertainty = "Global")
    ## 
    ## 
    ## Topic 1:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0357640  0.0061994   5.769 8.08e-09 ***
    ## groupTop3            0.0125632  0.0123920   1.014 0.310680    
    ## s(date)1             0.0104962  0.0122627   0.856 0.392037    
    ## s(date)2            -0.0030821  0.0061053  -0.505 0.613686    
    ## s(date)3             0.0124291  0.0083149   1.495 0.134983    
    ## s(date)4             0.0055462  0.0060820   0.912 0.361829    
    ## s(date)5             0.0228878  0.0076823   2.979 0.002892 ** 
    ## s(date)6            -0.0007898  0.0069710  -0.113 0.909794    
    ## s(date)7             0.0563467  0.0077895   7.234 4.85e-13 ***
    ## s(date)8            -0.0187779  0.0081029  -2.317 0.020488 *  
    ## s(date)9             0.0682950  0.0092123   7.413 1.27e-13 ***
    ## s(date)10            0.0247753  0.0066393   3.732 0.000191 ***
    ## groupTop3:s(date)1  -0.0026049  0.0219770  -0.119 0.905650    
    ## groupTop3:s(date)2   0.0048609  0.0128911   0.377 0.706119    
    ## groupTop3:s(date)3  -0.0009267  0.0151063  -0.061 0.951086    
    ## groupTop3:s(date)4   0.0066777  0.0130313   0.512 0.608352    
    ## groupTop3:s(date)5   0.0012379  0.0137463   0.090 0.928243    
    ## groupTop3:s(date)6   0.0031781  0.0146304   0.217 0.828036    
    ## groupTop3:s(date)7   0.0188396  0.0135476   1.391 0.164353    
    ## groupTop3:s(date)8  -0.0045210  0.0161696  -0.280 0.779789    
    ## groupTop3:s(date)9   0.0237133  0.0150802   1.572 0.115853    
    ## groupTop3:s(date)10  0.0042963  0.0130849   0.328 0.742656    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 2:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.310948   0.027456  11.325  < 2e-16 ***
    ## groupTop3           -0.028704   0.046601  -0.616 0.537929    
    ## s(date)1            -0.022797   0.052623  -0.433 0.664867    
    ## s(date)2            -0.004947   0.024059  -0.206 0.837092    
    ## s(date)3            -0.119672   0.034223  -3.497 0.000472 ***
    ## s(date)4            -0.082835   0.027828  -2.977 0.002917 ** 
    ## s(date)5            -0.240422   0.031820  -7.556 4.32e-14 ***
    ## s(date)6            -0.039064   0.028941  -1.350 0.177091    
    ## s(date)7            -0.252018   0.032856  -7.670 1.78e-14 ***
    ## s(date)8            -0.063559   0.029330  -2.167 0.030241 *  
    ## s(date)9            -0.199381   0.034146  -5.839 5.32e-09 ***
    ## s(date)10           -0.171170   0.028546  -5.996 2.05e-09 ***
    ## groupTop3:s(date)1  -0.010028   0.081112  -0.124 0.901607    
    ## groupTop3:s(date)2  -0.055279   0.045906  -1.204 0.228541    
    ## groupTop3:s(date)3  -0.028112   0.055728  -0.504 0.613948    
    ## groupTop3:s(date)4  -0.043580   0.046658  -0.934 0.350299    
    ## groupTop3:s(date)5   0.008851   0.051740   0.171 0.864168    
    ## groupTop3:s(date)6  -0.055603   0.049305  -1.128 0.259446    
    ## groupTop3:s(date)7  -0.003921   0.052188  -0.075 0.940114    
    ## groupTop3:s(date)8  -0.055316   0.050913  -1.086 0.277281    
    ## groupTop3:s(date)9  -0.024803   0.054139  -0.458 0.646854    
    ## groupTop3:s(date)10 -0.015824   0.048853  -0.324 0.746006    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 3:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0974563  0.0081704  11.928  < 2e-16 ***
    ## groupTop3           -0.0150122  0.0116736  -1.286 0.198457    
    ## s(date)1            -0.0008341  0.0150272  -0.056 0.955738    
    ## s(date)2            -0.0519881  0.0077750  -6.687 2.34e-11 ***
    ## s(date)3            -0.0137027  0.0094457  -1.451 0.146884    
    ## s(date)4            -0.0462054  0.0087080  -5.306 1.13e-07 ***
    ## s(date)5            -0.0292745  0.0089685  -3.264 0.001099 ** 
    ## s(date)6            -0.0465295  0.0092592  -5.025 5.07e-07 ***
    ## s(date)7            -0.0250235  0.0086714  -2.886 0.003908 ** 
    ## s(date)8            -0.0530482  0.0100612  -5.273 1.36e-07 ***
    ## s(date)9            -0.0115165  0.0096490  -1.194 0.232669    
    ## s(date)10           -0.0305486  0.0083037  -3.679 0.000235 ***
    ## groupTop3:s(date)1   0.0076040  0.0208023   0.366 0.714713    
    ## groupTop3:s(date)2   0.0169150  0.0115615   1.463 0.143468    
    ## groupTop3:s(date)3   0.0051851  0.0149570   0.347 0.728846    
    ## groupTop3:s(date)4   0.0170840  0.0120569   1.417 0.156512    
    ## groupTop3:s(date)5   0.0059426  0.0130311   0.456 0.648370    
    ## groupTop3:s(date)6   0.0148354  0.0129091   1.149 0.250476    
    ## groupTop3:s(date)7   0.0024186  0.0132883   0.182 0.855579    
    ## groupTop3:s(date)8   0.0136316  0.0137455   0.992 0.321347    
    ## groupTop3:s(date)9   0.0025556  0.0151128   0.169 0.865717    
    ## groupTop3:s(date)10  0.0069380  0.0125499   0.553 0.580382    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 4:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0763633  0.0145302   5.255 1.49e-07 ***
    ## groupTop3           -0.0186372  0.0254188  -0.733   0.4634    
    ## s(date)1             0.0683591  0.0294291   2.323   0.0202 *  
    ## s(date)2             0.0107263  0.0148958   0.720   0.4715    
    ## s(date)3             0.0229001  0.0186916   1.225   0.2205    
    ## s(date)4             0.1008054  0.0161521   6.241 4.42e-10 ***
    ## s(date)5             0.0295828  0.0164311   1.800   0.0718 .  
    ## s(date)6             0.0277475  0.0169901   1.633   0.1024    
    ## s(date)7             0.0680954  0.0172296   3.952 7.77e-05 ***
    ## s(date)8             0.0035640  0.0177488   0.201   0.8409    
    ## s(date)9             0.0165553  0.0196595   0.842   0.3997    
    ## s(date)10            0.0337968  0.0160016   2.112   0.0347 *  
    ## groupTop3:s(date)1  -0.0064625  0.0462005  -0.140   0.8888    
    ## groupTop3:s(date)2  -0.0027436  0.0267966  -0.102   0.9185    
    ## groupTop3:s(date)3  -0.0055441  0.0322237  -0.172   0.8634    
    ## groupTop3:s(date)4  -0.0301480  0.0265548  -1.135   0.2563    
    ## groupTop3:s(date)5  -0.0049348  0.0302107  -0.163   0.8702    
    ## groupTop3:s(date)6  -0.0056499  0.0290297  -0.195   0.8457    
    ## groupTop3:s(date)7  -0.0216104  0.0300066  -0.720   0.4714    
    ## groupTop3:s(date)8  -0.0070198  0.0298276  -0.235   0.8139    
    ## groupTop3:s(date)9   0.0003011  0.0331558   0.009   0.9928    
    ## groupTop3:s(date)10 -0.0153658  0.0275339  -0.558   0.5768    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 5:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.040917   0.008428   4.855 1.21e-06 ***
    ## groupTop3            0.015194   0.015449   0.983 0.325386    
    ## s(date)1             0.031478   0.016648   1.891 0.058661 .  
    ## s(date)2            -0.028491   0.008472  -3.363 0.000773 ***
    ## s(date)3             0.048957   0.011269   4.345 1.40e-05 ***
    ## s(date)4            -0.007651   0.008751  -0.874 0.381986    
    ## s(date)5             0.071606   0.009715   7.370 1.76e-13 ***
    ## s(date)6             0.011372   0.009875   1.152 0.249503    
    ## s(date)7             0.042271   0.010452   4.045 5.26e-05 ***
    ## s(date)8             0.045654   0.010818   4.220 2.45e-05 ***
    ## s(date)9             0.030813   0.011902   2.589 0.009636 ** 
    ## s(date)10            0.034964   0.009150   3.821 0.000133 ***
    ## groupTop3:s(date)1  -0.003825   0.028770  -0.133 0.894234    
    ## groupTop3:s(date)2   0.007687   0.015772   0.487 0.626016    
    ## groupTop3:s(date)3   0.004321   0.019236   0.225 0.822249    
    ## groupTop3:s(date)4   0.010441   0.016597   0.629 0.529295    
    ## groupTop3:s(date)5   0.011389   0.017841   0.638 0.523252    
    ## groupTop3:s(date)6   0.005406   0.017315   0.312 0.754888    
    ## groupTop3:s(date)7   0.020980   0.017595   1.192 0.233117    
    ## groupTop3:s(date)8   0.007959   0.019548   0.407 0.683886    
    ## groupTop3:s(date)9   0.019616   0.019734   0.994 0.320228    
    ## groupTop3:s(date)10  0.006544   0.016388   0.399 0.689649    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 6:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0404789  0.0056860   7.119 1.12e-12 ***
    ## groupTop3            0.0117502  0.0118245   0.994 0.320371    
    ## s(date)1            -0.0011642  0.0110134  -0.106 0.915817    
    ## s(date)2             0.0064907  0.0060863   1.066 0.286237    
    ## s(date)3             0.0040715  0.0073533   0.554 0.579788    
    ## s(date)4             0.0064015  0.0057828   1.107 0.268305    
    ## s(date)5             0.0265937  0.0073904   3.598 0.000321 ***
    ## s(date)6             0.0072968  0.0065494   1.114 0.265242    
    ## s(date)7             0.0310789  0.0071292   4.359 1.31e-05 ***
    ## s(date)8             0.0050463  0.0077427   0.652 0.514572    
    ## s(date)9             0.0410826  0.0082055   5.007 5.58e-07 ***
    ## s(date)10            0.0255035  0.0061596   4.140 3.48e-05 ***
    ## groupTop3:s(date)1  -0.0098970  0.0221349  -0.447 0.654791    
    ## groupTop3:s(date)2   0.0057204  0.0108332   0.528 0.597476    
    ## groupTop3:s(date)3  -0.0003577  0.0149161  -0.024 0.980867    
    ## groupTop3:s(date)4   0.0047971  0.0116475   0.412 0.680447    
    ## groupTop3:s(date)5   0.0011946  0.0142012   0.084 0.932964    
    ## groupTop3:s(date)6   0.0059457  0.0134464   0.442 0.658366    
    ## groupTop3:s(date)7  -0.0018418  0.0142075  -0.130 0.896857    
    ## groupTop3:s(date)8   0.0092414  0.0137872   0.670 0.502679    
    ## groupTop3:s(date)9   0.0011598  0.0145031   0.080 0.936265    
    ## groupTop3:s(date)10  0.0016373  0.0121275   0.135 0.892610    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 7:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.076251   0.008356   9.126  < 2e-16 ***
    ## groupTop3           -0.003656   0.015583  -0.235   0.8145    
    ## s(date)1            -0.009318   0.015947  -0.584   0.5590    
    ## s(date)2            -0.013198   0.008353  -1.580   0.1141    
    ## s(date)3            -0.001860   0.010530  -0.177   0.8598    
    ## s(date)4             0.006183   0.008279   0.747   0.4552    
    ## s(date)5            -0.020308   0.010098  -2.011   0.0443 *  
    ## s(date)6             0.007328   0.009777   0.750   0.4535    
    ## s(date)7            -0.014233   0.009738  -1.462   0.1439    
    ## s(date)8             0.045677   0.011267   4.054 5.05e-05 ***
    ## s(date)9            -0.014838   0.011752  -1.263   0.2068    
    ## s(date)10            0.005884   0.008955   0.657   0.5112    
    ## groupTop3:s(date)1  -0.012803   0.028968  -0.442   0.6585    
    ## groupTop3:s(date)2   0.003089   0.016513   0.187   0.8516    
    ## groupTop3:s(date)3  -0.007067   0.019037  -0.371   0.7104    
    ## groupTop3:s(date)4  -0.009406   0.016515  -0.570   0.5690    
    ## groupTop3:s(date)5  -0.008648   0.018024  -0.480   0.6314    
    ## groupTop3:s(date)6  -0.006253   0.016938  -0.369   0.7120    
    ## groupTop3:s(date)7  -0.013448   0.017283  -0.778   0.4365    
    ## groupTop3:s(date)8  -0.015640   0.018769  -0.833   0.4047    
    ## groupTop3:s(date)9  -0.009137   0.017722  -0.516   0.6061    
    ## groupTop3:s(date)10 -0.011820   0.016884  -0.700   0.4839    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 8:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0554103  0.0076296   7.263 3.92e-13 ***
    ## groupTop3            0.0071379  0.0161353   0.442   0.6582    
    ## s(date)1             0.0087685  0.0152565   0.575   0.5655    
    ## s(date)2            -0.0024412  0.0071040  -0.344   0.7311    
    ## s(date)3             0.0129058  0.0097377   1.325   0.1851    
    ## s(date)4             0.0148129  0.0084591   1.751   0.0799 .  
    ## s(date)5             0.0075535  0.0081459   0.927   0.3538    
    ## s(date)6             0.0108170  0.0087667   1.234   0.2173    
    ## s(date)7            -0.0011178  0.0087985  -0.127   0.8989    
    ## s(date)8             0.0494409  0.0102656   4.816 1.47e-06 ***
    ## s(date)9            -0.0140093  0.0091035  -1.539   0.1238    
    ## s(date)10            0.0101237  0.0080930   1.251   0.2110    
    ## groupTop3:s(date)1   0.0054410  0.0304037   0.179   0.8580    
    ## groupTop3:s(date)2   0.0084491  0.0138956   0.608   0.5432    
    ## groupTop3:s(date)3   0.0071673  0.0211747   0.338   0.7350    
    ## groupTop3:s(date)4   0.0097478  0.0164870   0.591   0.5544    
    ## groupTop3:s(date)5  -0.0018862  0.0171494  -0.110   0.9124    
    ## groupTop3:s(date)6   0.0081292  0.0180173   0.451   0.6519    
    ## groupTop3:s(date)7  -0.0019065  0.0178655  -0.107   0.9150    
    ## groupTop3:s(date)8   0.0002131  0.0188632   0.011   0.9910    
    ## groupTop3:s(date)9   0.0081926  0.0185486   0.442   0.6587    
    ## groupTop3:s(date)10  0.0039553  0.0161684   0.245   0.8067    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 9:
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          6.995e-02  7.543e-03   9.274  < 2e-16 ***
    ## groupTop3            1.297e-03  1.296e-02   0.100  0.92033    
    ## s(date)1            -2.554e-02  1.401e-02  -1.823  0.06838 .  
    ## s(date)2            -4.274e-03  7.260e-03  -0.589  0.55611    
    ## s(date)3            -1.461e-02  9.369e-03  -1.560  0.11882    
    ## s(date)4             1.609e-03  7.623e-03   0.211  0.83287    
    ## s(date)5             2.378e-02  8.943e-03   2.659  0.00785 ** 
    ## s(date)6            -3.629e-02  8.405e-03  -4.318 1.58e-05 ***
    ## s(date)7             3.005e-02  9.356e-03   3.212  0.00132 ** 
    ## s(date)8            -4.470e-02  9.629e-03  -4.642 3.46e-06 ***
    ## s(date)9             2.250e-02  1.014e-02   2.219  0.02647 *  
    ## s(date)10            5.390e-03  7.528e-03   0.716  0.47400    
    ## groupTop3:s(date)1   2.949e-03  2.281e-02   0.129  0.89713    
    ## groupTop3:s(date)2   8.295e-03  1.429e-02   0.580  0.56161    
    ## groupTop3:s(date)3   3.209e-05  1.557e-02   0.002  0.99836    
    ## groupTop3:s(date)4   3.842e-03  1.381e-02   0.278  0.78088    
    ## groupTop3:s(date)5  -3.223e-03  1.530e-02  -0.211  0.83315    
    ## groupTop3:s(date)6   2.909e-03  1.424e-02   0.204  0.83811    
    ## groupTop3:s(date)7  -3.538e-03  1.543e-02  -0.229  0.81858    
    ## groupTop3:s(date)8   2.637e-03  1.571e-02   0.168  0.86665    
    ## groupTop3:s(date)9  -2.948e-03  1.586e-02  -0.186  0.85251    
    ## groupTop3:s(date)10 -5.231e-03  1.343e-02  -0.390  0.69688    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 10:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.052326   0.007192   7.275 3.57e-13 ***
    ## groupTop3            0.002487   0.012772   0.195 0.845626    
    ## s(date)1            -0.003346   0.013684  -0.245 0.806827    
    ## s(date)2            -0.010960   0.006295  -1.741 0.081657 .  
    ## s(date)3             0.009587   0.009509   1.008 0.313374    
    ## s(date)4            -0.011846   0.007593  -1.560 0.118737    
    ## s(date)5             0.019211   0.008291   2.317 0.020500 *  
    ## s(date)6            -0.008456   0.008257  -1.024 0.305766    
    ## s(date)7             0.030061   0.008519   3.529 0.000418 ***
    ## s(date)8            -0.001239   0.009230  -0.134 0.893209    
    ## s(date)9             0.006026   0.009326   0.646 0.518175    
    ## s(date)10           -0.004031   0.007737  -0.521 0.602394    
    ## groupTop3:s(date)1   0.012672   0.023251   0.545 0.585758    
    ## groupTop3:s(date)2  -0.001358   0.013229  -0.103 0.918266    
    ## groupTop3:s(date)3   0.013159   0.016175   0.814 0.415910    
    ## groupTop3:s(date)4   0.012519   0.013280   0.943 0.345837    
    ## groupTop3:s(date)5   0.008530   0.015419   0.553 0.580149    
    ## groupTop3:s(date)6   0.006979   0.014059   0.496 0.619626    
    ## groupTop3:s(date)7   0.011038   0.015825   0.698 0.485491    
    ## groupTop3:s(date)8   0.010385   0.015342   0.677 0.498476    
    ## groupTop3:s(date)9   0.005932   0.016535   0.359 0.719770    
    ## groupTop3:s(date)10  0.004077   0.013066   0.312 0.755027    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 11:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.075707   0.008408   9.004  < 2e-16 ***
    ## groupTop3            0.009352   0.015668   0.597 0.550574    
    ## s(date)1            -0.061822   0.017315  -3.570 0.000357 ***
    ## s(date)2             0.037852   0.008603   4.400 1.09e-05 ***
    ## s(date)3            -0.018770   0.010222  -1.836 0.066330 .  
    ## s(date)4            -0.028527   0.008587  -3.322 0.000895 ***
    ## s(date)5             0.008674   0.009766   0.888 0.374457    
    ## s(date)6            -0.022371   0.009124  -2.452 0.014217 *  
    ## s(date)7            -0.002942   0.010650  -0.276 0.782332    
    ## s(date)8            -0.011208   0.010680  -1.049 0.293988    
    ## s(date)9            -0.005098   0.011483  -0.444 0.657034    
    ## s(date)10            0.003234   0.008830   0.366 0.714158    
    ## groupTop3:s(date)1   0.003112   0.030885   0.101 0.919731    
    ## groupTop3:s(date)2   0.010030   0.016896   0.594 0.552759    
    ## groupTop3:s(date)3   0.008784   0.019713   0.446 0.655885    
    ## groupTop3:s(date)4   0.012597   0.015486   0.813 0.415960    
    ## groupTop3:s(date)5   0.011124   0.018486   0.602 0.547353    
    ## groupTop3:s(date)6   0.004489   0.016944   0.265 0.791083    
    ## groupTop3:s(date)7   0.014175   0.017294   0.820 0.412441    
    ## groupTop3:s(date)8   0.012361   0.018165   0.680 0.496213    
    ## groupTop3:s(date)9   0.012152   0.019162   0.634 0.525966    
    ## groupTop3:s(date)10  0.015395   0.016827   0.915 0.360251    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 12:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.033863   0.006374   5.312 1.09e-07 ***
    ## groupTop3            0.007280   0.011960   0.609 0.542743    
    ## s(date)1             0.051576   0.012147   4.246 2.19e-05 ***
    ## s(date)2            -0.002806   0.006377  -0.440 0.659879    
    ## s(date)3             0.034012   0.007793   4.364 1.28e-05 ***
    ## s(date)4             0.025494   0.006699   3.806 0.000142 ***
    ## s(date)5             0.006960   0.007375   0.944 0.345317    
    ## s(date)6             0.051034   0.007483   6.820 9.35e-12 ***
    ## s(date)7             0.012358   0.007461   1.656 0.097674 .  
    ## s(date)8             0.022772   0.008146   2.795 0.005188 ** 
    ## s(date)9             0.015113   0.008208   1.841 0.065597 .  
    ## s(date)10            0.033952   0.007048   4.817 1.46e-06 ***
    ## groupTop3:s(date)1   0.010213   0.022349   0.457 0.647689    
    ## groupTop3:s(date)2   0.001149   0.011866   0.097 0.922860    
    ## groupTop3:s(date)3   0.002054   0.015149   0.136 0.892170    
    ## groupTop3:s(date)4   0.003747   0.012481   0.300 0.764040    
    ## groupTop3:s(date)5  -0.003508   0.014102  -0.249 0.803545    
    ## groupTop3:s(date)6   0.002894   0.013212   0.219 0.826636    
    ## groupTop3:s(date)7  -0.007788   0.013582  -0.573 0.566359    
    ## groupTop3:s(date)8   0.005168   0.013172   0.392 0.694782    
    ## groupTop3:s(date)9  -0.010987   0.014660  -0.749 0.453596    
    ## groupTop3:s(date)10 -0.002273   0.012120  -0.188 0.851244    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 13:
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.034423   0.012487   2.757  0.00584 ** 
    ## groupTop3           -0.001206   0.021830  -0.055  0.95596    
    ## s(date)1            -0.045686   0.025447  -1.795  0.07261 .  
    ## s(date)2             0.067479   0.011764   5.736 9.82e-09 ***
    ## s(date)3             0.023797   0.016847   1.413  0.15781    
    ## s(date)4             0.016500   0.012440   1.326  0.18473    
    ## s(date)5             0.073203   0.015565   4.703 2.58e-06 ***
    ## s(date)6             0.038000   0.013993   2.716  0.00662 ** 
    ## s(date)7             0.025221   0.015425   1.635  0.10205    
    ## s(date)8             0.020439   0.016340   1.251  0.21100    
    ## s(date)9             0.044962   0.015962   2.817  0.00486 ** 
    ## s(date)10            0.028323   0.013708   2.066  0.03883 *  
    ## groupTop3:s(date)1   0.004100   0.043322   0.095  0.92460    
    ## groupTop3:s(date)2  -0.006964   0.022987  -0.303  0.76193    
    ## groupTop3:s(date)3   0.001576   0.029429   0.054  0.95730    
    ## groupTop3:s(date)4   0.001722   0.022261   0.077  0.93833    
    ## groupTop3:s(date)5  -0.026066   0.027804  -0.937  0.34852    
    ## groupTop3:s(date)6   0.012978   0.024166   0.537  0.59124    
    ## groupTop3:s(date)7  -0.013143   0.026738  -0.492  0.62304    
    ## groupTop3:s(date)8   0.020839   0.025539   0.816  0.41452    
    ## groupTop3:s(date)9  -0.025606   0.030011  -0.853  0.39354    
    ## groupTop3:s(date)10  0.007841   0.023421   0.335  0.73779    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

연속형 변수를 s()로 감싸주면 위와 같이 분위수 회귀처럼 구간구간별로 쪼개서 상호작용 효과를 살펴볼 수 있습니다. 여기서는 통계적으로 유의한 토픽이 없네요. 그래도 이를 시각화하는 방법을 살펴보도록 하겠습니다.

### 상호작용효과 평활법으로 시각화하기1

``` r
extract.estimateEffect(stm_fit3, "date", moderator="group", 
                       moderator.value=c("Top3", "Others"), topics = c(9)) %>% 
    mutate(covariate.value=seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=36)) %>% 
    ggplot(aes(group=moderator.value, color=moderator.value)) +
    geom_line(aes(covariate.value, estimate), size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(legend.position=c(0.25, 0.85)) +
    scale_color_manual(values=c("tomato", "royalblue")) +
    labs(x="time", y="Expected Topic Proportion", title="토픽9에 대한 상호작용 효과")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-10-kakaotalk-topic-analysis7_files/figure-gfm/stm_regression4-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

데이터를 뽑아낼 때 moderator를 범주형 변수로 선택하고 moderator.value에 두 범주를 모두 입력합니다. 원하는 토픽을 topics에 입력합니다. 이전 포스트에서 언급했던 방법을 이용하면 2개 이상의 토픽을 시각화할 수 있습니다. 그런데 36개의 데이터를 두 범주가 나눠 갖다보니 각지게 표현되었네요. 어떻게 해야 부드럽게 표현할 수 있을까요?

### 상호작용효과 평활법으로 시각화하기2

``` r
extract.estimateEffect(stm_fit3, "date", method="continuous", moderator="group", 
                       moderator.value=c("Top3", "Others"), topics = c(9)) %>% 
    mutate(covariate.value=seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=100)) %>% 
    ggplot(aes(group=moderator.value, color=moderator.value)) +
    geom_line(aes(covariate.value, estimate), size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(legend.position=c(0.25, 0.85)) +
    scale_color_manual(values=c("tomato", "royalblue")) +
    labs(x="time", y="Expected Topic Proportion", title="토픽9에 대한 상호작용 효과")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-10-kakaotalk-topic-analysis7_files/figure-gfm/stm_regression5-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

method로 continuous를 입력하면 디폴트로 100개의 데이터를 뽑아낼 수 있습니다. 3배 가까이 데이터가 많아졌기 때문에 한결 부드러운 그래프를 그릴 수 있습니다.

## 예고

다음 글에서 이전 포스트에서 분석했던 것들을 가지고 이렇게도 해보고 저렇게도 해보는 벌짓거리들을 소개해 보도록 하겠습니다.
