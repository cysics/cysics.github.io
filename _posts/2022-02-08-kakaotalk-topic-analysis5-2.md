---
title: "카카오톡 토픽 분석5 (회귀분석 범주형 변수)"
last_modified_at: 2022-02-08
categories: [텍스트 마이닝, 토픽 분석]
tag: [카카오톡, 토픽 분석, 회귀분석, tidyverse, stm]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 토픽 분석5"
---
<div class="notice--success">
카카오톡으로 오고간 대화의 토픽 분석합니다. 이번 글에서는 다양한 변수에 따른 토픽 발현확률을 회귀분석으로 분석해 보겠습니다.
</div>

## 미션 이해

구조적 토픽모델은 자체 함수를 이용해서 메타 데이터에 있는 다른 변수와
회귀분석을 할 수 있습니다. 다양한 변수들이 독립변수가 되고 토픽
발현확률은 종속변수가 됩니다.

## 최종 결과 확인

### 형태소 분석하기

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

### 회귀분석 결과 보기

``` r
summary(stm_fit <- estimateEffect(formula=1:k ~ group + s(date), 
                                  stmobj=stm_topics, metadata=stm_out$meta, uncertainty="Global"))
```

    ## 
    ## Call:
    ## estimateEffect(formula = 1:k ~ group + s(date), stmobj = stm_topics, 
    ##     metadata = stm_out$meta, uncertainty = "Global")
    ## 
    ## 
    ## Topic 1:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.116040   0.008939  12.981  < 2e-16 ***
    ## groupTop3   -0.006529   0.001344  -4.856 1.21e-06 ***
    ## s(date)1    -0.054264   0.016745  -3.241  0.00119 ** 
    ## s(date)2    -0.004349   0.008312  -0.523  0.60084    
    ## s(date)3    -0.045754   0.010986  -4.165 3.13e-05 ***
    ## s(date)4    -0.037490   0.009015  -4.159 3.21e-05 ***
    ## s(date)5    -0.051090   0.010001  -5.108 3.27e-07 ***
    ## s(date)6    -0.045959   0.009651  -4.762 1.93e-06 ***
    ## s(date)7    -0.027384   0.009858  -2.778  0.00548 ** 
    ## s(date)8    -0.087377   0.010870  -8.038 9.53e-16 ***
    ## s(date)9     0.007505   0.010576   0.710  0.47794    
    ## s(date)10   -0.039141   0.009774  -4.005 6.23e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 2:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.178276   0.013391  13.313  < 2e-16 ***
    ## groupTop3   -0.035618   0.001993 -17.872  < 2e-16 ***
    ## s(date)1     0.026908   0.025240   1.066 0.286403    
    ## s(date)2    -0.023875   0.013504  -1.768 0.077072 .  
    ## s(date)3    -0.046075   0.015779  -2.920 0.003503 ** 
    ## s(date)4    -0.056324   0.014941  -3.770 0.000164 ***
    ## s(date)5    -0.102462   0.014687  -6.977 3.11e-12 ***
    ## s(date)6    -0.001302   0.014711  -0.088 0.929483    
    ## s(date)7    -0.144635   0.014994  -9.646  < 2e-16 ***
    ## s(date)8    -0.020851   0.016310  -1.278 0.201119    
    ## s(date)9    -0.129988   0.015191  -8.557  < 2e-16 ***
    ## s(date)10   -0.087468   0.013986  -6.254 4.07e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 3:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.093982   0.008407  11.179  < 2e-16 ***
    ## groupTop3    0.002211   0.001140   1.941 0.052302 .  
    ## s(date)1    -0.013803   0.015408  -0.896 0.370346    
    ## s(date)2    -0.015087   0.007989  -1.888 0.058974 .  
    ## s(date)3    -0.026486   0.010938  -2.421 0.015470 *  
    ## s(date)4    -0.019418   0.008119  -2.392 0.016776 *  
    ## s(date)5    -0.027955   0.010600  -2.637 0.008361 ** 
    ## s(date)6    -0.030212   0.008551  -3.533 0.000411 ***
    ## s(date)7    -0.012258   0.010421  -1.176 0.239502    
    ## s(date)8    -0.063727   0.009908  -6.432 1.28e-10 ***
    ## s(date)9    -0.019907   0.010318  -1.929 0.053712 .  
    ## s(date)10   -0.032161   0.009212  -3.491 0.000482 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 4:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.045437   0.010200   4.455 8.44e-06 ***
    ## groupTop3    0.007653   0.001916   3.994 6.52e-05 ***
    ## s(date)1    -0.094986   0.019650  -4.834 1.35e-06 ***
    ## s(date)2     0.085856   0.010523   8.159 3.56e-16 ***
    ## s(date)3     0.015667   0.012966   1.208  0.22692    
    ## s(date)4    -0.015455   0.010808  -1.430  0.15277    
    ## s(date)5     0.087815   0.012744   6.891 5.70e-12 ***
    ## s(date)6     0.004271   0.011859   0.360  0.71875    
    ## s(date)7     0.042275   0.013188   3.206  0.00135 ** 
    ## s(date)8     0.019015   0.013574   1.401  0.16129    
    ## s(date)9     0.018466   0.014164   1.304  0.19234    
    ## s(date)10    0.032524   0.011377   2.859  0.00426 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 5:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.070699   0.008587   8.233  < 2e-16 ***
    ## groupTop3    0.011455   0.001508   7.594 3.22e-14 ***
    ## s(date)1    -0.021096   0.016363  -1.289 0.197336    
    ## s(date)2     0.010699   0.008171   1.309 0.190438    
    ## s(date)3    -0.008538   0.010991  -0.777 0.437286    
    ## s(date)4     0.027196   0.008913   3.051 0.002282 ** 
    ## s(date)5     0.040515   0.009480   4.274 1.93e-05 ***
    ## s(date)6    -0.003693   0.009945  -0.371 0.710399    
    ## s(date)7     0.035173   0.010480   3.356 0.000792 ***
    ## s(date)8     0.038824   0.011340   3.424 0.000619 ***
    ## s(date)9    -0.007480   0.010347  -0.723 0.469736    
    ## s(date)10    0.011537   0.009248   1.248 0.212211    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 6:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.067912   0.010323   6.579 4.85e-11 ***
    ## groupTop3    0.001065   0.001588   0.671 0.502481    
    ## s(date)1     0.064660   0.019039   3.396 0.000684 ***
    ## s(date)2    -0.016728   0.009492  -1.762 0.078042 .  
    ## s(date)3     0.028721   0.012741   2.254 0.024188 *  
    ## s(date)4     0.054442   0.010604   5.134 2.86e-07 ***
    ## s(date)5     0.029046   0.011752   2.472 0.013460 *  
    ## s(date)6     0.035243   0.011438   3.081 0.002065 ** 
    ## s(date)7     0.042809   0.011710   3.656 0.000257 ***
    ## s(date)8     0.022525   0.013356   1.687 0.091709 .  
    ## s(date)9     0.006642   0.011834   0.561 0.574605    
    ## s(date)10    0.034510   0.010664   3.236 0.001214 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 7:
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0636457  0.0060800  10.468  < 2e-16 ***
    ## groupTop3   -0.0001197  0.0009835  -0.122  0.90316    
    ## s(date)1    -0.0198765  0.0115969  -1.714  0.08655 .  
    ## s(date)2     0.0089108  0.0062090   1.435  0.15126    
    ## s(date)3    -0.0226122  0.0078952  -2.864  0.00419 ** 
    ## s(date)4     0.0328445  0.0064841   5.065 4.11e-07 ***
    ## s(date)5    -0.0430477  0.0071302  -6.037 1.59e-09 ***
    ## s(date)6     0.0346743  0.0065727   5.275 1.34e-07 ***
    ## s(date)7    -0.0177050  0.0069029  -2.565  0.01033 *  
    ## s(date)8     0.0221084  0.0085052   2.599  0.00934 ** 
    ## s(date)9    -0.0124407  0.0077378  -1.608  0.10790    
    ## s(date)10   -0.0083963  0.0064110  -1.310  0.19032    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 8:
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0284926  0.0032644   8.728  < 2e-16 ***
    ## groupTop3    0.0096807  0.0005072  19.088  < 2e-16 ***
    ## s(date)1     0.0145347  0.0061596   2.360 0.018298 *  
    ## s(date)2    -0.0091853  0.0030005  -3.061 0.002206 ** 
    ## s(date)3     0.0180235  0.0040295   4.473 7.76e-06 ***
    ## s(date)4     0.0024922  0.0033223   0.750 0.453184    
    ## s(date)5     0.0136166  0.0037337   3.647 0.000266 ***
    ## s(date)6     0.0085909  0.0036804   2.334 0.019594 *  
    ## s(date)7     0.0266098  0.0036164   7.358 1.93e-13 ***
    ## s(date)8     0.0081005  0.0042688   1.898 0.057760 .  
    ## s(date)9     0.0147317  0.0039667   3.714 0.000205 ***
    ## s(date)10    0.0231527  0.0035603   6.503 8.03e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 9:
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0375190  0.0050817   7.383 1.60e-13 ***
    ## groupTop3    0.0258351  0.0010273  25.150  < 2e-16 ***
    ## s(date)1    -0.0319910  0.0101401  -3.155  0.00161 ** 
    ## s(date)2     0.0290506  0.0051483   5.643 1.69e-08 ***
    ## s(date)3    -0.0009800  0.0066939  -0.146  0.88361    
    ## s(date)4     0.0133407  0.0053250   2.505  0.01224 *  
    ## s(date)5     0.0192196  0.0059371   3.237  0.00121 ** 
    ## s(date)6     0.0026135  0.0062341   0.419  0.67505    
    ## s(date)7     0.0434001  0.0062534   6.940 4.02e-12 ***
    ## s(date)8     0.0005533  0.0069015   0.080  0.93610    
    ## s(date)9     0.0466776  0.0068627   6.802 1.06e-11 ***
    ## s(date)10    0.0307084  0.0054501   5.634 1.78e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 10:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.056435   0.007981   7.071 1.58e-12 ***
    ## groupTop3    0.017954   0.001179  15.228  < 2e-16 ***
    ## s(date)1     0.014253   0.014493   0.983 0.325377    
    ## s(date)2    -0.014738   0.007325  -2.012 0.044228 *  
    ## s(date)3     0.010031   0.010223   0.981 0.326502    
    ## s(date)4     0.006830   0.008108   0.842 0.399542    
    ## s(date)5     0.038029   0.009108   4.175 2.98e-05 ***
    ## s(date)6    -0.013511   0.008689  -1.555 0.119963    
    ## s(date)7     0.055442   0.009643   5.750 9.05e-09 ***
    ## s(date)8    -0.019936   0.009844  -2.025 0.042855 *  
    ## s(date)9     0.070249   0.009358   7.507 6.28e-14 ***
    ## s(date)10    0.034067   0.008971   3.797 0.000147 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 11:
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0514331  0.0050905  10.104  < 2e-16 ***
    ## groupTop3   -0.0110657  0.0008771 -12.616  < 2e-16 ***
    ## s(date)1     0.0036018  0.0096348   0.374  0.70853    
    ## s(date)2    -0.0055257  0.0049600  -1.114  0.26527    
    ## s(date)3     0.0103577  0.0066714   1.553  0.12054    
    ## s(date)4     0.0019203  0.0051480   0.373  0.70914    
    ## s(date)5     0.0164702  0.0061411   2.682  0.00732 ** 
    ## s(date)6     0.0044522  0.0056939   0.782  0.43427    
    ## s(date)7     0.0051069  0.0059162   0.863  0.38803    
    ## s(date)8     0.0436632  0.0064655   6.753 1.48e-11 ***
    ## s(date)9    -0.0010604  0.0068789  -0.154  0.87749    
    ## s(date)10    0.0127694  0.0054870   2.327  0.01996 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 12:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.173318   0.013916  12.455  < 2e-16 ***
    ## groupTop3   -0.027118   0.002030 -13.359  < 2e-16 ***
    ## s(date)1     0.029518   0.025701   1.149  0.25076    
    ## s(date)2    -0.006963   0.014205  -0.490  0.62399    
    ## s(date)3    -0.042995   0.016585  -2.592  0.00953 ** 
    ## s(date)4    -0.029325   0.015058  -1.947  0.05149 .  
    ## s(date)5    -0.086747   0.015188  -5.711 1.13e-08 ***
    ## s(date)6    -0.048857   0.015717  -3.108  0.00188 ** 
    ## s(date)7    -0.100689   0.015261  -6.598 4.26e-11 ***
    ## s(date)8    -0.016542   0.016678  -0.992  0.32131    
    ## s(date)9    -0.045846   0.016810  -2.727  0.00639 ** 
    ## s(date)10   -0.067422   0.014001  -4.815 1.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Topic 13:
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.016945   0.006694   2.531  0.01137 *  
    ## groupTop3    0.004557   0.001086   4.198 2.71e-05 ***
    ## s(date)1     0.082190   0.013162   6.244 4.33e-10 ***
    ## s(date)2    -0.038230   0.006151  -6.215 5.22e-10 ***
    ## s(date)3     0.110606   0.009131  12.113  < 2e-16 ***
    ## s(date)4     0.018662   0.006865   2.718  0.00657 ** 
    ## s(date)5     0.066601   0.008239   8.084 6.59e-16 ***
    ## s(date)6     0.053623   0.007391   7.255 4.14e-13 ***
    ## s(date)7     0.051656   0.008058   6.410 1.48e-10 ***
    ## s(date)8     0.053489   0.008887   6.019 1.78e-09 ***
    ## s(date)9     0.052465   0.007819   6.710 1.99e-11 ***
    ## s(date)10    0.055074   0.007041   7.822 5.41e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

종속변수는 1\~13까지의 토픽이고 독립변수는 group과 date로
설정하였습니다. group은 대화량이 많은 상위 3명인지의 여부입니다. 대화가
많은 사람은 그렇지 않은 사람들과 다른 관심사(토픽)이 있을 거라는 가설을
검증하고자 합니다. 아울러 date는 카톡방 초기부터의 개월수를 가리키는데
시간이 흐름에 따라 토픽 발현 확률이 달라질 거라는 가설을 검증하기 위한
변수입니다. date를 s()로 감싸면 연속변수를 10단계로 나누어서 분위수
회귀처럼 구간 구간별로 분석해줍니다. s() 함수를 제외하면 선형회귀분석을
적용하여 추세를 살펴볼 수 있습니다.

uncertainty 때문에 회귀분석을 실시할 때마다 조금씩 그 결과가 달라질 수
있습니다. 카톡방 글쓰기처럼 데이터가 매우 많은 빅데이터의 경우 웬만하면
다 통계적으로 유의하다는 결과가 나옵니다. 그렇지 않게 나온 것이 특이할
정도입니다. 빅데이터의 경우 통계적으로 유의한지의 여부보다는
효과크기(effect size)를 눈여겨 볼 필요가 있습니다. 회귀분석에서의 effect
size는 Estimate입니다. 이 값이 0.01보다 작은 경우 통계적으로
유의하더라도 큰 차이가 없는 것으로 볼 수 있습니다. 반대로 Estimate가
0.01보다 큰 경우 혹은 -0.01보다 작은 경우 의미있는 결과로 해석할 수
있습니다.

분석결과를 보면 groupTop3라고 나옵니다. 이는 Others에 비해 Top3가 어떻게
다른지를 나타내줍니다. Topic 5의 경우 0.01보다 크게 나옵니다. 이는 다른
사람들보다 Top3에 해당하는 사람들은 Topic 5에 더 관심이 있다는 뜻입니다.
반면에 Topic 2는 -0.01보다 작습니다. 이는 다른 사람들보다 Topic 2와
관련된 글은 잘 쓰지 않았다는 것을 의미합니다.

분석결과가 너무 복잡합니다. group에 해당하는 값만 뽑아서 볼 수는
없을까요?

### 범주형 변수에 따른 회귀계수 비교

``` r
extract.estimateEffect(stm_fit, covariate="group", method = "difference", 
                        cov.value1="Top3", cov.value2="Others")
```

    ##        method topic covariate covariate.value      estimate    std.error
    ## 1  difference     1     group     Top3-Others -6.501383e-03 0.0013290890
    ## 2  difference     2     group     Top3-Others -3.560739e-02 0.0020039307
    ## 3  difference     3     group     Top3-Others  2.247597e-03 0.0011429958
    ## 4  difference     4     group     Top3-Others  7.675776e-03 0.0019430477
    ## 5  difference     5     group     Top3-Others  1.144868e-02 0.0015237888
    ## 6  difference     6     group     Top3-Others  1.029437e-03 0.0015938454
    ## 7  difference     7     group     Top3-Others -8.774613e-05 0.0009867998
    ## 8  difference     8     group     Top3-Others  9.682212e-03 0.0005085546
    ## 9  difference     9     group     Top3-Others  2.581877e-02 0.0010339649
    ## 10 difference    10     group     Top3-Others  1.796209e-02 0.0011663131
    ## 11 difference    11     group     Top3-Others -1.108652e-02 0.0008854231
    ## 12 difference    12     group     Top3-Others -2.712903e-02 0.0020360425
    ## 13 difference    13     group     Top3-Others  4.558819e-03 0.0010922018
    ##    ci.level      ci.lower     ci.upper
    ## 1      0.95 -0.0090584043 -0.003881037
    ## 2      0.95 -0.0394040440 -0.031502003
    ## 3      0.95  0.0001352147  0.004522569
    ## 4      0.95  0.0039116550  0.011568983
    ## 5      0.95  0.0083848311  0.014349055
    ## 6      0.95 -0.0022885640  0.004005976
    ## 7      0.95 -0.0019283566  0.001935687
    ## 8      0.95  0.0086720951  0.010724924
    ## 9      0.95  0.0238162880  0.027773292
    ## 10     0.95  0.0157451078  0.020288049
    ## 11     0.95 -0.0128628215 -0.009379041
    ## 12     0.95 -0.0310065470 -0.023035178
    ## 13     0.95  0.0023392757  0.006635055
    ##                                                 label
    ## 1   Topic 1 (Covariate Level Top3 Compared to Others)
    ## 2   Topic 2 (Covariate Level Top3 Compared to Others)
    ## 3   Topic 3 (Covariate Level Top3 Compared to Others)
    ## 4   Topic 4 (Covariate Level Top3 Compared to Others)
    ## 5   Topic 5 (Covariate Level Top3 Compared to Others)
    ## 6   Topic 6 (Covariate Level Top3 Compared to Others)
    ## 7   Topic 7 (Covariate Level Top3 Compared to Others)
    ## 8   Topic 8 (Covariate Level Top3 Compared to Others)
    ## 9   Topic 9 (Covariate Level Top3 Compared to Others)
    ## 10 Topic 10 (Covariate Level Top3 Compared to Others)
    ## 11 Topic 11 (Covariate Level Top3 Compared to Others)
    ## 12 Topic 12 (Covariate Level Top3 Compared to Others)
    ## 13 Topic 13 (Covariate Level Top3 Compared to Others)

extract.estimateEffect() 함수에서 공변량(covariate)을 group으로,
method를 difference로 설정하면 두 집단의 회귀계수의 차이만 뽑아서 볼 수
있습니다.

이제 이 결과들을 한눈에 쉽게 볼 수 있도록 시각화 해봅시다.

### 범주형에 따른 회귀계수의 차1

``` r
plot(stm_fit, covariate="group", topics=c(1:k),   
     model=stm_fit, method="difference", 
     cov.value1="Top3", cov.value2="Others",      
     xlab="Others ... vs ... Top3", labeltype = "custom", xlim = c(-0.06, 0.06), 
     custom.labels = c('토픽 1', '세미나 공지', '공지 및 안내', '토픽 4', '토픽 5',
                       '토픽 6', '토픽 7', '토픽 8', '연구 논문', '토픽 10',
                       '토픽 11', '토픽12', '토픽13'))
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-08-kakaotalk-topic-analysis5_files/figure-gfm/stm_regression3-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

공변량을 group으로 설정하고 토픽은 1부터 13까지 모두 표현합니다.
method를 difference로 설정해서 차이를 시각화합니다. cov.value1은 공변량
범주 중 그래프의 오른쪽에 놓일 범주를 입력합니다. cov.value2는 그래프
x축의 왼쪽편에 놓일 범주입니다. xlim으로 출력 범위를 설정할 수 있고
custom.labels를 이용하여 토픽 이름을 표현할 수도 있습니다. 여기서는
앞에서 살펴본 토픽2, 3, 9만 토픽명을 입력해봤습니다.

토픽 9에 해당하는 “연구 논문”에 대한 토픽은 Top3가 나머지 사람들보다
토픽 발현 확률이 높은 편에 속합니다. 토픽 10도 의미있는 차이로 보입니다.
반면에 토픽 2에 해당하는 세미나 공지나 토픽 12의 발현 확률은 의미있게
낮은 것으로 나타났습니다.

### 범주형에 따른 회귀계수의 차2

``` r
extract.estimateEffect(stm_fit, "group", method = "difference",
                       cov.value1="Top3", cov.value2="Others") %>% 
    mutate(topic=paste0("Topic ", topic)) %>% 
    mutate(topic=recode(topic, "Topic 2"="세미나 공지",
                        "Topic 3"="공지 및 안내",
                        "Topic 9"="연구 논문")) %>% 
    mutate(topic=fct_reorder(topic, parse_number(label))) %>% 
    ggplot(aes(estimate, topic, color=topic)) +
    geom_point(show.legend=F) +
    geom_errorbar(aes(xmin=ci.lower, xmax=ci.upper), 
                  width=0.2, show.legend=F) +
    geom_vline(xintercept=0, linetype="dashed") +
    theme_bw() +
    labs(x="Other 대비 Top3", y="", title="group별 회귀계수 차이")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-08-kakaotalk-topic-analysis5_files/figure-gfm/stm_regression4-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

recode() 함수를 써서 Topic 명을 바꿔줍니다. fct\_reorder() 함수를 적용할
때 parse\_number는 topic이 아닌 label로 바꿔야 원래 토픽순으로 정렬이
됩니다.

시각화에서는 geom\_errorbar() 함수로 오차막대를 추가합니다. 최솟값으로는
ci.lower를 최댓값으로는 ci.upper로 지정합니다. geom\_vline() 함수로 0에
해당하는 선을 그려줍니다. linetype을 dashed로 설정하면 점선 형태로 선이
추가됩니다.

### 두 토픽 간 비교

``` r
plot(stm_topics, type="perspectives", topics=c(2, 9))
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-08-kakaotalk-topic-analysis5_files/figure-gfm/stm_regression5-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

두 토픽에서 각각 빈도수가 높은 단어의 크기로 나타내고 각각의 토픽의
특성을 나타내는 단어는 각각 좌우로 치우쳐서 나타나는 형태로 두 토픽을
비교하여 빈도수가 높은 단어들을 시각화시켜줍니다. 단점이 있다면 실행시킬
때마다 글자의 위치가 바뀌는데 그러다보면 글자들이 겹쳐서 보이는 경우가
많습니다. 적당하게 반복 실행시켜서 글자들이 잘 겹치지 않는 상태를
만들어서 사용하는 것이 좋습니다.

## 예고

다음 글에서 연속형 변수에 따른 회귀분석 결과를 시각화 해보도록
하겠습니다.
