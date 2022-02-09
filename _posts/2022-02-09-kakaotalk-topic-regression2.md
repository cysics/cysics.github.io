---
title: "카카오톡 토픽 분석6 (회귀분석 연속형 변수)"
last_modified_at: 2022-02-09
categories: [텍스트 마이닝, 토픽 분석]
tag: [카카오톡, 토픽 분석, 회귀분석, tidyverse, stm]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 토픽 분석6"
---

<div class="notice--success">

카카오톡으로 오고간 대화의 토픽 분석합니다. 이번 글에서는 연속형 변수에
따른 토픽 발현확률을 회귀분석으로 분석해 보겠습니다.

</div>

## 미션 이해

구조적 토픽모델은 자체 함수를 이용해서 메타 데이터에 있는 다른 변수와
회귀분석을 할 수 있습니다. 다양한 변수들이 독립변수가 되고 토픽
발현확률은 종속변수가 됩니다. 이번 포스트에서는 연속형 변수에 따른 토픽
발현 확률을 어떻게 시각화하는지 살펴보겠습니다.

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

데이터 전처리, 토픽분석, 회귀분석 과정입니다. 이전 글에서 설명한 내용 그대로입니다.

### 회귀분석 결과 보기

``` r
plot(stm_fit, covariate="date", model=stm_topics, method = "continuous", topics = 9, 
     printlegend = T, xaxt = "n", xlab = "Time")
monthseq <- seq(from = as.Date("2019-02-01"), to = as.Date("2021-12-01"), by = "month")
axis(1, at = 0:max(data$date), labels = as.Date(monthseq))
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-09-kakaotalk-topic-analysis6_files/figure-gfm/stm_regression1-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

stm 패키지에서 제공하는 함수를 이용하여 연속변인인 date에 따른 토픽발현확률의 변화를 시각화합니다. 기본함수라서 대략적으로만 설명드리겠습니다.

범주형 변수의 경우에는 method에 difference를 넣었지만 연속형 변수의 경우에는 continuous를 입력합니다. 그래프의 x축을 년도와 월을 표현하기 위해서 seq() 함수를 이용해서 시작날짜부터 끝 날짜까지를 month 단위로 만들어줍니다. axis() 함수를 이용해서 seq() 함수로 만든 날짜를 표시해줍니다.

논문에 싣기에 큰 무리가 없을 정도로 시각화 되지만 조금 더 예쁘게 시각화하고 싶습니다. 어떻게 해야할까요?

### 회귀분석 결과 이쁘게 만들기

``` r
stm_date <- extract.estimateEffect(stm_fit, "date", topics = 9) %>% 
    mutate(covariate.value=seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=36))
stm_date %>% ggplot() +
    geom_line(aes(covariate.value, estimate), color="royalblue", size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed", color="lightcoral") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed", color="lightcoral") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    labs(x="time", y="Expected Topic Proportion", title="시간 흐름에 따른 토픽 발현 확률의 변화")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-09-kakaotalk-topic-analysis6_files/figure-gfm/stm_regression2-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

tidysim 패키지에서 지원하는 extract.estimateEffect() 함수를 이용하면 회귀계수를 뽑아낼 수 있습니다. stm 패키지에서 회귀분석할 때 공변량으로 날짜 데이터를 사용할 수 없기 때문에 억지로 0에서부터 35까지의 숫자로 월 변화를 수정하였었는데 이를 다시 날짜 형식으로 환원하여 시각화해야 합니다. 그래서 seq()함수를 이용하여 36개의 월 단위 데이터로 치환합니다. 

이렇게 저장된 stm_date 데이터를 ggplot() 함수로 시각화할 수 있습니다. 선을 그리는데 평균값과 신뢰구간을 표현하는 값이 다르기 때문에 ggplot() 함수에 축을 설정하지 않고 geom_line() 함수에 aes()로 x축과 y축의 값을 각각 지정하였습니다. ggplot() 함수로 시각화하면 선의 색깔이나 굵기 등 다양한 옵션을 조정할 수 있습니다. x축에 월 단위로 숫자가 표현되면 글자가 겹쳐서 보이기 때문에 scale_x_date() 함수를 이용하여 date_breaks로 6개월 간격으로 표시하도록 하고 date_labels로 년과 월까지만 표현하도록 하였습니다.

### 회귀분석 결과 2개 토픽 비교

``` r
stm_date <- extract.estimateEffect(stm_fit, "date", topics = c(2, 9)) %>% 
    mutate(topic=as.factor(topic)) %>% 
    mutate(covariate.value=rep(seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=36), 2))
stm_date %>% ggplot(aes(group=topic, color=topic)) +
    geom_line(aes(covariate.value, estimate), size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(legend.position=c(0.85, 0.85)) +
    scale_color_manual(values=c("tomato", "royalblue")) +
    labs(x="time", y="Expected Topic Proportion", title="시간 흐름에 따른 토픽 발현 확률의 변화")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-09-kakaotalk-topic-analysis6_files/figure-gfm/stm_regression3-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

토픽 1개가 아닌 2개의 토픽을 비교해서 시각화하고 싶을 때도 있습니다. 이 때에는 extract.estimateEffect() 함수에서 topics에 두 개의 토픽을 지정해 주어야 합니다. 그리고 토픽을 서로 다른 색으로 구분해서 시각화를 해야하기 때문에 topic을 factor로 만들어 줍니다. covariate.value값은 x축으로 사용할 변수인데 숫자로 되어 있는 것을 날짜로 바꿔 주어야 합니다. 이 때 토픽이 2개이기 때문에 rep() 함수를 이용하여 토픽의 개수만큼 반복해서 날짜를 만들어 주어야 합니다. 위 예제에서는 2개의 토픽이기 때문에 rep(seq(), 2) 형태로 표현하였습니다. 만약 토픽이 3개인 경우 2대신 3으로 입력해야 합니다.

기본적인 시각화는 앞선 포스트에서 설명한 내용과 큰 차이가 없습니다. 이전에 볼 수 없었던 scale\_color\_manual() 함수만 추가로 설명하겠습니다. 그룹으로 topic을 설정하면 토픽별로 그래프를 그려주긴 하지만 둘 다 검정색으로 표현됩니다. color까지 topic으로 설정해주어야 두 토픽이 서로 다른 색으로 표현됩니다. 디폴트로 제공해주는 색이 있긴 한데 비슷한 분석을 많이 하다보면 일률적인 색깔이 조금 불편할 때가 있습니다. 이럴 때 scale\_color\_manual() 함수를 이용하여 색을 임의로 바꿀 수 있습니다. 여기서는 tomato와 royalblue로 약간의 변화를 주어봤습니다. 위의 예제와 같이 미리 정의된 색의 이름으로 그래프의 색을 변화시킬 수도 있지만 \#으로 시작하는 색상코드를 이용하여 표현할 수도 있습니다. [색에 대한 다양한 정보](https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=keb0105&logNo=60031502918) 링크를 통해 구체적인 색을 비교해보고 색의 이름이나 색상코드 정보를 얻어서 적용할 수 있습니다. 참고로 모든 색의 이름을 다 쓸 수 있는 것은 아닙니다만 웬만한 색 이름은 잘 적용될 것입니다.

### 선형 회귀 분석

``` r
stm_fit1 <- estimateEffect(1:k ~ group+date, stmobj=stm_topics, metadata=stm_out$meta, uncertainty="Global")
stm_date <- extract.estimateEffect(stm_fit1, "date", topics = c(2, 9)) %>% 
    mutate(topic=as.factor(topic)) %>% 
    mutate(covariate.value=rep(seq(as.Date("2019-02-01"), as.Date("2021-12-01"), length.out=36), 2))
stm_date %>% ggplot(aes(group=topic, color=topic)) +
    geom_line(aes(covariate.value, estimate), size=1) +
    geom_line(aes(covariate.value, ci.lower), linetype="dashed") +
    geom_line(aes(covariate.value, ci.upper), linetype="dashed") +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(legend.position=c(0.85, 0.85)) +
    scale_color_manual(values=c("tomato", "royalblue")) +
    labs(x="time", y="Expected Topic Proportion", title="시간 흐름에 따른 토픽 발현 확률의 변화")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-02-09-kakaotalk-topic-analysis6_files/figure-gfm/stm_regression4-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

회귀분석을 할 때 s()를 붙여서 s(date)로 변수를 지정하면 10개의 구간으로 나누어 분위수 회귀처럼 구간별로 회귀분석 결과를 출력합니다. 그리고 그 결과를 시각화하면 월별 변화를 아주 세분화해서 어떻게 변화하는지 알 수 있습니다. 하지만 때로는 전체적인 경향만 알고 싶을 때도 있습니다. 이 경우에는 s()함수를 빼고 변수를 설정해주면 됩니다. stm_fit1은 그렇게 만들어진 회귀모델입니다. 이로부터 extract.estimateEffect() 함수로 토픽 2와 토픽 9에 대한 정보를 뽑아내서 시각화해 본 결과입니다. 선형회귀분석을 적용하여 전체적인 추세를 보다 직관적으로 잘 표현해 준 것을 알 수 있습니다.

## 예고

다음 글에서 연속형 변수에 따른 회귀분석에서 상호작용 효과를 살펴보도록 하겠습니다.
