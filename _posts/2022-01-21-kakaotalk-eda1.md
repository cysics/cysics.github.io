---
title: "카카오톡 대화 EDA(1)"
last_modified_at: 2022-01-21
categories: TextMining
tag: [카카오톡, 전처리, 데이터 시각화, tidyverse, lubridate, ggplot2]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "카카오톡 대화 EDA(1)"
---
<div class="notice--success">
다양한 변인에 따른 발언수를 비교해 보겠습니다.
</div>

## 미션 이해

년도, 월, 일, 요일, 오전 오후, 시간 등 데이터 분석에 필요한 각종 변수를 만들었으니 이를 활용해서 다양한 변수에 따른 발언수를 비교해 보겠습니다. 전체 기간 동안 발언수가 가장 많은 10명은 누구인지, 요일별 발언수를 비교해보기도 하겠습니다. 이번 포스트에서는 범주형에 해당하는 닉네임, 오전과 오후, 요일에 대한 분석 위주로 설명드리겠습니다.

## 최종 결과 확인

### 발언 수가 많은 상위 10명

``` r
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
n_by_name <- data %>% select(name) %>% table() %>% as_tibble() %>% 
    rename(name=1) %>% arrange(desc(n)) %>% slice(1:10) # %>% 
    # mutate(name=as.factor(name) %>% droplevels()      # 실명을 감추기 위한 코드
    )
# levels(n_by_name$name) <- letters[seq(from=1, to=10)] # 실명을 감추기 위한 코드

n_by_name %>% ggplot(aes(n, reorder(name, n), fill=name)) +
    geom_col(show.legend = FALSE) + 
    geom_text(aes(n, reorder(name, n), label=comma(n)), hjust=1, size=4.5) +
    scale_x_continuous(label=comma) +
    theme_few() +
    scale_fill_brewer(palette="Set3")+
    labs(x = "발언 수", y = "", title = "발언 수가 많은 상위 10명")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-21-kakaotalk-eda1_files/figure-gfm/n_by_name-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

### 오전과 오후 대화량 비교

``` r
data %>% select(ampm) %>% table() %>% as_tibble() %>% 
    rename(ampm=1) %>% arrange(desc(n)) %>% slice(1:10) %>% 
    ggplot(aes(n, reorder(ampm, n), fill=ampm)) +
    geom_col(show.legend = FALSE) + 
    geom_text(aes(n, reorder(ampm, n), label=comma(n)), hjust=c(1,0), size=4.5) +
    scale_x_continuous(label=comma) +
    theme_few() +
    scale_fill_brewer(palette="Set3")+
    labs(x = "발언 수", y = "", title = "오전과 오후 대화량 비교")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-21-kakaotalk-eda1_files/figure-gfm/n_by_ampm-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

### 요일별 대화량 비교1

``` r
data %>% select(wday) %>% table() %>% as_tibble() %>% 
    rename(wday=1) %>% arrange(desc(n)) %>% slice(1:10) %>% 
    ggplot(aes(n, reorder(wday, n), fill=wday)) +
    geom_col(show.legend = FALSE) + 
    geom_text(aes(n, reorder(wday, n), label=comma(n)), hjust=1, size=4.5) +
    scale_x_continuous(label=comma) +
    theme_few() +
    scale_fill_brewer(palette="Set3")+
    labs(x = "발언 수", y = "", title = "요일별 대화량 비교")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-21-kakaotalk-eda1_files/figure-gfm/n_by_week1-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

### 요일별 대화량 비교2

``` r
data %>% select(wday) %>% table() %>% as_tibble() %>% 
    rename(wday=1) %>% arrange(desc(n)) %>% slice(1:10) %>% 
    mutate(wday=factor(wday, levels=c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))) %>% 
    # mutate(wday=factor(wday, levels=c("Monday", "Tuesday", "Wednesday", 
    #                                   "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
    ggplot(aes(wday, n, fill=wday)) + geom_col(alpha=0.8, show.legend=F) +
    geom_text(aes(label=comma(n)), hjust=0.5, vjust=1.5, nudge_y=0.5, size=4) +
    scale_y_continuous(label=comma) +
    theme_few() +
    labs(x="", y="발언 수", title = "요일별 대화량 비교")
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-01-21-kakaotalk-eda1_files/figure-gfm/n_by_week2-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

## 코드 설명

### 빈도수 만들기

data에서 문자로 된 변수는 name, ampm, wday가 있습니다. 각각 이름, 오전과 오후 구분, 요일 정보가 들어 있습니다. 각각의 변수들은 특정 범주에 해당하는 데이터만 가지고 있다는 특징이 있습니다. 이러한 변수를 범주형 변수라고 합니다. 정리하자면 범주형 변수에 따른 발언량을 비교한다고 볼 수 있습니다.

select() 함수를 사용해서 data에서 해당 변수만 선택한 후 table() 함수를 적용하면 범주별로 빈도수 통계를 확인할 수 있습니다. as\_tibble() 함수를 적용하면 tibble 형태의 데이터가 됩니다. 이 때 빈도수의 변수명은 n이 되는데 범주는 점(.)으로 이름이 명명 되어 있어서 이를 rename() 함수로 바꿔줍니다. 그리고 arrange() 함수로 정렬합니다. 이 때 desc() 함수를 적용해야 내림차순으로 정렬합니다. 그리고 상위 1에서 10까지만 선택하면 발언수가 많은 사람 10명을 선정할 수 있습니다. 이하 오전과 오후 구분이나 요일별 대화량 분석도 같은 맥락으로 데이터를 얻을 수 있습니다.

초보가 아니라면 group\_by() 함수와 summarise() 함수를 이용해서 구현할 수도 있습니다.

``` r
n_by_name <- data %>% group_by(name) %>% summarise(n=n()) %>% 
     arrange(desc(n)) %>% slice(1:10) %>% mutate(name=as.factor(name))
levels(n_by_name$name) <- letters[seq(from=1, to=10)]
n_by_name
```

    ## # A tibble: 10 x 2
    ##    name      n
    ##    <fct> <int>
    ##  1 b      4342
    ##  2 e      3220
    ##  3 g      3034
    ##  4 j      1170
    ##  5 a      1137
    ##  6 c      1059
    ##  7 h       904
    ##  8 d       597
    ##  9 i       587
    ## 10 f       513

mutate(name=as.factor(name) %&gt;% droplevels() 는 실명을 문자로 처리하기 위한 코드입니다. 여러분들은 사용할 필요가 없습니다.

### 그래프 그리기

그래프를 설정하는 함수가 ggplot() 입니다. aes()는 그래프의 축을 설정한다고 보면 됩니다. 요일별 대화량을 비교했던 그래프를 기준으로 보통 ggplot(aes(x=wday, y=n))으로 표현해서 x축은 wday(요일)이 되고 y축은 n(빈도수)가 되도록 설정하죠. 그런데 영어의 경우 단어 순서에 따라 주어, 동사 등을 구분하는 습관이 있다보니 맨 앞에 쓴 것은 x축이고 그 다음은 y축에 해당한다고 생각합니다. 그러다보니 문법에 의해 x축 y축을 지정하지 않고 ggplot(aes(wday, n))으로 표현할 수도 있습니다. fill은 색을 칠하는 변수를 지정하면 됩니다.

ggplot(aes(wday, n, fill=wday))는 그래프의 축만 설정한 것이고요, 여기에 선이나 막대를 그려서 시각화합니다. 플러스(+) 기호와 함께 geom\_col() 함수를 사용하면 막대 그래프를 그릴 수 있습니다. 참고로 범주형 데이터를 시각화할 때는 선그래프가 아닌 막대 그래프 형태로 나타내야 합니다. 물론 파이 그래프 등 다양하게 표현할 수도 있지만 일단 초보분들이 많을 거라 생각해서 막대 그래프로 통일해서 설명드리겠습니다. alpha는 파라미터들 중 하나인데 이는 투명도를 나타냅니다. 0은 완전 투명해서 하나도 안 보이는 것이고요, 1은 전혀 투명하지 않은 색이 됩니다. 0.8 정도면 약간 투명한 정도를 나타내죠. show.legend라는 파라미터는 범례를 표현할지의 여부를 나타내는데 F라고만 입력하면 FALSE로 인식해서 범례가 표시되지 않습니다. 축에 월요일부터 일요일까지 다 표시되는데 굳이 범례를 표시할 필요가 없습니다.

geom\_text() 함수는 막대 그래프에 수치를 표현하라는 함수입니다. aes() 함수의 label이라는 파라미터를 지정해서 어떤 값을 표기할지 정합니다. n(빈도수)를 표현하는데 이를 comma() 함수로 감쌌죠. comma는 scales 패키지에서 지원하는데 1000단위로 콤마(,)를 찍어서 구분해줍니다. 예를 들어 10만이 있을 경우 콤마가 없으면 100000이 되는데 comma(100000) 하면 100,000으로 표현되어 가독성을 높여줍니다. hjust, vjust, nudge\_y는 텍스트의 위치를 지정하는 파라미터이고, size는 글자 크기를 지정하는 파라미터입니다. 적절한 값을 입력해 보면서 원하는 위치에 원하는 크기로 표현될 수 있도록 조정할 수 있습니다.

scale\_y\_continuous() 함수를 이용해서 y축에 있는 값들도 1000을 넘어서면 콤마를 찍어서 구분하게 할 수 있고 경우에 따라서는 scale\_x\_continuous() 함수를 이용해서 x축에 있는 값들에 콤마를 찍을 수도 있습니다.

theme\_few() 함수는 ggthemes 패키지에서 지원하는 여러 가지 형태 중 심플한 형태의 그래프를 지정해서 출력할 수 있습니다.

다른 그래프의 경우 scale\_fill\_brewer() 함수를 사용해서 색상을 원하는 것으로 지정할 수도 있습니다. 남들이 만들어 놓은 좋은 색상인데 개수가 많지 않아서 범주가 많게 되면 사용하기 힘들다는 단점이 있습니다. 우선 Set1, Set2, Set3를 바꿔가면서 색깔이 어떻게 바뀌는지 확인해보는 것을 권유합니다.

labs() 함수는 x축, y축, 그래프 제목 등을 임의로 지정할 수 있는 함수입니다.

### 시각화 팁

대체로 범주의 개수가 4개\~7개 정도까지는 세로 막대 그래프가 예쁩니다. 범주의 개수가 3개 이하이거나 8개 이상일 경우 세로 막대보다는 가로 막대가 더 깔끔해요.. 특히 범주 이름이 긴 경우에는 가로 막대가 예쁘게 그려집니다.

### 예고

다음 글에서는 년도, 분기, 월 등 연속형 데이터에 따른 발언량을 비교해 보도록 하겠습니다. 고려해야할 사항이 몇 가지 있어서 따로 구분해서 설명하고자 합니다.
