---
title: "데이콘(생육 환경 최적화 경진대회) : 이미지 전처리1"
last_modified_at: 2022-04-16
categories: [데이콘, 이미지 전처리]
tag: [데이콘, 최적화, 이미지, 전처리]
author_profile: false
sidebar:
  nav: "docs"
toc: true
toc_sticky: true
toc_label: "이미지 전처리1"
---

<div class="notice--success">
데이콘(생육 환경 최적화 경진대회) : 어떻게 하면 예시로 제공된 3개의
이미지로 청경채만 구분해 낼 수 있을까?
</div>

## 최종 결과 확인

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result01.png){:style="display:block; margin-left:auto; margin-right:auto"}

### 이미지 오리기

``` r
#### 1. 분석 준비 ####
pacman::p_load(imager, magick, tidyverse)                  # 데이터 전처리 관련 패키지

#### 2. 이미지 오리기 ####
img <- image_read('data/sample1.jpg') %>% image_scale("328x246")   # magick 패키지 활용
paste1 <- image_crop(img, "75x20+10+130") %>% image_rotate(90)     # 배경 : 가로*세로 +x좌표+y좌표
paste2 <- image_crop(img, "15x15+165+100")                         # 청경채
paste1 %>% magick2cimg() %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/preprocessing-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
paste2 %>% magick2cimg() %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/preprocessing-2.png){:style="display:block; margin-left:auto; margin-right:auto"}

청경채가 자라는 판을 쉽게 들어 올리기 위해 만든 손잡이도 배경으로
지정해야 청경채를 바르게 잡아낼 수 있습니다. 하나의 이미지라면 아무
문제가 안되는데 청경채가 자라면서 손잡이를 가리는 경우도 있기 때문에
배경 지정이 애매해집니다. 그러다보니 사진마다 청경채를 제대로 잡아내기도
하지만 배경과 잘 구분되지 않는 경우도 생기더라구요.

그래서 손잡이 부분을 잘라서 회전시킨 다음 사진의 왼쪽 부분에 붙여
넣었습니다. 청경채가 다 자라도 가리기 힘든 부분에. 청경채도 잎맥 부분이
다른 곳보다 조금 더 밝아서 잎맥을 포함시켜서 오려서 청경채가 자라는 곳에
붙여 넣습니다. 모든 사진에 배경과 청경채를 붙여 넣은 후 이를 기반으로
청경채와 배경을 구분하려고 합니다.

### segmentation

``` r
ez_seg <- function(file="data/sample1.jpg", p1=paste1, p2=paste2, k=1, reverse_ok=FALSE){
  img <- image_read(file) %>% image_scale("328x246")                   # 이미지 불러서 크기를 줄인다.
  img <- image_composite(img, p1, offset="+0+100") %>%                 # 배경 붙이기(0,100) 
    image_composite(p2, offset="+165+100")                             # 청경채 붙이기 (165,100) 
  img <- magick2cimg(img)                                              # imager 데이터로 바꾼다.

  px.fg <- ((Xc(img) %inr% c(165, 180)) & (Yc(img) %inr% c(100, 130))) # 좌표 설정
  px.bg <- ((Xc(img) %inr% c(0, 20)) & (Yc(img) %inr% c(100,175)))     # x시작, x끝, y시작, y끝
  
  im.lab <- sRGBtoLab(img)                                             # RGB를 CIELAB 형태로 바꾼다.
  cvt.mat <- function(px) matrix(im.lab[px], sum(px)/3, 3)             # 이하 코드는 잘 모르겠다.
  fgMat <- cvt.mat(px.fg)
  bgMat <- cvt.mat(px.bg)
  labels <- c(rep(1, nrow(fgMat)), rep(0, nrow(bgMat)))
  testMat <- cvt.mat(px.all(img))
  out <- nabor::knn(rbind(fgMat, bgMat), testMat, k=k)                 # knn 적용하기
  out <- labels[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
  msk <- as.cimg(rep(out, 3), dim=dim(img))                            # 마스크 형태로 만든다.
  final <- as.pixset(1-threshold(msk,"16%"))                           # 이미지 반전
  ifelse(reverse_ok, return(final), return(msk))                       # 반전결과 출력
}
load.image('data/sample1.jpg') %>% resize(328, 246) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/ez_seg-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample1.jpg', k=1) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/ez_seg-2.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample1.jpg', k=1) %>% resize(33, 25) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/ez_seg-3.png){:style="display:block; margin-left:auto; margin-right:auto"}

데이콘에 샘플로 공개된 이미지가 3\~4M를 초과할 정도로 용량이 매우 큰
편입니다. 3280\*2464의 해상도를 가지고 있는데 이를 1/10 수준으로 줄여서
배경과 청경채를 붙여 넣습니다. 이 과정은 magick 패키지를 이용하였습니다.
일단 이미지를 불러오는 데 걸리는 시간이 짧고 크기가 다른 이미지를 합칠
수 있는 장점도 가지고 있습니다.

청경채와 배경을 구분하는 코드를 적용하기 위해 magick 파일을 imager에서
사용할 수 있는 형태로 바꾸었습니다. 나머지 코드는 [이
사이트](https://dahtah.github.io/imager/foreground_background.html){:target="_blank"}를
참고했습니다.

위의 결과물은 imager에서 이미지를 불러서 1/10 수준으로 줄인 다음
보여주는 것과 ez\_seg() 함수를 적용한 결과, 마지막은 ez\_seg() 함수를
적용한 후 다시 이미지 사이즈를 1/10 수준으로 줄인 결과를 보여줍니다.
어느 정도 크기의 이미지를 사용해야할지 아직 잘 모르기 때문에 대략적으로
구현해서 비교해 보았습니다.

참고로 ez\_seg() 함수를 적용할 때 k값의 크기를 바꿀 수 있는데 조금씩
결과물이 달라집니다. 현재로서는 k=1일 때가 가장 그럴듯해 보입니다.

### 적용 결과 마저 확인하기

``` r
load.image('data/sample2.jpg') %>% resize(328, 246) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-1.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample2.jpg', k=1) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-2.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample2.jpg', k=1) %>% resize(33, 25) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-3.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
load.image('data/sample3.jpg') %>% resize(328, 246) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-4.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample3.jpg', k=1) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-5.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample3.jpg', k=1) %>% resize(33, 25) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-6.png){:style="display:block; margin-left:auto; margin-right:auto"}

``` r
ez_seg('data/sample3.jpg', k=20) %>% resize(33, 25) %>% plot()
```

![](https://raw.githubusercontent.com/cysics/cysics.github.io/master/_posts/2022-04-16-digitizing_files/figure-gfm/result-7.png){:style="display:block; margin-left:auto; margin-right:auto"}

맨 마지막 결과물과 그 이전 결과물이 아주 조금 차이납니다. ez\_seg()
함수를 적용할 때 k값을 조정해서 미묘한 차이를 만들어 봤습니다.
