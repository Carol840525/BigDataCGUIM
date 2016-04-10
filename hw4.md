Facebook粉絲團分析 分析專頁：蔡英文 Tsai Ing-wen
================

分析蔡英文總統的粉絲專業中的每日發文、每日like數、每日評論數以及每日分享數，資料分析區間為2016/01/01至2016/04/10

讀取蔡英文粉絲團資料
--------------------

``` r
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
```

``` r
token <- 'EAACEdEose0cBAG0E5XOUiqvIHfVhtJcIfZAjIKaMcj185ZAaOflOjZAhoGfT0ZAzdAomIySgBGZACXiaHIVvuSOTGhT4imIwXUgdHh6Uvo3ZBqNfYZAPijPQ3tFfmnrbrNI0Gk6XMqHaCpvZAIOaYRFMxHzrIW8ozssiyF7eG16uagZDZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("tsaiingwen",token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
```

    ## 18 posts 26 posts 32 posts 11 posts 14 posts 12 posts 9 posts 6 posts 6 posts 8 posts 9 posts 10 posts 6 posts 7 posts 7 posts 6 posts 6 posts 8 posts 5 posts 6 posts

``` r
nrow(totalPage)
```

    ## [1] 212

2016/01/01至2016/04/10 蔡英文總統粉絲團一共有212篇文章

每日發文數分析
--------------

說明: 分析蔡英文總統粉絲團每天的發文數，因日期格式原先是以世界時區劃分，在換成台灣的日期會有誤差，因此將其先換為台灣台北的時區，再利用weekdays()讓每個日期都有星期幾的格式。

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time,
                                 format =  "%Y-%m-%dT%H:%M:%S+0000",
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei")
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)  
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | dateTPE    |   id|
|-----|:-----------|----:|
| 15  | 2016-01-15 |    8|
| 11  | 2016-01-11 |    7|
| 14  | 2016-01-14 |    7|
| 8   | 2016-01-08 |    6|
| 10  | 2016-01-10 |    6|
| 13  | 2016-01-13 |    6|

討論: 2016/01/15（週五）的發文數最多，一共有8篇，是因為2016/01/16是總統大選的日子，發文內容大多數是在台灣各地最後的競選活動。

每日likes數分析
---------------

說明: 以aggregate來分組將likes數以每天發文數來做平均計算，再用kable與head取前6名由高至低排序出來。

``` r
LikesNumber<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(LikesNumber[order(LikesNumber$likes_count,decreasing = T),]))
```

|     | dateTPE    |  likes\_count|
|-----|:-----------|-------------:|
| 17  | 2016-01-17 |      260415.0|
| 16  | 2016-01-16 |      241246.8|
| 89  | 2016-03-29 |      189566.0|
| 20  | 2016-01-20 |      121719.5|
| 42  | 2016-02-11 |      113708.0|
| 39  | 2016-02-08 |      101000.0|

討論: 2016/01/17的likes數最多，平均有260415個，發文內容只有一張圖片寫到「感謝人民一起點亮台灣」，來表達勝選的喜悅。次多的是2016/01/16，平均有241246個，當天是選舉日，蔡英文總統帶領的民進黨大勝，所發出的一寫感謝文。

每日comments數分析
------------------

說明: 以aggregate來分組將comments數以每天發文數來做平均計算，再用kable與head取前6名由高至低排序出來。

``` r
CommentsNumber<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(CommentsNumber[order(CommentsNumber$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 20  | 2016-01-20 |        27994.500|
| 21  | 2016-01-21 |        16111.667|
| 17  | 2016-01-17 |        10525.000|
| 19  | 2016-01-19 |         9388.000|
| 18  | 2016-01-18 |         9133.000|
| 16  | 2016-01-16 |         8233.833|

討論: 2016/01/20的comments數最多，平均有27994個，最多的文章是在執行選後的第一場民進黨中常會。次多的是2016/01/21，平均有16111個，對國人信心喊話的照片蔡英文總統到高雄及屏東感謝輔選幹部。

每日shares數分析
----------------

說明: 以aggregate來分組將shares數以每天發文數來做平均計算，再用kable與head取前6名由高至低排序出來。

``` r
SharesNumber<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(SharesNumber[order(SharesNumber$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 89  | 2016-03-29 |           6245|
| 16  | 2016-01-16 |           4195|
| 17  | 2016-01-17 |           3811|
| 38  | 2016-02-07 |           3119|
| 18  | 2016-01-18 |           3008|
| 42  | 2016-02-11 |           2570|

討論: 2016/03/29的shares數最多，平均有6245次，因為前幾天發生了女童在路上被砍的事件，蔡英文總統再發生後的這天寫了一封信給女童的媽媽以及到案發的現場悼念女童。次多的是2016/01/16，平均有4195次，當天是選舉日，蔡英文總統帶領的民進黨大勝，所發出的一寫感謝文。
