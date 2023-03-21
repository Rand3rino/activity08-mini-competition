Activity 8 - Mini-competition
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
inventory <- read.csv("data/inventory.csv")
```

``` r
summary(inventory)
```

    ##    item_no               week           sold        
    ##  Length:26352       Min.   : 0.0   Min.   :   0.00  
    ##  Class :character   1st Qu.:13.0   1st Qu.:   0.00  
    ##  Mode  :character   Median :26.5   Median :   0.00  
    ##                     Mean   :26.5   Mean   :  50.62  
    ##                     3rd Qu.:40.0   3rd Qu.:   2.00  
    ##                     Max.   :53.0   Max.   :7200.00

``` r
n_distinct(inventory$item_no)
```

    ## [1] 488

``` r
inventory %>% 
  group_by(item_no) %>% 
  summarise(min = min(sold),
            q1 = quantile(sold, 0.25),
            median = median(sold),
            mean = mean(sold),
            q3 = quantile(sold, 0.75),
            max = max(sold)) %>% 
  arrange(mean)
```

    ## # A tibble: 488 × 7
    ##    item_no           min    q1 median  mean    q3   max
    ##    <chr>           <int> <dbl>  <dbl> <dbl> <dbl> <int>
    ##  1 4MBTST-1117         0     0      0  9.26     0   500
    ##  2 4MBTST-2424         0     0      0  9.26     0   500
    ##  3 ACT-2-726           0     0      0  9.26     0   300
    ##  4 AS1324242208        0     0      0  9.26     0   160
    ##  5 AS40010RD04.750     0     0      0  9.26     0   500
    ##  6 AS40010RD04.875     0     0      0  9.26     0   500
    ##  7 BNMO25P3D           0     0      0  9.26     0   100
    ##  8 BPEM200P1SC         0     0      0  9.26     0   500
    ##  9 BPONG5P4P           0     0      0  9.26     0   200
    ## 10 CMPD2020            0     0      0  9.26     0   125
    ## # … with 478 more rows

``` r
inventory %>% 
  filter(item_no == "A510004") %>% 
  ggplot(aes(x = week, y = sold)) +
  geom_line() +
  theme_bw()
```

![](mini-competition_files/figure-gfm/plot%20data-1.png)<!-- -->
