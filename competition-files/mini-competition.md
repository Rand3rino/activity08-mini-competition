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
library(tidyr)
```

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

``` r
inventory_month <- inventory %>% 
  mutate(month = ceiling(week/4))
```

``` r
inventory_pivot <- inventory %>% 
           #if( week == 53) {"Current"} else{ paste("WeeksAway", 53 - week, sep = "") } ) %>% 
  mutate(weeks_away = paste("WeeksAway", 53 - week, sep = "")) %>% 
  select(item_no, weeks_away, sold) %>% 
  pivot_wider(names_from=weeks_away, values_from=sold)
```

``` r
inventory_pivot_no_item <-inventory_pivot %>% 
  select(-item_no)
model<-glm(WeeksAway0 ~ ., data = inventory_pivot_no_item)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = WeeksAway0 ~ ., data = inventory_pivot_no_item)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -184.52   -37.66   -14.78    12.99   713.08  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.9175918  5.4305521   1.274 0.203406    
    ## WeeksAway53  0.0251731  0.0617360   0.408 0.683655    
    ## WeeksAway52 -0.1005956  0.0443232  -2.270 0.023722 *  
    ## WeeksAway51  0.0060544  0.0452938   0.134 0.893726    
    ## WeeksAway50 -0.0253651  0.0449935  -0.564 0.573215    
    ## WeeksAway49  0.0213067  0.0454445   0.469 0.639411    
    ## WeeksAway48 -0.0053499  0.0455790  -0.117 0.906616    
    ## WeeksAway47  0.0435624  0.0435752   1.000 0.318010    
    ## WeeksAway46  0.0301707  0.0297581   1.014 0.311212    
    ## WeeksAway45 -0.0004396  0.0413342  -0.011 0.991520    
    ## WeeksAway44 -0.0888020  0.0540102  -1.644 0.100865    
    ## WeeksAway43 -0.0218549  0.0235968  -0.926 0.354867    
    ## WeeksAway42  0.0961262  0.0332094   2.895 0.003989 ** 
    ## WeeksAway41 -0.0639026  0.0321493  -1.988 0.047475 *  
    ## WeeksAway40 -0.0266001  0.0344971  -0.771 0.441078    
    ## WeeksAway39  0.0672254  0.0451231   1.490 0.136997    
    ## WeeksAway38 -0.0202717  0.0517674  -0.392 0.695552    
    ## WeeksAway37  0.0094342  0.0338697   0.279 0.780728    
    ## WeeksAway36  0.2105686  0.0495359   4.251 2.61e-05 ***
    ## WeeksAway35  0.0371745  0.0361002   1.030 0.303696    
    ## WeeksAway34  0.0875996  0.0493984   1.773 0.076875 .  
    ## WeeksAway33 -0.0828234  0.0340020  -2.436 0.015259 *  
    ## WeeksAway32 -0.1761160  0.0745715  -2.362 0.018632 *  
    ## WeeksAway31 -0.0462781  0.0426988  -1.084 0.279043    
    ## WeeksAway30 -0.1110226  0.0538782  -2.061 0.039935 *  
    ## WeeksAway29  0.0128525  0.0405435   0.317 0.751391    
    ## WeeksAway28 -0.0603898  0.0387988  -1.556 0.120321    
    ## WeeksAway27  0.0501052  0.0570274   0.879 0.380096    
    ## WeeksAway26  0.0370813  0.0485285   0.764 0.445215    
    ## WeeksAway25 -0.0567438  0.0315948  -1.796 0.073193 .  
    ## WeeksAway24  0.0399013  0.0334705   1.192 0.233860    
    ## WeeksAway23  0.0908047  0.0512190   1.773 0.076952 .  
    ## WeeksAway22  0.0141912  0.0239882   0.592 0.554433    
    ## WeeksAway21  0.0244568  0.0478169   0.511 0.609284    
    ## WeeksAway20  0.0901180  0.0485716   1.855 0.064222 .  
    ## WeeksAway19  0.0197291  0.0380139   0.519 0.604027    
    ## WeeksAway18  0.0600040  0.0373722   1.606 0.109094    
    ## WeeksAway17  0.0407855  0.0450593   0.905 0.365887    
    ## WeeksAway16  0.0036573  0.0138342   0.264 0.791625    
    ## WeeksAway15  0.0111709  0.0503248   0.222 0.824438    
    ## WeeksAway14  0.0362559  0.0457055   0.793 0.428066    
    ## WeeksAway13  0.0613284  0.0465218   1.318 0.188108    
    ## WeeksAway12  0.0293474  0.0405405   0.724 0.469515    
    ## WeeksAway11  0.0253082  0.0407991   0.620 0.535378    
    ## WeeksAway10  0.0361482  0.0359258   1.006 0.314885    
    ## WeeksAway9   0.0605566  0.0402848   1.503 0.133512    
    ## WeeksAway8   0.0350741  0.0400454   0.876 0.381591    
    ## WeeksAway7  -0.1385212  0.0641706  -2.159 0.031426 *  
    ## WeeksAway6   0.1627806  0.0430544   3.781 0.000178 ***
    ## WeeksAway5   0.1227601  0.0578025   2.124 0.034253 *  
    ## WeeksAway4  -0.0016844  0.0575455  -0.029 0.976661    
    ## WeeksAway3   0.0336846  0.0380438   0.885 0.376423    
    ## WeeksAway2  -0.0671546  0.0384079  -1.748 0.081091 .  
    ## WeeksAway1  -0.0599558  0.0317321  -1.889 0.059500 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 8745.579)
    ## 
    ##     Null deviance: 6259662  on 487  degrees of freedom
    ## Residual deviance: 3795581  on 434  degrees of freedom
    ## AIC: 5866.9
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
inventory_month_pivot <- inventory_month %>% 
  mutate(months_away = paste("MonthsAway", month, sep = "")) %>%
  group_by(item_no, months_away) %>% 
  summarise(sold = sum(sold)) %>% 
  pivot_wider(names_from=months_away, values_from=sold)
```

    ## `summarise()` has grouped output by 'item_no'. You can override using the
    ## `.groups` argument.

``` r
inventory_month_pivot_no_item <- inventory_month_pivot %>% 
  select(-item_no)
```

    ## Adding missing grouping variables: `item_no`

``` r
model<-glm(MonthsAway0 ~ . -item_no, data = inventory_month_pivot_no_item)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = MonthsAway0 ~ . - item_no, data = inventory_month_pivot_no_item)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -254.22   -24.85   -11.69     2.15   950.01  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.372e+00  4.717e+00   1.139   0.2554    
    ## MonthsAway1   1.499e-03  1.429e-02   0.105   0.9165    
    ## MonthsAway10 -9.270e-03  1.085e-02  -0.855   0.3931    
    ## MonthsAway11  1.652e-03  1.453e-02   0.114   0.9095    
    ## MonthsAway12  1.187e-01  1.980e-02   5.995 4.05e-09 ***
    ## MonthsAway13  2.821e-02  1.709e-02   1.651   0.0995 .  
    ## MonthsAway14  3.732e-02  4.047e-02   0.922   0.3569    
    ## MonthsAway2   2.019e-02  1.571e-02   1.286   0.1992    
    ## MonthsAway3  -1.665e-03  1.197e-02  -0.139   0.8894    
    ## MonthsAway4   2.739e-02  1.414e-02   1.937   0.0533 .  
    ## MonthsAway5   2.650e-03  1.251e-02   0.212   0.8323    
    ## MonthsAway6   6.542e-03  1.882e-02   0.348   0.7283    
    ## MonthsAway7  -8.865e-02  1.552e-02  -5.712 1.98e-08 ***
    ## MonthsAway8  -3.082e-05  1.334e-02  -0.002   0.9982    
    ## MonthsAway9   2.874e-02  1.642e-02   1.750   0.0808 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 8201.087)
    ## 
    ##     Null deviance: 5057212  on 487  degrees of freedom
    ## Residual deviance: 3879114  on 473  degrees of freedom
    ## AIC: 5799.5
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
org_inventory <- inventory %>% 
  group_by(week) %>% 
  summarise(sold = sum(sold)) 

org_inventory %>% 
  ggplot(aes(x=week, y=sold)) +
  geom_line() +
  theme_bw()
```

![](mini-competition_files/figure-gfm/visualize%20and%20model%20by%20sum-1.png)<!-- -->

``` r
org_inventory_pivot <- org_inventory %>%   
  mutate(weeks_away = paste("WeeksAway", 53 - week, sep = "")) %>% 
  select(weeks_away, sold) %>% 
  pivot_wider(names_from=weeks_away, values_from=sold)
```

``` r
glm(WeeksAway0~ ., data = org_inventory_pivot)
```

    ## 
    ## Call:  glm(formula = WeeksAway0 ~ ., data = org_inventory_pivot)
    ## 
    ## Coefficients:
    ## (Intercept)  WeeksAway53  WeeksAway52  WeeksAway51  WeeksAway50  WeeksAway49  
    ##       18792           NA           NA           NA           NA           NA  
    ## WeeksAway48  WeeksAway47  WeeksAway46  WeeksAway45  WeeksAway44  WeeksAway43  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway42  WeeksAway41  WeeksAway40  WeeksAway39  WeeksAway38  WeeksAway37  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway36  WeeksAway35  WeeksAway34  WeeksAway33  WeeksAway32  WeeksAway31  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway30  WeeksAway29  WeeksAway28  WeeksAway27  WeeksAway26  WeeksAway25  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway24  WeeksAway23  WeeksAway22  WeeksAway21  WeeksAway20  WeeksAway19  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway18  WeeksAway17  WeeksAway16  WeeksAway15  WeeksAway14  WeeksAway13  
    ##          NA           NA           NA           NA           NA           NA  
    ## WeeksAway12  WeeksAway11  WeeksAway10   WeeksAway9   WeeksAway8   WeeksAway7  
    ##          NA           NA           NA           NA           NA           NA  
    ##  WeeksAway6   WeeksAway5   WeeksAway4   WeeksAway3   WeeksAway2   WeeksAway1  
    ##          NA           NA           NA           NA           NA           NA  
    ## 
    ## Degrees of Freedom: 0 Total (i.e. Null);  0 Residual
    ## Null Deviance:       0 
    ## Residual Deviance: 0     AIC: -Inf
