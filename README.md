Practice Exam
================
Cameron Pak
2/27/2020

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.4
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ----------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(nycflights13)

myweather <- weather %>% mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year) %>%
  summarize(temp = mean(temp)) %>%
  ggplot(aes(x = day_of_year, y = temp)) +
  geom_line() + facet_wrap( ~ origin, ncol = 1)
myweather
```

![](Practice-Exam_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
non_tidy <- weather %>% mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year) %>%
  summarize(temp = mean(temp)) %>%
  pivot_wider(names_from = day_of_year, values_from = temp)
non_tidy
```

    ## # A tibble: 3 x 365
    ## # Groups:   origin [3]
    ##   origin   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`  `11`  `12`
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 EWR     36.8  28.7  29.6  34.3  36.6  39.9  40.3  38.6  42.1  43.6  42.0  46.0
    ## 2 JFK     36.9  28.6  30.1  34.7  36.8  39.3  40.1  39.4  42.7  43.6  41.3  45.0
    ## 3 LGA     37.2  28.8  30.3  35.8  38.3  41.0  41.4  42.3  44.9  44.3  40.3  43.9
    ## # ... with 352 more variables: `13` <dbl>, `14` <dbl>, `15` <dbl>, `16` <dbl>,
    ## #   `17` <dbl>, `18` <dbl>, `19` <dbl>, `20` <dbl>, `21` <dbl>, `22` <dbl>,
    ## #   `23` <dbl>, `24` <dbl>, `25` <dbl>, `26` <dbl>, `27` <dbl>, `28` <dbl>,
    ## #   `29` <dbl>, `30` <dbl>, `31` <dbl>, `32` <dbl>, `33` <dbl>, `34` <dbl>,
    ## #   `35` <dbl>, `36` <dbl>, `37` <dbl>, `38` <dbl>, `39` <dbl>, `40` <dbl>,
    ## #   `41` <dbl>, `42` <dbl>, `43` <dbl>, `44` <dbl>, `45` <dbl>, `46` <dbl>,
    ## #   `47` <dbl>, `48` <dbl>, `49` <dbl>, `50` <dbl>, `51` <dbl>, `52` <dbl>,
    ## #   `53` <dbl>, `54` <dbl>, `55` <dbl>, `56` <dbl>, `57` <dbl>, `58` <dbl>,
    ## #   `59` <dbl>, `60` <dbl>, `61` <dbl>, `62` <dbl>, `63` <dbl>, `64` <dbl>,
    ## #   `65` <dbl>, `66` <dbl>, `67` <dbl>, `68` <dbl>, `69` <dbl>, `70` <dbl>,
    ## #   `71` <dbl>, `72` <dbl>, `73` <dbl>, `74` <dbl>, `75` <dbl>, `76` <dbl>,
    ## #   `77` <dbl>, `78` <dbl>, `79` <dbl>, `80` <dbl>, `81` <dbl>, `82` <dbl>,
    ## #   `83` <dbl>, `84` <dbl>, `85` <dbl>, `86` <dbl>, `87` <dbl>, `88` <dbl>,
    ## #   `89` <dbl>, `90` <dbl>, `91` <dbl>, `92` <dbl>, `93` <dbl>, `94` <dbl>,
    ## #   `95` <dbl>, `96` <dbl>, `97` <dbl>, `98` <dbl>, `99` <dbl>, `100` <dbl>,
    ## #   `101` <dbl>, `102` <dbl>, `103` <dbl>, `104` <dbl>, `105` <dbl>,
    ## #   `106` <dbl>, `107` <dbl>, `108` <dbl>, `109` <dbl>, `110` <dbl>,
    ## #   `111` <dbl>, `112` <dbl>, ...

``` r
performance <- flights %>% mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year) %>%
  summarize(performance = mean(dep_delay < 60, na.rm = T))
performance
```

    ## # A tibble: 1,095 x 3
    ## # Groups:   origin [3]
    ##    origin day_of_year performance
    ##    <chr>        <dbl>       <dbl>
    ##  1 EWR              1       0.918
    ##  2 EWR              2       0.837
    ##  3 EWR              3       0.979
    ##  4 EWR              4       0.935
    ##  5 EWR              5       0.966
    ##  6 EWR              6       0.95 
    ##  7 EWR              7       0.921
    ##  8 EWR              8       0.982
    ##  9 EWR              9       0.976
    ## 10 EWR             10       0.980
    ## # ... with 1,085 more rows

``` r
w.summary <- weather %>% mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year) %>%
  summarize(
    precipitation = sum(precip, na.rm = T),
    visibility = min(visib, na.rm = T),
    windMean = mean(wind_speed, na.rm = T)
  )
w.summary
```

    ## # A tibble: 1,092 x 5
    ## # Groups:   origin [3]
    ##    origin day_of_year precipitation visibility windMean
    ##    <chr>        <dbl>         <dbl>      <dbl>    <dbl>
    ##  1 EWR              1             0         10    13.2 
    ##  2 EWR              2             0         10    10.9 
    ##  3 EWR              3             0         10     8.58
    ##  4 EWR              4             0         10    14.0 
    ##  5 EWR              5             0         10     9.40
    ##  6 EWR              6             0          6     9.11
    ##  7 EWR              7             0         10     7.34
    ##  8 EWR              8             0          8     7.19
    ##  9 EWR              9             0          6     5.99
    ## 10 EWR             10             0         10     8.92
    ## # ... with 1,082 more rows

``` r
mydata <- left_join(performance, w.summary)
```

    ## Joining, by = c("origin", "day_of_year")

``` r
mylm <-
  lm(performance ~ origin + precipitation + visibility + windMean,
     mydata)
summary(mylm)
```

    ## 
    ## Call:
    ## lm(formula = performance ~ origin + precipitation + visibility + 
    ##     windMean, data = mydata)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.45572 -0.01734  0.01755  0.03704  0.18800 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.8605210  0.0082091 104.825  < 2e-16 ***
    ## originJFK      0.0200180  0.0052986   3.778 0.000167 ***
    ## originLGA      0.0209328  0.0052113   4.017 6.31e-05 ***
    ## precipitation -0.0531217  0.0077869  -6.822 1.49e-11 ***
    ## visibility     0.0081510  0.0006977  11.682  < 2e-16 ***
    ## windMean      -0.0010823  0.0005174  -2.092 0.036684 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0698 on 1086 degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## Multiple R-squared:  0.2532, Adjusted R-squared:  0.2498 
    ## F-statistic: 73.65 on 5 and 1086 DF,  p-value: < 2.2e-16

``` r
ewr <- filter(mydata, origin == "EWR")
ewr.lm <-
  lm(performance ~ precipitation + visibility + windMean, ewr)
summary(ewr.lm)
```

    ## 
    ## Call:
    ## lm(formula = performance ~ precipitation + visibility + windMean, 
    ##     data = ewr)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32733 -0.03119  0.02363  0.04566  0.18642 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.8658870  0.0134506  64.375  < 2e-16 ***
    ## precipitation -0.0512542  0.0133869  -3.829 0.000152 ***
    ## visibility     0.0086600  0.0013316   6.504 2.62e-10 ***
    ## windMean      -0.0020805  0.0008928  -2.330 0.020341 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07406 on 360 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2366, Adjusted R-squared:  0.2302 
    ## F-statistic: 37.19 on 3 and 360 DF,  p-value: < 2.2e-16
