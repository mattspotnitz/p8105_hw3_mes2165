Homework3
================

\#Initialization

\#\#load the dataset

``` r
library(p8105.datasets)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(haven)
data("instacart")
```

\#\#inspect the dataset

``` r
df = instacart
df = janitor::clean_names(df)
str(df)
```

    ## tibble [1,384,617 × 15] (S3: tbl_df/tbl/data.frame)
    ##  $ order_id              : int [1:1384617] 1 1 1 1 1 1 1 1 36 36 ...
    ##  $ product_id            : int [1:1384617] 49302 11109 10246 49683 43633 13176 47209 22035 39612 19660 ...
    ##  $ add_to_cart_order     : int [1:1384617] 1 2 3 4 5 6 7 8 1 2 ...
    ##  $ reordered             : int [1:1384617] 1 1 0 0 1 0 0 1 0 1 ...
    ##  $ user_id               : int [1:1384617] 112108 112108 112108 112108 112108 112108 112108 112108 79431 79431 ...
    ##  $ eval_set              : chr [1:1384617] "train" "train" "train" "train" ...
    ##  $ order_number          : int [1:1384617] 4 4 4 4 4 4 4 4 23 23 ...
    ##  $ order_dow             : int [1:1384617] 4 4 4 4 4 4 4 4 6 6 ...
    ##  $ order_hour_of_day     : int [1:1384617] 10 10 10 10 10 10 10 10 18 18 ...
    ##  $ days_since_prior_order: int [1:1384617] 9 9 9 9 9 9 9 9 30 30 ...
    ##  $ product_name          : chr [1:1384617] "Bulgarian Yogurt" "Organic 4% Milk Fat Whole Milk Cottage Cheese" "Organic Celery Hearts" "Cucumber Kirby" ...
    ##  $ aisle_id              : int [1:1384617] 120 108 83 83 95 24 24 21 2 115 ...
    ##  $ department_id         : int [1:1384617] 16 16 4 4 15 4 4 16 16 7 ...
    ##  $ aisle                 : chr [1:1384617] "yogurt" "other creams cheeses" "fresh vegetables" "fresh vegetables" ...
    ##  $ department            : chr [1:1384617] "dairy eggs" "dairy eggs" "produce" "produce" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   order_id = col_integer(),
    ##   ..   product_id = col_integer(),
    ##   ..   add_to_cart_order = col_integer(),
    ##   ..   reordered = col_integer(),
    ##   ..   user_id = col_integer(),
    ##   ..   eval_set = col_character(),
    ##   ..   order_number = col_integer(),
    ##   ..   order_dow = col_integer(),
    ##   ..   order_hour_of_day = col_integer(),
    ##   ..   days_since_prior_order = col_integer(),
    ##   ..   product_name = col_character(),
    ##   ..   aisle_id = col_integer(),
    ##   ..   department_id = col_integer(),
    ##   ..   aisle = col_character(),
    ##   ..   department = col_character()
    ##   .. )

``` r
head(df)
```

    ## # A tibble: 6 × 15
    ##   order_id product_id add_to_cart_order reordered user_id eval_set order_number
    ##      <int>      <int>             <int>     <int>   <int> <chr>           <int>
    ## 1        1      49302                 1         1  112108 train               4
    ## 2        1      11109                 2         1  112108 train               4
    ## 3        1      10246                 3         0  112108 train               4
    ## 4        1      49683                 4         0  112108 train               4
    ## 5        1      43633                 5         1  112108 train               4
    ## 6        1      13176                 6         0  112108 train               4
    ## # … with 8 more variables: order_dow <int>, order_hour_of_day <int>,
    ## #   days_since_prior_order <int>, product_name <chr>, aisle_id <int>,
    ## #   department_id <int>, aisle <chr>, department <chr>

``` r
tail (df)
```

    ## # A tibble: 6 × 15
    ##   order_id product_id add_to_cart_order reordered user_id eval_set order_number
    ##      <int>      <int>             <int>     <int>   <int> <chr>           <int>
    ## 1  3421063      13565                 2         1  169679 train              30
    ## 2  3421063      14233                 3         1  169679 train              30
    ## 3  3421063      35548                 4         1  169679 train              30
    ## 4  3421070      35951                 1         1  139822 train              15
    ## 5  3421070      16953                 2         1  139822 train              15
    ## 6  3421070       4724                 3         1  139822 train              15
    ## # … with 8 more variables: order_dow <int>, order_hour_of_day <int>,
    ## #   days_since_prior_order <int>, product_name <chr>, aisle_id <int>,
    ## #   department_id <int>, aisle <chr>, department <chr>

\#\#There are 1384167 observations and 15 variables. Of those variables,
4 are characters. Some key variables are order identiifer, product
identifier, customer identifier, order day of the week, order hour,
aisle identiier and aisle name. From the header, we see one example of
an obseration is that the product “Bulgarian Yogurt” was in asile \#
120.

\#\#I will count the number of aislde IDs and their frequencies. I will
sort the frequencies in descending order.

``` r
df %>% group_by(aisle_id) %>% summarize(n_obs = n()) %>% arrange(desc(n_obs))
```

    ## # A tibble: 134 × 2
    ##    aisle_id  n_obs
    ##       <int>  <int>
    ##  1       83 150609
    ##  2       24 150473
    ##  3      123  78493
    ##  4      120  55240
    ##  5       21  41699
    ##  6      115  36617
    ##  7       84  32644
    ##  8      107  31269
    ##  9       91  26240
    ## 10      112  23635
    ## # … with 124 more rows

\#On the basis of this output, there are 134 aisle IDs. The aisle ids
with the highest frequencies are 83, 24, 123, 120, 21, 115, 84, 107, 91,
112.

\#I will make a data frame that filters to aisles with frequencies &gt;
10,000, and then plot frequencies (or number of items) vs. Aisle ID.

``` r
df_filter = df %>% group_by(aisle_id) %>% summarize(n_obs = n()) %>% filter(n_obs > 10000) %>% arrange(aisle_id)

ggplot(df_filter, aes(x = aisle_id, y = n_obs)) + geom_point() + 
  labs(
    title = "Number of Items vs. Aisle ID",
    x = "Aisle ID",
    y = "Number of Items",
    caption = "Data from Instacart"
  )  + theme_minimal()
```

![](Homework3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#\#Now I will make the table of the most common items it the aisles
named “baking ingredients”, “dog food care” or “packaged vegetables
fruits”. I will filter on those aisles, group them, count them, and then
filter to the 3 most common items.

``` r
df_food = df %>% filter (aisle == "baking ingredients" | aisle ==  "dog food care" | aisle == "packaged vegetables fruits") %>% group_by(aisle) %>% count(product_id, product_name)

df_food_filter = df_food %>% filter(rank(desc(n))<= 3) %>% arrange(aisle, desc(n))
                    
df_food_filter
```

    ## # A tibble: 9 × 4
    ## # Groups:   aisle [3]
    ##   aisle                      product_id product_name                           n
    ##   <chr>                           <int> <chr>                              <int>
    ## 1 baking ingredients              23537 Light Brown Sugar                    499
    ## 2 baking ingredients              23405 Pure Baking Soda                     387
    ## 3 baking ingredients              49533 Cane Sugar                           336
    ## 4 dog food care                     722 Snack Sticks Chicken & Rice Recip…    30
    ## 5 dog food care                   23329 Organix Chicken & Brown Rice Reci…    28
    ## 6 dog food care                   17471 Small Dog Biscuits                    26
    ## 7 packaged vegetables fruits      21903 Organic Baby Spinach                9784
    ## 8 packaged vegetables fruits      27966 Organic Raspberries                 5546
    ## 9 packaged vegetables fruits      39275 Organic Blueberries                 4966

\#\#Problem 1 -fourth part. Table of hour of the day when pink lady
apples and coffee ice cream are ordered. I am stumped on part four of
problem 1 too!!!

``` r
mutate_df = df  %>% filter ((product_name == "Pink Lady Apples") | (product_name == "Coffee Ice Cream")) %>% group_by(product_name, order_dow) %>% summarize(mean_hour = mean(order_hour_of_day))
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the `.groups` argument.

``` r
mutate_df %>%  pivot_wider(names_from = order_dow, values_from = mean_hour)
```

    ## # A tibble: 2 × 8
    ## # Groups:   product_name [2]
    ##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
    ## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9
