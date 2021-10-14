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

\#Problem 2 \#I will load the dataset for problem 2, and inspect the
head and sturcture of the data set.

``` r
library(p8105.datasets)
data("brfss_smart2010")
df_two = brfss_smart2010
df_two = janitor::clean_names(df_two)

head(df_two)
```

    ## # A tibble: 6 × 23
    ##    year locationabbr locationdesc  class  topic  question   response sample_size
    ##   <int> <chr>        <chr>         <chr>  <chr>  <chr>      <chr>          <int>
    ## 1  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Excelle…          94
    ## 2  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Very go…         148
    ## 3  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Good             208
    ## 4  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Fair             107
    ## 5  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Poor              45
    ## 6  2010 AL           AL - Jeffers… Healt… Fair … Health St… Good or…         450
    ## # … with 15 more variables: data_value <dbl>, confidence_limit_low <dbl>,
    ## #   confidence_limit_high <dbl>, display_order <int>, data_value_unit <chr>,
    ## #   data_value_type <chr>, data_value_footnote_symbol <chr>,
    ## #   data_value_footnote <chr>, data_source <chr>, class_id <chr>,
    ## #   topic_id <chr>, location_id <chr>, question_id <chr>, respid <chr>,
    ## #   geo_location <chr>

``` r
str(df_two)
```

    ## tibble [134,203 × 23] (S3: tbl_df/tbl/data.frame)
    ##  $ year                      : int [1:134203] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
    ##  $ locationabbr              : chr [1:134203] "AL" "AL" "AL" "AL" ...
    ##  $ locationdesc              : chr [1:134203] "AL - Jefferson County" "AL - Jefferson County" "AL - Jefferson County" "AL - Jefferson County" ...
    ##  $ class                     : chr [1:134203] "Health Status" "Health Status" "Health Status" "Health Status" ...
    ##  $ topic                     : chr [1:134203] "Overall Health" "Overall Health" "Overall Health" "Overall Health" ...
    ##  $ question                  : chr [1:134203] "How is your general health?" "How is your general health?" "How is your general health?" "How is your general health?" ...
    ##  $ response                  : chr [1:134203] "Excellent" "Very good" "Good" "Fair" ...
    ##  $ sample_size               : int [1:134203] 94 148 208 107 45 450 152 524 77 316 ...
    ##  $ data_value                : num [1:134203] 18.9 30 33.1 12.5 5.5 82 18 79.3 20.7 74.9 ...
    ##  $ confidence_limit_low      : num [1:134203] 14.1 24.9 28.2 9.5 3.5 78.6 14.6 74 15.4 68.6 ...
    ##  $ confidence_limit_high     : num [1:134203] 23.6 35 38 15.4 7.4 85.3 21.3 84.5 25.9 81.1 ...
    ##  $ display_order             : int [1:134203] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data_value_unit           : chr [1:134203] "%" "%" "%" "%" ...
    ##  $ data_value_type           : chr [1:134203] "Crude Prevalence" "Crude Prevalence" "Crude Prevalence" "Crude Prevalence" ...
    ##  $ data_value_footnote_symbol: chr [1:134203] NA NA NA NA ...
    ##  $ data_value_footnote       : chr [1:134203] NA NA NA NA ...
    ##  $ data_source               : chr [1:134203] "BRFSS" "BRFSS" "BRFSS" "BRFSS" ...
    ##  $ class_id                  : chr [1:134203] "CLASS08" "CLASS08" "CLASS08" "CLASS08" ...
    ##  $ topic_id                  : chr [1:134203] "Topic41" "Topic41" "Topic41" "Topic41" ...
    ##  $ location_id               : chr [1:134203] NA NA NA NA ...
    ##  $ question_id               : chr [1:134203] "GENHLTH" "GENHLTH" "GENHLTH" "GENHLTH" ...
    ##  $ respid                    : chr [1:134203] "RESP056" "RESP057" "RESP058" "RESP059" ...
    ##  $ geo_location              : chr [1:134203] "(33.518601, -86.814688)" "(33.518601, -86.814688)" "(33.518601, -86.814688)" "(33.518601, -86.814688)" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Year = col_integer(),
    ##   ..   Locationabbr = col_character(),
    ##   ..   Locationdesc = col_character(),
    ##   ..   Class = col_character(),
    ##   ..   Topic = col_character(),
    ##   ..   Question = col_character(),
    ##   ..   Response = col_character(),
    ##   ..   Sample_Size = col_integer(),
    ##   ..   Data_value = col_double(),
    ##   ..   Confidence_limit_Low = col_double(),
    ##   ..   Confidence_limit_High = col_double(),
    ##   ..   Display_order = col_integer(),
    ##   ..   Data_value_unit = col_character(),
    ##   ..   Data_value_type = col_character(),
    ##   ..   Data_Value_Footnote_Symbol = col_character(),
    ##   ..   Data_Value_Footnote = col_character(),
    ##   ..   DataSource = col_character(),
    ##   ..   ClassId = col_character(),
    ##   ..   TopicId = col_character(),
    ##   ..   LocationID = col_character(),
    ##   ..   QuestionID = col_character(),
    ##   ..   RESPID = col_character(),
    ##   ..   GeoLocation = col_character()
    ##   .. )

\#I will filter on the “Overall Health” and change the class of the
response variable into factor, exclude entries that are not between poor
and excellent, and order the factors from 1 = poor to 5 = excellent.

``` r
df_two_filter = df_two %>% filter(topic == "Overall Health") %>% filter (response != c("Poor", "Fair", "Good", "Very good", "Excellent")) %>% mutate(response = factor(response, levels =c("Poor", "Fair", "Good", "Very good", "Excellent") )) 
head(df_two_filter)
```

    ## # A tibble: 6 × 23
    ##    year locationabbr locationdesc  class  topic  question   response sample_size
    ##   <int> <chr>        <chr>         <chr>  <chr>  <chr>      <fct>          <int>
    ## 1  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Excelle…          94
    ## 2  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Very go…         148
    ## 3  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Fair             107
    ## 4  2010 AL           AL - Jeffers… Healt… Overa… How is yo… Poor              45
    ## 5  2010 AL           AL - Mobile … Healt… Overa… How is yo… Excelle…          91
    ## 6  2010 AL           AL - Mobile … Healt… Overa… How is yo… Very go…         177
    ## # … with 15 more variables: data_value <dbl>, confidence_limit_low <dbl>,
    ## #   confidence_limit_high <dbl>, display_order <int>, data_value_unit <chr>,
    ## #   data_value_type <chr>, data_value_footnote_symbol <chr>,
    ## #   data_value_footnote <chr>, data_source <chr>, class_id <chr>,
    ## #   topic_id <chr>, location_id <chr>, question_id <chr>, respid <chr>,
    ## #   geo_location <chr>
