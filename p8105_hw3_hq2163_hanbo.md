p8105\_hw3\_hq2163\_hanbo
================
Hanbo Qiu
October 7, 2018

Problem 1
---------

``` r
#Use code "data(package = "p8105.datasets")" to find the name of target dataset and load it.

data(brfss_smart2010)

#Create an overall_health dataset and focus on the “Overall Health” topic and organize responses as a factor taking levels from “Excellent” to “Poor”.

overall_health = janitor::clean_names(brfss_smart2010) %>% 
  filter(topic == "Overall Health") %>% 
  mutate(response = factor(response, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"), ordered = TRUE))
```

In 2002, which states were observed at 7 locations?

``` r
#Make a table with distinct location in 2002.
dist_loc_02 = filter(overall_health, year == 2002) %>% 
  distinct(locationabbr, locationdesc)

#Make a table with distinct location in 2002.
state_loc_7 = count(dist_loc_02, locationabbr) %>% 
  filter(n == 7)
state_loc_7$locationabbr
```

    ## [1] "CT" "FL" "NC"

In 2002, the states were observed at 7 locations is CT, FL, NC.

Make a “spaghetti plot” that shows the number of observations in each state from 2002 to 2010.

``` r
dist_loc = distinct(overall_health, year, locationabbr, locationdesc)

dist_loc %>% 
  ggplot(aes(x = year, color = locationabbr)) +
  geom_freqpoly(binwidth = 1) +
  scale_x_continuous(breaks = 2002:2010, limits = c(2002,2010)) +
  labs(
    title = "The number of observations in each state from 2002 to 2010",
    x = "Year",
    y = "No. of observations in each state",
    caption = "Data from the brfss_smart2010"
  ) +
  theme(legend.position="none")
```

    ## Warning: Removed 102 rows containing missing values (geom_path).

<img src="p8105_hw3_hq2163_hanbo_files/figure-markdown_github/unnamed-chunk-3-1.png" width="90%" /> The number of observations in State jumps to above 40 in 2007 and 2010. The number of observations all the other states kept under 20 for the rest of the years.

The mean and standard deviation of the proportion of “Excellent” responses across locations in NY State in 2002, 2006, and 2010.

``` r
overall_health %>% 
  filter(year %in% c(2002, 2006, 2010), 
         response == "Excellent", 
         locationabbr == "NY") %>% 
  group_by(year) %>% 
  summarise(excellent_mean = mean(data_value, na.rm = TRUE),
            excellent_sd = sd(data_value, na.rm = TRUE))
```

    ## # A tibble: 3 x 3
    ##    year excellent_mean excellent_sd
    ##   <int>          <dbl>        <dbl>
    ## 1  2002           24.0         4.49
    ## 2  2006           22.5         4.00
    ## 3  2010           22.7         3.57

The mean and standard deviation of the proportion of “Excellent” responses across locations in NY State in 2002 is 24% and 4.49%, in 2006 is 22.5% and 4%, in 2010 is 22.7% and 3.57%.

each year and state, compute the average proportion and make a five-panel plot

``` r
response_table = subset(overall_health, select = c(year:locationdesc, response, data_value)) %>% 
  group_by(response, year, locationabbr)

response_mean = summarise(response_table,
                          mean = mean(data_value, na.rm = TRUE))
response_mean %>%  
  ggplot(aes(x = year, y = mean, color = locationabbr)) +
  geom_line() +
  facet_grid(response ~., scales = "free_y")+
  labs(
    title = "Response distribution of state-level averages from 2002-2010",
    x = "Year",
    y = "The average proportion of each state in response category",
    caption = "Data from the brfss_smart2010"
  ) +
  theme(legend.position = "none")
```

<img src="p8105_hw3_hq2163_hanbo_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />

Problem 2
---------

``` r
# load the data from the p8105.datasets package and summarize the data
data(instacart)
summary(instacart)
```

    ##     order_id         product_id    add_to_cart_order   reordered     
    ##  Min.   :      1   Min.   :    1   Min.   : 1.000    Min.   :0.0000  
    ##  1st Qu.: 843370   1st Qu.:13380   1st Qu.: 3.000    1st Qu.:0.0000  
    ##  Median :1701880   Median :25298   Median : 7.000    Median :1.0000  
    ##  Mean   :1706298   Mean   :25556   Mean   : 8.758    Mean   :0.5986  
    ##  3rd Qu.:2568023   3rd Qu.:37940   3rd Qu.:12.000    3rd Qu.:1.0000  
    ##  Max.   :3421070   Max.   :49688   Max.   :80.000    Max.   :1.0000  
    ##     user_id         eval_set          order_number      order_dow    
    ##  Min.   :     1   Length:1384617     Min.   :  4.00   Min.   :0.000  
    ##  1st Qu.: 51732   Class :character   1st Qu.:  6.00   1st Qu.:1.000  
    ##  Median :102933   Mode  :character   Median : 11.00   Median :3.000  
    ##  Mean   :103113                      Mean   : 17.09   Mean   :2.701  
    ##  3rd Qu.:154959                      3rd Qu.: 21.00   3rd Qu.:5.000  
    ##  Max.   :206209                      Max.   :100.00   Max.   :6.000  
    ##  order_hour_of_day days_since_prior_order product_name      
    ##  Min.   : 0.00     Min.   : 0.00          Length:1384617    
    ##  1st Qu.:10.00     1st Qu.: 7.00          Class :character  
    ##  Median :14.00     Median :15.00          Mode  :character  
    ##  Mean   :13.58     Mean   :17.07                            
    ##  3rd Qu.:17.00     3rd Qu.:30.00                            
    ##  Max.   :23.00     Max.   :30.00                            
    ##     aisle_id     department_id      aisle            department       
    ##  Min.   :  1.0   Min.   : 1.00   Length:1384617     Length:1384617    
    ##  1st Qu.: 31.0   1st Qu.: 4.00   Class :character   Class :character  
    ##  Median : 83.0   Median : 8.00   Mode  :character   Mode  :character  
    ##  Mean   : 71.3   Mean   : 9.84                                        
    ##  3rd Qu.:107.0   3rd Qu.:16.00                                        
    ##  Max.   :134.0   Max.   :21.00

A short description of the dataset: Instacart is an online grocery service that allows you to shop online from local stores. The dataset contains 1,384,617 observations and 15variables of 131,209 unique users, where each row in the dataset is an order about product information. Key variables includes 1."order\_dow":the day of the week on which the order was placed. It can help us to analyse which kind of products are sold best at what time and then Instacart could get the info of promoting at that day 2.order\_hour\_of\_day: the hour of the day on which the order was placed.Like we can analyze when to promote specific products 3.product\_name: name of the product. 4.aisle: the name of the aisle

How many aisles are there, and which aisles are the most items ordered from?

``` r
dist_aisle = distinct(instacart, aisle_id)
n_aisle = nrow(dist_aisle)

item_aisle = count(instacart, aisle_id, sort = T)
item_aisle[1,1]
```

    ## # A tibble: 1 x 1
    ##   aisle_id
    ##      <int>
    ## 1       83

There are 134 aisles and 83is the most items ordered from.

Make a plot that shows the number of items ordered in each aisle. Order aisles sensibly, and organize your plot so others can read it.

Make a table showing the most popular item aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”

``` r
three_aisle = filter(instacart, aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  count(product_name)

product_aisle = select(instacart, product_name, aisle) %>% 
  distinct(product_name, .keep_all = T)

com_aisle_product = left_join(three_aisle, product_aisle, by = "product_name") %>% 
  group_by(aisle) %>% 
  filter(min_rank(desc(n)) == 1) %>% 
  select(aisle, product_name) %>% 
  arrange(aisle)

names(com_aisle_product)[1:2] = c("Aisle", "Most popular product")
com_aisle_product
```

    ## # A tibble: 3 x 2
    ## # Groups:   aisle [3]
    ##   Aisle                      `Most popular product`                       
    ##   <chr>                      <chr>                                        
    ## 1 baking ingredients         Light Brown Sugar                            
    ## 2 dog food care              Snack Sticks Chicken & Rice Recipe Dog Treats
    ## 3 packaged vegetables fruits Organic Baby Spinach

Make a table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week; format this table for human readers (i.e. produce a 2 x 7 table).

``` r
apple_icecream = filter(instacart, product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name, order_dow) %>% 
  summarise(mean = round(mean(order_hour_of_day), 0)) %>% 
  spread(key = order_dow, value = mean )
```
