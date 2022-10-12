ST558- Project 2
================
Rohan Prabhune, Naman Goel

-   <a href="#interacting-with-apis-financial-market-data"
    id="toc-interacting-with-apis-financial-market-data">Interacting with
    APIs: Financial Market Data</a>
    -   <a href="#requirements" id="toc-requirements">Requirements</a>
    -   <a href="#api-interaction-functions"
        id="toc-api-interaction-functions">API Interaction Functions</a>
        -   <a href="#aggregates-bars-endpoint"
            id="toc-aggregates-bars-endpoint"><span>Aggregates (Bars)</span>
            Endpoint</a>
            -   <a href="#get_stocks_agg"
                id="toc-get_stocks_agg"><code>get_stocks_agg</code></a>
        -   <a href="#tickers-endpoint"
            id="toc-tickers-endpoint"><span>Tickers</span> Endpoint</a>
            -   <a href="#get_ticker" id="toc-get_ticker"><code>get_ticker</code></a>
            -   <a href="#get_ticker_info"
                id="toc-get_ticker_info"><code>get_ticker_info</code></a>
        -   <a href="#grouped-daily-bars-endpoint"
            id="toc-grouped-daily-bars-endpoint"><span>Grouped Daily (Bars)</span>
            Endpoint</a>
            -   <a href="#get_grouped_daily"
                id="toc-get_grouped_daily"><code>get_grouped_daily</code></a>
        -   <a href="#ticker-types-endpoint"
            id="toc-ticker-types-endpoint"><span>Ticker Types</span> Endpoint</a>
            -   <a href="#get_ticker_type_details"
                id="toc-get_ticker_type_details"><code>get_ticker_type_details</code></a>
        -   <a href="#exchanges-endpoint"
            id="toc-exchanges-endpoint"><span>Exchanges</span> Endpoint</a>
            -   <a href="#get_exchanges_details"
                id="toc-get_exchanges_details"><code>get_exchanges_details</code></a>
-   <a href="#exploratory-data-analysis-eda"
    id="toc-exploratory-data-analysis-eda">Exploratory Data Analysis
    (EDA)</a>
    -   <a href="#combining-data-from-api-calls"
        id="toc-combining-data-from-api-calls">Combining data from API Calls</a>
    -   <a href="#creation-of-new-variables"
        id="toc-creation-of-new-variables">Creation of new variables</a>
        -   <a href="#plot-for-new-variable" id="toc-plot-for-new-variable">Plot for
            new variable</a>
    -   <a href="#contingency-tables" id="toc-contingency-tables">Contingency
        tables</a>
        -   <a href="#one-way" id="toc-one-way">One-way</a>
        -   <a href="#two-way" id="toc-two-way">Two-way</a>
    -   <a href="#numerical-summaries" id="toc-numerical-summaries">Numerical
        summaries</a>
    -   <a href="#box-plots" id="toc-box-plots">Box plots</a>
    -   <a href="#histogram" id="toc-histogram">Histogram</a>
    -   <a href="#bar-plot" id="toc-bar-plot">Bar plot</a>
    -   <a href="#scatter-plot" id="toc-scatter-plot">Scatter plot</a>

# Interacting with APIs: Financial Market Data

This vignette shows how to work with API. This will demonstrate how to
fetch data from multiple API endpoints and read it in a tibble(data
frame). This is followed by some basic exploratory data analysis (EDA)
to produce some plots to derive insights from the data fetched.  
The API that we have chosen for this project is [Financial Market
Data](https://polygon.io/docs/stocks).The Polygon.io Stocks API provides
REST endpoints that let you query the latest market data from all US
stock exchanges.

## Requirements

I used the following packages in the creation of the vignette:

-   `httr`: This is used to access the REST API endpoint.  
-   `jsonlite`: This is used to parse the fetched data into a data
    frame.  
-   `tidyverse`: This provides two important packages `dplyr` and
    `ggplot` which are used for data manipulation and plotting
    respectively.  
-   `kableExtra`: This provides better printing properties for
    contingency tables in markdown.

To get started, install(if these are not installed already) and load the
following packages:

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(kableExtra)
```

<!--*************************************************************************-->

## API Interaction Functions

This section describes the functions created by us to interact with the
API endpoints to fetch data as well as some metadata required for making
the plots more descriptive.

### [Aggregates (Bars)](https://polygon.io/docs/stocks/get_v2_aggs_ticker__stocksticker__range__multiplier___timespan___from___to) Endpoint

Get aggregate bars for a stock over a given date range in custom time
window sizes.

#### `get_stocks_agg`

This function has four modifications from the user. The user can provide
the following inputs to the functions:

-   **ticker** and **company_name**: The ticker symbol and registered
    name of the company. If the user does not have this information,
    this can be fetched using `get_ticker` function described ahead.  
-   **start_date**: The start of the aggregate time window (A date with
    the format YYYY-MM-DD).  
-   **end_date**: The end of the aggregate time window (A date with the
    format YYYY-MM-DD).  
-   **limit**: Limits the number of base aggregates queried to create
    the aggregate results.

This function returns a data frame with the close price, open price,
highest price, lowest price etc for the stock over the given date range.

``` r
get_stocks_agg <- function(ticker,company_name,start_date="2022-01-01",
                           end_date="2022-08-31",limit=50){
  url=paste0("https://api.polygon.io/v2/aggs/ticker/",ticker,
             "/range/1/day/",start_date,"/",end_date,
             "?adjusted=true&sort=asc&limit=",limit,"&apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  response_obj <- GET(url)
  parsed <- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results)
  df <- df %>% rename(close_price=c,highest_price=h,lowest_price=l,num_transactions=n,
                      open_price=o,timestamp=t,vol=v,weighted_avg_price=vw) 
  df$ticker <- rep(ticker,limit)
  df$company_name <- rep(company_name,limit)
  df$start_date <- start_date
  df$end_date <- end_date
  df <- df %>% select(ticker,company_name,everything())
  return(df)
}
```

### [Tickers](https://polygon.io/docs/stocks/get_v3_reference_tickers) Endpoint

Query all ticker symbols which are supported by Polygon.io. This API
currently includes Stocks/Equities, Crypto, and Forex.

#### `get_ticker`

This function searches only stocks market. The user can provide the name
of the company the user wants the ticker information of. For example: If
a user wants the ticker the ticker information for Apple, the user can
call the function as `get_ticker(name="Apple")`. This function will
return the ticker symbol **AAPL** and the registered company name
**Apple Inc.** as a list. The user can pass on the contents of this list
to `get_stocks_agg` function mentioned above to get the aggregate bars
over a date range. If there are multiple matches for a given name, the
function returns first ticker information.

``` r
get_ticker <- function(name){
  url=paste0("https://api.polygon.io/v3/reference/tickers?market=stocks&search=",name,"&active=true&sort=ticker&order=asc&limit=1000&apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  response_obj <- GET(url)
  parsed<- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results)
  return(list(df$ticker[[1]],df$name[[1]]))
}
```

#### `get_ticker_info`

This function provides details of the ticker for a given market. The
user can provide **market** value to this function. The possible values
of market can be **stocks**, **crypto**, **fx** or **otc**. This
function returns the primary exchange and type of ticker information for
each ticker in each market.

``` r
get_ticker_info <- function(market){
  url=paste0("https://api.polygon.io/v3/reference/tickers?market=",market,"&active=true&sort=ticker&order=asc&limit=1000&apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  response_obj <- GET(url)
  parsed<- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results) %>% select(ticker,name,primary_exchange,type)
  return(df)
}
```

### [Grouped Daily (Bars)](https://polygon.io/docs/stocks/get_v2_aggs_grouped_locale_us_market_stocks__date) Endpoint

Get the daily open, high, low, and close (OHLC) for the entire stocks
markets

#### `get_grouped_daily`

This function takes in **date** as an input from the user and returns
the open, high, low, and close (OHLC) for the entire stocks markets for
that particular date.

``` r
get_grouped_daily <- function(date="2020-10-14"){
  url=paste0("https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/",date,"?adjusted=true&include_otc=true&apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  response_obj <- GET(url)
  parsed <- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results)
  df <- df %>% rename(Ticker=T,volume=v,weighted_avg_price=vw,open_price=o,
                      close_price=c,highest_price=h,lowest_price=l,
                      num_transactions=n,timestamp=t)
  df$date <- date
  return(df)
}
```

### [Ticker Types](https://polygon.io/docs/stocks/get_v3_reference_tickers_types) Endpoint

#### `get_ticker_type_details`

This function is used to get get metadata information of all the ticker
types that Polygon.io knows about.

``` r
get_ticker_type_details <- function(){
  response_obj <- GET("https://api.polygon.io/v3/reference/tickers/types?apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  parsed <- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results) 
  return(df)
}
```

### [Exchanges](https://polygon.io/docs/stocks/get_v3_reference_exchanges) Endpoint

#### `get_exchanges_details`

This function is used to get metadata information of all the stock
exchanges that Polygon.io knows about.

``` r
get_exchanges_details <- function(){
  response_obj <- GET("https://api.polygon.io/v3/reference/exchanges?asset_class=stocks&apiKey=EdkA7_m2JhjS5POrGuXJbVlA4AjSl_4F")
  parsed <- fromJSON(rawToChar(response_obj$content))
  df <- as_tibble(parsed$results)  
  return(df)
}
```

<!--*************************************************************************-->

# Exploratory Data Analysis (EDA)

## Combining data from API Calls

Here we have combined the aggregate bars for stocks of 3 companies,
namely Apple, Tesla and Nvidia from 1 Jan 2022 to 31 August 2022.  
To find the stock information for Apple, we have passed “Apple” as an
input argument to `get_ticker` function. This function returns a list
`ticker_symbol1` which consists of ticker symbol “AAPL” and name of the
company which is “Apple Inc.”.

This information along with start date and end date is passed to the
function `get_stocks_agg`. The limit argument is not passed, so the
function takes the default value of 50. This function fetches the stock
information for Apple in the given date range and returns a data frame
`df1`. Similarly, this is done to get the stocks information for Tesla
and Nvidia in `df2` and `df3` respectively.

``` r
ticker_symbol1 <- get_ticker(name="Apple")
df1 <- get_stocks_agg(ticker_symbol1[[1]],ticker_symbol1[[2]],start_date="2022-01-01",end_date="2022-08-31")
df1
```

    ## # A tibble: 50 × 12
    ##    ticker company_name    vol weigh…¹ open_…² close…³ highe…⁴ lowes…⁵ times…⁶ num_t…⁷
    ##    <chr>  <chr>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
    ##  1 AAPL   Apple Inc.   1.05e8    181.    178.    182.    183.    178. 1.64e12  772691
    ##  2 AAPL   Apple Inc.   9.91e7    181.    183.    180.    183.    179. 1.64e12  831890
    ##  3 AAPL   Apple Inc.   9.45e7    177.    180.    175.    180.    175. 1.64e12  848513
    ##  4 AAPL   Apple Inc.   9.69e7    173.    173.    172     175.    172. 1.64e12  960340
    ##  5 AAPL   Apple Inc.   8.67e7    172.    173.    172.    174.    171. 1.64e12  716881
    ##  6 AAPL   Apple Inc.   1.07e8    170.    169.    172.    172.    168. 1.64e12  956337
    ##  7 AAPL   Apple Inc.   7.61e7    174.    172.    175.    175.    171. 1.64e12  649652
    ##  8 AAPL   Apple Inc.   7.48e7    176.    176.    176.    177.    175. 1.64e12  642756
    ##  9 AAPL   Apple Inc.   8.44e7    174.    176.    172.    177.    172. 1.64e12  692343
    ## 10 AAPL   Apple Inc.   8.04e7    172.    171.    173.    174.    171. 1.64e12  672552
    ## # … with 40 more rows, 2 more variables: start_date <chr>, end_date <chr>, and
    ## #   abbreviated variable names ¹​weighted_avg_price, ²​open_price, ³​close_price,
    ## #   ⁴​highest_price, ⁵​lowest_price, ⁶​timestamp, ⁷​num_transactions

``` r
ticker_symbol2 <- get_ticker(name="Tesla")
df2 <- get_stocks_agg(ticker_symbol2[[1]],ticker_symbol2[[2]],start_date="2022-01-01",end_date="2022-08-31")
df2
```

    ## # A tibble: 50 × 12
    ##    ticker company_name    vol weigh…¹ open_…² close…³ highe…⁴ lowes…⁵ times…⁶ num_t…⁷
    ##    <chr>  <chr>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
    ##  1 TSLA   Tesla, Inc.… 1.05e8    390.    383.    400.    400.    379. 1.64e12 1162844
    ##  2 TSLA   Tesla, Inc.… 9.98e7    387.    397.    383.    403.    374. 1.64e12 1051467
    ##  3 TSLA   Tesla, Inc.… 8.01e7    376.    382.    363.    390.    360. 1.64e12  811988
    ##  4 TSLA   Tesla, Inc.… 9.03e7    353.    359     355.    363.    340. 1.64e12  880974
    ##  5 TSLA   Tesla, Inc.… 8.40e7    346.    360.    342.    360.    337. 1.64e12  823560
    ##  6 TSLA   Tesla, Inc.… 9.18e7    339.    333.    353.    353.    327. 1.64e12  971558
    ##  7 TSLA   Tesla, Inc.… 6.60e7    353.    351.    355.    359.    346. 1.64e12  644108
    ##  8 TSLA   Tesla, Inc.… 8.37e7    365.    360.    369.    372.    358. 1.64e12  761538
    ##  9 TSLA   Tesla, Inc.… 9.69e7    356.    370.    344.    372.    342. 1.64e12  924351
    ## 10 TSLA   Tesla, Inc.… 7.29e7    345.    340.    350.    351.    338. 1.64e12  710334
    ## # … with 40 more rows, 2 more variables: start_date <chr>, end_date <chr>, and
    ## #   abbreviated variable names ¹​weighted_avg_price, ²​open_price, ³​close_price,
    ## #   ⁴​highest_price, ⁵​lowest_price, ⁶​timestamp, ⁷​num_transactions

``` r
ticker_symbol3 <- get_ticker(name="Nvidia")
df3 <- get_stocks_agg(ticker_symbol3[[1]],ticker_symbol3[[2]],start_date="2022-01-01",end_date="2022-08-31")
df3
```

    ## # A tibble: 50 × 12
    ##    ticker company_name    vol weigh…¹ open_…² close…³ highe…⁴ lowes…⁵ times…⁶ num_t…⁷
    ##    <chr>  <chr>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
    ##  1 NVDA   Nvidia Corp  3.92e7    302.    298.    301.    307.    298. 1.64e12  585469
    ##  2 NVDA   Nvidia Corp  5.27e7    291.    303.    293.    305.    283. 1.64e12  894297
    ##  3 NVDA   Nvidia Corp  4.98e7    283.    289.    276.    294.    275. 1.64e12  836624
    ##  4 NVDA   Nvidia Corp  4.54e7    280.    276.    282.    284.    271. 1.64e12  725603
    ##  5 NVDA   Nvidia Corp  4.10e7    275.    281.    272.    284.    271. 1.64e12  639610
    ##  6 NVDA   Nvidia Corp  5.95e7    264.    266.    274     275.    256. 1.64e12  991811
    ##  7 NVDA   Nvidia Corp  4.04e7    275.    273.    278.    281.    268. 1.64e12  572165
    ##  8 NVDA   Nvidia Corp  3.83e7    281.    281.    280.    286.    276. 1.64e12  562208
    ##  9 NVDA   Nvidia Corp  5.39e7    271.    284.    266.    285.    265. 1.64e12  845316
    ## 10 NVDA   Nvidia Corp  3.96e7    268.    263     269.    272.    262. 1.64e12  620045
    ## # … with 40 more rows, 2 more variables: start_date <chr>, end_date <chr>, and
    ## #   abbreviated variable names ¹​weighted_avg_price, ²​open_price, ³​close_price,
    ## #   ⁴​highest_price, ⁵​lowest_price, ⁶​timestamp, ⁷​num_transactions

Here we have combined df1, df2 and df3 into vertically into a data frame
`df_combined`. This gives us all the stock information for the 3
companies in a single data frame. This data frame is further used to
plot the **close_price** for the 3 companies in a given date range.  
For this the time stamp on x-axis is in Unix Msec. I tried to convert it
into Human readable datetime format using multiple ways but I was unable
to do it due deadline for the project. I am sure I would have gotten a
breakthrough had I worked more on this.

``` r
df_combined <- bind_rows(df1, df2, df3)

#Plot
ggplot(df_combined,aes(x=timestamp,y=close_price)) + 
  geom_line(aes(color=company_name),size=1) + 
  labs(x="Time",y="Closing price",
       title="Closing stock price over time for Apple, Nvidia and Tesla") +  
  scale_color_discrete(name = "Company Name")+
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-gfm/4_2-1.png" style="display: block; margin: auto;" />
From the plot we can see the stock price Tesla has dropped the most but
it is still having higher price than Nvidia and Apple between 1 Jan 2022
to 31 August 2022. Elon Musk’s deal with Twitter falling out can be one
of the factor for this sink.

<!--*************************************************************************-->

## Creation of new variables

Here we have called `get_grouped_daily` function to get the open, high,
low, and close (OHLC) for the entire stocks markets on 16 Nov 2020 (a
random date which user can select).

We have added a new variable **percent_change** which is the percent
rise/decline in the stock price throughout that day (Considered
open_price and close_price for calculating this). We have arranged the
data frame in the descending order of percent_change, and hence at the
top of the data frame we have tickers which have the maximum gain in
stock price and at the bottom we have tickers which have the maximum
loss.  
We have also added **percent_change_chr** variable, where we have
coerced percent_change as character which used to make the plot ahead
more descriptive.

``` r
df_grouped <- get_grouped_daily("2020-11-16")
df_grouped <- df_grouped %>% 
  mutate(percent_change = round(((close_price-open_price)/open_price)*100,2)) %>% 
  arrange(desc(percent_change))

df_grouped$percent_change_chr <- paste(as.character(df_grouped$percent_change),'%')
df_grouped %>% select(Ticker,open_price,close_price,percent_change_chr,everything())
```

    ## # A tibble: 9,084 × 12
    ##    Ticker open_price close_p…¹ perce…² volume weigh…³ highe…⁴ lowes…⁵ times…⁶ num_t…⁷
    ##    <chr>       <dbl>     <dbl> <chr>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
    ##  1 ZXZZT     10.3      20.0    93.31 % 3.93e4 13.3    20.0    10.3    1.61e12     332
    ##  2 AIRTW      0.0261    0.0395 51.34 % 4.92e4  0.0374  0.0399  0.0261 1.61e12      19
    ##  3 WWR        4.31      6.27   45.48 % 3.60e7  5.43    6.3     4.22   1.61e12  117274
    ##  4 CBAT       7.9      11.3    43.04 % 1.08e8  9.22   11.4     7.15   1.61e12  463824
    ##  5 PPSI       3.02      4.29   42.05 % 1.21e7  4.02    4.48    3.02   1.61e12   43723
    ##  6 ITACW      0.350     0.48   37.1 %  1.63e4  0.401   0.48    0.350  1.61e12      10
    ##  7 BLNKW      5.74      7.86   36.89 % 2.84e5  7.35    8.23    5.74   1.61e12    1493
    ##  8 NBACW      0.51      0.66   29.41 % 5.65e5  0.587   0.680   0.51   1.61e12    1162
    ##  9 SGOC       0.92      1.19   29.35 % 3.31e4  1.06    1.19    0.92   1.61e12      98
    ## 10 KLR.WS     1.06      1.36   28.29 % 3.98e3  1.10    1.36    1.06   1.61e12       7
    ## # … with 9,074 more rows, 2 more variables: date <chr>, percent_change <dbl>, and
    ## #   abbreviated variable names ¹​close_price, ²​percent_change_chr,
    ## #   ³​weighted_avg_price, ⁴​highest_price, ⁵​lowest_price, ⁶​timestamp,
    ## #   ⁷​num_transactions

### Plot for new variable

Here we have used `head()` and `tail()` to get the stock information of
10 tickers having the highest percent gain in `df_top10` and 10 tickers
having the highest percent loss in `df_bottom10`. This information is
plotted using `geom_col()`.  
As mentioned above **percent_change_chr** is used to add text on top of
the columns using `geom_text()`.

On the x-axis we could not map the ticker symbol to the company name
(which would have made the plot clearer) because there is a max limit of
1000 on the [Ticker
Endpoint](https://polygon.io/docs/stocks/get_v3_reference_tickers). So
we do not get all the data and hence there is a possibility that we have
a ticker symbol in `df_grouped` for which we have not been able to fetch
the company name.

``` r
#Top-10 gains
df_top10 <- head(df_grouped, 10)
df_top10$Ticker <- factor(df_top10$Ticker, 
                          levels=df_top10$Ticker[order(-df_top10$percent_change)])

# Plot
date <- unique(df_top10$date)
ggplot(df_top10, aes(x=Ticker, y=percent_change)) + 
geom_col(width=0.3, color='steelblue', fill='steelblue') + 
theme(axis.text.x=element_text(angle=90), text=element_text(size=12), 
      plot.title = element_text(hjust = 0.5)) + 
labs(y="Percent increase", x ="Stock ticker",
     title = paste0("Highest stock price increase on ",date)) + 
geom_text(aes(label = percent_change_chr), vjust = -0.5, size=3)
```

<img src="README_files/figure-gfm/6_1-1.png" style="display: block; margin: auto;" />

``` r
#Top-10 losses
df_bottom10 <- tail(df_grouped, 10)
df_bottom10$percent_change <- abs(df_bottom10$percent_change)
df_bottom10$Ticker <- factor(df_bottom10$Ticker, 
                             levels=df_bottom10$Ticker[order(-df_bottom10$percent_change)])

# Plot
date = unique(df_bottom10$date)
ggplot(df_bottom10, aes(x=Ticker, y=percent_change)) + 
geom_col(width=0.3, color='red', fill='red') + 
theme(axis.text.x = element_text(angle=90),text = element_text(size=12),
      plot.title = element_text(hjust = 0.5)) + 
labs(y="Percent decrease", x ="Stock ticker", 
     title = paste0("Highest stock price decrease on ",date)) + 
geom_text(aes(label=percent_change_chr), vjust =-0.5, size=3)
```

<img src="README_files/figure-gfm/6_1-2.png" style="display: block; margin: auto;" />

From the plot above we can see that on 16th Nov 2020, the stock price
for ZXZZT(NASDAQ TEST STOCK) noticed maximum gain of 93.91%. After that,
the highest gain was noticed by AIRTW (Air T, Inc.) which was 51.34% and
WWR(Westwater Resources, Inc.) which was 45.48%.  
On similar lines, the stock price for KTOVW(Kitov Pharma Ltd. Warrants)
noticed highest loss of 43.93%. Thus this plot allows the user to find
out the biggest gainers and losers on any given day.

<!--*************************************************************************-->

## Contingency tables

Here we have used `get_ticker_info` function to get ticker information
such as its type and its primary exchange for all the tickers supported
by Polygon.io. in the **stock** market. Hence the input argument to the
function call is “stocks”.

### One-way

Here we have created a contingency table using ticker type information
for the stock market. When fetching data from the [Ticker
Endpoint](https://polygon.io/docs/stocks/get_v3_reference_tickers) in
`df_info`, we get the abbrevations of ticker types. In order to fetch
the descriptions of ticker type (For example: **CS** means **Common
Stocks**) we have fetched data from the [Ticker Types
Endpoint](https://polygon.io/docs/stocks/get_v3_reference_tickers_types)
in `df_tickertype_metadata`. Then we took the `left_join` of the two
tibbles in order to get all the information in a single tibble which is
then used to create the contingency table.

``` r
df_info <- get_ticker_info("stocks")
Sys.sleep(5)
df_tickertype_metadata <- get_ticker_type_details() %>% select(code,description)
Sys.sleep(5)
df_tables <- left_join(df_info,df_tickertype_metadata,by=c("type"="code"))

tab1 <- table(df_tables$description,dnn=c("Ticker Types"))
tab1 %>%
  kbl(caption="Table for Ticker Types") %>%
  kable_classic(full_width = F)
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Table for Ticker Types
</caption>
<thead>
<tr>
<th style="text-align:left;">
Ticker.Types
</th>
<th style="text-align:right;">
Freq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
American Depository Receipt Common
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Common Stock
</td>
<td style="text-align:right;">
561
</td>
</tr>
<tr>
<td style="text-align:left;">
Exchange Traded Fund
</td>
<td style="text-align:right;">
110
</td>
</tr>
<tr>
<td style="text-align:left;">
Exchange Traded Note
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Fund
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Preferred Stock
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
Rights
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Structured Product
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Unit
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
Warrant
</td>
<td style="text-align:right;">
106
</td>
</tr>
</tbody>
</table>

From the table we can see that the number of CS (Common Stock) is 561
and the number of ARDC (American Depository Receipt Common) is 40.

### Two-way

Similarly here we have created a two way contingency table for the
number of ticker types for each stock exchange. When fetching data from
the [Ticker
Endpoint](https://polygon.io/docs/stocks/get_v3_reference_tickers) in
`df_info`, we get the abbrevations stock exchanges. Here we tried to use
[Exchange
Endpoint](https://polygon.io/docs/stocks/get_v3_reference_exchanges) to
get the full name of the stock exchanges (For example: **XNYS** is
**NYSE American, LLC**) but there we multiple exchange information for
each exchange which made the table very complicated to understand. For
that reason we let the exchange abbrevations be.

``` r
tab2 <- table(df_tables$description,df_info$primary_exchange,dnn=c("Ticker Types","Exchanges"))
tab2 %>%
  kbl(caption="Table for Ticker Types and Exchanges") %>%
  kable_classic(full_width = F)
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Table for Ticker Types and Exchanges
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
ARCX
</th>
<th style="text-align:right;">
BATS
</th>
<th style="text-align:right;">
XASE
</th>
<th style="text-align:right;">
XNAS
</th>
<th style="text-align:right;">
XNYS
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
American Depository Receipt Common
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Common Stock
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
359
</td>
<td style="text-align:right;">
183
</td>
</tr>
<tr>
<td style="text-align:left;">
Exchange Traded Fund
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Exchange Traded Note
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Fund
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Preferred Stock
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
Rights
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Structured Product
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Unit
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Warrant
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
25
</td>
</tr>
</tbody>
</table>

From the table above we can see that there are 359 **Common Stock** type
tickers in the **XNYS** (New York stock exchange). And similarly this
table helps us to identify the total number of each ticker type in each
exchange.

<!--*************************************************************************-->

## Numerical summaries

``` r
df_combined <- bind_rows(df1, df2, df3)
start_date = as.character(unique(df_combined$start_date))
end_date = as.character(unique(df_combined$end_date))
# Open Price
df_combined_open <- df_combined %>% 
  group_by(company_name) %>% 
  summarise("Min." = min(open_price),
            "1st Quartile" = quantile(open_price,0.25),
            "Median." = median(open_price),
            "Mean."=mean(open_price),
            "3rd Quartile" = quantile(open_price,0.75),
            "Max."= max(open_price),
            "Std. Dev." = sd(open_price))

# Close Price
df_combined_close <- df_combined %>% 
  group_by(company_name) %>% 
  summarise("Min." = min(close_price),
            "1st Quartile" = quantile(close_price,0.25),
            "Median." = median(close_price),
            "Mean."=mean(close_price),
            "3rd Quartile" = quantile(close_price,0.75),
            "Max."= max(close_price),
            "Std. Dev." = sd(close_price))

df_combined_open %>%
  kbl(caption=paste0("Summary Statistics for Open Price per Company between ",start_date," and ",end_date)) %>%
  kable_classic()
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics for Open Price per Company between 2022-01-01 and
2022-08-31
</caption>
<thead>
<tr>
<th style="text-align:left;">
company_name
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
1st Quartile
</th>
<th style="text-align:right;">
Median.
</th>
<th style="text-align:right;">
Mean.
</th>
<th style="text-align:right;">
3rd Quartile
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Std. Dev.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Apple Inc. 
</td>
<td style="text-align:right;">
150.9000
</td>
<td style="text-align:right;">
163.5850
</td>
<td style="text-align:right;">
169.4500
</td>
<td style="text-align:right;">
167.8854
</td>
<td style="text-align:right;">
172.6075
</td>
<td style="text-align:right;">
182.6300
</td>
<td style="text-align:right;">
7.00711
</td>
</tr>
<tr>
<td style="text-align:left;">
Nvidia Corp
</td>
<td style="text-align:right;">
210.1500
</td>
<td style="text-align:right;">
231.9650
</td>
<td style="text-align:right;">
243.2500
</td>
<td style="text-align:right;">
247.4482
</td>
<td style="text-align:right;">
260.5800
</td>
<td style="text-align:right;">
302.7700
</td>
<td style="text-align:right;">
22.16117
</td>
</tr>
<tr>
<td style="text-align:left;">
Tesla, Inc. Common Stock
</td>
<td style="text-align:right;">
233.4633
</td>
<td style="text-align:right;">
284.2208
</td>
<td style="text-align:right;">
302.3166
</td>
<td style="text-align:right;">
308.5580
</td>
<td style="text-align:right;">
333.0283
</td>
<td style="text-align:right;">
396.5167
</td>
<td style="text-align:right;">
35.48938
</td>
</tr>
</tbody>
</table>

``` r
df_combined_close %>%
  kbl(caption=paste0("Summary Statistics for Close Price per Company between ",start_date," and ",end_date)) %>%
  kable_classic()
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics for Close Price per Company between 2022-01-01 and
2022-08-31
</caption>
<thead>
<tr>
<th style="text-align:left;">
company_name
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
1st Quartile
</th>
<th style="text-align:right;">
Median.
</th>
<th style="text-align:right;">
Mean.
</th>
<th style="text-align:right;">
3rd Quartile
</th>
<th style="text-align:right;">
Max.
</th>
<th style="text-align:right;">
Std. Dev.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Apple Inc. 
</td>
<td style="text-align:right;">
150.62
</td>
<td style="text-align:right;">
162.7925
</td>
<td style="text-align:right;">
168.7600
</td>
<td style="text-align:right;">
167.6362
</td>
<td style="text-align:right;">
172.7300
</td>
<td style="text-align:right;">
182.0100
</td>
<td style="text-align:right;">
6.987583
</td>
</tr>
<tr>
<td style="text-align:left;">
Nvidia Corp
</td>
<td style="text-align:right;">
213.30
</td>
<td style="text-align:right;">
231.0350
</td>
<td style="text-align:right;">
242.4350
</td>
<td style="text-align:right;">
246.5286
</td>
<td style="text-align:right;">
263.4700
</td>
<td style="text-align:right;">
301.2100
</td>
<td style="text-align:right;">
21.101257
</td>
</tr>
<tr>
<td style="text-align:left;">
Tesla, Inc. Common Stock
</td>
<td style="text-align:right;">
254.68
</td>
<td style="text-align:right;">
280.3517
</td>
<td style="text-align:right;">
302.1667
</td>
<td style="text-align:right;">
306.7592
</td>
<td style="text-align:right;">
327.5708
</td>
<td style="text-align:right;">
399.9267
</td>
<td style="text-align:right;">
34.140396
</td>
</tr>
</tbody>
</table>
<!--*************************************************************************-->

## Box plots

``` r
start_date = as.character(unique(df_combined$start_date))
end_date = as.character(unique(df_combined$end_date))

ggplot(df_combined, aes(x=company_name, y=highest_price)) + 
  geom_boxplot(color="blue",fill="grey") + 
  labs(y="Highest price", x ="Company Name", 
       title=paste0("Boxplot for highest stock price between ",start_date," and ",end_date)) 
```

<img src="README_files/figure-gfm/6_2-1.png" style="display: block; margin: auto;" />

``` r
ggplot(df_combined, aes(x=company_name, y=lowest_price)) + 
  geom_boxplot(color="red",fill="grey") + 
  labs(y="Lowest price", x ="Company Name", 
       title=paste0("Boxplot for lowest stock price between ",start_date," and ",end_date)) 
```

<img src="README_files/figure-gfm/6_2-2.png" style="display: block; margin: auto;" />

## Histogram

``` r
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
start_date = as.character(unique(df_combined$start_date))
end_date = as.character(unique(df_combined$end_date))

my_title <- paste0("Histogram of weighted average price per company between ",start_date," and ",end_date)

ggplot(df_combined, aes(x=weighted_avg_price)) + 
  geom_histogram(aes(fill=company_name),binwidth=8) + 
  labs(x ="Weighted average price") + 
  scale_fill_discrete(name = "Company Name") + 
  ggtitle(wrapper(my_title, width=80))
```

<img src="README_files/figure-gfm/6_3-1.png" style="display: block; margin: auto;" />

## Bar plot

``` r
ggplot(df_info, aes(x=type)) + 
  geom_bar(fill="steelblue") + 
  labs(x ="Type of tickers",
       title="Bar plot for number of stock tickers for each type") + 
  theme(text=element_text(size=12), plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)
```

<img src="README_files/figure-gfm/6_4-1.png" style="display: block; margin: auto;" />

## Scatter plot

The scatter plot has been used to plot the percentage change in returns
for the stocks Apple, Tesla and Nvidia for a date range and can be
differentiated based on the different colours. From the plot, we can see
how varied returns have been on a daily basis but if we look carefully
we can see that there is a certain correlation in the returns and on
most days the movement for all 3 technology stocks is in the same
direction of either gaining or losing

``` r
df_comb <- df_combined %>% mutate(percent_change = round(((close_price-open_price)/open_price)*100,2))
ggplot(df_comb, aes(x=timestamp, y=percent_change,color=company_name)) + 
geom_point()+
facet_grid(cols = vars(company_name)) + 
scale_color_discrete(name = "Company Name")
```

<img src="README_files/figure-gfm/6_5-1.png" style="display: block; margin: auto;" />
