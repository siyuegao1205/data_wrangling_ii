Strings and Factors
================

## String vectors

``` r
string_vec = c("my", "name", "is", "jeff")

str_detect(string_vec, "Jeff") # case-sensitive
```

    ## [1] FALSE FALSE FALSE FALSE

``` r
str_detect(string_vec, "a")
```

    ## [1] FALSE  TRUE FALSE FALSE

``` r
str_replace(string_vec, "jeff", "Jeff")
```

    ## [1] "my"   "name" "is"   "Jeff"

``` r
str_replace(string_vec, "m", "M")
```

    ## [1] "My"   "naMe" "is"   "jeff"

When you need to deal with some **regular expressions** …

``` r
string_vec = c(
  "i think we all rule for participating",
  "i think i have been caught",
  "i think this will be quite fun actually",
  "it will be fun, i think"
  )

str_detect(string_vec, "^i think") # ^: begin with the regular expression
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
str_detect(string_vec, "i think$") # $: end with the regular expression
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
string_vec = c(
  "Y'all remember Pres. HW Bush?",
  "I saw a green bush",
  "BBQ and Bushwalking at Molonglo Gorge",
  "BUSH -- LIVE IN CONCERT!!"
  )

str_detect(string_vec, "Bush")
```

    ## [1]  TRUE FALSE  TRUE FALSE

``` r
str_detect(string_vec, "[Bb]ush") # upper case or lower case b
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
str_detect(string_vec, "[Bb][Uu][Ss][Hh]")
```

    ## [1] TRUE TRUE TRUE TRUE

A number followed by letter(s)

``` r
string_vec = c(
  '7th inning stretch',
  '1st half soon to begin. Texas won the toss.',
  'she is 5 feet 4 inches tall', # number followed by space
  '3AM - cant sleep :('
  )

str_detect(string_vec, "[0-9][a-zA-Z]")
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'Its 7:11 in the evening',
  'want to go to 7-11?',
  'my flight is AA711',
  'NetBios: scanning ip 203.167.114.66'
  )

str_detect(string_vec, "7.11") # "." indicates anything except for nothing
```

    ## [1]  TRUE  TRUE FALSE  TRUE

To find a real bracket ..?

``` r
string_vec = c(
  'The CI is [2, 5]',
  ':-]',
  ':-[',
  'I found the answer on pages [6-7]'
  )

str_detect(string_vec, "\\[")
```

    ## [1]  TRUE FALSE  TRUE  TRUE

``` r
str_detect(string_vec, "\\[[0-9]")
```

    ## [1]  TRUE FALSE FALSE  TRUE

## Why factors are weird

``` r
factor_vec = factor(c("male", "male", "female", "female"))

as.numeric(factor_vec) # convert into numeric
```

    ## [1] 2 2 1 1

``` r
factor_vec = fct_relevel(factor_vec, "male")

as.numeric(factor_vec)
```

    ## [1] 1 1 2 2

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

table_marj = 
  read_html(nsduh_url) %>% 
  html_table() %>% 
  first() %>% 
  slice(-1)
```

tidy up the NSDUH data …

``` r
marj_df = 
  table_marj %>% 
  select(-contains("P value")) %>% 
  pivot_longer(
    -State,
    names_to = "age_year",
    values_to = "percent"
  ) %>% 
  mutate(
    percent = str_replace(percent, "[a-b]$", ""),
    percent = as.numeric(percent)
  ) %>% 
  separate(age_year, into = c("age", "year"), sep = "\\(") %>% 
  mutate(
    year = str_replace(year, "\\)", "")
  ) %>% 
  filter(
    !(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West", "District of Columbia"))
  )
```

``` r
marj_df %>% 
  filter(age == "12-17") %>% 
  mutate(State = fct_reorder(State, percent)) %>% 
  ggplot(aes(x = State, y = percent, color = year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

## Restaurant data

``` r
data("rest_inspec")
```

``` r
rest_inspec %>% 
  slice(1:1000)
```

    ## # A tibble: 1,000 × 18
    ##    action boro  build…¹  camis criti…² cuisi…³ dba   inspection_date     inspe…⁴
    ##    <chr>  <chr> <chr>    <int> <chr>   <chr>   <chr> <dttm>              <chr>  
    ##  1 Viola… MANH… 425     4.15e7 Not Cr… Italian SPIN… 2016-10-05 00:00:00 Cycle …
    ##  2 Viola… MANH… 37      4.12e7 Critic… Korean  SHIL… 2015-07-17 00:00:00 Cycle …
    ##  3 Viola… MANH… 15      4.11e7 Not Cr… CafÃ©/… CITY… 2017-03-06 00:00:00 Admini…
    ##  4 Viola… MANH… 35      4.13e7 Critic… Korean  MADA… 2015-01-29 00:00:00 Cycle …
    ##  5 Viola… MANH… 1271    5.00e7 Critic… Americ… THE … 2014-11-13 00:00:00 Pre-pe…
    ##  6 Viola… MANH… 155     5.00e7 Not Cr… Donuts  DUNK… 2016-11-28 00:00:00 Cycle …
    ##  7 Viola… MANH… 1164    5.00e7 Critic… Salads  SWEE… 2015-03-12 00:00:00 Cycle …
    ##  8 Viola… MANH… 37      4.12e7 Not Cr… Korean  SHIL… 2016-01-22 00:00:00 Cycle …
    ##  9 Viola… MANH… 299     5.01e7 Not Cr… Americ… PRET… 2017-08-28 00:00:00 Calori…
    ## 10 Viola… MANH… 53      4.04e7 Not Cr… Korean  HAN … 2016-05-10 00:00:00 Cycle …
    ## # … with 990 more rows, 9 more variables: phone <chr>, record_date <dttm>,
    ## #   score <int>, street <chr>, violation_code <chr>,
    ## #   violation_description <chr>, zipcode <int>, grade <chr>, grade_date <dttm>,
    ## #   and abbreviated variable names ¹​building, ²​critical_flag,
    ## #   ³​cuisine_description, ⁴​inspection_type

``` r
rest_inspec %>% 
  group_by(boro, grade) %>% 
  summarise(
    n_obs = n()
  ) %>% 
  pivot_wider(
    names_from = grade,
    values_from = n_obs
  )
```

    ## `summarise()` has grouped output by 'boro'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 6 × 8
    ## # Groups:   boro [6]
    ##   boro              A     B     C `Not Yet Graded`     P     Z  `NA`
    ##   <chr>         <int> <int> <int>            <int> <int> <int> <int>
    ## 1 BRONX         13688  2801   701              200   163   351 16833
    ## 2 BROOKLYN      37449  6651  1684              702   416   977 51930
    ## 3 MANHATTAN     61608 10532  2689              765   508  1237 80615
    ## 4 Missing           4    NA    NA               NA    NA    NA    13
    ## 5 QUEENS        35952  6492  1593              604   331   913 45816
    ## 6 STATEN ISLAND  5215   933   207               85    47   149  6730

``` r
rest_inspec =
  rest_inspec %>% 
  filter(grade %in% c("A", "B", "C"), boro != "Missing") %>% 
  mutate(boro = str_to_title(boro))

rest_inspec %>% 
  group_by(boro, grade) %>% 
  summarise(
    n_obs = n()
  ) %>% 
  pivot_wider(
    names_from = grade,
    values_from = n_obs
  )
```

    ## `summarise()` has grouped output by 'boro'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   boro [5]
    ##   boro              A     B     C
    ##   <chr>         <int> <int> <int>
    ## 1 Bronx         13688  2801   701
    ## 2 Brooklyn      37449  6651  1684
    ## 3 Manhattan     61608 10532  2689
    ## 4 Queens        35952  6492  1593
    ## 5 Staten Island  5215   933   207

Let’s find pizza data …

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(boro = fct_infreq(boro)) %>% 
  ggplot(aes(x = boro)) +
  geom_bar()
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(
    boro = fct_infreq(boro),
    boro = str_replace(boro, "Manhattan", "The City")
  ) %>% 
  ggplot(aes(x = boro)) +
  geom_bar()
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(
    boro = fct_infreq(boro),
    boro = fct_recode(boro, "The City" = "Manhattan")
  ) %>% 
  ggplot(aes(x = boro)) +
  geom_bar()
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-16-2.png" width="90%" />
