tidy_data
================
2025-09-23

## `pivot_longer data`

load pulse data

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") |>
  janitor::clean_names()
pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       id   age sex    bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>         <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male              7             1             2             0
    ##  2 10015  72.5 male              6            NA            NA            NA
    ##  3 10022  58.5 male             14             3             8            NA
    ##  4 10026  72.7 male             20             6            18            16
    ##  5 10035  60.4 male              4             0             1             2
    ##  6 10050  84.7 male              2            10            12             8
    ##  7 10078  31.3 male              4             0            NA            NA
    ##  8 10088  56.9 male              5            NA             0             2
    ##  9 10091  76.0 male              0             3             4             0
    ## 10 10092  74.2 female           10             2            11             6
    ## # ℹ 1,077 more rows

wide format to long format…

``` r
pulse_tidy_df = 
  pivot_longer(
    pulse_df, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    values_to = "bdi")
```

rewrite, combine and add a mutate

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") |>
  janitor::clean_names()
  pivot_longer(
    pulse_df, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    values_to = "bdi"
    ) %>% 
    relocate(id, visit) %>% 
    mutate(visit = recode(visit, "bdi_score_bl" = "bdi_score_00m"),
    visit = factor(visit))
```

    ## # A tibble: 4,348 × 5
    ##       id visit           age sex     bdi
    ##    <dbl> <fct>         <dbl> <chr> <dbl>
    ##  1 10003 bdi_score_00m  48.0 male      7
    ##  2 10003 bdi_score_01m  48.0 male      1
    ##  3 10003 bdi_score_06m  48.0 male      2
    ##  4 10003 bdi_score_12m  48.0 male      0
    ##  5 10015 bdi_score_00m  72.5 male      6
    ##  6 10015 bdi_score_01m  72.5 male     NA
    ##  7 10015 bdi_score_06m  72.5 male     NA
    ##  8 10015 bdi_score_12m  72.5 male     NA
    ##  9 10022 bdi_score_00m  58.5 male     14
    ## 10 10022 bdi_score_01m  58.5 male      3
    ## # ℹ 4,338 more rows
