# Simulated Exposure Response Data

A dataset containing data suitable for logistic regression

## Usage

``` r
logistic_data
```

## Format

A data frame with 600 rows and 10 variables

- STUDY:

  Study identifier

- ID:

  Subject Identifier

- DOSE:

  Dose, in mg

- GBDS:

  Dose, in alternative salt

- SEX:

  Sex of the subject

- AGE:

  age of the subject, in years

- WT:

  weight of the subject, in kg

- RACE:

  Race of the subject

- CRCL:

  Creatinine clearance

- BRLS:

  RLS score

- PRLS:

  RLS score

- AUC:

  Area under the curve exposure

- CMAX:

  Maximun concentration exposure

- ICGI:

  response 0/1

- ICGI7:

  response 1 to 7

## Source

inspired from a real data submission

## Examples

``` r
logistic_data
#> # A tibble: 749 × 15
#>    STUDY    ID  DOSE  GBDS   SEX   AGE    WT  RACE  CRCL  BRLS  PRLS   AUC  CMAX
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1    52   150  1200   625     1    55   105     5   125    19     0  89.1   6  
#>  2    52   151  1200   625     2    52    74     1    96    29     8 114.    8.6
#>  3    52   152  1200   625     1    50    77     5    97    19     3  75.6   5.8
#>  4    52   153  1200   625     1    38   103     5   209    16     4  69.8   4.6
#>  5    52   155  1200   625     2    51    91     5   159    25     9  74.3   6.8
#>  6    52   156  1200   625     2    47    83     5   131    23     5  90.1   7.5
#>  7    52   157  1200   625     2    52    69     5    71    20     6  61.6   4.9
#>  8    52   158  1200   625     1    36    85     5   155    26     8  71.3   5.4
#>  9    52   159  1200   625     1    46    85     5   138    16     5  64.3   5.7
#> 10    52   160  1200   625     2    58    78     5   108    19     1 101.    8.2
#> # ℹ 739 more rows
#> # ℹ 2 more variables: ICGI <dbl>, ICGI7 <dbl>
```
