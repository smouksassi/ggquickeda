# Simulated Pharmacokinetic Concentration Data

A dataset containing concentration-time data with the given dose and
some subject characteristics to help in the app exploration.

## Usage

``` r
sample_data
```

## Format

A data frame with 600 rows and 10 variables

- ID:

  Subject Identifier, an integer from 1 to 150

- Time:

  Time of dose given or drug sample measured, in hours

- Amt:

  dose given at the corresponding Time, in milligrams

- Conc:

  drug concentrations in the plasma sample, in mg/L

- Age:

  age of the subject, in years

- Weight:

  weight of the subject, in kg

- Gender:

  Sex of the subject, a factor with Female and Male levels

- Race:

  Race of the subject, a factor with Asian, Black, Caucasian, Hispanic
  and Other levels

- Dose:

  dose group of the subject, in milligrams

- AGECAT:

  age category of the subject, a variable cutting Age into two values
  0/1

## Source

"sd_oral_richpk" from 'PKPDmisc' R package with an additional AGECAT
variable

## Examples

``` r
sample_data
#> # A tibble: 1,800 × 10
#>       ID  Time   Amt  Conc   Age Weight Gender Race      Dose AGECAT
#>    <int> <dbl> <int> <dbl> <int>  <int> <chr>  <chr>    <int>  <int>
#>  1     1  0     5000  0       56     94 Male   Hispanic  5000      0
#>  2     1  0.25     0  8.61    56     94 Male   Hispanic  5000      0
#>  3     1  0.5      0 19.4     56     94 Male   Hispanic  5000      0
#>  4     1  1        0 34.0     56     94 Male   Hispanic  5000      0
#>  5     1  2        0 30.2     56     94 Male   Hispanic  5000      0
#>  6     1  3        0 31.3     56     94 Male   Hispanic  5000      0
#>  7     1  4        0 25.0     56     94 Male   Hispanic  5000      0
#>  8     1  6        0 23.4     56     94 Male   Hispanic  5000      0
#>  9     1  8        0 23.5     56     94 Male   Hispanic  5000      0
#> 10     1 12        0 14.7     56     94 Male   Hispanic  5000      0
#> # ℹ 1,790 more rows
```
