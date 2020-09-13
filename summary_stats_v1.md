summary\_stats
================
DE
9/11/2020

``` r
library(survey)
library(haven)
library(tidyverse)
vaccine = read_sas("NHANES_WOMEN_vaccine_2007_2016_v2.sas7bdat",NULL)
```

## SUMMARY STATISTICS AND TABLES FOR NHANES DATASET

Let’s create the survey object,

``` r
vaccine$weights = vaccine$WTMEC2YR/5
vac_design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~weights, data=vaccine, nest=TRUE)
vac_design
```

    ## Stratified 1 - level Cluster Sampling design (with replacement)
    ## With (154) clusters.
    ## svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~weights, 
    ##     data = vaccine, nest = TRUE)

## Age

``` r
mean_age = svymean(~RIDAGEYR, vac_design)
age_table = svytable(~agecat, vac_design)
names(age_table) = c("Age Grp 18-24", "Age Grp 25-29", "Age Grp 30-34", "Age Grp 35-39", 
                     "Age Grp 40-44","Age Grp 45-49", "Age Grp 50-54", "Age Grp 55-59")
age_table
```

    ## Age Grp 18-24 Age Grp 25-29 Age Grp 30-34 Age Grp 35-39 Age Grp 40-44 
    ##      14071930      10591922       9923136      10340538      10884692 
    ## Age Grp 45-49 Age Grp 50-54 Age Grp 55-59 
    ##      11070600      11110700      10041175

``` r
age_vac_table = svytable(~agecat+hpv_vaccine, vac_design)
row.names(age_vac_table) = c("Age Grp 18-24", "Age Grp 25-29", "Age Grp 30-34", "Age Grp 35-39", 
                       "Age Grp 40-44","Age Grp 45-49", "Age Grp 50-54", "Age Grp 55-59")
colnames(age_vac_table) = c("Yes", "No", "Refused", "Don't Know")
age_vac_table
```

    ##                hpv_vaccine
    ## agecat                   Yes           No      Refused   Don't Know
    ##   Age Grp 18-24  5107852.624  8419751.092     3454.456   535076.815
    ##   Age Grp 25-29  2100996.138  8133581.592     2937.442   354406.603
    ##   Age Grp 30-34   778767.759  8963082.803        0.000   177500.440
    ##   Age Grp 35-39   306694.043  9723710.055        0.000   306659.178
    ##   Age Grp 40-44   239117.060 10464374.152        0.000   177843.640
    ##   Age Grp 45-49   191720.196 10623567.505        0.000   255312.626
    ##   Age Grp 50-54   139194.615 10836645.881     7699.034   122086.968
    ##   Age Grp 55-59    53035.973  9902742.198        0.000    82778.413

## vaccine

``` r
vac_status = svytable(~hpv_vaccine, vac_design)
names(vac_status) = c("Yes", "No", "Refused", "Don't Know")
vac_status
```

    ##         Yes          No     Refused  Don't Know 
    ##  8917378.41 77067455.28    14090.93  2011664.68

## HPV positive/negative (roche)

``` r
rochehpv_table = svytable(~LBDRPCR,vac_design)
names(rochehpv_table) = c("Positive", "Negative", "Inadequate")
rochehpv_table
```

    ##   Positive   Negative Inadequate 
    ## 32267058.3 47247068.2   458246.7

``` r
rochehpv_vac_table = svytable(~LBDRPCR+hpv_vaccine,vac_design)
row.names(rochehpv_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(rochehpv_vac_table) = c("Yes", "No", "Refused", "Don't Know")
rochehpv_vac_table
```

    ##             hpv_vaccine
    ## LBDRPCR               Yes           No      Refused   Don't Know
    ##   Positive    4279580.050 27290694.970        0.000   688234.609
    ##   Negative    3678082.921 42516720.688     5938.944  1034555.610
    ##   Inadequate    53238.038   400433.137        0.000     4575.512

## HPV genotype

``` r
hpv16_table = svytable(~hpv16, vac_design)
names(hpv16_table) = c("Positive", "Negative", "Inadequate")
hpv16_table
```

    ##   Positive   Negative Inadequate 
    ##  3120736.9 76393389.6   458246.7

``` r
hpv16_vac_table = svytable(~hpv16+hpv_vaccine, vac_design)
row.names(hpv16_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(hpv16_vac_table) = c("Yes", "No", "Refused", "Don't Know")
hpv16_vac_table
```

    ##             hpv_vaccine
    ## hpv16                 Yes           No      Refused   Don't Know
    ##   Positive     249684.993  2812393.203        0.000    58658.730
    ##   Negative    7707977.978 66995022.455     5938.944  1664131.490
    ##   Inadequate    53238.038   400433.137        0.000     4575.512

``` r
hpv18_table = svytable(~hpv18, vac_design)
names(hpv18_table) = c("Positive", "Negative", "Inadequate")
hpv18_table
```

    ##   Positive   Negative Inadequate 
    ##  1184316.9 78329809.6   458246.7

``` r
hpv18_vac_table = svytable(~hpv18+hpv_vaccine, vac_design)
row.names(hpv18_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(hpv18_vac_table) = c("Yes", "No", "Refused", "Don't Know")
hpv18_vac_table
```

    ##             hpv_vaccine
    ## hpv18                 Yes           No      Refused   Don't Know
    ##   Positive      87437.331  1079781.228        0.000    17098.330
    ##   Negative    7870225.640 68727634.430     5938.944  1705691.890
    ##   Inadequate    53238.038   400433.137        0.000     4575.512

``` r
hpv45_table = svytable(~hpv45, vac_design)
names(hpv45_table) = c("Positive", "Negative", "Inadequate")
hpv45_table
```

    ##   Positive   Negative Inadequate 
    ##  1427986.2 78086140.3   458246.7

``` r
hpv45_vac_table = svytable(~hpv45+hpv_vaccine, vac_design)
row.names(hpv45_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(hpv45_vac_table) = c("Yes", "No", "Refused", "Don't Know")
hpv45_vac_table
```

    ##             hpv_vaccine
    ## hpv45                 Yes           No      Refused   Don't Know
    ##   Positive     329743.308  1068301.506        0.000    29941.391
    ##   Negative    7627919.663 68739114.152     5938.944  1692848.828
    ##   Inadequate    53238.038   400433.137        0.000     4575.512

``` r
hpv_high_table = svytable(~hpv_high, vac_design)
names(hpv_high_table) = c("Positive", "Negative", "Inadequate")
hpv_high_table
```

    ##   Positive   Negative Inadequate 
    ##  4816331.9 74697794.6   458246.7

``` r
hpv_high_vac_table = svytable(~hpv_high+hpv_vaccine, vac_design)
row.names(hpv_high_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(hpv_high_vac_table) = c("Yes", "No", "Refused", "Don't Know")
hpv_high_vac_table
```

    ##             hpv_vaccine
    ## hpv_high              Yes           No      Refused   Don't Know
    ##   Positive     850004.526  3814718.006        0.000   151609.407
    ##   Negative    7107658.445 65992697.652     5938.944  1571180.812
    ##   Inadequate    53238.038   400433.137        0.000     4575.512

``` r
hpv_low_table = svytable(~hpv_low, vac_design)
names(hpv_low_table) = c("Positive", "Negative", "Inadequate")
hpv_low_table
```

    ##   Positive   Negative Inadequate 
    ## 10806793.4 68707333.2   458246.7

``` r
hpv_low_vac_table = svytable(~hpv_low+hpv_vaccine, vac_design)
row.names(hpv_low_vac_table) = c("Positive", "Negative", "Inadequate")
colnames(hpv_low_vac_table) = c("Yes", "No", "Refused", "Don't Know")
hpv_low_vac_table
```

    ##             hpv_vaccine
    ## hpv_low               Yes           No      Refused   Don't Know
    ##   Positive    1962562.805  8599493.024        0.000   244737.525
    ##   Negative    5995100.166 61207922.634     5938.944  1478052.695
    ##   Inadequate    53238.038   400433.137        0.000     4575.512
