Data\_manipulation
================
DE
9/11/2020

## NHANES DATA

``` r
library(haven)
library(tidyverse)
```

## Read data

Read NHANES dataset and create some new variables.
“NHANES\_WOMEN\_vaccine\_2007\_2016.sas7bdat” dataset is the subset
the original dataset which includes data from only women and between
years 2007 to
2016.

``` r
vaccine = read_sas("~/Inclusion-of-Vaccination-in-Guidelines/NHANES_WOMEN_vaccine_2007_2016.sas7bdat",NULL)  
# combine the vaccination status variables
vaccine$hpv_vaccine = ifelse(is.na(vaccine$IMQ040),vaccine$IMQ060,vaccine$IMQ040)
vaccine$agecat = as.factor(vaccine$agecat)

## hpv genotypes
vaccine$hpv16 = vaccine$LBDR16
vaccine$hpv18 = vaccine$LBDR18
vaccine$hpv45 = vaccine$LBDR45
vaccine$hpv_high = ifelse(vaccine$LBDR31==1 | vaccine$LBDR33==1 | vaccine$LBDR52==1 | vaccine$LBDR58==1, 1,
                          ifelse(vaccine$LBDR31==3 & vaccine$LBDR33==3 & vaccine$LBDR52==3 & vaccine$LBDR58==3,3,
                                 ifelse(is.na(vaccine$LBDR31) & is.na(vaccine$LBDR33) & is.na(vaccine$LBDR52) 
                                        & is.na(vaccine$LBDR58),NA,2)))

vaccine$hpv_low = ifelse(vaccine$LBDR35==1 | vaccine$LBDR39==1 | vaccine$LBDR51==1 | vaccine$LBDR56==1 | 
                           vaccine$LBDR59==1 | vaccine$LBDR66==1 | vaccine$LBDR68==1, 1,
                          ifelse(vaccine$LBDR35==3 & vaccine$LBDR39==3 & vaccine$LBDR51==3 & vaccine$LBDR56==3 &
                                   vaccine$LBDR59==3 & vaccine$LBDR66==3 & vaccine$LBDR68==3,3,
                                 ifelse(is.na(vaccine$LBDR35) & is.na(vaccine$LBDR39) & is.na(vaccine$LBDR51) &
                                          is.na(vaccine$LBDR56) & is.na(vaccine$LBDR59) & is.na(vaccine$LBDR66) &
                                          is.na(vaccine$LBDR68),NA,2)))

write_sas(vaccine, "NHANES_WOMEN_vaccine_2007_2016_v2.sas7bdat")
# save the dataset with new variables
```
