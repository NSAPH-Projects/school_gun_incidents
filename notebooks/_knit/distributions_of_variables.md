# Distributions of variables

Creates Table 1 in our manuscript, i.e., summary statistics of our variables.


```r
## Load packages ----
library(data.table)

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))
```

## Prepare datasets


```r
# prepare dataset for main analysis
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates))

# for sensitivity analysis: get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates, "urban_rural"))

# get 1st/99th and 5th/95th percentiles of exposure (for trimming)
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))

data_with_state_trimmed <- data_with_state[data_with_state$a >= exposure5.95[1] & 
                                             data_with_state$a <= exposure5.95[2], ]
data_with_urbanity_state_trimmed <- data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & 
                                                               data_with_urbanity_state$a <= exposure5.95[2], ]
```

## Get summary statistics of (binary) outcome and (continuous) exposure


```r
nrow(data_with_state) # number of observations, i.e., census tracts
```

```
## [1] 46199
```

```r
sum(data_with_state$y)
```

```
## [1] 692
```

```r
# mean(data_with_state$y)
median(data_with_state$a)
```

```
## [1] 3.06351
```


```r
nrow(data_with_state_trimmed) # number of observations, i.e., census tracts
```

```
## [1] 41579
```

```r
sum(data_with_state_trimmed$y)
```

```
## [1] 655
```

```r
# mean(data_with_state_trimmed$y)
median(data_with_state_trimmed$a)
```

```
## [1] 3.06351
```

## Get summary statistics of quantitative covariates


```r
for (var in quantitative_covariates){
  print(var)
  
  print(mean(data_with_state[[var]]))
  print(sd(data_with_state[[var]]))
  
  print(mean(data_with_state_trimmed[[var]]))
  print(sd(data_with_state_trimmed[[var]]))
}
```

```
## [1] "total_population_2020"
## [1] 4161.967
## [1] 1692.614
## [1] 4246.485
## [1] 1699.11
## [1] "housing_units_per_100_sq_miles"
## [1] 0.1122535
## [1] 0.2961108
## [1] 0.1178142
## [1] 0.3065287
## [1] "area_sq_miles"
## [1] 78296157
## [1] 421369520
## [1] 46913337
## [1] 234064571
## [1] "log_median_hh_income"
## [1] 10.99805
## [1] 0.7346813
## [1] 11.01186
## [1] 0.75541
## [1] "schools_per_100_sq_miles"
## [1] 0.0001235912
## [1] 0.0003300618
## [1] 0.0001303541
## [1] 0.0003421681
## [1] "log_median_hh_income_15to24"
## [1] 10.52112
## [1] 1.000951
## [1] 10.52651
## [1] 1.032415
## [1] "total_crime_2021"
## [1] 102.0498
## [1] 79.42503
## [1] 102.7892
## [1] 79.11357
## [1] "dealers_per_100_sq_miles"
## [1] 1.939517e-05
## [1] 4.950815e-05
## [1] 1.68319e-05
## [1] 4.056306e-05
## [1] "mental_health_index"
## [1] 0.002570827
## [1] 0.0005681803
## [1] 0.002576253
## [1] 0.0005523323
## [1] "daytime_pop_2021"
## [1] 4195.074
## [1] 2454.461
## [1] 4266.755
## [1] 2319.038
## [1] "prop_white_only"
## [1] 0.6218074
## [1] 0.2820271
## [1] 0.6136873
## [1] 0.2822881
## [1] "prop_black_only"
## [1] 0.1351932
## [1] 0.2078134
## [1] 0.1402264
## [1] 0.2106765
## [1] "prop_asian_only"
## [1] 0.03959967
## [1] 0.07091813
## [1] 0.04192862
## [1] 0.07290302
## [1] "prop_multiracial"
## [1] 0.03985212
## [1] 0.01989486
## [1] 0.03999392
## [1] 0.01968921
## [1] "prop_hispanic_latino"
## [1] 0.1497253
## [1] 0.1947728
## [1] 0.1521605
## [1] 0.1951285
## [1] "prop_food_stamps_2019"
## [1] 0.1311483
## [1] 0.1168203
## [1] 0.1302045
## [1] 0.1177077
## [1] "prop_public_assist_income_2019"
## [1] 0.02384327
## [1] 0.02779345
## [1] 0.02381657
## [1] 0.02793021
## [1] "prop_below_poverty_2019"
## [1] 0.137416
## [1] 0.105345
## [1] 0.1356535
## [1] 0.1058187
## [1] "prop_without_vehicles_2019"
## [1] 0.0415247
## [1] 0.07462355
## [1] 0.04199549
## [1] 0.0765744
## [1] "prop_hunted_with_shotgun_2021"
## [1] 0.03358458
## [1] 0.01647768
## [1] 0.03239032
## [1] 0.01564426
## [1] "prop_bachelor_deg_25plus_2021"
## [1] 0.1943138
## [1] 0.1032403
## [1] 0.1984077
## [1] 0.1042174
## [1] "prop_grad_deg_25plus_2021"
## [1] 0.1228787
## [1] 0.1025039
## [1] 0.1267208
## [1] 0.1043039
## [1] "prop_unemployed_2021"
## [1] 0.06281584
## [1] 0.04749265
## [1] 0.06304255
## [1] 0.04769271
## [1] "prop_unemployed_16to24_2021"
## [1] 0.01429858
## [1] 0.0177743
## [1] 0.01440175
## [1] 0.01791271
## [1] "prop_institutional_group"
## [1] 0.01217366
## [1] 0.0486099
## [1] 0.01182623
## [1] 0.0473857
## [1] "prop_noninstitutional_group"
## [1] 0.01145422
## [1] 0.05600041
## [1] 0.01175267
## [1] 0.05786849
## [1] "prop_18plus"
## [1] 0.7784827
## [1] 0.05363322
## [1] 0.7780307
## [1] 0.05386618
```

## Get summary statistics of categorical covariates


```r
prop.table(table(data_with_state$State_Name))
```

```
## 
##              Alabama              Arizona             Arkansas           California             Colorado 
##          0.000000000          0.000000000          0.000000000          0.000000000          0.000000000 
##          Connecticut             Delaware District of Columbia              Florida              Georgia 
##          0.000000000          0.003225178          0.003030369          0.062901794          0.034156583 
##                Idaho             Illinois              Indiana                 Iowa               Kansas 
##          0.008073768          0.053269551          0.026169398          0.014913743          0.014069569 
##             Kentucky            Louisiana                Maine             Maryland        Massachusetts 
##          0.018463603          0.019762333          0.006969848          0.021818654          0.025000541 
##             Michigan            Minnesota          Mississippi             Missouri              Montana 
##          0.042230351          0.023160674          0.012164765          0.026472434          0.005757700 
##             Nebraska               Nevada        New Hampshire           New Jersey           New Mexico 
##          0.010043507          0.009891989          0.006190610          0.035390376          0.009740471 
##             New York       North Carolina         North Dakota                 Ohio             Oklahoma 
##          0.067836966          0.036754042          0.004437325          0.045239074          0.018160566 
##               Oregon         Pennsylvania         Rhode Island       South Carolina         South Dakota 
##          0.016277409          0.049243490          0.004069352          0.017922466          0.004653780 
##            Tennessee                Texas                 Utah              Vermont             Virginia 
##          0.025714842          0.104136453          0.012034893          0.003896188          0.030628369 
##           Washington        West Virginia            Wisconsin              Wyoming 
##          0.027922682          0.009091106          0.025996234          0.003116951
```

```r
prop.table(table(data_with_urbanity_state$urban_rural)) # for sensitivity analysis using urbanity as covariate
```

```
## 
##          1          2          3          4          5          6 
## 0.24803567 0.25372844 0.19548042 0.10262127 0.11021884 0.08991537
```

```r
prop.table(table(data_with_state_trimmed$State_Name))
```

```
## 
##              Alabama              Arizona             Arkansas           California             Colorado 
##          0.000000000          0.000000000          0.000000000          0.000000000          0.000000000 
##          Connecticut             Delaware District of Columbia              Florida              Georgia 
##          0.000000000          0.003439236          0.003318983          0.063998653          0.035618942 
##                Idaho             Illinois              Indiana                 Iowa               Kansas 
##          0.007335434          0.053729046          0.026864523          0.013468337          0.012939224 
##             Kentucky            Louisiana                Maine             Maryland        Massachusetts 
##          0.018254407          0.018783521          0.006637966          0.023112629          0.026575916 
##             Michigan            Minnesota          Mississippi             Missouri              Montana 
##          0.043483489          0.022318959          0.011351884          0.025589841          0.003824046 
##             Nebraska               Nevada        New Hampshire           New Jersey           New Mexico 
##          0.009067077          0.009788595          0.006349359          0.038240458          0.008537964 
##             New York       North Carolina         North Dakota                 Ohio             Oklahoma 
##          0.069963203          0.037951851          0.002886072          0.047042978          0.016594916 
##               Oregon         Pennsylvania         Rhode Island       South Carolina         South Dakota 
##          0.015464537          0.051540441          0.004232906          0.018134154          0.003318983 
##            Tennessee                Texas                 Utah              Vermont             Virginia 
##          0.025902499          0.103754299          0.011568340          0.003799995          0.031386036 
##           Washington        West Virginia            Wisconsin              Wyoming 
##          0.027273383          0.008369610          0.026118954          0.002068352
```

```r
prop.table(table(data_with_urbanity_state_trimmed$urban_rural)) # for sensitivity analysis using urbanity as covariate
```

```
## 
##          1          2          3          4          5          6 
## 0.26162245 0.26749080 0.19906684 0.10014671 0.10038721 0.07128599
```
