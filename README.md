
# XLS

<!-- badges: start -->
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3.0)
[![](https://img.shields.io/github/last-commit/sametsoekel/eXtreme-Least-Squares.svg)](https://github.com/sametsoekel/eXtreme-Least-Squares/commits/main)
<!-- badges: end -->

An R package that allows modeling with partial weighted regression to remove over effect in time
series models that are heavily loaded on the lag variable and to fit the model in a healthy state. 

## Installation

You can install the released version of XLS from [Github](https://github.com/sametsoekel/eXtreme-Least-Squares) with:

``` r
devtools::install_github("sametsoekel/eXtreme-Least-Squares")
```

## Example

Fit an eXtreme Least Squares Model:

``` r
library(XLS)

df <- datasets::airquality

ordered_df <- df[with(df,order(Month,Day)),]

model <- xls.fit(Ozone ~ Solar.R + Wind + Temp,na.omit(ordered_df),
				 error_weights = c(0.4,0.3,0.2,0.1),error_ahead_level = 4)
```

