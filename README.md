
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SSSIHLCASFraudClassifyR <img src="man/figures/logo.png" align="right" height="134"/>

SSSIHLCASFraudClassifyR is one of the outputs of the 2022 CAS( Casualty
Actuarial Society) Individual Grant Award Project.

The objective of the project is to provide a framework where actuarial
inputs can be integrated into Machine Learning models to improve fraud
detection and more importantly, make business sense of the outcomes.

The package contains various functions that add flags to the claims data
based on different criteria. These flags can in turn be used by users as
inputs into any statistical or machine learning models. In short, we
infuse actuarial/insurance/behavioral triggers as a feature engineering
step of the model development. You will find various vignettes in the
package that illustrate model fitting process and quantify the
improvement in model fit due to the addition of these triggers.

## Installation

You can install the development version of SSSIHLCASFraudClassifyR like
so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example for adding fraud trigger to your claims data
using the package. Claim amount trigger is a ratio of the actual paid
claim amount and the procedure amount. For cashless claims, cost of the
procedure is often agreed between insurer and medical service provider
upfront.

``` r
library(SSSIHLCASFraudClassifyR)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

data("claims_data_sample")
data("trigger_data")
## basic example code

claims_data_sample <-
  claim_amount_trigger_map(
    claims_file = claims_data_sample,
    claim_paid_field = "approved_allowed_amount",
    triggers_file = trigger_data,
    procedure_code = "primary_procedure_code",
    procedure_amount_field = "package_amount_trigger"
  )
```

``` r
summary(claims_data_sample$claim_amount_ratio)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.07627 0.72000 0.96000 1.00396 1.30120 5.38200
```

For those claims where the ratio is higher than 1, it must be
investigated further to understand the reasons. It could be an
indication of fraud by the medical service provider - that the medical
service provider has levied additional charges for the procedure that
are otherwise not required or agreed with contractually.

Once you map triggers to the data, these fields can be used as
explanatory variables in fraud detection and classification models. In
the package vignettes, we have demonstrated how triggers can be added to
health insurance data to significantly improve predictability of various
Machine learning and Statistical models.
