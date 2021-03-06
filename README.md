# bcEpiRate <a href="https://github.com/bcgov/bcEpiRate"><img src="man/figures/logo.png" align="right" height="138.5"/></a>

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![R-CMD-check](https://github.com/bcgov/bcEpiRate/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/bcEpiRate/actions)
[![test-coverage](https://codecov.io/gh/bcgov/bcEpiRate/branch/main/graph/badge.svg)](https://codecov.io/gh/bcgov/bcEpiRate?branch=main)

:toolbox: Tools for standardized rates in population health surveillance
and epidemiology

### Features

-   `get_spec_rt()` calculates a specific/crude rate and constructs a
    confidence interval around the estimate.
-   `get_ds_rt()` calculates a directly standardized rate and constructs
    a confidence interval around the estimate using a user-specified
    standard population.
-   `get_smr()` calculates a standardized morbidity/mortality ratio and
    constructs a confidence interval around the estimate.

This package is inspired by the R package
[dsr](https://cran.r-project.org/web/packages/dsr/index.html), which has
now been removed from the CRAN repository, and the SAS/STAT [STDRATE
procedure](https://support.sas.com/documentation/onlinedoc/stat/151/stdrate.pdf).
This package is also designed to play nicely with the
[tidyverse](https://www.tidyverse.org/), as shown in the vignette.

Curious as to why there’s a bathtub on the logo? Check out [this
article](https://www.publichealth.hscni.net/node/5277).

### Installation

To install the package from GitHub, you will need the
[remotes](https://github.com/r-lib/remotes) package.

``` r
install.packages("remotes")
```

Next, install the `bcEpiRate` package along with the vignette using:

``` r
remotes::install_github("bcgov/bcEpiRate", build_vignettes = TRUE)
```

### Usage

See the vignette for this package using:

``` r
browseVignettes("bcEpiRate")
```

### Project Status

In the coming months, there are plans to add features for calculating
measures of association (e.g., risk ratio, rate ratio).

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/bcEpiRate/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### Citation


    To cite package 'bcEpiRate' in publications use:

      Reiko Okamoto, Henry Ngo and Ioana Sevcenco (2022). bcEpiRate: Tools
      for Standardized Rates in Population Health Surveillance and
      Epidemiology. R package version 1.0.1.
      https://github.com/bcgov/bcEpiRate

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {bcEpiRate: Tools for Standardized Rates in Population Health Surveillance and Epidemiology},
        author = {Reiko Okamoto and Henry Ngo and Ioana Sevcenco},
        year = {2022},
        note = {R package version 1.0.1},
        url = {https://github.com/bcgov/bcEpiRate},
      }

### License

    Copyright 2022 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
