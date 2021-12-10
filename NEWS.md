# bcEpiRate

# bcEpiRate 1.0.0

### Breaking changes

-   `get_ci()` removed.

    -   `get_ci_norm()` replaces `get_ci(..., dist = "normal")` to calculate a normal distribution confidence interval.
    -   `get_ci_lnorm()` replaces `get_ci(..., dist = "log normal")` to calculate a log-normal distribution confidence interval.
    -   `get_ci_pois()` replaces `get_ci(..., dist = "poisson")` to calculate a Poisson distribution confidence interval.
    -   `get_ci_gamma()` has been introduced to calculate a gamma distribution confidence interval.

-   `@details` tag in the documentation of each function listed above specifies where it appears throughout the package.

-   `std_r` parameter replaces `std_rt` in `get_smr()` so function can support the calculation of both rate- and risk-standardized mortality/morbidity ratio in the future.

### New features

-   `get_smr()` can now construct confidence intervals. It uses `get_ci_norm()`, `get_ci_lnorm()`, and `get_ci_pois()` to build normal, log-normal, and Poisson distribution confidence intervals, respectively.

### Minor improvements and fixes

-   `get_ds_rt()` now uses `get_ci_norm()`, `get_ci_lnorm()`, `get_ci_gamma()` to build normal, log-normal, and gamma distribution confidence intervals, respectively.

-   `get_ds_rt()` now includes additional input validation for `scale` and `power` parameters.

-   `get_spec_rt()` now uses `get_ci_norm()`, `get_ci_lnorm()`, `get_ci_pois()` to build normal, log-normal, and Poisson distribution confidence intervals, respectively.

-   `get_smr()` no longer issues a warning or throws an error when there are `NA` values in its input arguments.

-   Input validation: when an input has to satisfy multiple conditions, all conditions are returned in the error message regardless of which one triggered it.

### Documentation improvements

-   `@references` tag in the function documentation points users to the [SAS STDRATE Procedure](http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm) documentation.

-   Terminology made more consistent across the package (e.g. replace all instances of "whole number" to "integer").

# bcEpiRate 0.1.0

-   Adopted version scheme.
