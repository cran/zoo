

<!-- README.md is generated from README.qmd via: quarto render README.qmd --to gfm -->

# zoo: S3 Infrastructure for Regular and Irregular Time Series

## Overview

The R package [zoo](https://zeileis.codeberg.page/zoo/) provides an S3
class `zoo` for working with regular and irregular time series. The
class stores data as numeric vectors/matrices (or factors) along with a
time `index` which can be of arbitrary class (including `numeric`,
`Date`, `POSIXct`, `chron`, `yearmon`, `yearqtr`). Functions and methods
for `zoo` are consistent with the `ts` class and base R and also extend
standard generics. Tools include:

- Data import/export and coercion to and from other time series classes
  (`ts`, `xts`, `timeSeries`, etc.).
- Visualization using different engines (base R, `ggplot2`, `lattice`, `tinyplot`).
- Alignment and merging of series recorded at different time indexes.
- Aggregation to coarser time intervals.
- Lags and subsets.
- Rolling analytics over moving time windows.
- Time-based interpolation and filling.

## Installation

The stable version of `zoo` is available from
[CRAN](https://CRAN.R-project.org/package=zoo):

``` r
install.packages("zoo")
```

The latest development version can be installed from
[R-universe](https://zeileis.R-universe.dev/zoo):

``` r
install.packages("zoo", repos = "https://zeileis.R-universe.dev")
```

## License

The package is available under the [General Public License version
3](https://www.gnu.org/licenses/gpl-3.0.html) or [version
2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

## Citation

Zeileis A, Grothendieck G (2005). “zoo: S3 Infrastructure for Regular
and Irregular Time Series.” *Journal of Statistical Software*,
**14**(6), 1-27.
[doi:10.18637/jss.v014.i06](https://doi.org/10.18637/jss.v014.i06)
