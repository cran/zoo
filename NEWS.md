## zoo 1.9-0 (2026-07-30)

* Code repository changed from R-Forge to Codeberg at:
  <https://codeberg.org/zeileis/zoo/>

* Turned all vignettes (except the _Journal of Statistical Software_ paper)
  from Rnw to Rmd format with HTML output.

* Added altdoc-based website with overview, documentation, and vignettes at:
  <https://zeileis.codeberg.page/zoo/>

* When coercing `timeSeries` objects to `zoo` it has long not been necessary
  to employ `timeSeries::time()` but instead the base `stats::time()` generic
  can be used (reported by Georgi Boshnakov).

* The case of `read.zoo(file, index.column = 0)` is handled correctly now if
  `file` is already a `data.frame`.

* Updated `structure()` calls to use `names = ...` instead of `.Names = ...` etc.

* Fixed errors due to unprotected variable index in `zoo_lag` in C.


## zoo 1.8-15 (2025-12-15)

* Documentation fix in BibTeX files requested by CRAN: Use `doi` field rather
  than `url` for DOIs, and updates to various URLs.


## zoo 1.8-14 (2025-04-10)

* In the `scale_x_yearmon()` function the `scale_x_continuous()` function
  is now called with `transform` argument rather than the old `trans` argument
  (deprecated since `ggplot2` 3.5.0). The analogous changes are made for
  `scale_y_*` and `yearqtr`. (Patch provided by Jari Karppinen.)

* Improve example with AM/PM times in `vignette("zoo-read", package = "zoo")`:
  The `%p` specification for the AM/PM indicator should be combined with `%I`
  (and not `%H`) for the hours in 01-12 (rather than 00-23). (Reported by
  Brian D. Ripley.)
  
* In the C code use the `isS4()` API rather than `IS_S4_OBJECT()`.

* In `vignette("zoo-quickref", package = "zoo")` update the Yahoo! Finance
  example (using the MSFT symbol instead of the discontinued SUNW symbol)
  and omit the Oanda example (because the Oanda service has been discontinued).
  (Reported by Robert Woodman.)


## zoo 1.8-13 (2025-02-22)

* New `tinyplot()` method for `"zoo"` objects that is similar to the `autoplot()`
  method but employs the `tinyplot` package rather than `ggplot2` for drawing.

* In `"yearmon"` and `"yearqtr"` try harder to assure that the underlying vector
  type remains numeric (without getting coerced). Also added nicer handling
  and printing of zero-length `"yearmon"`/`"yearqtr"` vectors and added a `[<-` method
  for `"yearmon"` and `"yearqtr"` objects and (suggested by Kyle F. Butts).

* The `diff()` method for `"zoo"` objects gained the argument `log = FALSE` so that
  `diff(x, log = TRUE)` can also be used instead of `diff(log(x))`.

* The `as.vector()` method for `"zoo"` objects now applies to `coredata()` directly
  rather than going through `as.matrix()`. The latter was used in previous
  versions setting up unnecessary additional row/column names etc. with
  unnecessary computational overhead. (Reported by Ethan Smith at
  <https://github.com/joshuaulrich/xts/issues/406>.)

* In `write.zoo()`, by default, the index is formatted using `format()` rather
  than `as.character()` now (reported in <https://stackoverflow.com/questions/77553026>).
  Alternatively, the function `FUN` to format the time index can also be specified
  by the user.

* The `zoo` documentation had aliases for `head.ts` and `tail.ts`, although these
  methods were only very briefly part of the `zoo` development version and never
  released. The aliases have been removed now and the base `stats` package
  will actually provide `head.ts` and `tail.ts` starting from R 4.5.0.


## zoo 1.8-12 (2023-04-13)

* The `autoplot()` method for `"zoo"` objects now uses `ggplot()`  directly rather
  than the `qplot()` function which has been deprecated in recent versions of
  `ggplot2`.

* Support start = NA and end = NA in addition to start = NULL and end = NULL
  for unspecified start and end time of a window() of a zoo series.

* Remove unnecessary S3 method declarations for `axis.yearmon`/`yearqtr` as
  `axis()` is no S3 generic, only `Axis()` is (reported by Kurt Hornik).

* The helper function `index2char()` now calls `format()` instead of `as.character()`
  for objects inheriting from class POSIXt.


## zoo 1.8-11 (2022-09-17)

* The `as.ts()` methods for both `"zoo"` and `"zooreg"` objects now allow to pass on
  the `fill` argument (through `...`) to the underlying `merge()` call. Thus, you can
  say `as.ts(z, fill = 0)` to fill potential gaps with `0`s instead of `NA`s.

* The `na.spline()` method for `"ts"` objects had been provided internally in the
  package but has only been correctly registered in the `NAMESPACE` now.

* The `merge()` method gained a `sep = "."` argument to optionally use a different
  separator character when making pasting suffixes to column names for making
  them unique (suggested by Joshua Ulrich).

* The `MATCH()` methods for `"Date"`/`"POSIXct"`/`"POSIXlt"` object now coerce the `table`
  argument (if necessary) to `"Date"` or `"POSIXct"`, respectively, before calling
  `match(unclass(x), unclass(table), ...)` internally.

* In case of a `"Date"`/`"POSIXct"`/`"POSIXlt"` `table` and a plain atomic argument `x`,
  the default `MATCH()` method coerces `x` to `"Date"` or `"POSIXct"`, respectively,
  before calling `match(unclass(x), unclass(table), ...)` internally.

* As the `fts` package has been archived on CRAN for several months, it has been
  excluded from the `Suggests` of the package. The `as.zoo()` method for `fts`
  objects continues to be part of the package, though, just the hyperlinks in
  the documentation have been removed.


## zoo 1.8-10 (2022-04-15)

* The `transform()` method now internally uses a list of `"zoo"` series (as opposed
  to a data frame of numeric variables, as used in previous versions). This
  means that the transformations can really operate on full `"zoo"` series.

* Added `MATCH()` methods for classes `"Date"`, `"POSIXct"`, and `"POSIXlt"`, all
  of which essentially match the underlying numeric vector (suggested by
  Kurt Hornik).

* In `zooreg()` the `ts.eps` argument is now also used (in squares) as the
  tolerance in the `all.equal()` comparisons used to determine the underlying
  regular time grid.


## zoo 1.8-9 (2021-03-09) 

* Added a basic `boxplot(x, ...)` method for `"zoo"` objects that simply calls
  `boxplot(coredata(x), ...)`.

* Bug fix in `[` and `[<-` methods where indexing with matrices did not
  work correctly anymore in R 4.x.y because `"matrix"` objects now additionally
  inherit from `"array"` objects (reported by Bill Cunliffe).

* Improved internal functionality in `na.spline.default` in the same way
  `na.approx.default` was enhanced in zoo 1.7-14 and 1.7-13. One difference
  is that while `na.approx()` requires at least two non-`NA` observations,
  `na.spline()` just needs one non-`NA` observation (using a constant to
  interpolate in that case).

* If the `FUN` in `rollapply()` returns a matrix or data frame with more than
  one row per element/time point, then this is flattened now so that the
  result is again a matrix with one row per element/time point.

* `merge()` method now handles the case of all indexes being integer or numeric
  by coercing the integer indexes to numeric (reported by Simone Giannerini).

* `zooreg()` now allows to create an integer index if `start` (and `end`, if any)
  are integer and the implied `deltat` is not numerically different from an
  integer value.


## zoo 1.8-8 (2020-05-02) 

* `diff.zoo(x, arithmetic = FALSE)` now also works if `x` contains negative
  data.

* `merge()` method now handles the combination of `"Date"` and numeric indexes
  explicitly to work around the new behavior of `c.Date` in R >= 4.1.0.


## zoo 1.8-7 (2020-01-10) 

* Added `scale_type()` methods for `"yearmon"`/`"yearqtr"` to facilitate `ggplot2`
  visualizations of `"zoo"` series with `"yearmon"`/`"yearqtr"` time index (suggested
  by Brian Diggs). This requires at least `ggplot2` 3.0.0.
  
* In `merge()` method `"character"` columns are now processed in the same way
  as `"logical"` columns.
  
* Replaced the "fruitohms" example in the "zoo" vignette because the
  corresponding `DAAG` package is not actively maintained on CRAN anymore.
  Instead the "Journals" data from the `AER` package is used.


## zoo 1.8-6 (2019-05-28) 

* Adding a zero-width `"zoo"` series via `$<-` to an existing `"zoo"` series
  erroneously changed the last column name. This has been fixed now
  (prompted by <https://github.com/joshuaulrich/xts/issues/45>).

* Set random seed (along with `suppressWarnings(RNGversion("3.5.0"))`) in all
  manual pages that use `rnorm()` as well. Regenerated reference output
  of examples.


## zoo 1.8-5 (2019-03-21) 

* Added dedicated `diff()` methods for `"yearmon"` and `"yearqtr"` objects.

* Fix potential problems when logic comparisons in `if()` are not of
  length `1` (reported by Kurt Hornik).

* Fix potential protect calls found by rchk. `ScalarInteger()` and
  `MAKE_CLASS()` potentially allocate (reported and patched by Kurt Hornik).

* Set `useDynamicSymbols` to `FALSE` as it is better practice to not allow
  the DLL to be searched for entry points via strings (suggested by Kurt
  Hornik).

* Replace `as.zoo.xts` with `xts:::as.zoo.xts`. Both `zoo` and `xts` register
  an `as.zoo.xts` method with the `xts` version being slightly better. Hence
  the `zoo` version was replaced with the `xts` version as the package that owns
  the class should define the conversion method.

* `read.zoo(x)` did not process columns correctly who had column names
  that are intergers in `1:ncol(x)`.

* Add example on `?ggplot2.zoo` that illustrates how to use `scale_x_yearmon`
  for a discrete labeling (rather than continuous). Adapted from:
  <https://stackoverflow.com/questions/54591625/missing-yearmon-labels-using-ggplot-scale-x-yearmon/54592173#54592173>

* Added `suppressWarnings(RNGversion("3.5.0"))` in all vignettes and manual
  pages that use `set.seed()` to make computations reproducible. This is
  necessitated by recent (> 3.5.0) improvements in R's default RNG to
  yield the same output in all CRAN checks.

* Added `maxgap = Inf` argument to `na.trim()`. In particular `na.approx()` and
  `na.spline()` now pass on `maxgap = maxgap` to `na.trim()` so that leading/trailing
  `NA`s are handled consistently with inner `NA`s (pointed out by Ken Williams).


## zoo 1.8-4 (2018-09-19) 

* Conditional registration of S3 methods simplified in R-devel (aka
  3.6.0-to-be) which is leveraged in `"zoo"` for the `autoplot()` and
  `fortify()` methods if `ggplot2` is loaded.


## zoo 1.8-3 (2018-07-16) 

* The `width` argument in `rollapply()` is now `trunc()`-ated to an integer
  rather than implicitly coercing it (which lead to inconsistent and/or
  unintended behavior for non-integer widths in previous versions).
  Analogously for argument `k` in the other `roll*()` functions.

* Consistently use `read.zoo(text = ...)` and `read.table(text = ...)`
  instead of `read.zoo(textConnection(...))` and `read.table(textConnection(...))`
  in order to cleanly open/close the text connection.


## zoo 1.8-2 (2018-06-11) 

* Properly register `autoplot()` and `fortify()` methods for `"zoo"` objects
  provided that `ggplot2` is loaded.

* If `rollapply()`, `rollmean()`, etc. are applied to zero-length objects,
  these objects are returned unmodified.

* The `rollmean(x, k, ...)` method now calls `rollapply(x, k, (mean), ...)` in
  case `x` contains any `NA`s (as the fast `cumsum()`-based solution in `rollmean()` is
  not applicable in this case). Analogously for `rollsum()` and `rollmedian()`
  (reported by Jan Gorecki).

* Added new `na.fill0()` function for simpler filling of `NA`s in sufficiently
  plain vectors, `"Date"` objects, etc.

* Applying `na.fill()` to all-`NA` vectors/columns now consistently treats
  all `NA`s as leading `NA`s. Also filling character vectors/columns has been
  improved.

* In `write.zoo()` the time index column is now coerced to `as.character()`
  prior to calling `write.table()` so that `quote = TRUE` is applied when writing
  time indexes with spaces (e.g., `"Jan 2000"` for `"yearmon"`).


## zoo 1.8-1 (2018-01-08) 

* New `.DollarNames` method (contributed by Josh Ulrich) to enable tab
  completion when using `zoo_object$...`

* New default method for `zoo::as.Date()` that simply dispatches to
  `base::as.Date()`.

* `rollapply(x, k, FUN, align = "right")` leads to an error if `length(x) < k`.
  This has been corrected to match the behavior of the other align types.

* `rollmean()`, `rollsum()`, `rollmedian()`, and `rollmax()` no longer throw an
  error if the series is shorter than the window size. Instead `rollapply()`
  is called which either returns an empty series (if `fill` is not specified)
  or a padded series (if `fill` is specified, e.g., to `NA`).

* `read.zoo()` now also handles `"tbl"` objects by coercing them to
  `"data.frame"` first. Also gained a `read = read.table` argument to simplify
  convenience interfaces `read.csv.zoo()` etc.

* The `fortify()` method gained a `names` argument so that the column names
  of the (melted) series can be modified.


## zoo 1.8-0 (2017-04-13) 

* `zoo()` and `zooreg()` gained a `calendar = getOption("zoo.calendar", TRUE)`
  argument so that, by default, `"yearqtr"` and `"yearmon"` are used as the time
  index for regular `"zoo"` series with frequency 4 and 12, respectively. With
  `calendar = FALSE` the behavior from previous versions with plain
  unclassed time indexes can be obtained. Set `options(zoo.calendar = FALSE)`
  to always retain the old behavior.

* `na.locf()` is made faster by using a `diff()`-based solution rather than
  `cumsum()` (suggested by Ruben Arslan). Also, the workhorse function
  `na.locf0()` underlying the default `na.locf()` method is now exported
  in the user interface as it is faster (but also supports less options).

* Better support of vector-valued `split` arguments in `read.zoo()`.

* Special-cased `rollmax(..., k = 1)` to return the input `coredata()`.

* `as.character()` for `"yearqtr"` objects did not process `NA` elements correctly.


## zoo 1.7-14 (2016-12-19) 

* Several convenience interfaces to `read.zoo()` have been added:
  `read.table.zoo()`, `read.csv.zoo()`, `read.csv2.zoo()`, `read.delim.zoo()`,
  `read.delim2.zoo()`. All of these first call the corresponding `read.*()`
  function to read the data from a text file (with the respective default
  settings) and subsequently call `read.zoo()` to turn the data into
  a `"zoo"` series.

* `na.approx.default` did not properly process the `maxgap` argument
  if `y` did not contain any `NA`s but `NA`s were only created implicitly
  by `xout` being different from `x` (pointed out by Stefan Metzger).
  A suitable fix has been added that merges `x` and `xout` first.

* `aggregate()` and `rollapply()` methods gained the argument `coredata = TRUE`
  which can control whether only the coredata is passed to each subset
  (previous behavior) or the full `"zoo"` series.

* The `its` package has been archived on CRAN and hence removed from
  the list of `zoo`'s suggested packages. However, various methods for
  objects of class `"its"` continue to be available in `zoo` in case someone
  still uses the legacy code.


## zoo 1.7-13 (2016-05-03) 

* The `window()` method erroneously dropped the dimension in case of
  1-column zoo series. Fixed now.

* Bug fix in `NA` handling of `rollmax()` pointed out by Cory Fletcher.
  
* A few `as.yearmon`/`as.yearqtr` methods were not registered in
  the NAMESPACE but are now.
  
* If there are less then two non-`NA`s in `na.approx()` then `approx()`
  cannot be applied. Instead of throwing an error (as up to version
  1.7-12) simply no `NA`s are replaced now.

* Bug fix for `lag(z, k = k, na.pad = TRUE)` which ignored `na.pad = TRUE`
  if `k` was a vector of lags.


## zoo 1.7-12 (2015-03-16) 

* `read.zoo()` with a data frame argument now defaults to `FUN = identity`
  if the `index.column` is not character or factor.  See `?read.zoo` for
  additional details.

* `read.zoo()` heuristic improved so that `"POSIXct"` rather than `"Date"` is
  detected in cases like: `read.zoo(text = "2010-01-01 12:05:03 88.1", index = 1:2)`.
  
* Added a `dim<-` method that checks whether the new dimension value
  appears to be ok and then calls the default.

* The `index<-` and `time<-` methods now enforce that the new time index
  is actually correctly ordered (suggested by Joshua Ulrich).
  
* The `fortify()` method now has a `col.names` argument that allows to set
  (some of) the column names of the resulting data frame.

* Various small bug fixes and enhancements.


## zoo 1.7-11 (2014-02-27) 

* Extended the license from "GPL-2" to "GPL-2 | GPL-3".

* Avoid duplications in `Suggests`/`Imports`/`Depends` in the package's
  `DESCRIPTION`. Use only `::` instead of `:::` to access certain functions
  from other namespaces.

* Added `as.list()` methods for `"yearmon"`/`"yearqtr"`.

* Added workaround in `Ops.zoo` if first argument is not a `"zoo"` series
  (prompted by Josh Ulrich).

* The `transform()` method for `"zoo"` series now does what the method for
  `"data.frame"` does (instead of just calling it) in order to get
  non-standard evaluation.


## zoo 1.7-10 (2013-06-14) 

* All methods that had previously been fully exported (`merge.zoo`,
  `MATCH.default`, ..., and many more) in order to be accessible to
  _all_ conceivable generics are now additionally registered as
  `S3method()`s for the standard generics. Exception: `as.Date.*` are
  still only fully exported.

* Added a new `mean()` method for `"zoo"` objects that simply does
  `mean(coredata(obj), ...)`.

* Added `yearmon_trans()`, `scale_x_yearmon()` and `scale_y_yearmon()` and
  `yearqtr_trans()`, `scale_x_yearqtr()` and `scale_y_yearqtr()` to `ggplot2` interface.

* Removed the use of the shape and linetype aesthetic by default
  in `autoplot()` method.

* Bug fix in `na.fill()` for univariate series based on 1-column matrices
  (provided by Josh Ulrich).

* Added `[[` methods for `"yearmon"` and `"yearqtr"`.

* Constructs like `time(obj)[ORDER(time(obj))]` are now split up into
  two steps in the package code. If `zoo` is only imported but not
  loaded, R can otherwise have problems correctly dispatching to
  the new generics `ORDER() and `MATCH()`.
  
* `as.Date()` now also works for `"yearqtr"`/`"yearmon"` that are all `NA`
  (especially needed for `format()` and `print()` method).


## zoo 1.7-9 (2012-11-04)

* Added `ggplot2` interface through `autoplot()` and `fortify()` methods.  Also
  support function `facet_free()`.


## zoo 1.7-8 (2012-10-06)

* Added `rollsum()`.

* Bugfix in `src/lag.c` for the case of `k > NROW`.


## zoo 1.7-7 (2012-02-11)

* Fixed bug where column names were dropped when merging with zero width
  `"zoo"` object.

* Fixed slow-down in `rollapply()` compared to versions up to 1.6-x.

* C code now correctly declares GPL-2 (or later) license in the
  `src/*.c` files.


## zoo 1.7-6 (2011-11-02)

* Removed dependency on `fCalendar` which was only for historical purposes.
  `vignette("zoo", package = "zoo")` outlines how `timeDate` instead of
  `fCalendar` can be used together with `zoo`.


## zoo 1.7-5 (2011-10-25)

* Fixed a bug in the `aggregate()` method which occured when using
  it with `"zoo"` objects that have a `"timeDate"` index.

* In `read.zoo()` `index.column` can refer to column names or numbers (previously
  only numbers)

* Fully export all `as.Date.*` methods so that they can be used more
  easily (i.e., without extra registration) with `as.Date()` generics from
  other packages (`base` in particular).


## zoo 1.7-4 (2011-08-22)

* Fixed bug in `rollapply()` and slowness in `rollmax()`.

* `read.zoo()` gains `text=` argument similar to that in new `read.table()`.


## zoo 1.7-3 (2011-08-11)

* Added `drop = TRUE` argument to `merge()` method. When set to `drop = FALSE`
  this allows to merge a zero-column series with a `"zoo"` vector to a
  one-column matrix. The latter is the default in the `cbind()` method.
  
* Enhanced the `$<-` method so that `z$a <- value` works even if `z` is a
  `"zoo"` series without data.

* Bug fix in C version of `lag()` method for `"xts"`.


## zoo 1.7-2 (2011-07-23)

* Modified `as.Date.numeric` is now in the `zoo` namespace rather than in
  the base namespace, own `as.Date()` generic to assure dispatch.
  For packages depending on `zoo`, this means that they may need to import
  it from `zoo` (or call `zoo:::as.Date.numeric` directly).

* Small bug fixes.


## zoo 1.7-1 (2011-07-18)

* Internal change to `roll*` routines to increase compatibility with classes
  built on top of `"zoo"` (especially `"xts"`).  This has no effect on `"zoo"`.

* `as.Date.numeric(x, origin = "0000-00-00")` is now also accepted being
  equivalent to `origin = as.Date("0000-01-01") - 1`. (for MATLAB dates)


## zoo 1.7-0 (2011-07-12)

* A `[<-` method was added and the behavior is now consistent with the
  `[` method.

* The `check.names` argument of the `merge()` method now defaults to `FALSE`.

* Added a new `as.zoo()` method for `"matrix"` objects in order to preserve
  column names. Also, a new `"data.frame"` method leverages the `"matrix"`
  method.

* In `rollapply()` `width` may be a list whose components are offsets. The `ascending`
  argument is no longer supported. Use `width`'s list form instead.

* In `rollapply()` new `partial` and `fill` arguments. The `na.pad` argument is deprecated.

* `rollapply()` with `FUN = mean` will no longer call `rollmean` if the data has
  any `NA` values.

* Added a `yax.flip` argument to the `plot()` method for `"zoo"` objects to be consistent
  with the `"ts"` method.

* New vignette "zoo-read" introducing `read.zoo()` including several new
  features (see also below).

* In `read.zoo()` if `FUN` is not specified but `FUN2` is specified then `FUN2`
  is applied in place of `FUN`. Previously `FUN2` was ignored in this case.
  Also `format` and `tz` arguments can be `NULL` in which case they are treated
  as missing.

* `read.zoo()` can accept a vector of filenames in which case each is read in
  and a single `"zoo"` object is returned formed by merging the individual ones.

* If `FUN` is omitted in `read.zoo()` and `index.column` specifies multiple columns
  then the columns are pasted together separated by spaces and processing
  continues as if there were one column.

* In `read.zoo()` `index.column = 0` is now valid in which case 1, 2, 3, ... is
  used for the index.

* `read.zoo()` can now read in a `"zoo"` object with an index but no data.

* In most situations in which a plain numeric vector for `index.column` in
  `read.zoo()` would give an error it now also tries using 
  `as.list(index.column)`. As a result its almost always possible to use a 
  plain numeric `index.column` rather than a list.

* A new `src/` directory containing C code backported from the `xts` package is now
  included. This is the start of the process to align some of the faster C
  internals of `xts` with `zoo`.  `xts` is now using the `LinkingTo` option to
  call certain code in the `zoo` package (previously in `xts/src`). Subsequent
  `zoo` releases will begin to use the new C, which in the current release are
  not yet available.

* New `roll*r()` wrappers which default to `align = "right"`.

* New `na.fill()` function.

* New `na.StructTS()` function for seasonal `NA` interpolation using R's `StructTS()`.

* `rev()` method for `"zooreg"` objects added.

* `transform()` method added.

* Added `frequency` argument to `as.zooreg()` generic and associated methods.


## zoo 1.6-5 (2011-04-08)

* Fixed a technical error in the `Sweave` commands of "zoo-faq.Rnw" which
  throws an error in the current R-devel (2.14.0 to be).


## zoo 1.6-4 (2010-07-09)

* Names were automatically added to indexes in some cases hugely increasing
  the size of the index. This is no longer done.

* `diff()` can have a negative `lag` argument (for forward diffs). Previously
  only positive was allowed.

* `na.locf()` now calls `na.approx` with `method = "constant"` unless `fromLast = TRUE`.
  In particular, arguments `x` and `xout` can be passed to `na.locf()` which is used
  in alignment applications.

* `merge()` has `check.names` argument. If `check.names = TRUE` (the default)
  then column names will be valid syntactic names.

* `read.zoo()`'s `index.column` argument can be a list such that `FUN` is called via
  `do.call(FUN, L)` where `i` in list `L` is replaced with `x[,i]`.

* Added workaround in `Ops.zoo` to circumvent rare bug in base R.

* Added a `median()` method.

* Added a `quantile()` method.


## zoo 1.6-3 (2010-04-23)

* New generic function `xblocks()` for plotting contiguous blocks along
  the x-axis (intended for base graphics). A default method along with
  `"zoo"` and `"ts"` methods is provided.

* New function `na.aggregate()`.

* New `maxgap` argument for `na.*()` methods. Further refinements for
  many `na.*()` methods, especially `na.approx()` and `na.spline()`.

* `xyplot()` method rewritten; is now a wrapper around the `xyplot()`
  method for `"ts"` objects in the `lattice` package (`lattice:::xyplot.ts`).
  Accompanying infrastructure (`llines()`, `lpoints()`, `ltext()`, etc.),
  help page, and examples have been substantially revised and expanded.

* New question \#13 added to "zoo-faq" vignette.

* `FUN` argument of aggregate() method now defaults to sum for consistency with
  `aggregate()` method for `"ts"`. Previously it had no default.

* new MATCH() method for "times" class.


## zoo 1.6-2 (2009-11-23)

* Minor improvement in "zoo-faq" for new R-devel.


## zoo 1.6-1 (2009-11-19)

* `xtfrm()` methods for `"yearmon"`/`"yearqtr"` added.

* Enhanced documentation to emphasize that there is typically no
  need for an `ORDER()` method if there is an appropriate `xtfrm()`
  method.

* Bug fix in group generic functions for `"yearmon"`/`"yearqtr"` with
  non-numeric return values.


## zoo 1.6-0 (2009-11-18)

* `tis` graphics support.

* Workaround to eliminate `as.Date.numeric` warning when `zoo` loads.

* `read.zoo()` now optionally accepts a data frame for the `file` argument.

* Better error message in `read.zoo()` if `NA`s found in index.

* `read.zoo()` now accepts a `split` argument that allows reading in of datasets
  in long format (where long is as defined in R's `reshape()` command).

* Methods for `cbind()`, `merge()`, `rbind()`, and `c()` now ignore `NULL` arguments.

* Fixed unique() methods for `"yearmon"` and `"yearqtr"` objects.

* Fixed a bug in group generic functions and `-() generic for
  `"yearmon"` and `"yearqtr"` objects.

* `plot()` method now accepts vector `lwd`.

* New `rev()` method for `"zoo"` objects.


## zoo 1.5-8 (2009-07-22)

* Bug fixes in `unique()` methods for `"yearmon"`/`"yearqtr"`.


## zoo 1.5-7 (2009-07-22)

* Added `"zoo"` methods for `xtfrm()`, `split()`, and `subset()`.

* `as.zoo()` methods for `"xts"`, `"tis"`, `"fts"`, and `"mcmc"` objects
  (the other direction being handled by the respective packages).

* Improvements in `"yearmon"` and `"yearqtr"`:
  - Added `range()`, `unique()`, and `is.numeric()` methods.
  - `is.numeric()` methods return `FALSE`.
  - `as.yearmon()` now also accepts `"%b %Y"` as a default format so
    `as.yearmon(as.character(ym))` where `ym` is `"yearmon"` now works.

* Fixed `strip` argument in `xyplot()` method (thanks to Christian Gunning).

* `lag()` method for `"zooreg"` objects now has an `na.pad` argument,
  like the corresponding `"zoo"` method. Additinal bug fix.

* `head()` and `tail()` methods now allow second argument to be negative

* Added `is.na` argument to `na.trim()`.

* `ORDER()` methods for `"chron"`, `"dates"`, and `"times"` objects added.
  Without these order has become very slow when using older versions of
  the `chron` package (although the most recent version of `chron` has no
  slowdown even without this fix as it implements `xtfrm()`).


## zoo 1.5-6 (2009-05-22)

* Changed dependency in coercion functions and vignettes from
  `fCalendar`/`fSeries` to `timeDate`/`timeSeries`.


## zoo 1.5-5 (2009-02-05)

* Documentation enhancements for new Rd parser.


## zoo 1.5-4 (2008-07-09)

* Improvments in `read.zoo()`.

* Updates in `zoo` FAQ.

* Small bug fix in `plot()` method (`ylim` handling).

* Small bug fixes in `axis.yearmon` and `axis.yearqtr`.

* Small bug fixes in `aggregate.zoo`.

* Extended examples for `plot.zoo`, `xyplot.zoo`, `aggregate.zoo`.

* Interactive plots/demos using the packages `TeachingDemos`
  (`plot.zoo`) and `playwith` (`xyplot.zoo`).

* `mean.yearmon` and `Sys.yearmon` are defined. Similarly for `yearqtr`.

* `min()`, `max()` and `range()` methods for `"yearmon"` are defined via `Summary()`.


## zoo 1.5-3 (2008-04-22)

* Export `as.Date.numeric` so that it does not get shadowed
  by the new `as.Date.numeric` in `stats` (from 2.7.0 on).
  Both functions are almost identical, the version in `zoo`
  just sets the default `origin = "1970-01-01"`.

* Added some glue for communication between `"zoo"` and "timeSeries":
  new `MATCH()` and `ORDER()` methods for `"timeDate"`, new `as.zoo()`
  method for `"timeSeries"` objects (the inverse will be added to
  `fSeries`).
  
* Improved the "zoo" vignette, updating/correcting the information
  about `timeDate` and `timeSeries`.


## zoo 1.5-2 (2008-04-05)

* `read.zoo()` has new aggregate function argument to aggregate duplicate times.

* New `with()` method added.

* FAQ updated.


## zoo 1.5-0 (2008-03-14)

* New `$` and `$<-` methods.

* `read.zoo()` now passes `format` and `tz` to `FUN` if they are specified. If
  `FUN` is not specified but `tz` and `format` specified then `"POSIXct"` translation 
  is done with respect to the specified format string.

* `months.yearmon`, `quarters.yearmon`, `cycle.yearmon` methods added.

* `months.yearqtr`, `quarters.yearqtr`, `cycle.yearqtr` methods added.

* `as.yearqtr.character` now accepts `%q` and default formats are
  `"%Y Q%q"`, `"%Y q%q"` and `"%Y-%q"`.

* `yearqtr.factor` and `yearmon.factor` added.

* `as.yearmon.yearqtr` now has `frac` argument consistent with `as.Date.yearmon`.

* `format.yearqtr` and `as.character.yearqtr` performance speedup for the
  common case of default format.


## zoo 1.4-2 (2008-01-28)

* The processing of regular series in `aggregate.zoo`
  was changed. By default, a regular series is only
  created if the original series was regular. Otherwise,
  a regular series is only created if indicated (by
  setting argument `regular = TRUE` or by supplying a
  specific frequency).

* `zoo` FAQ vignette added.

* `axis.yearqtr` and `axis.yearmon` improved. They no longer
  use `axis.Date`.
  
* `as.yearmon.character` accepts `"%Y-%m"` default format (as well 
  as prior default of `"%Y-%m-%d"`).

* New `summary()` methods for `yearmon`/`yearqtr` objects which
  simply report the summary of the underlying numeric vector.

* `na.locf.default` now uses `fromLast=` (consistent with the R 
  duplicated function). It is similar to the `rev=` argument
  (which will be deprecated).

* Comparisons between `yearmon` variables and character variables
  that can be coerced to `yearmon` via `as.yearmon` now work. This 
  also lets character variables be used in `start=` and `end=`
  arguments of `window()` method for `zoo` series with a yearmon index
  (since `window.zoo` already allows such usage for any index class 
  that can be compared to character). Similarly for `yearqtr`.


## zoo 1.4-1 (2007-12-14)

* Replaced `by()` calls by `tapply()` due to fixes in R
  2.6.1-patched that caused errors in `plot.zoo`.
  

## zoo 1.4-0 (2007-10-12)

* Added a `write.zoo()` function that writes zoo series to
  a text file via `write.table()`. The index is included in
  the first column so that it can be easily read again
  using `read.zoo()`.

* `read.zoo()` has a new argument `index.column` (default: `1`)
  which can be used to specify in which column of a data
  file the index/time is stored.

* `rapply()` was removed from `zoo` - after being deprecated
  since zoo 1.2-0 (and R 2.4.0) and replaced by `rollapply()`.

* Improved `str()` method for `"zoo"` and `"zooreg"` series, now
  gives explicit information about class, start and end,
  as well as data, index (and frequency).
  
* `zoo()` now has a default first argument like `ts()` has.
  By default, `zoo()` is now `zoo(NULL)` and returns an empty
  series. This differs somewhat from the default `ts()`
  and `zooreg()` that return `ts(NA)` and `zooreg(NA)`, respectively.

* Added `...` arguments to the `as.POSIXct()` and `as.POSIXlt()`
  methods.
  
* Implementation of `as.Date.numeric()` has been modified:
  being backward compatible with the old `zoo:::as.Date.numeric`.
  It now matches the new (2.7.0 to be) `base:::as.Date.numeric`,
  but also sets a default `"origin"` argument (1970-01-01).

* `plot.type` argument of `plot()` method now chooses its default 
  value via the screen argument.


## zoo 1.3-2 (2007-06-26)

* `merge.zoo()` has been improved so that unnecessary checking of
  frequencies is avoided for `"zoo"` objects.


## zoo 1.3-1 (2007-05-14)

* `as.yearmon()` can take format arguments that do not involve day,
  e.g., `as.yearmon("2007-01", "%Y-%m")`.

* Added `as.data.frame()` methods for `"yearmon"` and `"yearqtr"`
  objects.
  

## zoo 1.3-0 (2007-04-20)

* Checking for non-unique index entries has been improved:
  `zoo()` throws a warning if a series with non-unique index entries
  is created. `merge()` gives a more useful error message.

* Inconsistencies when checking for regularity of series have been
  resolved: `is.regular()`, `frequency()`, `frequency<-()` etc. all behave
  consistently now.

* Custom panel functions in `plot()` method can now refer to 
  `parent.frame()$panel.number` to determine the current panel. Also
  added an example of this to `?plot.zoo`.

* `read.zoo()` will behave consistently when the file contains no data.

* Column names (if any) are checked now in `rbind.zoo()`, producing a
  similar behaviour as in `rbind.data.frame()`.


## zoo 1.2-2 (2006-12-18)

* The order of arguments (`...` now come after `FUN`) in rollapply has
  been changed to allow unnamed additional arguments to be specified
  for `FUN`. Previously they had to be named.

* Bug in `as.ts.zooreg` fixed.


## zoo 1.2-1 (2006-09-19)

* One of the examples on `?xyplot.zoo` does not run with the
  R 2.4.0 `lattice`. It is currently placed in `\dontrun{}`
  (see comments in `?xyplot.zoo`).


## zoo 1.2-0 (2006-08-18)

* `rapply()` was re-named to `rollapply()` because from R 2.4.0 on,
  base R provides a function `rapply()` for recursive (not rolling)
  application of functions, which was already described in the Green
  Book. `zoo::rapply()` still exists for backward compatibility, however,
  it is flagged as deprecated and now dispatches to `rollapply()` methods.

* Added methods for `xyplot()` from package `lattice` for classes `"zoo"`,
  `"ts"`, `"its"`. These functions are still under development and the
  interface and functionality might be modified/extended in future
  releases.

* New function `make.par.list()` (which was previously a local function
  `parm()` in `zoo`'s `plot()` method) for processing named argument lists.
  Useful in plotting routines like the `plot()` and `xyplot()` methods,
  see `?make.par.list`.

* Subscripting by a `"zoo"` object whose data is logical is now defined, e.g.,

  `z <- zoo(1:10); z[z > 3]`

  However, assignment such as
  
  `z[z > 3] <- 2 * z[z > 3]`
  
  does not work.

* Fixed a bug in `zoo()` when ordered factors are supplied (`"ordered"`
  class was dropped)

* Fixed bug in subscripting for `drop = TRUE` and length of result is 1

* Fixed bug in `na.trim(x, "right")`.

* `all=` argument of `merge()` method is coerced to logical so one can do this:
  `merge(zoo(2:4, 2:4), zoo(1:3), all = 0:1)`.


## zoo 1.1-1 (2006-07-03)

* Added `na.spline()` generic and default method for replacing
  `NA`s via cubic spline interpolation
  
* `rbind()` method now exported explicitly again.


## zoo 1.1-0 (2006-06-06)

* Added a NAMESPACE, many S3 methods are not exported explicitly
  anymore.

* New argument `regular` in `read.zoo()` which is set to `FALSE`. Only if
  set to `TRUE` the series read is coerced to `"zooreg"` (if possible), 
  which was the previous default behavior.
  
* `suppressWarnings()` was added in `is.regular()` and `frequency()` functions
  which `try()` to convert indexes to numerics which might lead to errors
  or warnings and `NA`s (e.g., for characters), both should be fully suppressed
  now.


## zoo 1.0-7 (2006-05-22)

* Added a `...` argument to `as.data.frame()` method for
  complying with R 2.4.0.

* Improved handling of `by` argument in `aggregate()` method.


## zoo 1.0-6 (2006-04-07)

* `?zoo` now explicitly points out that the index of 
  `"zoo"` objects should have unique observations (aka 
  time stamps).
  
* The `summary()` was fixed to work also with duplicated
  indexes.
  
* `scale()` method added.

* `lines(x, y, ...)` now supported.

* `points()` method added.

* `-.yearmon` and `-.yearqtr` added.

* `axis.yearmon` and `axis.yearqtr` added.

* `Axis.yearmon` and `Axis.yearqtr` added.

* `na.trim` generic and default method added.

## zoo 1.0-5 (2006-02-06)

* Added a `y` argument to `plot()` method that allows to
  generate scatter plots of univariate `"zoo"` series
  (just as `plot.ts(x, y = NULL, ...)` does).
  
* Fixed the usage of `%in%` in the `window()` methods,
  now call `MATCH()` (rather than `match()`) directly.


## zoo 1.0-4 (2006-01-19)

* Added a `rev` argument to `na.locf()` which allows to
  eliminate `NA`s by NOCB (next observation carried backward).


## zoo 1.0-3 (2005-12-01)

* Added a `barplot.zoo()` method.

* Fixed `frequency.zoo()` which returned `NA` instead of `NULL`
  for indexes of class `"character"`.

* Added an example to "zoo-quickref" for querying daily
  exchange rates from oanda.com via `get.hist.quote()`.
  This contains a worked example how to omit weekends
  from daily series.
  
* Added an `na.pad` argument to `diff.zoo()` method.


## zoo 1.0-2 (2005-09-08)

* Small enhancements of `plot.zoo()`.

* Bug fixes in `na.locf.default()`, `as.data.frame.zoo()`,
  `lag.zoo()`, `lag.zooref()`.
  

## zoo 1.0-1 (2005-06-22)

* Convenience function `read.zoo()` for reading `"zoo"` series directly
  from plain text files.
  
* New vignette "zoo-quickref" with a quick reference particularly
  aimed at (daily) financial series (contributed by Ajay Shah).

* `plot()` method now has `screens=`, `widths=` and `heights=` arguments for controlling
  which series are plotted in which graphs and widths and heights of graphs.
  The `ylim=` argument has been enhanced.

* Argument `k=` can be vector in `lag()` method (suggested by Roger Koenker).

* `na.locf.default` bug fixed.


## zoo 1.0-0 (2005-05-25)

* This release accompanies the publication in the _Journal of Statistical
  Software_ ("zoo: S3 Infrastructure for Regular and Irregular Time Series",
  JSS, **14**(6), 1-27) that essentially corresponds to the vignette contained
  in the package. Please use this paper to cite `zoo` in publications.

* aggregate.zoo allows a function as the argument for computing
  the aggregation groups. The return value is coerced to `"zooreg"`
  if it is.regular.

* extended NA handling for lists and data.frames: na.locf.data.frame,
  na.locf.list, na.contiguous.data.frame, na.contiguous.list
  
* pair notation, e.g. c(1985,2), for `"zooreg"` series is now also
  allowed in window.zoo and window<-.zoo.

* fixed bug in rapply: result was transposed when by.column = FALSE and
  a non-scalar function FUN was used.


## zoo 0.9-9 (2005-04-27)

* Regular `"zoo"` series: objects of class `"zooreg"` (inheriting from `"zoo"`)
  can be used to store strictly regular series (similar to `"ts"` objects)
  or series with an underlying regularity (as before but with observations
  omitted). They have a frequency attribute that can be used for conversion
  between `"zoo"` and `"ts"`. The function `is.regular()` can be used for 
  checking the regularity of a series.

* Improved `merge()` method: `merge.zoo` now accepts non-zoo arguments
  (other than first) if all non-zoo args have the same `NROW` value
  as the first argument (or are scalar). In that case the non-zoo
  args are given the index of the first series. Scalars are added
  for the full index of the merged series.

* `merge()` method can now optionally return a `"data.frame"` that contains
  the numeric columns as `"zoo"` series and the `"zoo"` objects created
  from factors converted back to `"factor"`.
  
* `[` method now allows indexing using observations from the index scale
  (and not only observation numbers).

* `rapply`, `rollmean`, `rollmax`, `rollmedian` to perform rolling analyses.

* Extended functionality to `plot()` method `type` argument

* When `plot()` method is used with one series `list(...)` can be omitted from 
  various plotting parameter arguments

* `print()` method documentation fix for R 2.1.0

* `yearmon` and `yearqtr` datetime classes.

* `head.ts`, `tail.ts`.

* `c.zoo`, `range.zoo`.

* `coredata.default`, `coredata.ts`.


## zoo 0.9-1 (2004-12-21)

* New generic functions `ORDER()` and `MATCH()` (with
  `order()` and `match()` as the default) so that `zoo()`
  can handle  arbitrary index/time classes when
  suitable methods for the generic function `c()`,
  `length()`, `order()`, `match()` and subsetting `[`, are
  supplied.

* Improved printing of `"zoo"` objects and added
  a `summary()` method.
  
* Extended coercion functionality to and from
  `"zoo"` objects. `"its"` objects can be coerced to
  `"zoo"` and vice versa. `"zoo"` objects can be 
  coerced to vector, matrix, data.frame or list.
  
* Added functionality to extract/assign to
  the `coredata()` of a `"zoo"` object.
  
* Added/improved functionality to extract/assign
  to the `window()` of a `"zoo"` object.
  
* Added/improved functionality to extract/assign
  to the `index()` or `time()` of a `"zoo"` object.
  
* Added `lag()`, `diff()`, `start()`, `end()`, `head()`,
  `tail()` methods.

* Improved `plot.zoo()` by more flexible expansion
  of plotting parameters such as `col`, `lty`, and `pch`.

* Added a `cbind()` method for `"zoo"` objects (almost
  synonymous with `merge()`)

* `NA` handling for `"zoo"` objects via `na.omit()`,
  `na.contiguous()`, `na.approx()` and `na.locf()`.

* `na.locf()` generic function with default method (suitable
  for `"zoo"` objects) which implements Last Observation
  Carried Forward.

* `na.approx()` generic function with default method (suitable
  for `"zoo"` objects) which implements elimination of `NA`s
  by interpolation.

* Added mathematical methods: group generic functions
  for `"zoo"` objects, `t()`, `cumsum()`, `cumprod()`, `cummin()`,
  and `cummax()`.

* Added `model.frame.AsIs` and `model.frame()` method to support
  regression based on `"zoo"` objects, in particular with `lm()`
  (but also many other regression functions).

* Zero length vector zoo objects may have non-zero index vectors 
  intended to be used in merge to extend `"zoo"` objects. `zoo()`
  changed to enable the creation of such objects by omitting
  first argument.

* Added a vignette explaining the new features


## zoo 0.2-0 (2004-08-12)

* `zoo()` now has defaults for both arguments `x` and `order.by`,
  which mimic the default behaviour of `ts()`.

* Added new `aggregate()` method for computing summary
  statistics of `"zoo"` objects along a coarser index grid.

* Improved `merge()` method in three directions:

  1. Handling of `"zoo"` objects with zero columns,
  2. Naming of columns in the merged `"zoo"` object
     which behaves more like `merge.data.frame()`;
     a corresponding suffixes argument has also been added,
  3. Introced a fill argument which allows to fill gaps
     by another value than `NA`.
     
* Improved documentation with extended examples.


## zoo 0.1-4 (2004-05-27)

* Improvements in `merge()` method.


## zoo 0.1-3 (2004-04-14)

* Fix package `INDEX`.


## zoo 0.1-2 (2004-04-08)

* Gabor Grothendieck joins as co-author of the package.

* Add methods `merge()` and `rbind()` which allow aligning and
  joining/intersecting `"zoo"` series with different time indexes.
  
* Add methods for `window()` and `str()`.


## zoo 0.1-1 (2004-02-20)

* First CRAN release of `zoo` package for "Z's Ordered Observations",
  an S3 class which can be used for representing numeric data (vector/matrix)
  with an ordered index. This could include (irregular) time series
  data but also data ordered by other variables (other than date/time).
  
* Functionality includes printing, plotting, subsetting, and coercion
  from classes `"ts"` and `"irts"`.

* The package serves as infrastructure for `strucchange`, especially
  when visualizing parameter instability tests across some variable other
  than time.
