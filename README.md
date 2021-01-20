
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unstruwwel <img src="man/figures/logo.png" align="right" width="120" />

[![Lifecycle
badge](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4451796.svg)](https://doi.org/10.5281/zenodo.4451796)
[![CRAN
badge](http://www.r-pkg.org/badges/version/unstruwwel)](https://cran.r-project.org/package=unstruwwel)
[![Travis CI Build
status](https://travis-ci.org/stefanieschneider/unstruwwel.svg?branch=master)](https://travis-ci.org/stefanieschneider/unstruwwel)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/stefanieschneider/unstruwwel?branch=master&svg=true)](https://ci.appveyor.com/project/stefanieschneider/unstruwwel)
[![Coverage
status](https://codecov.io/github/stefanieschneider/unstruwwel/coverage.svg?branch=master)](https://codecov.io/github/stefanieschneider/unstruwwel?branch=master)

## Overview

This R package provides means to detect and parse historic dates, e.g.,
to ISO 8601:2-2019. It automatically converts language-specific verbal
information, e.g., “circa 1st half of the 19th century,” into its
standardized numerical counterparts, e.g., “1801-01-01\~/1850-12-31\~.”
The package follows the recommendations of the MIDAS (Marburger
Informations-, Dokumentations- und Administrations-System), see, e.g.,
<https://doi.org/10.11588/artdok.00003770>. It internally uses
[lubridate](https://github.com/tidyverse/lubridate). The name of the
package is inspired by Heinrich Hoffmann’s rhymed story
“[Struwwelpeter](http://www.gutenberg.org/files/12116/12116-h/12116-h.htm#Shock-headed_Peter)”,
which goes as follows:

> Just look at him\! there he stands, with his nasty hair and hands.
> See\! his nails are never cut; they are grimed as black as soot; and
> the sloven, I declare, never once has combed his hair; anything to me
> is sweeter than to see Shock-headed Peter.

For the German-language original text, see the online digital library
[Wikisource](https://de.wikisource.org/wiki/Der_Struwwelpeter/Struwwelpeter).

## Installation

You can install the released version of unstruwwel from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("unstruwwel")
```

To install the development version from
[GitHub](https://github.com/stefanieschneider/unstruwwel) use:

``` r
# install.packages("devtools")
devtools::install_github("stefanieschneider/unstruwwel")
```

## Usage

The unstruwwel package contains only one function, `unstruwwel()`, that
does all the *magic* language-specific standardization. `unstruwwel()`
returns a named list, where each element is the result of applying the
function to the corresponding element in the input vector.

### English-language examples

``` r
dates <- c(
  "5th century b.c.", "unknown", "late 16th century", "mid-12th century",
  "mid-1880s", "June 1963", "August 11, 1958", "ca. 1920", "before 1856"
)

# returns valid ISO 8601:2-2019 dates
unlist(unstruwwel(dates, "en", scheme = "iso-format"), use.names = FALSE)
#> [1] "-0500-12-31/-0401-01-01" NA                        "1586-01-01/1600-12-31"  
#> [4] "1146-01-01/1155-12-31"   "1884-01-01/1885-12-31"   "1963-06-01/1963-06-30"  
#> [7] "1958-08-11/1958-08-11"   "1920-01-01~/1920-12-31~" "..1855-12-31"

# returns a numerical interval of length 2 
unstruwwel(dates, language = "en", scheme = "time-span") %>%
  tibble::as_tibble() %>% dplyr::mutate(id = dplyr::row_number()) %>% 
  tidyr::gather(key = id) %>% tidyr::unnest_wider(value) %>% 
  dplyr::rename_all(dplyr::funs(c("text", "start", "end")))
#> # A tibble: 9 x 3
#>   text              start   end
#>   <chr>             <dbl> <dbl>
#> 1 5th century b.c.   -500  -401
#> 2 unknown              NA    NA
#> 3 late 16th century  1586  1600
#> 4 mid-12th century   1146  1155
#> 5 mid-1880s          1884  1885
#> 6 June 1963          1963  1963
#> 7 August 11, 1958    1958  1958
#> 8 ca. 1920           1920  1920
#> 9 before 1856        -Inf  1855
```

### German-language examples

``` r
dates <- c(
  "letztes Drittel 15. und 1. Hälfte 16. Jahrhundert", "undatiert", "1460?",
  "wohl nach 1923", "spätestens 1750er Jahre", "1897 (Guss vmtl. vor 1906)"
)

# returns valid ISO 8601:2-2019 dates
unlist(unstruwwel(dates, "de", scheme = "iso-format"), use.names = FALSE)
#> [1] "1467-01-01/1550-12-31"   NA                        "1460-01-01~/1460-12-31~"
#> [4] "1924-01-01?.."           "..1749-12-31"            "..1905-12-31?"

# returns a numerical interval of length 2 
unstruwwel(dates, language = "de", scheme = "time-span") %>%
  tibble::as_tibble() %>% dplyr::mutate(id = dplyr::row_number()) %>% 
  tidyr::gather(key = id) %>% tidyr::unnest_wider(value) %>% 
  dplyr::rename_all(dplyr::funs(c("text", "start", "end")))
#> # A tibble: 6 x 3
#>   text                                              start   end
#>   <chr>                                             <dbl> <dbl>
#> 1 letztes Drittel 15. und 1. Hälfte 16. Jahrhundert  1467  1550
#> 2 undatiert                                            NA    NA
#> 3 1460?                                              1460  1460
#> 4 wohl nach 1923                                     1924   Inf
#> 5 spätestens 1750er Jahre                            -Inf  1749
#> 6 1897 (Guss vmtl. vor 1906)                         -Inf  1905
```

## Contributing

Please report issues, feature requests, and questions to the [GitHub
issue tracker](https://github.com/stefanieschneider/unstruwwel/issues).
We have a [Contributor Code of
Conduct](https://github.com/stefanieschneider/unstruwwel/blob/master/CODE_OF_CONDUCT.md).
By participating in unstruwwel you agree to abide by its terms.
