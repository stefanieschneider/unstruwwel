
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unstruwwel <img src="man/figures/logo.png" align="right" width="120" />

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis CI Build
status](https://travis-ci.org/stefanieschneider/unstruwwel.svg?branch=master)](https://travis-ci.org/stefanieschneider/unstruwwel)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/stefanieschneider/unstruwwel?branch=master&svg=true)](https://ci.appveyor.com/project/stefanieschneider/unstruwwel)
[![Coverage
status](http://codecov.io/github/stefanieschneider/unstruwwel/coverage.svg?branch=master)](http://codecov.io/github/stefanieschneider/unstruwwel?branch=master)

## Overview

This R package provides means to detect and parse historic dates, e.g.,
to ISO 8601:2-2019. It automatically converts language-specific verbal
information, e.g., “1st half of the 19th century,” into its standardized
numerical counterparts, e.g., “1801/1850.” The package follows the
recommendations of the MIDAS (Marburger Informations-, Dokumentations-
und Administrations-System), see, e.g.,
<https://doi.org/10.11588/artdok.00003770>. The name of the package is
inspired by Heinrich Hoffmann’s rhymed story
“[Struwwelpeter](http://www.gutenberg.org/files/12116/12116-h/12116-h.htm#Shock-headed_Peter)”,
which goes as follows:

> Just look at him\! there he stands, with his nasty hair and hands.
> See\! his nails are never cut; they are grimed as black as soot; and
> the sloven, I declare, never once has combed his hair; anything to me
> is sweeter than to see Shock-headed Peter.

For the German-language original text, see the online digital library
[Wikisource](https://de.wikisource.org/wiki/Der_Struwwelpeter/Struwwelpeter).

## Installation

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

``` r
dates <- c(
  "zwischen letztem Drittel 15. und 1. Hälfte 16. Jahrhundert",
  "wohl nach 1923", "undatiert", "spätestens 1750er Jahre"
)

# returns valid ISO 8601:2-2019 dates
unlist(unstruwwel(dates, language = "de", scheme = "iso 8601"))
#> zwischen letztem Drittel 15. und 1. Hälfte 16. Jahrhundert 
#>                                                "1467/1550" 
#>                                             wohl nach 1923 
#>                                                  "1923?.." 
#>                                                  undatiert 
#>                                                         NA 
#>                                    spätestens 1750er Jahre 
#>                                                   "..1759"

# returns a numerical interval of length 2 
unstruwwel(dates, language = "de", scheme = "interval") %>%
  tibble::as_tibble() %>% dplyr::mutate(id = row_number()) %>% 
  tidyr::gather(key = id) %>% tidyr::unnest_wider(value) %>% 
  dplyr::rename_all(dplyr::funs(c("text", "start", "end")))
#> # A tibble: 4 x 3
#>   text                                                       start   end
#>   <chr>                                                      <dbl> <dbl>
#> 1 zwischen letztem Drittel 15. und 1. Hälfte 16. Jahrhundert  1467  1550
#> 2 wohl nach 1923                                              1923   Inf
#> 3 undatiert                                                     NA    NA
#> 4 spätestens 1750er Jahre                                     -Inf  1759
```

## Contributing

Please report issues, feature requests, and questions to the [GitHub
issue tracker](https://github.com/stefanieschneider/unstruwwel/issues).
We have a [Contributor Code of
Conduct](https://github.com/stefanieschneider/unstruwwel/blob/master/CODE_OF_CONDUCT.md).
By participating in unstruwwel you agree to abide by its terms.
