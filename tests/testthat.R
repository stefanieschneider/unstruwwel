Sys.setenv("R_TESTS" = "")

library(testthat)
library(unstruwwel)

test_check("unstruwwel")
