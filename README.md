# org.eclipse.daanse.r
Repository for R related code

## Helper

### NAMESPACE gen
setwd("~/git/daanse/org.eclipse.daanse.r")
roxygen2::roxygenise()

### run/call function
install.packages(".", repos = NULL, type = "source")
library(eclipse.daanse.xmla)
hello("Eclipse")

or

library(pkgload)
pkgload::load_all()
setwd("~/git/daanse/org.eclipse.daanse.r")
roxygen2::roxygenise()
discover_catalogs(
  "http://foo.bar.buzz:8080/xmla",
  "user",
  "secret"
)


### Run test
setwd("~/git/daanse/org.eclipse.daanse.r")
testthat::test_dir("tests/testthat")


### Dependency management
install.packages("renv")
renv::init()

### Depencency snapshot vor commit
renv::activate()
renv::status()
renv::snapshot()