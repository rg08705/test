# ATRIDMtools

## Installation

### Use install_github from devtools
+ generate github access token <https://help.github.com/articles/creating-an-access-token-for-command-line-use/>.
+ enable organization SSO after creating key.
+ put `Sys.setenv(GITHUB_PAT="copy your github access token here")` to add to .Rprofile.
+ `devtools::install_github('atrihub/ATRIDMtools')`

### Download, build, and install from github folder
+ clone or download the whole package.
+ set working directory to package `setwd("github directory path")`.
+ `devtools::build()`
+ `devtools::install()`

## Updating package functions
+ set working directory to package `setwd("github directory path")`
+ update documentation files, `roxygen2::roxygenise()`
+ push commit and update package `devtools::build()`, `devtools::install()`