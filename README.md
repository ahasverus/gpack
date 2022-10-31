
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpack <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/ahasverus/gpack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahasverus/gpack/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/ahasverus/gpack/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ahasverus/gpack/actions/workflows/pkgdown.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/gpack)](https://CRAN.R-project.org/package=gpack)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The goal of the R package `gpack` is to provide tools to web scraping
Google Services (Scholar, Pictures, Trends, Search). As Google does not
provide any API and does not allow web scraping, user public IP address
can be banned. This package relies on the software OpenVPN to
periodically change the IP address and the user-agent (i.e. the
technical information about your system).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ahasverus/gpack")
```

Then you can attach the package `gpack`:

``` r
library("gpack")
```

## System requirements

Before using the package `gpack` you must follow these instructions:

### Operating system

The package `gpack` has been developed **only for Unix platforms**
(macOS and GNU/Linux). If you are on Windows, you can use Docker to
start a GNU/Linux container.

**Important:** the package `gpack` must be run **outside RStudio**
(e.g. under a terminal).

### OpenVPN

The package `gpack` uses [**OpenVPN**](https://openvpn.net/). This
software is a Virtual Private Network (VPN) system. It creates secure
connection to VPN server. To install this software please follows these
[**instructions**](https://gist.github.com/ahasverus/41f8a99583149534cac08e7b8f13c51b).

You also need to store your Unix user password (`openvpn` requires super
user rights to be controlled): Under R, run the following command:
`usethis::edit_r_environ()`. Add the following line:
`UNIX_PASSWD='xxx99_999xXxx'`

### Docker engine

The software [**Docker**](https://www.docker.com/) must be installed and
running. The technology [Selenium](https://www.selenium.dev/) will be
run inside a Docker container.

### Selenium image

The Docker image
[`selenium/standalone-firefox`](https://hub.docker.com/r/selenium/standalone-firefox)
must be installed. This image contains the Selenium technology running a
Firefox browser.

## Overview

The package `gpack` provides two main function:

- `check_system()`: must be run first to change the integrity of the
  system
- `scrap_gscholar()`: get references metadata from Google Scholar

## Citation

Please cite this package as:

> Casajus N (2022) gpack: An R package to web scrap Google Services
> (Scholar, Pictures, Trends, Search). R package version 0.0.1.

## Code of Conduct

Please note that the `gpack` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
