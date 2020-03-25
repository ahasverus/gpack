gpack <img src="man/figures/gpack.png" height="120" align="right"/>
=========================================================

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/ahasverus/gpack.svg?token=yHzmKJ7Fz8oZEQsjxUY1&branch=master)](https://travis-ci.com/ahasverus/gpack)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

This package provides tools for webscraping Google Services (Scholar, Pictures, Trends, Search) using OpenVPN to get around IP bans.



Installation
--------

As this is a private repository, you can use the following approach on a terminal:

```shell
$ cd ~/Desktop
$ git clone https://github.com/ahasverus/gpack.git
$ cd gpack

$ R
```

And in R:

```R
R> devtools::install()
R> q("no")
```

You can remove the git repository:

```shell
$ rm -rf ~/Desktop/gpack
```



Important
--------

If you want to use this package with OpenVPN, please follow instructions available [here](https://gist.github.com/ahasverus/41f8a99583149534cac08e7b8f13c51b).

You will also need to store your user password (`openvpn` requires super user rights):

```shell
$ nano ~/.Renviron
```

Add the following line (after changing your password):

```shell
UNIX_PASSWD='xxx99_999xXxx'
```

Press `CTRL+X`, then `Y` and finally `Enter` to exit and save changes.

Finally, we will protect this file from other users on the computer system (only the owner of this file can read it):

```shell
$ sudo chmod 400 ~/.Renviron
```
