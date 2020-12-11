# MadRat playground

R package **mrplayground**, version **0.6.0**

[![Travis build status](https://travis-ci.com/pik-piam/mrplayground.svg?branch=master)](https://travis-ci.com/pik-piam/mrplayground)  

## Purpose and Functionality

This package is a collection of unused read- calc- and tool- functions from the madrat universe. Most of them were used at some point, but not required anymore. They are stored here in case that they might become relevant again. PLEASE NEVER USE THIS PACKAGE FOR YOUR WORK DIRECTLY. If you find something useful here, please move it to our project instead. This package is not actively managed and might change over time in functionality or even vanish completely!


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrplayground")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **mrplayground** in publications use:

Dietrich J, Chen D, Malik A, Karstens K, Baumstark L, Bodirsky B, Mishra
A, Wehner J, Rodrigues R, Leip D, Schreyer F, Steinmetz N, Weindl I,
Kreidenweis U, Wang X, Shankar A, Araujo E, Humpenoeder F, Marcolino M,
Wirth S, Klein D, Martinelli E, Oeser J (2020). _mrplayground: MadRat
playground_. R package version 0.6.0, <URL:
https://github.com/pik-piam/mrplayground>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrplayground: MadRat playground},
  author = {Jan Philipp Dietrich and David Chen and Aman Malik and Kristine Karstens and Lavinia Baumstark and Benjamin Leon Bodirsky and Abhijeet Mishra and Jasmin Wehner and Renato Rodrigues and Debbora Leip and Felix Schreyer and Nele Steinmetz and Isabelle Weindl and Ulrich Kreidenweis and Xiaoxi Wang and Atreya Shankar and Ewerton Araujo and Florian Humpenoeder and Marcos Marcolino and Stephen Wirth and David Klein and Eleonora Martinelli and Julian Oeser},
  year = {2020},
  note = {R package version 0.6.0},
  url = {https://github.com/pik-piam/mrplayground},
}
```

