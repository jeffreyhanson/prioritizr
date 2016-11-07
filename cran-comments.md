Comments for submission to CRAN 
===============================

# Test environments
* [Ubuntu 14.04, R 3.3.2 (travis-ci)](https://travis-ci.org/mstrimas/prioritizr/builds)
* [Ubuntu 14.04, R 3.4.0 (travis-ci)](https://travis-ci.org/mstrimas/prioritizr/builds)
* [Mac OSX 10.9.5, R 3.3.2 (travis-ci](https://travis-ci.org/mstrimas/prioritizr/builds)
* [Windows Server 2012 R2 (x64), R 3.3.2 (appveyor)](https://ci.appveyor.com/project/mstrimas/prioritizr)
* [Windows Server 2012 R2 (x64), R devel 2016-11-02 r71617 (appveyor)](https://ci.appveyor.com/project//prioritizr)
* Windows Server 2008 (x64), R 3.3.2 (win-builder)
* Windows Server 2008 (x64), R 3.4.0 (win-builder)

# R CMD check results from Win-Builder
There were no ERRORs or WARNINGs.

There were 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matthew Strimas-Mackey <mstrimas@gmail.com>'
  New submission
  
  _This note has occurred because I have not submitted a package to CRAN before._
  
* checking package dependencies ... NOTE
  package suggested but not available for checking: 'gurobi', 'lpsymphony', 'marxan'
  
  _This package is enhanced by several R packages not available on CRAN. The 'marxan' package is available on GitHub and the 'lpsymphony' package is available on Bioconductor. 'gurobi' R package is an R package that is distributed along with the gurobi program. Several existing R packages on CRAN use the gurobi R package (eg. cherry, DESP)._
  
* Examples with CPU or elapsed time > 10s
  
  _This package performs solves combinatoral optimization problems, which necessarily take a time even for simple example problems_
  
# Downstream dependencies
This package is not present on CRAN and therefore has no packages depending on it. 
