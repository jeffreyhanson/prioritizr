#pragma once
#ifndef PACKAGE_H
#define PACKAGE_H

/* Disable run-time debugging for faster code */
#define BOOST_DISABLE_ASSERTS true

/* Load header files, set plugins, load Rcpp namespace */

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

#include <vector>
#include <algorithm>
#include <math.h>
#include <string>

#endif
