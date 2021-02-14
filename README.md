
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DTAsens

<!-- badges: start -->

<!-- badges: end -->

The goal of DTAsens is to conduct sensitivity analysis on DTA
meta-analysis

## Installation

You can install the released version of DTAsens from
[GitHub](https://github.com/meta2020/DTAsens) with:

``` r
devtools::install_git("meta2020/DTAsens")
```

## Example

This is an example which shows you how to solve a common problem. To
take the data “IVD” as example, print the first several lines of data.

``` r
## Load package

library(DTAsens)

## Load data

data(IVD)
kable(head(IVD))
```

| study | TP | FN |  FP |  TN |
| ----: | -: | -: | --: | --: |
|     1 | 12 |  0 |  29 | 289 |
|     2 | 10 |  2 |  14 |  72 |
|     3 | 17 |  1 |  36 |  85 |
|     4 | 13 |  0 |  18 |  67 |
|     5 |  4 |  0 |  21 | 225 |
|     6 | 15 |  2 | 122 | 403 |

### Function: dtasens1

This function need to pre-specify the c contrast in the selection
function.

1.  Given a certain selection probability \(p\), say, \(p = 0.7\), we
    can get the estimation as follows.

<!-- end list -->

``` r
## Use default parameters setting
## Print parameters and profile-likelihood confidence interval

dtasens1(IVD, p = 0.7)
#> $par
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.380  1.740  0.596  0.845 -0.354 -0.178  2.000 -4.290  0.500  0.500  0.851 
#>     se     sp 
#>  0.799  0.851 
#> 
#> $value
#> [1] 22.9
#> 
#> $counts
#> function gradient 
#>       18       18 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.380  1.081  1.67
#> u2    1.740  1.459  2.02
#> t1    0.596  0.303  1.01
#> t2    0.845  0.645  1.15
#> r    -0.354 -0.748  0.15
#> b     2.000  0.972    NA
#> 
#> $auc.all
#> 0.851 with absolute error < 1.1e-06
#> 
#> $p.hat
#> [1] 0.7
#> 
#> $data
#>     sens  spec     y1    y2     v1      v2 ldor.t
#> 1  0.962 0.908  3.219 2.284 2.0800 0.03735   3.78
#> 2  0.833 0.837  1.609 1.638 0.6000 0.08532   3.92
#> 3  0.944 0.702  2.833 0.859 1.0588 0.03954   3.52
#> 4  0.964 0.785  3.296 1.294 2.0741 0.06887   3.14
#> 5  0.900 0.913  2.197 2.350 2.2222 0.05095   3.02
#> 6  0.882 0.768  2.015 1.195 0.5667 0.01068   4.22
#> 7  0.900 0.548  2.197 0.194 0.2222 0.06513   4.46
#> 8  0.818 0.658  1.504 0.656 0.3056 0.02201   3.77
#> 9  0.917 0.750  2.398 1.099 2.1818 0.11594   2.31
#> 10 0.471 0.865 -0.118 1.856 0.2361 0.07708   3.11
#> 11 0.917 0.894  2.398 2.136 2.1818 0.14908   2.97
#> 12 0.846 0.833  1.705 1.609 0.5909 0.00984   4.28
#> 13 0.833 0.960  1.609 3.185 1.2000 0.17356   4.09
#> 14 0.583 0.932  0.336 2.616 0.3429 0.04292   4.75
#> 15 0.909 0.761  2.303 1.158 1.1000 0.01413   3.28
#> 16 0.500 0.869  0.000 1.889 0.4000 0.02808   2.89
#> 17 0.917 0.775  2.398 1.239 2.1818 0.08321   2.42
#> 18 0.809 0.980  1.442 3.872 0.0951 0.05373  13.78
#> 19 0.750 0.714  1.099 0.916 0.6667 0.11667   2.28
#> 20 0.618 0.980  0.480 3.872 0.0623 0.05373  12.78
#> 21 0.625 0.881  0.511 2.001 0.5333 0.22703   2.88
#> 22 0.964 0.916  3.296 2.390 2.0741 0.09492   3.86
#> 23 0.976 0.921  3.714 2.463 2.0488 0.04429   4.27
#> 24 0.538 0.847  0.154 1.712 0.3095 0.09081   2.95
#> 25 0.960 0.758  3.178 1.142 0.5208 0.08794   5.54
#> 26 0.917 0.837  2.398 1.638 1.0909 0.08532   3.72
#> 27 0.750 0.842  1.099 1.670 0.2667 0.03713   5.02
#> 28 0.840 0.688  1.655 0.788 0.0916 0.29091   3.95
#> 29 0.929 0.935  2.565 2.667 1.0769 0.21389   4.61
#> 30 0.727 0.830  0.981 1.588 0.4583 0.01825   3.72
#> 31 0.929 0.749  2.565 1.095 1.0769 0.01362   3.50
#> 32 0.906 0.997  2.269 5.740 0.7356 2.00643   4.84
#> 33 0.800 0.938  1.386 2.708 0.6250 0.26667   4.34
#> 
#> $func.name
#> [1] "dtasens1"
#> 
#> $pars.info
#> $pars.info$p
#> [1] 0.7
#> 
#> $pars.info$c1
#> [1] 0.707
#> 
#> $pars.info$correct.value
#> [1] 0.5
#> 
#> $pars.info$correct.type
#> [1] "single"
#> 
#> $pars.info$start5
#> NULL
#> 
#> $pars.info$b0
#> [1] 0.1
#> 
#> $pars.info$b.interval
#> [1] 0 2
#> 
#> $pars.info$a.interval
#> [1] -5  3
#> 
#> $pars.info$pos.r
#> [1] TRUE
#> 
#> $pars.info$ci.level
#> [1] 0.95
#> 
#> $pars.info$show.warn.message
#> [1] FALSE
#> 
#> $pars.info$a.root.extendInt
#> [1] "downX"
#> 
#> 
#> attr(,"class")
#> [1] "DTAsens"

## If we change b.interval

dtasens1(IVD, p = 0.7, b.interval = c(0,1))
#> $par
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.367  1.729  0.592  0.845 -0.353 -0.177  1.000 -2.077  0.500  0.500  0.849 
#>     se     sp 
#>  0.797  0.849 
#> 
#> $value
#> [1] 24.7
#> 
#> $counts
#> function gradient 
#>       16       16 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.367  1.060 1.672
#> u2    1.729  1.441 2.015
#> t1    0.592  0.298 1.005
#> t2    0.845  0.645 1.151
#> r    -0.353 -0.748 0.152
#> b     1.000  0.633    NA
#> 
#> $auc.all
#> 0.849 with absolute error < 1.1e-06
#> 
#> $p.hat
#> [1] 0.7
#> 
#> $data
#>     sens  spec     y1    y2     v1      v2 ldor.t
#> 1  0.962 0.908  3.219 2.284 2.0800 0.03735   3.78
#> 2  0.833 0.837  1.609 1.638 0.6000 0.08532   3.92
#> 3  0.944 0.702  2.833 0.859 1.0588 0.03954   3.52
#> 4  0.964 0.785  3.296 1.294 2.0741 0.06887   3.14
#> 5  0.900 0.913  2.197 2.350 2.2222 0.05095   3.02
#> 6  0.882 0.768  2.015 1.195 0.5667 0.01068   4.22
#> 7  0.900 0.548  2.197 0.194 0.2222 0.06513   4.46
#> 8  0.818 0.658  1.504 0.656 0.3056 0.02201   3.77
#> 9  0.917 0.750  2.398 1.099 2.1818 0.11594   2.31
#> 10 0.471 0.865 -0.118 1.856 0.2361 0.07708   3.11
#> 11 0.917 0.894  2.398 2.136 2.1818 0.14908   2.97
#> 12 0.846 0.833  1.705 1.609 0.5909 0.00984   4.28
#> 13 0.833 0.960  1.609 3.185 1.2000 0.17356   4.09
#> 14 0.583 0.932  0.336 2.616 0.3429 0.04292   4.75
#> 15 0.909 0.761  2.303 1.158 1.1000 0.01413   3.28
#> 16 0.500 0.869  0.000 1.889 0.4000 0.02808   2.89
#> 17 0.917 0.775  2.398 1.239 2.1818 0.08321   2.42
#> 18 0.809 0.980  1.442 3.872 0.0951 0.05373  13.78
#> 19 0.750 0.714  1.099 0.916 0.6667 0.11667   2.28
#> 20 0.618 0.980  0.480 3.872 0.0623 0.05373  12.78
#> 21 0.625 0.881  0.511 2.001 0.5333 0.22703   2.88
#> 22 0.964 0.916  3.296 2.390 2.0741 0.09492   3.86
#> 23 0.976 0.921  3.714 2.463 2.0488 0.04429   4.27
#> 24 0.538 0.847  0.154 1.712 0.3095 0.09081   2.95
#> 25 0.960 0.758  3.178 1.142 0.5208 0.08794   5.54
#> 26 0.917 0.837  2.398 1.638 1.0909 0.08532   3.72
#> 27 0.750 0.842  1.099 1.670 0.2667 0.03713   5.02
#> 28 0.840 0.688  1.655 0.788 0.0916 0.29091   3.95
#> 29 0.929 0.935  2.565 2.667 1.0769 0.21389   4.61
#> 30 0.727 0.830  0.981 1.588 0.4583 0.01825   3.72
#> 31 0.929 0.749  2.565 1.095 1.0769 0.01362   3.50
#> 32 0.906 0.997  2.269 5.740 0.7356 2.00643   4.84
#> 33 0.800 0.938  1.386 2.708 0.6250 0.26667   4.34
#> 
#> $func.name
#> [1] "dtasens1"
#> 
#> $pars.info
#> $pars.info$p
#> [1] 0.7
#> 
#> $pars.info$c1
#> [1] 0.707
#> 
#> $pars.info$correct.value
#> [1] 0.5
#> 
#> $pars.info$correct.type
#> [1] "single"
#> 
#> $pars.info$start5
#> NULL
#> 
#> $pars.info$b0
#> [1] 0.1
#> 
#> $pars.info$b.interval
#> [1] 0 1
#> 
#> $pars.info$a.interval
#> [1] -5  3
#> 
#> $pars.info$pos.r
#> [1] TRUE
#> 
#> $pars.info$ci.level
#> [1] 0.95
#> 
#> $pars.info$show.warn.message
#> [1] FALSE
#> 
#> $pars.info$a.root.extendInt
#> [1] "downX"
#> 
#> 
#> attr(,"class")
#> [1] "DTAsens"

## To get full results list

opt1 <- dtasens1(IVD, p = 0.7)

str(opt1)
#> List of 11
#>  $ par        : Named num [1:13] 1.38 1.74 0.596 0.845 -0.354 ...
#>   ..- attr(*, "names")= chr [1:13] "u1" "u2" "t1" "t2" ...
#>  $ value      : num 22.9
#>  $ counts     : Named int [1:2] 18 18
#>   ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>  $ convergence: int 0
#>  $ message    : chr "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#>  $ ci         : num [1:6, 1:3] 1.38 1.74 0.596 0.845 -0.354 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:6] "u1" "u2" "t1" "t2" ...
#>   .. ..$ : chr [1:3] "estimate" "ci.low" "ci.up"
#>  $ auc.all    :List of 5
#>   ..$ value       : num 0.851
#>   ..$ abs.error   : num 1.09e-06
#>   ..$ subdivisions: int 10
#>   ..$ message     : chr "OK"
#>   ..$ call        : language integrate(f = function(x) {     plogis(u1 - (r * t1/t2) * (qlogis(x) + u2)) ...
#>   ..- attr(*, "class")= chr "integrate"
#>  $ p.hat      : num 0.7
#>  $ data       :'data.frame': 33 obs. of  7 variables:
#>   ..$ sens  : num [1:33] 0.962 0.833 0.944 0.964 0.9 ...
#>   ..$ spec  : num [1:33] 0.908 0.837 0.702 0.785 0.913 ...
#>   ..$ y1    : num [1:33] 3.22 1.61 2.83 3.3 2.2 ...
#>   ..$ y2    : num [1:33] 2.284 1.638 0.859 1.294 2.35 ...
#>   ..$ v1    : num [1:33] 2.08 0.6 1.06 2.07 2.22 ...
#>   ..$ v2    : num [1:33] 0.0374 0.0853 0.0395 0.0689 0.0509 ...
#>   ..$ ldor.t: num [1:33] 3.78 3.92 3.52 3.14 3.02 ...
#>  $ func.name  : chr "dtasens1"
#>  $ pars.info  :List of 12
#>   ..$ p                : num 0.7
#>   ..$ c1               : num 0.707
#>   ..$ correct.value    : num 0.5
#>   ..$ correct.type     : chr "single"
#>   ..$ start5           : NULL
#>   ..$ b0               : num 0.1
#>   ..$ b.interval       : num [1:2] 0 2
#>   ..$ a.interval       : num [1:2] -5 3
#>   ..$ pos.r            : logi TRUE
#>   ..$ ci.level         : num 0.95
#>   ..$ show.warn.message: logi FALSE
#>   ..$ a.root.extendInt : chr "downX"
#>  - attr(*, "class")= chr "DTAsens"
```

2.  Given a series of selection probabilities, say,
    \(p = 1, 0.9, 0,8, ...,0.1\). Notice, \(p\) must greater than 0 and
    cannot equal to 0. (\(p>0\))

<!-- end list -->

``` r

## Set p vectors

p.seq <- seq(1, 0.1, -0.1)

## Get estimations for each p in p.seq vector

est1 <- sapply(p.seq, function(p) dtasens1(IVD, p)$par)

## Print estimation

colnames(est1)<- paste0("p = ", p.seq)
kable(est1)
```

|     |   p = 1 | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :-- | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| u1  |   1.484 |   1.478 |   1.442 |   1.380 |   1.301 |   1.207 |   1.091 |   0.933 |   0.704 |   0.280 |
| u2  |   1.824 |   1.819 |   1.791 |   1.740 |   1.671 |   1.582 |   1.464 |   1.299 |   1.054 |   0.610 |
| t1  |   0.597 |   0.599 |   0.600 |   0.596 |   0.592 |   0.594 |   0.605 |   0.634 |   0.688 |   0.793 |
| t2  |   0.826 |   0.828 |   0.835 |   0.845 |   0.857 |   0.873 |   0.897 |   0.933 |   0.986 |   1.077 |
| r   | \-0.424 | \-0.415 | \-0.388 | \-0.354 | \-0.310 | \-0.254 | \-0.176 | \-0.068 |   0.071 |   0.256 |
| t12 | \-0.209 | \-0.206 | \-0.195 | \-0.178 | \-0.158 | \-0.132 | \-0.096 | \-0.040 |   0.048 |   0.218 |
| b   |   0.100 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   1.930 |   1.820 |
| a   |  10.650 | \-2.553 | \-3.636 | \-4.290 | \-4.757 | \-5.123 | \-5.425 | \-5.679 | \-5.729 | \-5.664 |
| c11 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |
| c22 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |
| auc |   0.874 |   0.872 |   0.864 |   0.851 |   0.834 |   0.810 |   0.778 |   0.729 |   0.657 |   0.540 |
| se  |   0.815 |   0.814 |   0.809 |   0.799 |   0.786 |   0.770 |   0.749 |   0.718 |   0.669 |   0.570 |
| sp  |   0.861 |   0.860 |   0.857 |   0.851 |   0.842 |   0.829 |   0.812 |   0.786 |   0.742 |   0.648 |

### Function: dtasens2

This function do not need to pre-specify the c contrast in the selection
function.

1.  Given a certain selection probability \(p\), say, \(p = 0.7\), we
    can get the estimation as follows.

<!-- end list -->

``` r
## Use default parameters setting
## Print parameters and profile-likelihood confidence interval

dtasens2(IVD, p = 0.7)
#> $par
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.379  1.741  0.596  0.844 -0.353 -0.178  2.000 -4.254  0.509  0.491  0.851 
#>     se     sp 
#>  0.799  0.851 
#> 
#> $value
#> [1] 22.9
#> 
#> $counts
#> function gradient 
#>       19       19 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.379  1.079 1.672
#> u2    1.741  1.460 2.018
#> t1    0.596  0.303 1.009
#> t2    0.844  0.645 1.149
#> r    -0.353 -0.748 0.150
#> b     2.000  0.977    NA
#> c1    0.714  0.418 0.918
#> 
#> $auc.all
#> 0.851 with absolute error < 1.1e-06
#> 
#> $p.hat
#> [1] 0.7
#> 
#> $data
#>     sens  spec     y1    y2     v1      v2 ldor.t
#> 1  0.962 0.908  3.219 2.284 2.0800 0.03735   3.78
#> 2  0.833 0.837  1.609 1.638 0.6000 0.08532   3.92
#> 3  0.944 0.702  2.833 0.859 1.0588 0.03954   3.52
#> 4  0.964 0.785  3.296 1.294 2.0741 0.06887   3.14
#> 5  0.900 0.913  2.197 2.350 2.2222 0.05095   3.02
#> 6  0.882 0.768  2.015 1.195 0.5667 0.01068   4.22
#> 7  0.900 0.548  2.197 0.194 0.2222 0.06513   4.46
#> 8  0.818 0.658  1.504 0.656 0.3056 0.02201   3.77
#> 9  0.917 0.750  2.398 1.099 2.1818 0.11594   2.31
#> 10 0.471 0.865 -0.118 1.856 0.2361 0.07708   3.11
#> 11 0.917 0.894  2.398 2.136 2.1818 0.14908   2.97
#> 12 0.846 0.833  1.705 1.609 0.5909 0.00984   4.28
#> 13 0.833 0.960  1.609 3.185 1.2000 0.17356   4.09
#> 14 0.583 0.932  0.336 2.616 0.3429 0.04292   4.75
#> 15 0.909 0.761  2.303 1.158 1.1000 0.01413   3.28
#> 16 0.500 0.869  0.000 1.889 0.4000 0.02808   2.89
#> 17 0.917 0.775  2.398 1.239 2.1818 0.08321   2.42
#> 18 0.809 0.980  1.442 3.872 0.0951 0.05373  13.78
#> 19 0.750 0.714  1.099 0.916 0.6667 0.11667   2.28
#> 20 0.618 0.980  0.480 3.872 0.0623 0.05373  12.78
#> 21 0.625 0.881  0.511 2.001 0.5333 0.22703   2.88
#> 22 0.964 0.916  3.296 2.390 2.0741 0.09492   3.86
#> 23 0.976 0.921  3.714 2.463 2.0488 0.04429   4.27
#> 24 0.538 0.847  0.154 1.712 0.3095 0.09081   2.95
#> 25 0.960 0.758  3.178 1.142 0.5208 0.08794   5.54
#> 26 0.917 0.837  2.398 1.638 1.0909 0.08532   3.72
#> 27 0.750 0.842  1.099 1.670 0.2667 0.03713   5.02
#> 28 0.840 0.688  1.655 0.788 0.0916 0.29091   3.95
#> 29 0.929 0.935  2.565 2.667 1.0769 0.21389   4.61
#> 30 0.727 0.830  0.981 1.588 0.4583 0.01825   3.72
#> 31 0.929 0.749  2.565 1.095 1.0769 0.01362   3.50
#> 32 0.906 0.997  2.269 5.740 0.7356 2.00643   4.84
#> 33 0.800 0.938  1.386 2.708 0.6250 0.26667   4.34
#> 
#> $func.name
#> [1] "dtasens2"
#> 
#> $pars.infor
#> $pars.infor$p
#> [1] 0.7
#> 
#> $pars.infor$correct.value
#> [1] 0.5
#> 
#> $pars.infor$correct.type
#> [1] "single"
#> 
#> $pars.infor$start5
#> NULL
#> 
#> $pars.infor$b0
#> [1] 0.1
#> 
#> $pars.infor$c10
#> [1] 0.707
#> 
#> $pars.infor$b.interval
#> [1] 0 2
#> 
#> $pars.infor$a.interval
#> [1] -5  3
#> 
#> $pars.infor$pos.r
#> [1] TRUE
#> 
#> $pars.infor$ci.level
#> [1] 0.95
#> 
#> $pars.infor$show.warn.message
#> [1] FALSE
#> 
#> $pars.infor$a.root.extendInt
#> [1] "downX"
#> 
#> 
#> attr(,"class")
#> [1] "DTAsens"

## To get full results list

opt2 <- dtasens2(IVD, p = 0.7)

str(opt2)
#> List of 11
#>  $ par        : Named num [1:13] 1.379 1.741 0.596 0.844 -0.353 ...
#>   ..- attr(*, "names")= chr [1:13] "u1" "u2" "t1" "t2" ...
#>  $ value      : num 22.9
#>  $ counts     : Named int [1:2] 19 19
#>   ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>  $ convergence: int 0
#>  $ message    : chr "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#>  $ ci         : num [1:7, 1:3] 1.379 1.741 0.596 0.844 -0.353 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:7] "u1" "u2" "t1" "t2" ...
#>   .. ..$ : chr [1:3] "estimate" "ci.low" "ci.up"
#>  $ auc.all    :List of 5
#>   ..$ value       : num 0.851
#>   ..$ abs.error   : num 1.09e-06
#>   ..$ subdivisions: int 10
#>   ..$ message     : chr "OK"
#>   ..$ call        : language integrate(f = function(x) {     plogis(u1 - (r * t1/t2) * (qlogis(x) + u2)) ...
#>   ..- attr(*, "class")= chr "integrate"
#>  $ p.hat      : num 0.7
#>  $ data       :'data.frame': 33 obs. of  7 variables:
#>   ..$ sens  : num [1:33] 0.962 0.833 0.944 0.964 0.9 ...
#>   ..$ spec  : num [1:33] 0.908 0.837 0.702 0.785 0.913 ...
#>   ..$ y1    : num [1:33] 3.22 1.61 2.83 3.3 2.2 ...
#>   ..$ y2    : num [1:33] 2.284 1.638 0.859 1.294 2.35 ...
#>   ..$ v1    : num [1:33] 2.08 0.6 1.06 2.07 2.22 ...
#>   ..$ v2    : num [1:33] 0.0374 0.0853 0.0395 0.0689 0.0509 ...
#>   ..$ ldor.t: num [1:33] 3.78 3.92 3.52 3.14 3.02 ...
#>  $ func.name  : chr "dtasens2"
#>  $ pars.infor :List of 12
#>   ..$ p                : num 0.7
#>   ..$ correct.value    : num 0.5
#>   ..$ correct.type     : chr "single"
#>   ..$ start5           : NULL
#>   ..$ b0               : num 0.1
#>   ..$ c10              : num 0.707
#>   ..$ b.interval       : num [1:2] 0 2
#>   ..$ a.interval       : num [1:2] -5 3
#>   ..$ pos.r            : logi TRUE
#>   ..$ ci.level         : num 0.95
#>   ..$ show.warn.message: logi FALSE
#>   ..$ a.root.extendInt : chr "downX"
#>  - attr(*, "class")= chr "DTAsens"
```

2.  Given a series of selection probabilities, say,
    \(p = 1, 0.9, 0,8, ...,0.1\). Notice, \(p\) must greater than 0 and
    cannot equal to 0. (\(p>0\))

<!-- end list -->

``` r
## Set p vectors

p.seq <- seq(1, 0.1, -0.1)

## Get estimations for each p in p.seq vector

est2 <- sapply(p.seq, function(p) dtasens2(IVD, p)$par)

## Print estimation

colnames(est2)<- paste0("p = ", p.seq)
kable(est2)
```

|     |   p = 1 | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :-- | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| u1  |   1.484 |   1.478 |   1.439 |   1.379 |   1.309 |   1.230 |   1.137 |   1.018 |   0.867 |   0.587 |
| u2  |   1.824 |   1.820 |   1.794 |   1.741 |   1.665 |   1.563 |   1.427 |   1.237 |   0.960 |   0.434 |
| t1  |   0.597 |   0.600 |   0.601 |   0.596 |   0.590 |   0.586 |   0.587 |   0.599 |   0.618 |   0.668 |
| t2  |   0.826 |   0.828 |   0.833 |   0.844 |   0.859 |   0.881 |   0.911 |   0.957 |   1.022 |   1.142 |
| r   | \-0.424 | \-0.416 | \-0.388 | \-0.353 | \-0.312 | \-0.261 | \-0.194 | \-0.103 |   0.005 |   0.172 |
| t12 | \-0.209 | \-0.206 | \-0.195 | \-0.178 | \-0.158 | \-0.135 | \-0.104 | \-0.059 |   0.003 |   0.131 |
| b   |   0.100 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   1.981 |   1.836 |   1.736 |
| a   |  10.650 | \-2.201 | \-3.450 | \-4.254 | \-4.855 | \-5.329 | \-5.708 | \-5.979 | \-5.889 | \-5.888 |
| c11 |   0.500 |   0.603 |   0.551 |   0.509 |   0.475 |   0.450 |   0.431 |   0.417 |   0.399 |   0.384 |
| c22 |   0.500 |   0.397 |   0.449 |   0.491 |   0.525 |   0.550 |   0.569 |   0.583 |   0.601 |   0.616 |
| auc |   0.874 |   0.872 |   0.864 |   0.851 |   0.834 |   0.813 |   0.786 |   0.749 |   0.704 |   0.632 |
| se  |   0.815 |   0.814 |   0.808 |   0.799 |   0.787 |   0.774 |   0.757 |   0.735 |   0.704 |   0.643 |
| sp  |   0.861 |   0.861 |   0.857 |   0.851 |   0.841 |   0.827 |   0.806 |   0.775 |   0.723 |   0.607 |

### Plot sROC

1.  Single sROC

<!-- end list -->

``` r
par(mfrow = c(1,2))

## This is the standard method: Reistma model
## Without taking publication bias (PB) into consideration

library(mada)
#> Loading required package: mvtnorm
#> Loading required package: ellipse
#> 
#> Attaching package: 'ellipse'
#> The following object is masked from 'package:graphics':
#> 
#>     pairs
#> Loading required package: mvmeta
#> This is mvmeta 1.0.3. For an overview type: help('mvmeta-package').
fit <- reitsma(IVD, correction.control = "single", method = "ml")
plot(sroc(fit, type = "naive"), type = "l", ylim = c(0,1), xlim = c(0,1), col = "red")
title("Reistma model from mada")

## Extact the estimation from Reistma model

par0 <- c(fit$coefficients*c(1,-1), c(1,-1)*sqrt(fit$Psi[c(1,4)]), fit$Psi[3]/prod(sqrt(fit$Psi[c(1,4)])))

## Our model that takes PB into consideration

# opt1 <- dtasens1(IVD, p = 0.5)
# opt2 <- dtasens2(IVD, p = 0.5)

## Plot ROC from Reistma model
sROC(par0, roc.col = "red", spoint.col ="red")

## Add sROC
sROC(opt1, add = TRUE, roc.col = "black", roc.lty = 2, spoint.pch = 1, spoint.col = "black")
sROC(opt2, add = TRUE, roc.col = "darkgray", roc.lty = 2, spoint.col = "darkgray")

## Also works by using the extracted parameters

par1 <- opt1$par
par2 <- opt2$par
sROC(par1, add = TRUE, roc.col = "black", roc.lty = 2, spoint.pch = 1, spoint.col = "black")
sROC(par1, add = TRUE, roc.col = "darkgray", roc.lty = 2, spoint.col = "darkgray")

## Calculate the data points (fpr, sens) of IVD

with(IVD, points(FP/(FP+TN), TP/(TP+FN), pch = 4, cex = 0.5))
legend("bottomright", c("Reistma", "dtasens1", "dtasens2", "IVD"), 
       col = c("red", "black", "darkgray", "black"), lty = c(1,2,2, 0), pch = c(19,1,19, 4))
title("When selection prob = 0.0.5")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
par(mfrow = c(1,1))
```

2.  Multiple sROC

<!-- end list -->

``` r
## p vector and model

# p.seq <- seq(1, 0.1, -0.1)
# est1 <- sapply(p.seq, function(p) dtasens1(IVD, p)$par)
# est2 <- sapply(p.seq, function(p) dtasens2(IVD, p)$par)

## Plot multiple sROC
par(mfrow = c(1,2))
msROC(est1, legend = TRUE, p.vec = p.seq, legend.cex = 0.5)
sROC(par0, add = TRUE, roc.col = "red")
title("dtasens1")

msROC(est2, legend = TRUE, p.vec = p.seq, legend.cex = 0.5)
sROC(par0, add = TRUE, roc.col = "red")
title("dtasens2")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
par(mfrow = c(1,1))
```

### Calculate sAUC and confident interval

Although sAUC has output together with the parameters in `dtasens1` and
`dtasens2` functions. We can still calculate by using `sAUC` function.

The confidence interval (CI) is calculated by parametric bootstrapping.

1.  Single sROC

<!-- end list -->

``` r
## Use dtasens object

sAUC(opt1)
#> 0.851 with absolute error < 1.1e-06
sAUC(opt2)
#> 0.851 with absolute error < 1.1e-06

## Use parameter vector
par1
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.380  1.740  0.596  0.845 -0.354 -0.178  2.000 -4.290  0.500  0.500  0.851 
#>     se     sp 
#>  0.799  0.851
sAUC(par1)
#> 0.851 with absolute error < 1.1e-06
par2
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.379  1.741  0.596  0.844 -0.353 -0.178  2.000 -4.254  0.509  0.491  0.851 
#>     se     sp 
#>  0.799  0.851
sAUC(par2)
#> 0.851 with absolute error < 1.1e-06

## Calculate Parametric Bootstrap CI
## B (Bootstrapping times) is suggested to be 1000. To save computing time, we use B = 10 to show the functionality.

sAUC.ci(opt1, B=10)
#>   |                                                |                                        |   0%  |                                                |====                                    |  10%  |                                                |========                                |  20%  |                                                |============                            |  30%  |                                                |================                        |  40%  |                                                |====================                    |  50%  |                                                |========================                |  60%  |                                                |============================            |  70%  |                                                |================================        |  80%  |                                                |====================================    |  90%  |                                                |========================================| 100%
#> $sAUC
#> [1] 0.851
#> 
#> $CI.L
#> [1] 0.791
#> 
#> $CI.U
#> [1] 0.907
#> 
#> $cluster
#> socket cluster with 4 nodes on host 'localhost'
#> 
#> attr(,"class")
#> [1] "sAUC.ci"
sAUC.ci(opt2, B=10)
#>   |                                                |                                        |   0%  |                                                |====                                    |  10%  |                                                |========                                |  20%  |                                                |============                            |  30%  |                                                |================                        |  40%  |                                                |====================                    |  50%  |                                                |========================                |  60%  |                                                |============================            |  70%  |                                                |================================        |  80%  |                                                |====================================    |  90%  |                                                |========================================| 100%
#> $sAUC
#> [1] 0.851
#> 
#> $CI.L
#> [1] 0.756
#> 
#> $CI.U
#> [1] 0.917
#> 
#> $cluster
#> socket cluster with 4 nodes on host 'localhost'
#> 
#> attr(,"class")
#> [1] "sAUC.ci"
```

2.  Multiple sAUC

<!-- end list -->

``` r

## Calculate Parametric Bootstrap CI

## B (Bootstrapping times) is suggested to be 1000. To save computing time, we use B = 5 to show the functionality.

p.seq <- seq(1, 0.1, -0.1)

sauc1 <- sapply(p.seq, function(p) {
  opt1 <- dtasens1(IVD, p)
  sauc <- sAUC.ci(opt1, B=5, hide.progress = TRUE)
  c(sauc[[1]], sauc[[2]], sauc[[3]])
  })


sauc2 <- sapply(p.seq, function(p) {
  opt2 <- dtasens2(IVD, p)
  sauc <- sAUC.ci(opt2, B=5, hide.progress = TRUE)
  c(sauc[[1]], sauc[[2]], sauc[[3]])
  })

colnames(sauc1)<- paste0("p = ", p.seq)
rownames(sauc1)<- c("sAUC", "CI.L", "CI.U")
kable(sauc1)
```

|      | p = 1 | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :--- | ----: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| sAUC | 0.874 |   0.872 |   0.864 |   0.851 |   0.834 |   0.810 |   0.778 |   0.729 |   0.657 |   0.540 |
| CI.L | 0.836 |   0.861 |   0.839 |   0.841 |   0.703 |   0.731 |   0.645 |   0.555 |   0.435 |   0.444 |
| CI.U | 0.901 |   0.882 |   0.884 |   0.860 |   0.925 |   0.922 |   0.866 |   0.894 |   0.817 |   0.787 |

``` r

colnames(sauc2)<- paste0("p = ", p.seq)
rownames(sauc2)<- c("sAUC", "CI.L", "CI.U")
kable(sauc2)
```

|      | p = 1 | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :--- | ----: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| sAUC | 0.874 |   0.872 |   0.864 |   0.851 |   0.834 |   0.813 |   0.786 |   0.749 |   0.704 |   0.632 |
| CI.L | 0.859 |   0.858 |   0.832 |   0.776 |   0.816 |   0.724 |   0.664 |   0.568 |   0.535 |   0.509 |
| CI.U | 0.881 |   0.896 |   0.908 |   0.879 |   0.855 |   0.869 |   0.904 |   0.837 |   0.813 |   0.757 |

3.  Plot sAUC

This is an example of hwo to plot sAUC and CI. In analysis, please set
`B = 1000` and then reproduce the plots.

``` r
par(mfrow = c(1,2))

## Use matplot to plot the sAUC and CI

matplot(t(sauc1), type = "b", lty = c(1,2,2), 
        pch = 19, col = c("black", "grey", "grey"),
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("dtasens1")

matplot(t(sauc2), type = "b", lty = c(1,2,2), 
        pch = 19, col = c("black", "grey", "grey"),
        xlab = "p", ylab = "sAUC",
        ylim = c(0,1),
        xaxt = "n")
axis(1, at = 1:10, labels = p.seq)
title("dtasens2")
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r

par(mfrow = c(1,1))
```
