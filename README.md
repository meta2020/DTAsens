
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

This is a basic example which shows you how to solve a common problem:

``` r
library(DTAsens)
## basic example code
```

### Example data 1

``` r

data(dta)
kable(head(dta))
```

|    se |    sp |    y1 |    y2 |    v1 |    v2 | ldor.t |
| ----: | ----: | ----: | ----: | ----: | ----: | -----: |
| 0.819 | 0.746 | 1.509 | 1.077 | 0.137 | 0.005 |  6.839 |
| 0.883 | 0.751 | 2.024 | 1.102 | 0.086 | 0.053 |  8.404 |
| 0.816 | 0.857 | 1.490 | 1.794 | 0.043 | 0.139 |  7.699 |
| 0.725 | 0.653 | 0.967 | 0.634 | 1.124 | 1.458 |  0.996 |
| 0.768 | 0.843 | 1.196 | 1.681 | 0.343 | 0.302 |  3.584 |
| 0.519 | 0.877 | 0.075 | 1.961 | 0.375 | 0.232 |  2.614 |

### Optim 1: dtasens1

For a certain selection probability, e.g., p = 0.5

``` r
dtasens1(dta, p = 0.5)
#> $par
#>     u1     u2     t1     t2    t12      r    auc      b      a 
#>  1.428  1.432  0.318  0.459 -0.146 -1.000  0.874  0.270 -0.867 
#> 
#> $value
#> [1] -4.01
#> 
#> $counts
#> function gradient 
#>       38       38 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> attr(,"class")
#> [1] "DTAsens"
dtasens1(dta, p = 0.5, opt.type = "nlminb")
#> $par
#>     u1     u2     t1     t2    t12      r    auc      b      a 
#>  1.428  1.432  0.318  0.459 -0.146 -1.000  0.874  0.270 -0.867 
#> 
#> $objective
#> [1] -4.01
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 27
#> 
#> $evaluations
#> function gradient 
#>       39      186 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
#> attr(,"class")
#> [1] "DTAsens"

dtasens1(dta, p = 0.5, p.hat = TRUE, auc.all = TRUE, show.data = TRUE)
#> $data
#>       se    sp     y1    y2       v1       v2 ldor.t
#> 1  0.819 0.746 1.5089 1.077 1.37e-01 5.45e-03  6.839
#> 2  0.883 0.751 2.0236 1.102 8.56e-02 5.27e-02  8.404
#> 3  0.816 0.857 1.4900 1.794 4.25e-02 1.39e-01  7.699
#> 4  0.725 0.653 0.9669 0.634 1.12e+00 1.46e+00  0.996
#> 5  0.768 0.843 1.1964 1.681 3.43e-01 3.02e-01  3.584
#> 6  0.519 0.877 0.0749 1.961 3.75e-01 2.32e-01  2.614
#> 7  0.859 0.791 1.8074 1.330 1.17e+00 1.45e-02  2.884
#> 8  0.883 0.897 2.0205 2.165 7.20e-01 5.30e-01  3.745
#> 9  0.748 0.931 1.0877 2.601 6.27e-02 9.34e-01  3.696
#> 10 0.883 0.855 2.0216 1.773 8.93e-02 1.20e+00  3.335
#> 11 0.810 0.817 1.4497 1.494 3.84e-01 2.75e+00  1.662
#> 12 0.658 0.898 0.6527 2.180 2.33e-01 1.02e-01  4.899
#> 13 0.783 0.852 1.2839 1.751 2.36e-02 6.01e-03 17.646
#> 14 0.832 0.715 1.6023 0.919 2.53e-03 6.43e-01  3.136
#> 15 0.730 0.527 0.9926 0.107 4.27e-01 1.50e+00  0.793
#> 16 0.823 0.842 1.5373 1.675 3.34e-01 3.83e-01  3.793
#> 17 0.723 0.752 0.9596 1.107 3.87e-01 2.17e-02  3.233
#> 18 0.945 0.812 2.8408 1.466 2.12e+00 1.21e-01  2.880
#> 19 0.890 0.588 2.0940 0.357 1.84e-07 1.81e-02 18.205
#> 20 0.652 0.638 0.6265 0.565 4.30e-01 1.59e+00  0.840
#> 21 0.778 0.884 1.2564 2.035 1.05e+00 1.27e-01  3.031
#> 22 0.884 0.818 2.0341 1.503 6.82e-01 1.42e-01  3.898
#> 23 0.872 0.948 1.9203 2.907 7.82e-03 1.59e+00  3.821
#> 24 0.802 0.888 1.3998 2.072 4.99e-02 4.05e-01  5.145
#> 25 0.686 0.953 0.7832 3.002 4.24e-03 9.18e-01  3.941
#> 26 0.857 0.655 1.7897 0.643 5.20e-01 1.89e+00  1.569
#> 27 0.939 0.741 2.7324 1.049 2.82e-01 2.65e-05  7.122
#> 28 0.713 0.892 0.9117 2.114 9.16e-01 1.84e-01  2.885
#> 29 0.762 0.837 1.1647 1.638 5.89e-03 2.98e-01  5.085
#> 30 0.851 0.888 1.7437 2.067 7.76e-01 1.55e+00  2.497
#> 
#> $opt
#> $par
#>     u1     u2    t11    t22    t12      r    auc      b      a  p.hat 
#>  1.428  1.432  0.318  0.459 -0.146 -1.000  0.874  0.270 -0.867  0.500 
#> 
#> $value
#> [1] -4.01
#> 
#> $counts
#> function gradient 
#>       38       38 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $auc
#> 0.874 with absolute error < 5.5e-05
#> 
#> attr(,"class")
#> [1] "DTAsens"
```

For a series of selection probabilities, e.g., p = 0.9, 0,8, …,0.1

``` r
p.seq <- seq(0.9, 0.1, -0.1)

estimates <- sapply(p.seq, function(p) dtasens1(dta, p)$par)
colnames(estimates)<- paste0("p = ", p.seq)
kable(estimates)
```

|     | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :-- | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| u1  |   1.434 |   1.432 |   1.431 |   1.429 |   1.428 |   1.427 |   1.426 |   1.425 |   1.422 |
| u2  |   1.462 |   1.451 |   1.443 |   1.437 |   1.432 |   1.428 |   1.424 |   1.418 |   1.408 |
| t1  |   0.315 |   0.316 |   0.317 |   0.317 |   0.318 |   0.318 |   0.319 |   0.320 |   0.321 |
| t2  |   0.468 |   0.465 |   0.462 |   0.460 |   0.459 |   0.457 |   0.456 |   0.454 |   0.451 |
| t12 | \-0.147 | \-0.147 | \-0.146 | \-0.146 | \-0.146 | \-0.146 | \-0.145 | \-0.145 | \-0.145 |
| r   | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 |
| auc |   0.876 |   0.875 |   0.874 |   0.874 |   0.874 |   0.873 |   0.873 |   0.873 |   0.872 |
| b   |   0.570 |   0.475 |   0.385 |   0.318 |   0.270 |   0.232 |   0.202 |   0.179 |   0.158 |
| a   | \-0.156 | \-0.478 | \-0.618 | \-0.734 | \-0.867 | \-1.017 | \-1.200 | \-1.448 | \-1.820 |

### Optim 2: dtasens2

For a certain selection probability, e.g., p = 0.5

``` r

dtasens2(dta, p = 0.5)
#> $par
#>      u1      u2      t1      t2     t12       r     auc       b       a     c11 
#>  1.4867  1.3528  0.3227  0.4740 -0.1530 -1.0000  0.8738  0.4261 -1.0260  0.0127 
#>     c22 
#>  0.9873 
#> 
#> $value
#> [1] -5.09
#> 
#> $counts
#> function gradient 
#>       40       40 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> attr(,"class")
#> [1] "DTAsens"
dtasens2(dta, p = 0.5, opt.type = "nlminb")
#> $par
#>      u1      u2      t1      t2     t12       r     auc       b       a     c11 
#>  1.4867  1.3528  0.3227  0.4740 -0.1530 -1.0000  0.8738  0.4260 -1.0261  0.0127 
#>     c22 
#>  0.9873 
#> 
#> $objective
#> [1] -5.09
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 31
#> 
#> $evaluations
#> function gradient 
#>       47      250 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
#> attr(,"class")
#> [1] "DTAsens"


dtasens2(dta, p = 0.5, p.hat = TRUE, auc.all = TRUE, show.data = TRUE)
#> $data
#>       se    sp     y1    y2       v1       v2 ldor.t
#> 1  0.819 0.746 1.5089 1.077 1.37e-01 5.45e-03  6.839
#> 2  0.883 0.751 2.0236 1.102 8.56e-02 5.27e-02  8.404
#> 3  0.816 0.857 1.4900 1.794 4.25e-02 1.39e-01  7.699
#> 4  0.725 0.653 0.9669 0.634 1.12e+00 1.46e+00  0.996
#> 5  0.768 0.843 1.1964 1.681 3.43e-01 3.02e-01  3.584
#> 6  0.519 0.877 0.0749 1.961 3.75e-01 2.32e-01  2.614
#> 7  0.859 0.791 1.8074 1.330 1.17e+00 1.45e-02  2.884
#> 8  0.883 0.897 2.0205 2.165 7.20e-01 5.30e-01  3.745
#> 9  0.748 0.931 1.0877 2.601 6.27e-02 9.34e-01  3.696
#> 10 0.883 0.855 2.0216 1.773 8.93e-02 1.20e+00  3.335
#> 11 0.810 0.817 1.4497 1.494 3.84e-01 2.75e+00  1.662
#> 12 0.658 0.898 0.6527 2.180 2.33e-01 1.02e-01  4.899
#> 13 0.783 0.852 1.2839 1.751 2.36e-02 6.01e-03 17.646
#> 14 0.832 0.715 1.6023 0.919 2.53e-03 6.43e-01  3.136
#> 15 0.730 0.527 0.9926 0.107 4.27e-01 1.50e+00  0.793
#> 16 0.823 0.842 1.5373 1.675 3.34e-01 3.83e-01  3.793
#> 17 0.723 0.752 0.9596 1.107 3.87e-01 2.17e-02  3.233
#> 18 0.945 0.812 2.8408 1.466 2.12e+00 1.21e-01  2.880
#> 19 0.890 0.588 2.0940 0.357 1.84e-07 1.81e-02 18.205
#> 20 0.652 0.638 0.6265 0.565 4.30e-01 1.59e+00  0.840
#> 21 0.778 0.884 1.2564 2.035 1.05e+00 1.27e-01  3.031
#> 22 0.884 0.818 2.0341 1.503 6.82e-01 1.42e-01  3.898
#> 23 0.872 0.948 1.9203 2.907 7.82e-03 1.59e+00  3.821
#> 24 0.802 0.888 1.3998 2.072 4.99e-02 4.05e-01  5.145
#> 25 0.686 0.953 0.7832 3.002 4.24e-03 9.18e-01  3.941
#> 26 0.857 0.655 1.7897 0.643 5.20e-01 1.89e+00  1.569
#> 27 0.939 0.741 2.7324 1.049 2.82e-01 2.65e-05  7.122
#> 28 0.713 0.892 0.9117 2.114 9.16e-01 1.84e-01  2.885
#> 29 0.762 0.837 1.1647 1.638 5.89e-03 2.98e-01  5.085
#> 30 0.851 0.888 1.7437 2.067 7.76e-01 1.55e+00  2.497
#> 
#> $opt
#> $par
#>      u1      u2      t1      t2     t12       r     auc       b       a     c11 
#>  1.4867  1.3528  0.3227  0.4740 -0.1530 -1.0000  0.8738  0.4261 -1.0260  0.0127 
#>     c22   p.hat 
#>  0.9873  0.5000 
#> 
#> $value
#> [1] -5.09
#> 
#> $counts
#> function gradient 
#>       40       40 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $auc
#> 0.874 with absolute error < 6.5e-05
#> 
#> attr(,"class")
#> [1] "DTAsens"
```

For a series of selection probabilities, e.g., p = 0.9, 0.8, …,0.1

``` r
p.seq <- seq(0.9, 0.1, -0.1)

estimates <- sapply(p.seq, function(p) dtasens2(dta, p)$par)
colnames(estimates)<- paste0("p = ", p.seq)
kable(estimates)
```

|     | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :-- | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| u1  |   1.455 |   1.465 |   1.478 |   1.488 |   1.487 |   1.494 |   1.506 |   1.520 |   1.540 |
| u2  |   1.438 |   1.414 |   1.387 |   1.361 |   1.353 |   1.330 |   1.300 |   1.263 |   1.213 |
| t1  |   0.319 |   0.321 |   0.323 |   0.324 |   0.323 |   0.324 |   0.325 |   0.327 |   0.329 |
| t2  |   0.482 |   0.482 |   0.483 |   0.482 |   0.474 |   0.471 |   0.469 |   0.467 |   0.465 |
| t12 | \-0.154 | \-0.155 | \-0.156 | \-0.156 | \-0.153 | \-0.153 | \-0.153 | \-0.153 | \-0.153 |
| r   | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 | \-1.000 |
| auc |   0.876 |   0.875 |   0.875 |   0.874 |   0.874 |   0.873 |   0.872 |   0.871 |   0.870 |
| b   |   0.893 |   0.584 |   0.526 |   0.485 |   0.426 |   0.409 |   0.396 |   0.379 |   0.347 |
| a   |   0.085 | \-0.225 | \-0.522 | \-0.773 | \-1.026 | \-1.272 | \-1.524 | \-1.803 | \-2.163 |
| c11 |   0.000 |   0.000 |   0.000 |   0.000 |   0.013 |   0.020 |   0.024 |   0.025 |   0.027 |
| c22 |   1.000 |   1.000 |   1.000 |   1.000 |   0.987 |   0.980 |   0.976 |   0.975 |   0.973 |

### Example data 2

``` r
data("IVD")

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

### Plot ROC

One ROC

``` r
par(mfrow = c(1,2))

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
fit <- reitsma(IVD)
plot(fit)
title("Reistma model from mada")


par0 <- c(fit$coefficients*c(1,-1), c(1,-1)*sqrt(fit$Psi[c(1,4)]), fit$Psi[3]/prod(sqrt(fit$Psi[c(1,4)])))
par1 <- dtasens1(IVD, 0.9)$par[c(1:4,6)]
par2 <- dtasens2(IVD, 0.9)$par[c(1:4,6)]

sROC(par1, pch = 19)
sROC(par2, add = TRUE, col=2, pch = 19)
sROC(par0, add = TRUE, col=2, pch = 19, lty = 3)

with(IVD, points(FP/(FP+TN), TP/(TP+FN), pch = 19, cex = 0.5))
legend("bottomright", c("dtasens1", "dtasens2"), col = c(1,2,2), lty = c(1,1,3), pch = c(19,19,1))
title("When selection prob = 0.9")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
par(mfrow = c(1,1))
```

A series of ROC

``` r

p.seq <- seq(0.9, 0.1, -0.1)
estimates1 <- sapply(p.seq, function(p) dtasens1(IVD, p)$par)[c(1:4,6),]
estimates2 <- sapply(p.seq, function(p) dtasens2(IVD, p)$par)[c(1:4,6),]

par(mfrow = c(1,2))
sROC.bunch(par.matrix = estimates1)
sROC(par0, add = TRUE, col = 2, pch = 19, lty =3)

title("dtasens1")

sROC.bunch(par.matrix = estimates2, p.vec = p.seq)
sROC(par0, add = TRUE, col = 2, pch = 19, lty =3)

title("dtasens2")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
par(mfrow = c(1,1))
```
