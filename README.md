
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
#>  1.299  1.717  0.552  0.842 -0.343 -0.159  2.000 -4.308  0.500  0.500  0.836 
#>     se     sp 
#>  0.786  0.848 
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.299  1.017 1.576
#> u2    1.717  1.436 1.993
#> t1    0.552  0.275 0.933
#> t2    0.842  0.643 1.149
#> r    -0.343 -0.746 0.164
#> b     2.000  0.929    NA

## If we change b.interval

dtasens1(IVD, p = 0.7, b.interval = c(0,1))
#> $par
#>     u1     u2     t1     t2      r    t12      b      a    c11    c22    auc 
#>  1.288  1.707  0.548  0.842 -0.344 -0.159  1.000 -2.106  0.500  0.500  0.834 
#>     se     sp 
#>  0.784  0.846 
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.288  1.000 1.574
#> u2    1.707  1.420 1.992
#> t1    0.548  0.272 0.930
#> t2    0.842  0.643 1.149
#> r    -0.344 -0.747 0.164
#> b     1.000  0.623    NA

## To get full results list

opt1 <- dtasens1(IVD, p = 0.7)

str(opt1)
#> List of 12
#>  $ par        : Named num [1:13] 1.299 1.717 0.552 0.842 -0.343 ...
#>   ..- attr(*, "names")= chr [1:13] "u1" "u2" "t1" "t2" ...
#>  $ objective  : num 20.9
#>  $ convergence: int 0
#>  $ iterations : int 18
#>  $ evaluations: Named int [1:2] 30 145
#>   ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>  $ message    : chr "relative convergence (4)"
#>  $ ci         : num [1:6, 1:3] 1.299 1.717 0.552 0.842 -0.343 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:6] "u1" "u2" "t1" "t2" ...
#>   .. ..$ : chr [1:3] "estimate" "ci.low" "ci.up"
#>  $ auc.all    :List of 5
#>   ..$ value       : num 0.836
#>   ..$ abs.error   : num 8.84e-07
#>   ..$ subdivisions: int 10
#>   ..$ message     : chr "OK"
#>   ..$ call        : language integrate(f = function(x) {     plogis(u1 - (r * t1/t2) * (qlogis(x) + u2)) ...
#>   ..- attr(*, "class")= chr "integrate"
#>  $ p.hat      : num 0.7
#>  $ data       :'data.frame': 33 obs. of  7 variables:
#>   ..$ sens  : num [1:33] 0.962 0.808 0.921 0.964 0.9 ...
#>   ..$ spec  : num [1:33] 0.908 0.833 0.701 0.785 0.913 ...
#>   ..$ y1    : num [1:33] 3.22 1.44 2.46 3.3 2.2 ...
#>   ..$ y2    : num [1:33] 2.284 1.609 0.851 1.294 2.35 ...
#>   ..$ v1    : num [1:33] 2.08 0.495 0.724 2.074 2.222 ...
#>   ..$ v2    : num [1:33] 0.0374 0.0828 0.0391 0.0689 0.0509 ...
#>   ..$ ldor.t: num [1:33] 3.78 4 3.79 3.14 3.02 ...
#>  $ func.name  : chr "dtasens1"
#>  $ pars.info  :List of 12
#>   ..$ p                : num 0.7
#>   ..$ c1               : num 0.707
#>   ..$ correct.value    : num 0.5
#>   ..$ correct.type     : chr "all"
#>   ..$ start5           : NULL
#>   ..$ b0               : num 0.1
#>   ..$ b.interval       : num [1:2] 0 2
#>   ..$ a.interval       : num [1:2] -5 3
#>   ..$ pos.r            : logi TRUE
#>   ..$ ci.level         : num 0.95
#>   ..$ show.warn.message: logi FALSE
#>   ..$ a.root.extendInt : chr "downX"
#>  - attr(*, "class")= chr "dtasens"
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
| u1  |   1.388 |   1.383 |   1.352 |   1.299 |   1.230 |   1.146 |   1.040 |   0.902 |   0.707 |   0.322 |
| u2  |   1.804 |   1.799 |   1.769 |   1.717 |   1.645 |   1.553 |   1.430 |   1.267 |   1.036 |   0.584 |
| t1  |   0.545 |   0.548 |   0.550 |   0.552 |   0.554 |   0.561 |   0.578 |   0.606 |   0.649 |   0.745 |
| t2  |   0.819 |   0.822 |   0.830 |   0.842 |   0.857 |   0.876 |   0.903 |   0.940 |   0.990 |   1.085 |
| r   | \-0.423 | \-0.412 | \-0.382 | \-0.343 | \-0.294 | \-0.233 | \-0.151 | \-0.050 |   0.069 |   0.252 |
| t12 | \-0.189 | \-0.186 | \-0.175 | \-0.159 | \-0.140 | \-0.114 | \-0.079 | \-0.029 |   0.044 |   0.204 |
| b   |   0.100 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   1.915 |   1.759 |   1.674 |
| a   |  10.650 | \-2.556 | \-3.647 | \-4.308 | \-4.781 | \-5.151 | \-5.456 | \-5.506 | \-5.353 | \-5.358 |
| c11 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |
| c22 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |   0.500 |
| auc |   0.859 |   0.857 |   0.849 |   0.836 |   0.819 |   0.795 |   0.763 |   0.720 |   0.659 |   0.554 |
| se  |   0.800 |   0.799 |   0.794 |   0.786 |   0.774 |   0.759 |   0.739 |   0.711 |   0.670 |   0.580 |
| sp  |   0.859 |   0.858 |   0.854 |   0.848 |   0.838 |   0.825 |   0.807 |   0.780 |   0.738 |   0.642 |

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
#>  1.297  1.719  0.552  0.841 -0.342 -0.159  2.000 -4.257  0.514  0.486  0.836 
#>     se     sp 
#>  0.785  0.848 
#> 
#> $ci
#>    estimate ci.low ci.up
#> u1    1.297  1.015 1.574
#> u2    1.719  1.438 1.995
#> t1    0.552  0.275 0.936
#> t2    0.841  0.642 1.147
#> r    -0.342 -0.745 0.164
#> b     2.000  0.937    NA
#> c1    0.717  0.411 0.919

## To get full results list

opt2 <- dtasens2(IVD, p = 0.7)

str(opt2)
#> List of 12
#>  $ par        : Named num [1:13] 1.297 1.719 0.552 0.841 -0.342 ...
#>   ..- attr(*, "names")= chr [1:13] "u1" "u2" "t1" "t2" ...
#>  $ objective  : num 20.9
#>  $ convergence: int 0
#>  $ iterations : int 24
#>  $ evaluations: Named int [1:2] 37 212
#>   ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>  $ message    : chr "relative convergence (4)"
#>  $ ci         : num [1:7, 1:3] 1.297 1.719 0.552 0.841 -0.342 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:7] "u1" "u2" "t1" "t2" ...
#>   .. ..$ : chr [1:3] "estimate" "ci.low" "ci.up"
#>  $ auc.all    :List of 5
#>   ..$ value       : num 0.836
#>   ..$ abs.error   : num 8.84e-07
#>   ..$ subdivisions: int 10
#>   ..$ message     : chr "OK"
#>   ..$ call        : language integrate(f = function(x) {     plogis(u1 - (r * t1/t2) * (qlogis(x) + u2)) ...
#>   ..- attr(*, "class")= chr "integrate"
#>  $ p.hat      : num 0.7
#>  $ data       :'data.frame': 33 obs. of  7 variables:
#>   ..$ sens  : num [1:33] 0.962 0.808 0.921 0.964 0.9 ...
#>   ..$ spec  : num [1:33] 0.908 0.833 0.701 0.785 0.913 ...
#>   ..$ y1    : num [1:33] 3.22 1.44 2.46 3.3 2.2 ...
#>   ..$ y2    : num [1:33] 2.284 1.609 0.851 1.294 2.35 ...
#>   ..$ v1    : num [1:33] 2.08 0.495 0.724 2.074 2.222 ...
#>   ..$ v2    : num [1:33] 0.0374 0.0828 0.0391 0.0689 0.0509 ...
#>   ..$ ldor.t: num [1:33] 3.78 4 3.79 3.14 3.02 ...
#>  $ func.name  : chr "dtasens2"
#>  $ pars.infor :List of 12
#>   ..$ p                : num 0.7
#>   ..$ correct.value    : num 0.5
#>   ..$ correct.type     : chr "all"
#>   ..$ start5           : NULL
#>   ..$ b0               : num 0.1
#>   ..$ c10              : num 0.707
#>   ..$ b.interval       : num [1:2] 0 2
#>   ..$ a.interval       : num [1:2] -5 3
#>   ..$ pos.r            : logi TRUE
#>   ..$ ci.level         : num 0.95
#>   ..$ show.warn.message: logi FALSE
#>   ..$ a.root.extendInt : chr "downX"
#>  - attr(*, "class")= chr "dtasens"
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
| u1  |   1.388 |   1.382 |   1.349 |   1.297 |   1.236 |   1.168 |   1.086 |   0.998 |   0.867 |   0.624 |
| u2  |   1.804 |   1.800 |   1.773 |   1.719 |   1.640 |   1.535 |   1.396 |   1.215 |   0.938 |   0.405 |
| t1  |   0.545 |   0.549 |   0.552 |   0.552 |   0.551 |   0.552 |   0.558 |   0.564 |   0.582 |   0.624 |
| t2  |   0.819 |   0.821 |   0.828 |   0.841 |   0.859 |   0.884 |   0.919 |   0.963 |   1.029 |   1.154 |
| r   | \-0.423 | \-0.412 | \-0.382 | \-0.342 | \-0.297 | \-0.243 | \-0.174 | \-0.104 | \-0.003 |   0.156 |
| t12 | \-0.189 | \-0.186 | \-0.175 | \-0.159 | \-0.141 | \-0.119 | \-0.089 | \-0.056 | \-0.002 |   0.112 |
| b   |   0.100 |   2.000 |   2.000 |   2.000 |   2.000 |   2.000 |   1.994 |   1.801 |   1.683 |   1.600 |
| a   |  10.650 | \-2.176 | \-3.440 | \-4.257 | \-4.870 | \-5.352 | \-5.723 | \-5.566 | \-5.537 | \-5.580 |
| c11 |   0.500 |   0.615 |   0.558 |   0.514 |   0.477 |   0.450 |   0.431 |   0.408 |   0.393 |   0.378 |
| c22 |   0.500 |   0.385 |   0.442 |   0.486 |   0.523 |   0.550 |   0.569 |   0.592 |   0.607 |   0.622 |
| auc |   0.859 |   0.857 |   0.849 |   0.836 |   0.819 |   0.799 |   0.773 |   0.744 |   0.704 |   0.643 |
| se  |   0.800 |   0.799 |   0.794 |   0.785 |   0.775 |   0.763 |   0.748 |   0.731 |   0.704 |   0.651 |
| sp  |   0.859 |   0.858 |   0.855 |   0.848 |   0.838 |   0.823 |   0.802 |   0.771 |   0.719 |   0.600 |

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
To save computing time, we set bootstrapping times as 5 (`B = 5`). Hence
the results are not reliable.

#### 1\. Single sROC

``` r
## Use dtasens object

sAUC(opt1)
#> 0.836 with absolute error < 8.8e-07
sAUC(opt2)
#> 0.836 with absolute error < 8.8e-07

## Use parameter vector
par1 <- opt1$par
par2 <- opt2$par
sAUC(par1)
#> 0.836 with absolute error < 8.8e-07
sAUC(par2)
#> 0.836 with absolute error < 8.8e-07


par(mfrow = c(1,2))

sROC(opt1)
title("dtasens1")
sROC(opt2)
title("dtasens2")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r

par(mfrow = c(1,1))
```

#### 2\. Single sROC with CI

``` r

## Calculate Parametric Bootstrap CI
## B (Bootstrapping times) is suggested to be 1000. To save computing time, we use B = 10 to show the functionality.
par(mfrow = c(1,2))

sAUC.ci(opt1, B=5, plot.ROC.ci = TRUE, hide.progress = TRUE)
#>  sAUC  CI.L  CI.L 
#> 0.836 0.791 0.879
title("dtasens1")
sAUC.ci(opt2, B=5, plot.ROC.ci = TRUE, hide.progress = TRUE)
#>  sAUC  CI.L  CI.L 
#> 0.836 0.789 0.892
title("dtasens2")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r

par(mfrow = c(1,1))
```

#### 3\. Multiple sAUC

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
| sAUC | 0.859 |   0.857 |   0.849 |   0.836 |   0.819 |   0.795 |   0.763 |   0.720 |   0.659 |   0.554 |
| CI.L | 0.809 |   0.770 |   0.817 |   0.735 |   0.746 |   0.773 |   0.697 |   0.631 |   0.540 |   0.371 |
| CI.U | 0.901 |   0.915 |   0.881 |   0.894 |   0.887 |   0.843 |   0.884 |   0.828 |   0.796 |   0.930 |

``` r

colnames(sauc2)<- paste0("p = ", p.seq)
rownames(sauc2)<- c("sAUC", "CI.L", "CI.U")
kable(sauc2)
```

|      | p = 1 | p = 0.9 | p = 0.8 | p = 0.7 | p = 0.6 | p = 0.5 | p = 0.4 | p = 0.3 | p = 0.2 | p = 0.1 |
| :--- | ----: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| sAUC | 0.859 |   0.857 |   0.849 |   0.836 |   0.819 |   0.799 |   0.773 |   0.744 |   0.704 |   0.643 |
| CI.L | 0.814 |   0.840 |   0.799 |   0.759 |   0.760 |   0.750 |   0.478 |   0.704 |   0.575 |   0.422 |
| CI.U | 0.881 |   0.892 |   0.906 |   0.872 |   0.871 |   0.833 |   0.900 |   0.792 |   0.865 |   0.874 |

#### 4\. Plot sAUC

This is an example of how to plot sAUC and CI. In analysis, please set
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

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r

par(mfrow = c(1,1))
```
