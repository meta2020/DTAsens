
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DTAmetasa: Sensitivity Analysis for Publication Bias in Meta-analysis of Diagnostic Test Accuracy

<!-- badges: start -->
<!-- badges: end -->

[`dtametasa`](https://meta2020.github.io/dtametasa/) is for
likelihood-based sensitivity analysis for publication bias on SROC/SAUC
in meta-analysis of diagnostic studies

## Installation

You can install the released version from
[GitHub](https://github.com/meta2020/dtametasa) with:

``` r

# install.packages("devtools")
devtools::install_github("meta2020/dtametasa")
library(dtametasa)
```

## Data Format

Two formats of data are applicable:

- Data with `TP, FN, FP, TN` as column names

- Data after logit-transformation with `y1, y2, v1, v2` as column names

- The Rshiny application can be found [here](https://alain003.phs.osaka-u.ac.jp/mephas_web/11DTA-Meta/).

<span style="color:red">**The column names of data must be either of the
above.**</span>

<!-- ## Example -->
<!-- This is an example which shows you how to solve a common problem.  -->
<!-- To take the data `IVD` as example, print the first several lines of data.  -->
<!-- ```{r} -->
<!-- ## Load package -->
<!-- library(dtametasa) -->
<!-- ## Load data -->
<!-- data(IVD) -->
<!-- kable(head(IVD)) -->
<!-- data(IVD2) -->
<!-- kable(head(IVD2)) -->
<!-- ``` -->
<!-- ### Main function 1: dtametasa.fc -->
<!-- This function need to pre-specify the c contrast in the selection function.  -->
<!-- ##### 1. Given a certain selection probability $p$, say, $p = 0.7$, we can get the estimation as follows. -->
<!-- ```{r} -->
<!-- ## Use default parameters setting -->
<!-- ## Print parameters' estimates -->
<!-- (sa1 <- dtametasa.fc(IVD, p = 0.7, b.interval = c(0, 2))) -->
<!-- ## If we change b.interval -->
<!-- (sa1 <- dtametasa.fc(IVD, p = 0.7, b.interval = c(0, 5))) -->
<!-- ## Use str() to get full results list -->
<!-- # str(sa1) -->
<!-- ``` -->
<!-- ##### 2. Given a series of selection probabilities, say, $p = 1, 0.9, 0,8, ...,0.1$.  -->
<!-- Attention: **$p$ must greater than 0 and cannot equal to 0. ($p>0$).** -->
<!-- ```{r} -->
<!-- ## Set p vectors -->
<!-- p.seq <- seq(1, 0.1, -0.1) -->
<!-- ## Get estimations for each p in p.seq vector -->
<!-- est1 <- sapply(p.seq, function(p) dtametasa.fc(IVD, p, b.interval = c(0, 2))$par.all) -->
<!-- ## Print estimation -->
<!-- colnames(est1)<- paste0("p = ", p.seq) -->
<!-- kable(est1) -->
<!-- ``` -->
<!-- ### Main function 2: dtametasa.rc -->
<!-- This function do not need to pre-specify the c contrast in the selection function.  -->
<!-- ##### 1. Given a certain selection probability $p$, say, $p = 0.7$, we can get the estimation as follows. -->
<!-- ```{r} -->
<!-- ## Use default parameters setting -->
<!-- ## Print parameters' estimates -->
<!-- (sa2 <- dtametasa.rc(IVD, p = 0.7)) -->
<!-- ## To get full results list -->
<!-- # str(sa2) -->
<!-- ``` -->
<!-- ##### 2. Given a series of selection probabilities, say, $p = 1, 0.9, 0,8, ...,0.1$.  -->
<!-- Attention: **$p$ must greater than 0 and cannot equal to 0. ($p>0$).** -->
<!-- ```{r} -->
<!-- ## Set p vectors -->
<!-- p.seq <- seq(1, 0.1, -0.1) -->
<!-- ## Get estimations for each p in p.seq vector -->
<!-- est2 <- sapply(p.seq, function(p) dtametasa.rc(IVD, p, b.interval = c(0, 2))$par.all) -->
<!-- ## Print estimation -->
<!-- colnames(est2)<- paste0("p = ", p.seq) -->
<!-- kable(est2) -->
<!-- ``` -->
<!-- ### Plot sroc -->
<!-- ##### 1. Single sroc -->
<!-- ```{r, fig.height=5, fig.width=10} -->
<!-- par(mfrow = c(1,2)) -->
<!-- ## This is the standard method: Reistma model -->
<!-- ## Without taking publication bias (PB) into consideration -->
<!-- library(mada) -->
<!-- fit <- reitsma(IVD, correction.control = "all", method = "ml") -->
<!-- plot(sroc(fit, type = "naive"), type = "l", ylim = c(0,1), xlim = c(0,1), col = "red") -->
<!-- ## Extact the estimation from Reistma model -->
<!-- par0 <- c(c(1,-1)*fit$coefficients, sqrt(fit$Psi[c(1,4)]), -fit$Psi[2]/prod(sqrt(fit$Psi[c(1,4)]))) -->
<!-- ## Add sroc -->
<!-- sroc.vec(sa1, add = TRUE, sroc.col = "black", sroc.lty = 1, spoint.pch = 1, spoint.col = "black") -->
<!-- with(IVD, points(FP/(FP+TN), TP/(TP+FN), pch = 4, cex = 0.5)) -->
<!-- legend("bottomright", c("Reistma", "dtametasa.fc", "IVD"),  -->
<!--        col = c("red", "black", "black"), lty = c(1,2, 0), pch = c(19,1, 4)) -->
<!-- title("When selection prob = 0.7, c1 = c2") -->
<!-- sroc.vec(par0, sroc.col = "red", spoint.col ="red") -->
<!-- ## Add sroc -->
<!-- sroc.vec(sa2, add = TRUE, sroc.col = "darkgray", sroc.lty = 1, spoint.col = "darkgray") -->
<!-- with(IVD, points(FP/(FP+TN), TP/(TP+FN), pch = 4, cex = 0.5)) -->
<!-- legend("bottomright", c("Reistma", "dtametasa.rc", "IVD"),  -->
<!--        col = c("red", "darkgray", "black"), lty = c(1, 2, 0), pch = c(19,19, 4)) -->
<!-- title("When selection prob = 0.7, estimate c1 c2") -->
<!-- par(mfrow = c(1,1)) -->
<!-- ``` -->
<!-- ##### 2. Multiple sroc -->
<!-- ```{r, fig.height=5, fig.width=10} -->
<!-- ## p vector and model -->
<!-- p.seq <- seq(1, 0.1, -0.1) -->
<!-- est1 <- sapply(p.seq, function(p) dtametasa.fc(IVD, p)$par) -->
<!-- est2 <- sapply(p.seq, function(p) dtametasa.rc(IVD, p)$par) -->
<!-- ## Plot multiple sroc -->
<!-- par(mfrow = c(1,2)) -->
<!-- sroc.mat(est1[1:5, ]) -->
<!-- sroc.vec(par0, add = TRUE, sroc.col = "red") -->
<!-- title("dtametasa.fc") -->
<!-- sroc.mat(est1[1:5, ]) -->
<!-- sroc.vec(par0, add = TRUE, sroc.col = "red") -->
<!-- title("dtametasa.rc") -->
<!-- par(mfrow = c(1,1)) -->
<!-- ``` -->
<!-- ### Calculate sAUC and confident interval -->
<!-- #### 1. Single sroc with CI -->
<!-- ```{r, fig.width=12, fig.height=6} -->
<!-- ## Use parameter vector -->
<!-- sa1 <- dtametasa.fc(IVD, p = 0.5, b.interval = c(0, 2)) -->
<!-- sa2 <- dtametasa.rc(IVD, p = 0.5, b.interval = c(0, 2)) -->
<!-- par(mfrow = c(1,2)) -->
<!-- sroc.vec(sa1) -->
<!-- title("dtametasa.fc") -->
<!-- sroc.vec(sa2) -->
<!-- title("dtametasa.rc") -->
<!-- par(mfrow = c(1,1)) -->
<!-- ``` -->
<!-- #### 2. Multiple sAUC -->
<!-- ```{r} -->
<!-- p.seq <- seq(1, 0.1, -0.1) -->
<!-- sauc1 <- sapply(p.seq, function(p) dtametasa.fc(IVD, p)$sauc.ci) -->
<!-- sauc2 <- sapply(p.seq, function(p) dtametasa.rc(IVD, p)$sauc.ci) -->
<!-- colnames(sauc1)<- paste0("p = ", p.seq) -->
<!-- kable(sauc1) -->
<!-- colnames(sauc2)<- paste0("p = ", p.seq) -->
<!-- kable(sauc2) -->
<!-- ``` -->
<!-- #### 4. Plot sAUC -->
<!-- This is an example of how to plot sAUC and CI. In analysis, please set `B = 1000` and then reproduce the plots. -->
<!-- ```{r, fig.height=5, fig.width=10} -->
<!-- par(mfrow = c(1,2)) -->
<!-- ## Use matplot to plot the sAUC and CI -->
<!-- matplot(t(sauc1), type = "b", lty = c(1,2,2),  -->
<!--         pch = 19, col = c("black", "grey", "grey"), -->
<!--         xlab = "p", ylab = "sAUC", -->
<!--         ylim = c(0,1), -->
<!--         xaxt = "n") -->
<!-- axis(1, at = 1:10, labels = p.seq) -->
<!-- title("dtametasa.fc") -->
<!-- matplot(t(sauc2), type = "b", lty = c(1,2,2),  -->
<!--         pch = 19, col = c("black", "grey", "grey"), -->
<!--         xlab = "p", ylab = "sAUC", -->
<!--         ylim = c(0,1), -->
<!--         xaxt = "n") -->
<!-- axis(1, at = 1:10, labels = p.seq) -->
<!-- title("dtametasa.rc") -->
<!-- par(mfrow = c(1,1)) -->
<!-- ``` -->
