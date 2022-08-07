## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
x <- c(0,0.3,0.2,0.1, 10,11,12,13)
(bs.fit <- binsegRcpp::binseg("meanvar_norm", x))

## -----------------------------------------------------------------------------
myvar <- function(y)mean((y-mean(y))^2)
nll <- function(y)-sum(dnorm(y, mean(y), sqrt(myvar(y)), log=TRUE))
expected.loss <- c(
  nll(x),
  nll(x[1:4])+nll(x[5:8]),
  nll(x[1:4])+nll(x[5:6])+nll(x[7:8]),
  nll(x[1:2])+nll(x[3:4])+nll(x[5:6])+nll(x[7:8]))
rbind(binsegRcpp=bs.fit$splits$loss, expected=expected.loss)

## -----------------------------------------------------------------------------
cpt.fit <- changepoint::cpt.meanvar(
  x, penalty="Manual", pen.value=0, method="BinSeg")
changepoint::logLik(cpt.fit)

## -----------------------------------------------------------------------------
changepoint::param.est(cpt.fit)
coef(bs.fit, 2L)

## -----------------------------------------------------------------------------
cpt.fit1 <- changepoint::cpt.meanvar(
  x, penalty="Manual", pen.value=0, method="BinSeg", Q=1)
changepoint::param.est(cpt.fit1)

## -----------------------------------------------------------------------------
rbind(
  changepoint=changepoint::logLik(cpt.fit1)/2,
  binsegRcpp=bs.fit$splits$loss[2])

## -----------------------------------------------------------------------------
coef(bs.fit)

## -----------------------------------------------------------------------------
penaltyLearning::modelSelection(bs.fit$splits, "loss", "segments")

## -----------------------------------------------------------------------------
try(changepoint::cpt.meanvar(
  x, penalty="CROPS", method="BinSeg", pen.value = c(0, Inf)))

## -----------------------------------------------------------------------------
pen.changepoint.list <- list()
for(penalty in seq(0, 50)){
  pen.fit <- changepoint::cpt.meanvar(
    x, penalty="Manual", method="BinSeg", pen.value=penalty)
  pen.changepoint.list[[paste(penalty)]] <- data.frame(
    package="changepoint",
    segments=length(changepoint::cpts(pen.fit))+1L,
    penalty)
}
pen.changepoint <- do.call(rbind, pen.changepoint.list)
library(ggplot2)
(gg.penalty <- ggplot()+
  geom_point(aes(
    penalty, segments, color=package),
    shape=1,
    data=pen.changepoint))

## -----------------------------------------------------------------------------
library(data.table)
models <- data.table(
  package="binsegRcpp+penaltyLearning",
  bs.fit$splits
)[, cpt.loss := loss*2]
pen.df <- penaltyLearning::modelSelection(models, "cpt.loss", "segments")
gg.penalty+
  geom_segment(aes(
    min.lambda, segments,
    color=package,
    xend=max.lambda, yend=segments),
    size=1,
    data=pen.df)

