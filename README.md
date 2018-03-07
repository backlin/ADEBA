Adaptive density estimation by Bayesian averaging
=====
This repo contains an R implementation of the ADEBA density estimation method
and a benchmark study comparing it to a number of other methods.

[Original publication here](https://www.sciencedirect.com/science/article/pii/S0031320318300062).
Notice that the link only access a preview of the paper,
wihch is accepted for publication in June 2018
(Pattern Recognition Volume 78, June 2018, Pages 133-143, Pages 133-143).


## Installation
Latest release version:

```
R> install.packages("adeba")
```

Latest dev version (currently the same):

```
devtools::install_github("backlin/adeba/adeba")
```

Some extra plot functionality is provided in the support package
[`adebaExtra`](./adebaExtra) (not on CRAN).

```
devtools::install_github("backlin/adeba/adebaExtra")
```

## Key features
The ADEBA family of estimators has two features that differentiates it from traditional
estimators (those included in the base distributions of R and Matlab).
These are demonstrated below.


### Adaptive bandwith estimation
ADEBA has two hyperparameters, one that sets the overall bandwidth and one that adjusts
it to the local data density of each kernel. This allows it to:

Capture sharp modes:

```
pic here plz
```

Remove spurious modes in the tails:

![Univariate example](example_adaptive.png)



### Multivariate estimation
ADEBA extends naturally to multivariate densities.

