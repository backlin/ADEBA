Adaptive density estimation by Bayesian averaging
=====
This repo contains an R implementation of the ADEBA density estimation method
and a benchmark study comparing it to a number of other methods.

More details on ADEBA and a reference to the original article will be posted here
once it has been accepted by a scientific journal (at the moment it is in submission).

## Installation
Latest release version:

```
R> install.packages("CRAN")
```

Latest dev version (currently the same):

```
library(devtools)
install_github("backlin/ADEBA/adeba")
```

Some extra plot functionality is provided in the support package [`adebaExtra`](./adebaExtra).
