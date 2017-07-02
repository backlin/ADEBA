# `adebaExtra`: Extra functionality for the ADEBA package
This package contains nice-to-have plots, but since they have some heavy dependencies
(`rgl` and `ggplot2`) they were moved out of the main adeba package.

## Installation
This package is not on CRAN, but can easily be installed using the
`devtools` package.

```
library(devtools)
install_github("backlin/ADEBA/adebaExtra")
```

## Usage
```
library(adebaExtra)
examples(plot_alpha)
examples(plot3d)
```

## Troubleshooting
Some issues I've found using a fresh install of the currently latest R version 3.4.1.

### `plot3d` error: `Error in rgl.clear(type) : object 'rgl_clear' not found`
You probably need to upgrade rgl to the latest version.
If you run Ubuntu check [this StackOverflow post](https://stackoverflow.com/questions/43670145/rgl-error-in-rgl-cleartype-subscene-subscene-object-rgl-clear-not-fo).

```bash
sudo apt-get install libglu1-mesa-dev
wget https://cran.r-project.org/src/contrib/rgl_0.98.1.tar.gz
R CMD INSTALL rgl_0.98.1.tar.gz
```

### `plot_alpha` error about things not being string
Upgrade `plyr` to the latest version (currently 1.8.4).
`ggplot2` depends on this, but if you installed it through the `tidyverse`
package bundle you'll only get `plyr` 1.8.
