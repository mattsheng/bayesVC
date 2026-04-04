
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesVC

<!-- badges: start -->

<!-- badges: end -->

bayesVC implements the VC-measure based nonparametric variable selection
method introduce in [*Posterior Summarization for Variable Selection in
Bayesian Tree Ensembles*](https://arxiv.org/abs/2509.07121).

The current package utilizes Dirichlet Additive Regression Trees
([DART](https://www.tandfonline.com/doi/full/10.1080/01621459.2016.1264957))
as the nonparametric model, though other Bayesian tree ensemble models
can also be used. We refer to VC-measure with DART backend as **DART
VC-measure**.

## Installation

You can install the development version of `bayesVC` from
[GitHub](https://github.com/mattsheng/bayesVC) with:

``` r
# install.packages("pak")
pak::pkg_install("mattsheng/bayesVC")
```

## Backends

The bayesVC R package currently supports two flavors of DART
implementations:

- `BART`: installed automatically with `bayesVC`, or can be easily
  installed via `install.packages('BART')`
- `dartMachine`: requires installation from
  <https://github.com/theodds/dartMachine>

## Example

We demonstrate DART VC-measure using the Friedman equation:

$$y = 10\sin(\pi x_1x_2) + 20(x_3-0.5)^2 + 10x_4 + 5x_5 + \varepsilon,$$

where $\varepsilon \sim N(0, 2^2)$. Predictors $x_1,\ldots,x_{100}$ are
sampled iid from an Uniform(0, 1) distribution. The goal is to identify
the truly relevant predictors, namely $(x_1,x_2,x_3,x_4,x_5)$, from all
$p=100$ predictors.

To use the `BART` backend, simply load the `bayesVC` package and run the
following code:

``` r
library(bayesVC)

set.seed(123)
n <- 1000
p <- 100
Lrep <- 10

# Generate data (y, X)
X <- matrix(runif(n * p), n, p)
y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
eps <- rnorm(n, mean = 0, sd = 2)
y <- y_mu + eps

# Perform DART VC-measure using `BART` backend
VC_result <- DartVC(
  y = y, 
  X = X, 
  seed = 123, 
  Lrep = Lrep, 
  backend = "BART"
)

# Examine the selected predictors
VC_result$pos_idx
#> [1] 1 2 3 4 5
```

To use the `dartMachine` backend, we must allocate memory for Java
before loading the `bayesVC` package:

``` r
# Must allocate memory before loading `bayesVC` package when using `dartMachine` backend
# Here I allocated 5GB of memory for Java
options(java.parameters = c("-Xmx5g"))
library(bayesVC)

set.seed(123)
n <- 1000
p <- 100
Lrep <- 10

# Generate data (y, X)
X <- matrix(runif(n * p), n, p)
y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
eps <- rnorm(n, mean = 0, sd = 2)
y <- y_mu + eps

# Perform DART VC-measure using `dartMachine` backend
VC_result <- DartVC(
  y = y, 
  X = X, 
  seed = 123, 
  Lrep = Lrep, 
  backend = "dartMachine"
)

# Examine the selected predictors
VC_result$pos_idx
```

## Parallel processing

`DartVC` runs the `Lrep` replications via
[`future.apply::future_lapply`](https://future.apply.futureverse.org/),
so parallelism is controlled by the
[`future`](https://future.futureverse.org/) package. By default it runs
sequentially. To use all available cores, set a `multisession` plan
before calling `DartVC`:

``` r
library(future)
library(bayesVC)

# Use all available cores
plan(multisession)

set.seed(123)
n <- 1000
p <- 100
Lrep <- 10

X <- matrix(runif(n * p), n, p)
y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
eps <- rnorm(n, mean = 0, sd = 2)
y <- y_mu + eps

VC_result <- DartVC(
  y = y,
  X = X,
  seed = 123,
  Lrep = Lrep,
  backend = "BART"
)
VC_result$pos_idx

# Restore sequential execution
plan(sequential)
```

## Reproduce paper results

### Submit simulations

Experiments for each method can be launched by running the bash script
files from the [experiment/](experiment/) folder. For instance, running

``` bash
bash submit_DART_VC-measure.sh
```

within the [experiment/](experiment/) folder will submit all simulations
for the DART VC-measure method. Raw results, e.g., VC and VC Rank
matrices for DART VC-measure, will be stored as JSON files under the
[results/](results/) folder.

### Collect raw results

When all raw simulation results are available, simply run the Jupyter
notebook
[postprocessing/json_collection.ipynb](postprocessing/json_collection.ipynb)
to collect all JSON files and prepare them for the variable selection
step. Collected results will be stored in
[results/no_idx/](results/no_idx/) as `.feather` files.

### Perform variable selection

The Jupyter notebook
[postprocessing/variable_selection.ipynb](postprocessing/variable_selection.ipynb)
performs variable selection for some of the methods, e.g., the original
DART, DART VC-measure, etc., and calculates the selection accuracy for
all methods. Selection accuracy results and the selected variables for
each simulation and method are stored as `.feather` files under
[results/with_idx/](results/with_idx/)

### Reproduce paper figures

R functions in the [postprocessing/](postprocessing/) folders reproduce
all figures in the paper. For instance,
[postprocessing/fig3_12_13.R](postprocessing/fig3_12_13.R) reproduces
Figures 3, 12, and 13 in the paper.

## Reference

Ye, S. and Li, M. (2025) Posterior Summarization for Variable Selection
in Bayesian Tree Ensembles. [arXiv](https://arxiv.org/abs/2509.07121)
