#' Perform variable selection using DART VIP Rank
#'
#' @description
#' This function trains `Lrep` (default to 10) DART models (sparse BART with
#' Dirichlet prior on variable selection) using different random seeds, extracts
#' the Variable Inclusion Proportion (VIP) from each fit, and computes average
#' VIP rankings across replications. Hierarchical Agglomerative Clustering (HAC)
#' is then performed on the average VIP rankings to identify important predictors.
#'
#' @param y Response vector. For `mode = "classification"`, must be a numeric
#'   vector with exactly 2 unique values.
#' @param X Predictor or feature matrix.
#' @param seed Random seed.
#' @param Lrep Number of replications.
#' @param backend The R package used to implement DART. Choices are `"BART"` and
#'   `"dartMachine"` (the one used in the paper). The `"dartMachine"` backend is
#'   only supported for `mode = "regression"`.
#' @param mode One of `"regression"` (default) or `"classification"`. When set to
#'   `"classification"`, uses probit BART (`BART::pbart()`) for binary classification
#'   and requires `y` to contain exactly 2 unique values. Only binary classification
#'   is supported.
#'
#' @return A list containing
#' * `pos_idx`: selected variable indices
#' * `vip_avg`: average VIP across replications (vector of length `ncol(X)`)
#' * `vip_rank_avg`: average VIP rank across replications (vector of length `ncol(X)`)
#' @importFrom stats dist hclust cutree
#' @importFrom future.apply future_lapply
#'
#' @examples
#' # Regression example using the `BART` backend
#' \dontrun{
#' library(bayesVC)
#'
#' set.seed(123)
#' n <- 1000
#' p <- 100
#' Lrep <- 10
#'
#' X <- matrix(runif(n * p), n, p)
#' y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
#' eps <- rnorm(n, mean = 0, sd = 2)
#' y <- y_mu + eps
#'
#' result <- DartVIP(y = y, X = X, seed = 123, Lrep = Lrep, backend = "BART")
#' result$pos_idx
#' }
#'
#' # Regression example using the `dartMachine` backend
#' \dontrun{
#' # Allocate memory before loading the package (5 GB here)
#' options(java.parameters = c("-Xmx5g"))
#' library(bayesVC)
#'
#' set.seed(123)
#' n <- 1000
#' p <- 100
#' Lrep <- 10
#'
#' X <- matrix(runif(n * p), n, p)
#' y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
#' eps <- rnorm(n, mean = 0, sd = 2)
#' y <- y_mu + eps
#'
#' result <- DartVIP(y = y, X = X, seed = 123, Lrep = Lrep, backend = "dartMachine")
#' result$pos_idx
#' }
#'
#' # Classification example using the `BART` backend
#' \dontrun{
#' library(bayesVC)
#'
#' set.seed(123)
#' n <- 500
#' p <- 100
#' Lrep <- 10
#'
#' X <- matrix(runif(n * p), n, p)
#' mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 +
#'   10 * X[, 4] + 5 * X[, 5]
#' mu_scaled <- (mu - mean(mu)) / sd(mu)
#' y <- rbinom(n, size = 1, prob = pnorm(mu_scaled))
#'
#' result <- DartVIP(
#'   y = y, X = X, seed = 123, Lrep = Lrep, backend = "BART",
#'   mode = "classification"
#' )
#' result$pos_idx
#' }
#'
#' # Parallel processing example using the `future` package
#' \dontrun{
#' library(bayesVC)
#' library(future)
#'
#' plan(multisession)
#'
#' set.seed(123)
#' n <- 1000
#' p <- 100
#' Lrep <- 10
#'
#' X <- matrix(runif(n * p), n, p)
#' y_mu <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
#' eps <- rnorm(n, mean = 0, sd = 2)
#' y <- y_mu + eps
#'
#' result <- DartVIP(y = y, X = X, seed = 123, Lrep = Lrep, backend = "BART")
#' result$pos_idx
#'
#' plan(sequential)
#' }

#' @export
DartVIP <- function(y, X, seed = 123, Lrep = 10,
                    backend = c("dartMachine", "BART"),
                    mode = c("regression", "classification")) {
  backend <- match.arg(backend)
  mode <- match.arg(mode)

  if (mode == "classification" && backend == "dartMachine") {
    stop("'dartMachine' backend does not support mode = \"classification\". Use backend = \"BART\".")
  }

  if (!is.numeric(y) || !is.null(dim(y))) {
    stop("'y' must be a numeric vector.")
  }
  if (anyNA(y)) {
    stop("'y' contains NA values.")
  }
  if (mode == "classification" && length(unique(y)) != 2L) {
    stop("Only binary classification is supported. 'y' must contain exactly 2 unique values.")
  }

  if (!is.matrix(X) || !is.numeric(X)) {
    stop("'X' must be a numeric matrix.")
  }
  if (anyNA(X)) {
    stop("'X' contains NA values.")
  }

  if (nrow(X) != length(y)) {
    stop(sprintf("nrow(X) (%d) must equal length(y) (%d).", nrow(X), length(y)))
  }

  set.seed(seed)
  seeds <- sample.int(10000, size = Lrep)

  rng_states <- lapply(seeds, function(s) {
    set.seed(s)
    .Random.seed
  })

  if (backend == "dartMachine") {
    if (!requireNamespace("dartMachine", quietly = TRUE)) {
      stop("Package dartMachine is required but not installed. Please install it from https://github.com/theodds/dartMachine")
    }
    worker <- DartVIP_dartMachine
    X <- as.data.frame(X)
  } else {
    if (!requireNamespace("BART", quietly = TRUE)) {
      stop("Package BART is required but not installed. Please install it with install.packages('BART').")
    }
    worker <- if (mode == "classification") DartVIP_pbart else DartVIP_BART
  }

  vip_list <- future_lapply(seq_len(Lrep), worker,
    y = y, Xmat = X, seeds = seeds,
    future.seed = rng_states
  )

  vip <- do.call(rbind, vip_list)

  # Average VIP across replications
  vip_avg <- colMeans(vip, na.rm = TRUE)

  # Average VIP rank across replications (rank 1 = most included)
  vip_rank <- t(apply(vip, 1, function(x) rank(-x)))
  vip_rank_avg <- colMeans(vip_rank, na.rm = TRUE)

  # Hierarchical clustering on the average rank vector
  rank_dist <- dist(vip_rank_avg, method = "euclidean")
  hclust_result <- hclust(rank_dist, method = "average")

  # Cut the tree to obtain 2 clusters
  clusters <- cutree(hclust_result, k = 2)
  cluster_means <- tapply(vip_rank_avg, clusters, mean)

  # Select the cluster with the smallest mean rank (= best-ranked variables)
  pos_cls_id <- as.numeric(names(which.min(cluster_means)))
  pos_idx <- as.integer(which(clusters == pos_cls_id))

  return(list(pos_idx = pos_idx, vip_avg = vip_avg, vip_rank_avg = vip_rank_avg))
}

DartVIP_dartMachine <- function(l, y, Xmat, seeds) {
  temp_output <- tempfile()
  sink(temp_output)
  on.exit({
    sink()
    unlink(temp_output)
  })
  dm <- dartMachine::bartMachine(
    X = Xmat,
    y = y,
    num_trees = 20,
    num_burn_in = 5000,
    num_iterations_after_burn_in = 5000,
    run_in_sample = FALSE,
    serialize = FALSE,
    seed = seeds[l],
    verbose = FALSE,
    do_ard = TRUE,
    do_prior = TRUE
  )
  dartMachine::get_var_props_over_chain(dm)
}

DartVIP_BART <- function(l, y, Xmat, ...) {
  temp_output <- tempfile()
  sink(temp_output)
  on.exit({
    sink()
    unlink(temp_output)
  })
  bart <- BART::wbart(
    x.train    = Xmat,
    y.train    = y,
    sparse     = TRUE,
    ntree      = 20,
    nskip      = 5000,
    ndpost     = 5000,
    printevery = 10000
  )
  # Normalise each MCMC draw's variable counts to proportions,
  # then average across draws. na.rm guards against all-zero draws.
  vip <- apply(bart$varcount, 1, function(x) x / sum(x)) # p x ndpost
  rowMeans(vip, na.rm = TRUE)
}

DartVIP_pbart <- function(l, y, Xmat, ...) {
  temp_output <- tempfile()
  sink(temp_output)
  on.exit({
    sink()
    unlink(temp_output)
  })
  dart <- BART::pbart(
    x.train    = Xmat,
    y.train    = y,
    sparse     = TRUE,
    ntree      = 20,
    nskip      = 5000,
    ndpost     = 5000,
    printevery = 10000
  )
  # Normalise each MCMC draw's variable counts to proportions,
  # then average across draws. na.rm guards against all-zero draws.
  vip <- apply(dart$varcount, 1, function(x) x / sum(x)) # p x ndpost
  rowMeans(vip, na.rm = TRUE)
}
