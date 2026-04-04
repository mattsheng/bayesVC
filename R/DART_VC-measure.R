#' Perform variable selection using DART VC-measure
#'
#' @description
#' This function trains `Lrep` (default to 10) DART models using different random
#' seeds, and extracts the VC-measure from each fit. Hierarchical Agglomerative Clustering (HAC)
#' is then performed on the VC-measure and identifies important predictors.
#'
#' @param y Response vector.
#' @param X Predictor or feature matrix.
#' @param seed Random seed.
#' @param Lrep Number of replications.
#' @param backend The R package used to implement DART. The two choices are `"BART"` and `"dartMachine"` (the one used in the paper).
#'
#' @return A list containing
#' * `pos_idx`: selected variable indices
#' * `Z`: VC-measure summary matrix ([log1p()] transformed follow by [scale()])
#' * `Z_raw`: the raw VC-measure summary matrix
#' @importFrom stats quantile dist hclust cutree
#'
#' @examples
#' #' # Example code using `BART` R package as the backend
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
#' VC_result <- DartVC(
#'   y = y,
#'   X = X,
#'   seed = 123,
#'   Lrep = Lrep,
#'   backend = "BART"
#' )
#' VC_result$pos_idx
#' }
#'
#' # Example code using `dartMachine` R package as the backend
#' \dontrun{
#' # Must allocate memory before loading `bayesVC` package when using `dartMachine` backend
#' # Here I allocated 5GB of memory for Java
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
#' VC_result <- DartVC(
#'   y = y,
#'   X = X,
#'   seed = 123,
#'   Lrep = Lrep,
#'   backend = "dartMachine"
#' )
#' VC_result$pos_idx
#' }
#'

#' @export
DartVC <- function(y, X, seed = 123, Lrep = 10, backend = c("dartMachine", "BART")) {
  set.seed(seed)
  seeds <- sample.int(10000, size = Lrep)

  if (backend == "dartMachine") {
    if (!requireNamespace("dartMachine", quietly = TRUE)) {
      stop("Package dartMachine is required but not installed. Please install it from https://github.com/theodds/dartMachine")
    }
    results <- DartVC_dartMachine(y, X, Lrep, seeds)
  } else if (backend == "BART") {
    if (!requireNamespace("BART", quietly = TRUE)) {
      stop("Package BART is required but not installed. Please install it with install.packages('BART').")
    }
    results <- DartVC_BART(y, X, Lrep, seeds)
  } else {
    stop('backend must be either "dartMachine" or "BART"')
  }

  # Calculate summary statistics
  vc_avg <- colMeans(results$vc)
  vc_q25 <- apply(results$vc, 2, function(x) quantile(x, 0.25))
  vc_rank_avg <- colMeans(results$vc_rank)
  vc_rank_q75 <- apply(results$vc_rank, 2, function(x) quantile(x, 0.75))

  # Combine summary statistics
  Z_raw <- cbind(vc_avg, vc_q25, vc_rank_avg, vc_rank_q75)
  Z <- log1p(Z_raw)
  Z <- scale(Z)

  # Hierarchical clustering on Z
  Z_dist <- dist(Z, method = "euclidean")
  hclust_result <- hclust(Z_dist, method = "average")

  # Cut the tree to obtain 2 clusters
  clusters <- cutree(hclust_result, k = 2)
  cluster_means <- tapply(Z[, 1], clusters, mean)

  # Find predictors corresponding to the cluster with the largest VC average
  pos_cls_id <- as.numeric(names(which.max(cluster_means)))
  pos_idx <- as.integer(which(clusters == pos_cls_id))

  # Return results as a list
  return(list(pos_idx = pos_idx, Z = Z, Z_raw = Z_raw))
}

DartVC_dartMachine <- function(y, X, Lrep, seeds) {
  X <- as.data.frame(X)
  vc <- matrix(NA, nrow = Lrep, ncol = ncol(X))

  # Train Lrep DART models using different seeds
  temp_output <- tempfile()
  sink(temp_output)
  for (l in 1:Lrep) {
    dm <- dartMachine::bartMachine(
      X = X,
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
    vc_full <- dartMachine::get_var_counts_over_chain(dm)
    vc[l, ] <- colMeans(vc_full)
  }
  sink()
  unlink(temp_output)
  vc_rank <- t(apply(vc, 1, function(x) rank(-x)))

  return(list(vc = vc, vc_rank = vc_rank))
}

DartVC_BART <- function(y, X, Lrep, seeds) {
  vc <- matrix(NA, nrow = Lrep, ncol = ncol(X))

  # Train Lrep DART models using different seeds
  temp_output <- tempfile()
  sink(temp_output)
  for (l in 1:Lrep) {
    set.seed(seeds[l])
    dart <- BART::wbart(
      x.train = X,
      y.train = y,
      sparse = TRUE,
      ntree = 20,
      nskip = 5000,
      ndpost = 5000,
      printevery = 10000
    )
    vc[l, ] <- dart$varcount.mean
  }
  sink()
  unlink(temp_output)
  vc_rank <- t(apply(vc, 1, function(x) rank(-x)))

  return(list(vc = vc, vc_rank = vc_rank))
}
