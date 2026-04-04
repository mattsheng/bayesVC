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
#' @importFrom stats dist hclust cutree
#' @importFrom future.apply future_lapply
#' @importFrom matrixStats colQuantiles
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
#' # Parallel processing example using the `future` package
#' \dontrun{
#' library(bayesVC)
#' library(future)
#'
#' # Use all available cores
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
#' VC_result <- DartVC(
#'   y = y,
#'   X = X,
#'   seed = 123,
#'   Lrep = Lrep,
#'   backend = "BART"
#' )
#' VC_result$pos_idx
#'
#' # Restore sequential execution
#' plan(sequential)
#' }
#'

#' @export
DartVC <- function(y, X, seed = 123, Lrep = 10, backend = c("dartMachine", "BART")) {
  backend <- match.arg(backend)

  if (!is.numeric(y) || !is.null(dim(y)))
    stop("'y' must be a numeric vector.")
  if (anyNA(y))
    stop("'y' contains NA values.")

  if (!is.matrix(X) || !is.numeric(X))
    stop("'X' must be a numeric matrix.")
  if (anyNA(X))
    stop("'X' contains NA values.")

  if (nrow(X) != length(y))
    stop(sprintf("nrow(X) (%d) must equal length(y) (%d).", nrow(X), length(y)))

  set.seed(seed)
  seeds <- sample.int(10000, size = Lrep)

  rng_states <- lapply(seeds, function(s) { set.seed(s); .Random.seed })

  if (backend == "dartMachine") {
    if (!requireNamespace("dartMachine", quietly = TRUE)) {
      stop("Package dartMachine is required but not installed. Please install it from https://github.com/theodds/dartMachine")
    }
    worker <- DartVC_dartMachine
    X <- as.data.frame(X)
  } else {
    if (!requireNamespace("BART", quietly = TRUE)) {
      stop("Package BART is required but not installed. Please install it with install.packages('BART').")
    }
    worker <- DartVC_BART
  }

  vc_list <- future_lapply(seq_len(Lrep), worker, y = y, Xmat = X, seeds = seeds,
                           future.seed = rng_states)

  vc <- do.call(rbind, vc_list)
  vc_rank <- t(apply(vc, 1, function(x) rank(-x)))

  # Calculate summary statistics
  vc_avg <- colMeans(vc)
  vc_q25 <- matrixStats::colQuantiles(vc, probs = 0.25)
  vc_rank_avg <- colMeans(vc_rank)
  vc_rank_q75 <- matrixStats::colQuantiles(vc_rank, probs = 0.75)

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

DartVC_dartMachine <- function(l, y, Xmat, seeds) {
  temp_output <- tempfile()
  sink(temp_output)
  on.exit({ sink(); unlink(temp_output) })
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
  vc_full <- dartMachine::get_var_counts_over_chain(dm)
  colMeans(vc_full)
}

DartVC_BART <- function(l, y, Xmat, ...) {
  temp_output <- tempfile()
  sink(temp_output)
  on.exit({ sink(); unlink(temp_output) })
  dart <- BART::wbart(
    x.train = Xmat,
    y.train = y,
    sparse = TRUE,
    ntree = 20,
    nskip = 5000,
    ndpost = 5000,
    printevery = 10000
  )
  dart$varcount.mean
}
