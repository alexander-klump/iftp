# Distribution specification constructor (internal) -------------------------

new_iftp_dist <- function(cdf, qf = NULL, rf = NULL,
                          name, params = list()) {
  structure(
    list(
      cdf = cdf, qf = qf, rf = rf,
      name = name, params = params
    ),
    class = "iftp_dist"
  )
}

#' @export
print.iftp_dist <- function(x, ...) {
  cat(sprintf("IFTP distribution: %s\n", x$name))
  if (length(x$params) > 0L) {
    params_str <- paste(names(x$params), x$params, sep = " = ", collapse = ", ")
    cat(sprintf("  Parameters: %s\n", params_str))
  }
  cat(sprintf(
    "  Quantile function: %s\n",
    if (is.null(x$qf)) "not available" else "available"
  ))
  cat(sprintf(
    "  Sample function: %s\n",
    if (is.null(x$rf)) "not available" else "available"
  ))
  invisible(x)
}

# Extract CDF from a dist object or raw function (internal) -----------------

extract_cdf <- function(cdf, name = "cdf") {
  if (inherits(cdf, "iftp_dist")) {
    return(cdf$cdf)
  }
  if (is.function(cdf)) {
    return(cdf)
  }
  abort(paste0("`", name, "` must be a CDF function or an iftp_dist object."))
}

extract_sample <- function(cdf, name = "cdf") {
  if (inherits(cdf, "iftp_dist")) {
    return(cdf$rf)
  }
  NULL
}

extract_quantile <- function(cdf, qf = NULL) {
  if (inherits(cdf, "iftp_dist") && is.null(qf)) qf <- cdf$qf
  if (is.null(qf) || !is.function(qf)) {
    abort(paste0(
      "`qf` (quantile function) is required. ",
      "Supply it directly or use an iftp_dist object."
    ))
  }
  qf
}

extract_dist_name <- function(cdf) {
  if (inherits(cdf, "iftp_dist")) cdf$name else NULL
}

# Build non-equidistant time grid from distribution -------------------------

build_time_grid <- function(cdf, qf, rf, n_steps, time_horizon,
                            method = c("quantiles", "order_statistics")) {
  method <- match.arg(method)

  if (method == "quantiles") {
    max_steps <- floor(cdf(time_horizon) * n_steps)
    if (max_steps < 1L) {
      abort(c(
        "Target CDF has negligible mass up to `time_horizon`.",
        i = "Increase `time_horizon` or check `cdf`."
      ))
    }
    grid_probs <- seq(1 / n_steps, max_steps / n_steps, by = 1 / n_steps)
    grid <- qf(grid_probs)
    if (is.infinite(grid[length(grid)])) {
      grid[length(grid)] <- time_horizon
    }
  } else {
    if (is.null(rf)) {
      abort(c(
        "This distribution does not provide a sampling function.",
        i = "Use an iftp_dist object or method = \"quantiles\"."
      ))
    }
    grid <- sort(rf(n_steps))
    grid <- grid[grid <= time_horizon]
    if (length(grid) == 0L) {
      abort(c(
        "All sampled order statistics exceed `time_horizon`.",
        i = "Increase `time_horizon` or `n_steps`."
      ))
    }
  }

  c(0, grid)
}

# Distribution factories -----------------------------------------------------

#' Create an exponential distribution specification
#'
#' @param rate Rate parameter (inverse mean). Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_exponential(rate = 2)
#' d$cdf(1)
#' d$qf(0.5)
#'
#' @family distributions
#' @export
dist_exponential <- function(rate = 1) {
  check_positive_scalar(rate, "rate")
  new_iftp_dist(
    cdf = function(x) stats::pexp(x, rate = rate),
    qf = function(p) stats::qexp(p, rate = rate),
    rf = function(n) stats::rexp(n, rate = rate),
    name = "Exponential",
    params = list(rate = rate)
  )
}

#' Create a Weibull distribution specification
#'
#' @param shape Shape parameter. Defaults to 1.
#' @param scale Scale parameter. Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_weibull(shape = 2, scale = 1)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_weibull <- function(shape = 1, scale = 1) {
  check_positive_scalar(shape, "shape")
  check_positive_scalar(scale, "scale")
  new_iftp_dist(
    cdf = function(x) stats::pweibull(x, shape = shape, scale = scale),
    qf = function(p) stats::qweibull(p, shape = shape, scale = scale),
    rf = function(n) stats::rweibull(n, shape = shape, scale = scale),
    name = "Weibull",
    params = list(shape = shape, scale = scale)
  )
}

#' Create a log-logistic distribution specification
#'
#' Requires the \pkg{flexsurv} package at runtime.
#'
#' @param shape Shape parameter. Defaults to 1.
#' @param scale Scale parameter. Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' if (requireNamespace("flexsurv", quietly = TRUE)) {
#'   d <- dist_log_logistic(shape = 8)
#'   d$cdf(1)
#' }
#'
#' @family distributions
#' @export
dist_log_logistic <- function(shape = 1, scale = 1) {
  check_positive_scalar(shape, "shape")
  check_positive_scalar(scale, "scale")
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    abort(c(
      "Package {flexsurv} is required for the log-logistic distribution.",
      i = "Install it with: install.packages(\"flexsurv\")"
    ))
  }
  new_iftp_dist(
    cdf = function(x) flexsurv::pllogis(x, shape = shape, scale = scale),
    qf = function(p) flexsurv::qllogis(p, shape = shape, scale = scale),
    rf = function(n) flexsurv::rllogis(n, shape = shape, scale = scale),
    name = "Log-logistic",
    params = list(shape = shape, scale = scale)
  )
}

#' Create a Frechet distribution specification
#'
#' Type II extreme value distribution with CDF
#' `F(x) = exp(-(x/scale)^(-shape))` for `x > 0`.
#'
#' @param shape Shape parameter (alpha). Defaults to 1.
#' @param scale Scale parameter (sigma). Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_frechet(shape = 2)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_frechet <- function(shape = 1, scale = 1) {
  check_positive_scalar(shape, "shape")
  check_positive_scalar(scale, "scale")
  new_iftp_dist(
    cdf = function(x) {
      ifelse(x > 0, exp(-(x / scale)^(-shape)), 0)
    },
    qf = function(p) {
      scale * (-log(p))^(-1 / shape)
    },
    rf = function(n) scale * (-log(stats::runif(n)))^(-1 / shape),
    name = "Frechet",
    params = list(shape = shape, scale = scale)
  )
}

#' Create a Lomax (Pareto Type II) distribution specification
#'
#' CDF: `F(x) = 1 - (1 + x/scale)^(-shape)` for `x > 0`.
#'
#' @param shape Shape parameter (alpha). Defaults to 1.
#' @param scale Scale parameter (sigma). Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_lomax(shape = 2, scale = 1)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_lomax <- function(shape = 1, scale = 1) {
  check_positive_scalar(shape, "shape")
  check_positive_scalar(scale, "scale")
  new_iftp_dist(
    cdf = function(x) {
      ifelse(x > 0, 1 - (1 + x / scale)^(-shape), 0)
    },
    qf = function(p) {
      scale * ((1 - p)^(-1 / shape) - 1)
    },
    rf = function(n) scale * ((1 - stats::runif(n))^(-1 / shape) - 1),
    name = "Lomax",
    params = list(shape = shape, scale = scale)
  )
}

#' Create a Gamma distribution specification
#'
#' @param shape Shape parameter. Defaults to 1.
#' @param rate Rate parameter (inverse scale). Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_gamma(shape = 2, rate = 1)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_gamma <- function(shape = 1, rate = 1) {
  check_positive_scalar(shape, "shape")
  check_positive_scalar(rate, "rate")
  new_iftp_dist(
    cdf = function(x) stats::pgamma(x, shape = shape, rate = rate),
    qf = function(p) stats::qgamma(p, shape = shape, rate = rate),
    rf = function(n) stats::rgamma(n, shape = shape, rate = rate),
    name = "Gamma",
    params = list(shape = shape, rate = rate)
  )
}

#' Create a uniform distribution specification
#'
#' @param min Lower bound. Defaults to 0.
#' @param max Upper bound. Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_uniform(min = 0, max = 2)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_uniform <- function(min = 0, max = 1) {
  check_numeric_scalar(min, "min")
  check_numeric_scalar(max, "max")
  if (min >= max) abort("`min` must be less than `max`.")
  new_iftp_dist(
    cdf = function(x) stats::punif(x, min = min, max = max),
    qf = function(p) stats::qunif(p, min = min, max = max),
    rf = function(n) stats::runif(n, min = min, max = max),
    name = "Uniform",
    params = list(min = min, max = max)
  )
}

#' Create an arcsine distribution specification
#'
#' The standard arcsine distribution on \eqn{[0, 1]} with CDF
#' `F(x) = (2/pi) * asin(sqrt(x))`.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @examples
#' d <- dist_arcsine()
#' d$cdf(0.5)
#'
#' @family distributions
#' @export
dist_arcsine <- function() {
  new_iftp_dist(
    cdf = function(x) {
      ifelse(x <= 0, 0, ifelse(x >= 1, 1, (2 / pi) * asin(sqrt(x))))
    },
    qf = function(p) {
      sin(pi * p / 2)^2
    },
    rf = function(n) sin(pi * stats::runif(n) / 2)^2,
    name = "Arcsine",
    params = list()
  )
}

# Internal: survival function for soft-killing with constant barrier --------

sk_constant_barrier_survival <- function(t, abs_c, alpha, killing_rate) {
  first_term <- exp(-killing_rate * alpha * t) *
    (2 * stats::pnorm(sqrt(abs_c / t)) - 1)

  integrand <- function(u) {
    z <- -killing_rate * (t - u)
    # 1F1(1/2; 1; z) = exp(z/2) * I_0(z/2), DLMF 13.6.9 with nu = 0.
    # abs(z) because z < 0 here and besselI requires x >= 0; I_0 is even.
    kummer <- exp(z / 2) * besselI(abs(z) / 2, nu = 0)
    exp(-killing_rate * alpha * u) *
      kummer *
      sqrt(abs_c / (2 * pi * u^3)) *
      exp(-abs_c / (2 * u))
  }

  integral <- stats::integrate(integrand,
    lower = 0, upper = t,
    subdivisions = 200L,
    rel.tol = 1e-8
  )$value

  first_term + integral
}

#' Create a soft-killing (constant barrier) distribution specification
#'
#' The distribution of the first-passage time under soft killing with a
#' constant barrier \eqn{c \in \mathbb{R} \setminus \{0\}} and killing rate
#' \eqn{\lambda > 0}. Its survival function is
#'
#' \deqn{G_{c,\lambda}(t) =
#'   e^{-\lambda \alpha t} \bigl(2\Phi(\sqrt{|c|/t}) - 1\bigr)
#'   + \int_0^t e^{-\lambda \alpha u}
#'     {}_1F_1\!\left(\tfrac{1}{2};\,1;\,-\lambda(t-u)\right)
#'     \sqrt{\frac{|c|}{2\pi u^3}}\, e^{-|c|/(2u)}\,\mathrm{d}u,}
#'
#' where \eqn{\alpha = 1} if \eqn{c > 0} and \eqn{\alpha = 0} if \eqn{c < 0},
#' and \eqn{\Phi} denotes the standard normal CDF.
#'
#' @param barrier Constant barrier level \eqn{c \in \mathbb{R} \setminus \{0\}}.
#'   Defaults to 1.
#' @param killing_rate Killing rate \eqn{\lambda > 0}. Defaults to 1.
#'
#' @return An `iftp_dist` object with `cdf` and `quantile` functions.
#'
#' @references
#' Klump, A. (2022). *The classical and the soft-killing Inverse First-Passage Time
#' Problem: a stochastic order approach*. PhD thesis, University of Paderborn.
#' Lemma B.1.12, Example 3.3.3.
#'
#' @examples
#' d <- dist_soft_killing_constant_barrier(barrier = 1, killing_rate = 1)
#' d$cdf(1)
#'
#' @family distributions
#' @export
dist_soft_killing_constant_barrier <- function(barrier = 1, killing_rate = 1) {
  check_numeric_scalar(barrier, "barrier")
  if (!is.finite(barrier) || barrier == 0) {
    abort("`barrier` must be finite and nonzero.")
  }
  check_positive_scalar(killing_rate, "killing_rate")
  abs_c <- abs(barrier)
  alpha <- if (barrier > 0) 1 else 0
  cdf_fn <- function(t) {
    vapply(t, function(ti) {
      if (ti <= 0) {
        return(0)
      }
      1 - sk_constant_barrier_survival(ti, abs_c, alpha, killing_rate)
    }, numeric(1L))
  }
  quantile_fn <- function(p) {
    vapply(p, function(pi) {
      if (pi <= 0) {
        return(0)
      }
      if (pi >= 1) {
        return(Inf)
      }
      upper <- 100
      while (cdf_fn(upper) < pi) {
        upper <- upper * 2
        if (upper > 1e6) {
          abort("Quantile search exceeded upper bound of 1e6.")
        }
      }
      stats::uniroot(function(t) cdf_fn(t) - pi,
        interval = c(1e-6, upper),
        tol = 1e-6
      )$root
    }, numeric(1L))
  }
  new_iftp_dist(
    cdf = cdf_fn,
    qf = quantile_fn,
    name = "Soft-killing (constant barrier)",
    params = list(barrier = barrier, killing_rate = killing_rate)
  )
}
