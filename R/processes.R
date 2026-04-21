# Sub-step expansion (shared by nbmp_loop and qtps_loop) ---------------------

expand_substeps <- function(deltas, path_resolution, n_events) {
  n_subs <- pmax(as.integer(ceiling(deltas / path_resolution)), 1L)
  step_deltas <- rep(deltas / n_subs, times = n_subs)
  total_rows <- length(step_deltas) + 1L
  times <- c(0, cumsum(step_deltas))

  event_rows <- 1L + cumsum(n_subs)[seq_len(n_events)]
  is_event <- logical(total_rows)
  is_event[event_rows] <- TRUE

  list(
    step_deltas = step_deltas,
    total_rows = total_rows,
    times = times,
    is_event = is_event
  )
}

# Process factories ----------------------------------------------------------

#' Create a Brownian motion process
#'
#' Returns a transition kernel for standard Brownian motion with optional
#' volatility parameter: `X_{t+dt} = X_t + sigma * sqrt(dt) * Z`,
#' where `Z ~ N(0,1)`.
#'
#' @param sigma Volatility (standard deviation per unit time). Defaults to 1.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' bm <- process_bm()
#' set.seed(42)
#' x <- rep(0, 1000)
#' x_new <- bm(x, 0.01)
#' mean(x_new) # approximately 0
#'
#' @family processes
#' @export
process_bm <- function(sigma = 1) {
  check_positive_scalar(sigma, "sigma")
  f <- function(x, dt) {
    x + stats::rnorm(length(x), mean = 0, sd = sigma * sqrt(dt))
  }
  structure(f,
    class = "iftp_process", process_type = "bm",
    process_params = list(sigma = sigma)
  )
}

#' Create a drifted Brownian motion plus Poisson process
#'
#' Returns a transition kernel for the sum of a (possibly drifted) Brownian
#' motion and an independent Poisson process. When `poisson_intensity = 0`,
#' reduces to a drifted Brownian motion without jumps.
#'
#' @param mu Drift coefficient. Defaults to 0.
#' @param sigma BM volatility. Defaults to 1.
#' @param poisson_intensity Poisson intensity (non-negative). Defaults to 1.
#'   Set to 0 for pure drifted BM without jumps.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`. When
#'   `poisson_intensity > 0`, the return value carries an attribute
#'   `"jump_size"` (numeric vector): the Poisson jump component of each
#'   particle's increment. Used by particle system loops to create segment
#'   breaks at jump discontinuities.
#'
#' @examples
#' proc <- process_bm_drift_poisson(mu = 0.5)
#' set.seed(42)
#' x_new <- proc(rep(0, 1000), 0.01)
#'
#' # Without jumps (pure drifted BM)
#' proc_no_jump <- process_bm_drift_poisson(mu = 0.5, poisson_intensity = 0)
#'
#' @family processes
#' @export
process_bm_drift_poisson <- function(mu = 0, sigma = 1, poisson_intensity = 1) {
  check_numeric_scalar(mu, "mu")
  check_positive_scalar(sigma, "sigma")
  check_non_negative_scalar(poisson_intensity, "poisson_intensity")
  if (poisson_intensity == 0) {
    f <- function(x, dt) {
      x + stats::rnorm(length(x), mean = mu * dt, sd = sigma * sqrt(dt))
    }
  } else {
    f <- function(x, dt) {
      n <- length(x)
      jump <- as.numeric(stats::rpois(n, poisson_intensity * dt))
      result <- x + stats::rnorm(n, mean = mu * dt, sd = sigma * sqrt(dt)) + jump
      attr(result, "jump_size") <- jump
      result
    }
  }
  structure(f,
    class = "iftp_process", process_type = "bm_drift_poisson",
    process_params = list(mu = mu, sigma = sigma, poisson_intensity = poisson_intensity)
  )
}

#' Create a negative Gamma process
#'
#' Returns a transition kernel for a negated Gamma subordinator.
#' Increments are negative (the process decreases), matching the IFTP
#' convention where particles move in the negative direction.
#' The step is `X_{t+dt} = X_t - Gamma(shape = rate * dt, rate = 1)`.
#'
#' @param rate Rate parameter of the Gamma distribution. Defaults to 1.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' gamma_proc <- process_negative_gamma()
#' set.seed(42)
#' x <- rep(0, 1000)
#' x_new <- gamma_proc(x, 0.01)
#' all(x_new <= 0)
#'
#' @family processes
#' @export
process_negative_gamma <- function(rate = 1) {
  check_positive_scalar(rate, "rate")
  f <- function(x, dt) {
    x - stats::rgamma(length(x), shape = rate * dt)
  }
  structure(f,
    class = "iftp_process", process_type = "negative_gamma",
    process_params = list(rate = rate)
  )
}

#' Create a Cauchy process
#'
#' Returns a transition kernel for a symmetric Cauchy process (symmetric
#' \eqn{\alpha}{alpha}-stable with \eqn{\alpha = 1}{alpha = 1}).
#' Increments over a time step \eqn{\Delta t}{dt} follow a Cauchy
#' distribution with scale \eqn{\gamma \Delta t}{gamma * dt}.
#'
#' @param gamma Scale parameter (positive number). Defaults to 1.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' cauchy <- process_cauchy()
#' set.seed(42)
#' x <- rep(0, 1000)
#' x_new <- cauchy(x, 0.01)
#'
#' @family processes
#' @export
process_cauchy <- function(gamma = 1) {
  check_positive_scalar(gamma, "gamma")
  f <- function(x, dt) {
    x + stats::rcauchy(length(x), scale = gamma * dt)
  }
  structure(f,
    class = "iftp_process", process_type = "cauchy",
    process_params = list(gamma = gamma)
  )
}

# Diffusion process factories ------------------------------------------------

#' Create an Ornstein-Uhlenbeck process
#'
#' Returns a transition kernel for the Ornstein-Uhlenbeck diffusion
#' \eqn{\mathrm{d}X_t = \theta(\mu - X_t)\,\mathrm{d}t +
#' \sigma\,\mathrm{d}W_t}{dX = theta*(mu - X)*dt + sigma*dW}.
#' The process is mean-reverting toward \eqn{\mu}{mu} with speed
#' \eqn{\theta}{theta} and constant volatility \eqn{\sigma}{sigma}.
#' Discretised via the Euler-Maruyama scheme.
#'
#' @param theta Mean-reversion speed (positive number). Defaults to 1.
#' @param mu Long-term mean (numeric). Defaults to 0.
#' @param sigma Volatility (positive number). Defaults to 1.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' ou <- process_ou(theta = 1, mu = 0, sigma = 1)
#' set.seed(42)
#' x <- rep(5, 1000)
#' for (i in seq_len(100)) x <- ou(x, 0.01)
#' mean(x) # reverts toward 0
#'
#' @family processes
#' @export
process_ou <- function(theta = 1, mu = 0, sigma = 1) {
  check_positive_scalar(theta, "theta")
  check_numeric_scalar(mu, "mu")
  check_positive_scalar(sigma, "sigma")
  f <- function(x, dt) {
    x + theta * (mu - x) * dt + sigma * sqrt(dt) * stats::rnorm(length(x))
  }
  structure(f,
    class = "iftp_process", process_type = "ou",
    process_params = list(theta = theta, mu = mu, sigma = sigma)
  )
}

#' Create a constant elasticity of variance (CEV) process
#'
#' Returns a transition kernel for the CEV diffusion
#' \eqn{\mathrm{d}X_t = \mu X_t\,\mathrm{d}t +
#' \sigma X_t^\gamma\,\mathrm{d}W_t}{dX = mu*X*dt + sigma*X^gamma*dW}.
#' The process takes values in \eqn{(0, \infty)}{(0, Inf)}.
#' Discretised via the Euler-Maruyama scheme; positions are clamped to
#' a small positive value to maintain strict positivity.
#'
#' @param mu Drift coefficient (numeric). Defaults to 0.
#' @param sigma Volatility (positive number). Defaults to 1.
#' @param gamma Elasticity exponent (positive number). Defaults to 1.
#'
#' @details
#' The special case \eqn{\gamma = 1}{gamma = 1} gives geometric Brownian
#' motion (GBM), and \eqn{\gamma = 1/2}{gamma = 0.5} gives a CIR-like
#' square-root volatility without mean reversion.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' # Geometric Brownian motion (gamma = 1)
#' gbm <- process_cev(mu = 0.05, sigma = 0.2, gamma = 1)
#' set.seed(42)
#' x <- rep(1, 1000)
#' for (i in seq_len(100)) x <- gbm(x, 0.01)
#' all(x > 0)
#'
#' @family processes
#' @export
process_cev <- function(mu = 0, sigma = 1, gamma = 1) {
  check_numeric_scalar(mu, "mu")
  check_positive_scalar(sigma, "sigma")
  check_positive_scalar(gamma, "gamma")
  f <- function(x, dt) {
    n <- length(x)
    x_safe <- pmax(x, .Machine$double.eps)
    x_new <- x_safe + mu * x_safe * dt +
      sigma * x_safe^gamma * sqrt(dt) * stats::rnorm(n)
    pmax(x_new, .Machine$double.eps)
  }
  structure(f,
    class = "iftp_process", process_type = "cev",
    process_params = list(mu = mu, sigma = sigma, gamma = gamma)
  )
}

#' Create a Cox-Ingersoll-Ross process
#'
#' Returns a transition kernel for the Cox-Ingersoll-Ross (CIR) diffusion
#' \eqn{\mathrm{d}X_t = \kappa(\theta - X_t)\,\mathrm{d}t +
#' \sigma\sqrt{X_t}\,\mathrm{d}W_t}{dX = kappa*(theta-X)*dt +
#' sigma*sqrt(X)*dW}.
#' The process is mean-reverting toward \eqn{\theta}{theta} and takes
#' values in \eqn{[0, \infty)}{[0, Inf)}.
#' Discretised via the Euler-Maruyama scheme with reflection at 0.
#'
#' @param kappa Mean-reversion speed (positive number). Defaults to 1.
#' @param theta Long-term mean (positive number). Defaults to 1.
#' @param sigma Volatility (positive number). Defaults to 1.
#'
#' @details
#' The Feller condition \eqn{2\kappa\theta \ge \sigma^2}{2*kappa*theta >= sigma^2}
#' ensures the process stays strictly positive. When this condition is
#' violated, the process can reach zero; the Euler-Maruyama discretisation
#' uses reflection at 0 in both cases.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' cir <- process_cir(kappa = 2, theta = 1, sigma = 0.5)
#' set.seed(42)
#' x <- rep(1, 1000)
#' for (i in seq_len(100)) x <- cir(x, 0.01)
#' all(x >= 0)
#'
#' @family processes
#' @export
process_cir <- function(kappa = 1, theta = 1, sigma = 1) {
  check_positive_scalar(kappa, "kappa")
  check_positive_scalar(theta, "theta")
  check_positive_scalar(sigma, "sigma")
  f <- function(x, dt) {
    n <- length(x)
    x_new <- x + kappa * (theta - x) * dt +
      sigma * sqrt(pmax(x, 0)) * sqrt(dt) * stats::rnorm(n)
    pmax(x_new, 0)
  }
  structure(f,
    class = "iftp_process", process_type = "cir",
    process_params = list(kappa = kappa, theta = theta, sigma = sigma)
  )
}

#' Create a reflected Bessel process
#'
#' Returns a transition kernel for a reflected Bessel process of dimension
#' \eqn{\delta}{delta} on \eqn{(0, \infty)}{(0, Inf)}, satisfying the SDE
#' \eqn{\mathrm{d}X_t = \frac{\delta - 1}{2 X_t}\,\mathrm{d}t +
#' \mathrm{d}W_t}{dX = (delta-1)/(2*X)*dt + dW}.
#' Discretised via the Euler-Maruyama scheme with reflection at 0.
#'
#' @param delta Dimension parameter (positive number). Defaults to 3.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' bes <- process_bessel(delta = 3)
#' set.seed(42)
#' x <- rep(1, 1000)
#' for (i in seq_len(100)) x <- bes(x, 0.01)
#' all(x > 0)
#'
#' @references
#' Klump, A. and Savov, M. (2025). Conditions for existence and uniqueness
#' of the inverse first-passage time problem applicable for \enc{Lévy}{Levy} processes
#' and diffusions. *Ann. Appl. Probab.*, 35(3), 1791--1827.
#' \doi{10.1214/25-AAP2157}
#'
#' See Definition 2.15 and Remark 2.17 for the reflected Bessel process
#' as an example of a diffusion for which the IFTP has a unique solution.
#'
#' @family processes
#' @export
process_bessel <- function(delta = 3) {
  check_positive_scalar(delta, "delta")
  f <- function(x, dt) {
    n <- length(x)
    x_safe <- pmax(x, .Machine$double.eps)
    x_new <- x_safe + (delta - 1) / (2 * x_safe) * dt +
      sqrt(dt) * stats::rnorm(n)
    abs(x_new)
  }
  structure(f,
    class = "iftp_process", process_type = "bessel",
    process_params = list(delta = delta)
  )
}

#' Create a neutral Wright-Fisher diffusion
#'
#' Returns a transition kernel for the neutral Wright-Fisher diffusion
#' on the interval \eqn{[a, b]}{[a, b]}, satisfying the SDE
#' \eqn{\mathrm{d}X_t = \tfrac{1}{2}[\theta_1(b - X_t) -
#' \theta_2(X_t - a)]\,\mathrm{d}t +
#' \sqrt{(X_t - a)(b - X_t)}\,\mathrm{d}W_t}{dX =
#' 0.5*[theta1*(b - X) - theta2*(X - a)]*dt +
#' sqrt((X - a)*(b - X))*dW}.
#' The drift is determined by mutation rates \eqn{\theta_1}{theta1}
#' and \eqn{\theta_2}{theta2}; "neutral" refers to the absence of
#' natural selection. The volatility vanishes at both boundaries.
#' Discretised via the Euler-Maruyama scheme with clamping to
#' \eqn{[a, b]}{[a, b]}.
#'
#' @param theta1 Mutation rate toward the upper boundary (positive number).
#'   Defaults to 1.
#' @param theta2 Mutation rate toward the lower boundary (positive number).
#'   Defaults to 1.
#' @param lower Lower boundary of the interval. Defaults to 0.
#' @param upper Upper boundary of the interval. Defaults to 1.
#'
#' @return A function with signature `function(x, dt)` that takes a
#'   numeric vector of current positions and returns the updated positions
#'   after one step. Has class `"iftp_process"`.
#'
#' @examples
#' wf <- process_wright_fisher(theta1 = 1, theta2 = 1)
#' set.seed(42)
#' x <- rep(0.5, 1000)
#' for (i in seq_len(100)) x <- wf(x, 0.01)
#' all(x >= 0 & x <= 1)
#'
#' @references
#' Jenkins, P.A. and \enc{Spanò}{Spano}, D. (2017). Exact simulation of the
#' Wright--Fisher diffusion. *Ann. Appl. Probab.*, 27(3), 1478--1509.
#' \doi{10.1214/16-AAP1236}
#'
#' @family processes
#' @export
process_wright_fisher <- function(theta1 = 1, theta2 = 1,
                                  lower = 0, upper = 1) {
  check_positive_scalar(theta1, "theta1")
  check_positive_scalar(theta2, "theta2")
  check_numeric_scalar(lower, "lower")
  check_numeric_scalar(upper, "upper")
  if (lower >= upper) {
    abort("`lower` must be strictly less than `upper`.")
  }
  f <- function(x, dt) {
    n <- length(x)
    x_safe <- pmax(pmin(x, upper), lower)
    drift <- 0.5 * (theta1 * (upper - x_safe) - theta2 * (x_safe - lower))
    vol <- sqrt((x_safe - lower) * (upper - x_safe))
    x_new <- x_safe + drift * dt + vol * sqrt(dt) * stats::rnorm(n)
    pmax(pmin(x_new, upper), lower)
  }
  structure(f,
    class = "iftp_process", process_type = "wright_fisher",
    process_params = list(
      theta1 = theta1, theta2 = theta2,
      lower = lower, upper = upper
    )
  )
}
