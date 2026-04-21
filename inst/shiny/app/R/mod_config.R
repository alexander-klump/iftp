# inst/shiny/app/R/mod_config.R

# %||% is not available in sourced files (not part of package namespace)
`%||%` <- function(x, y) if (is.null(x)) y else x # nolint

# -- Constants ----------------------------------------------------------------

DIST_CHOICES <- c(
  "Exponential" = "exponential",
  "Weibull" = "weibull",
  "Gamma" = "gamma",
  "Uniform" = "uniform",
  "Log-logistic" = "log_logistic",
  "Frechet" = "frechet",
  "Lomax" = "lomax",
  "Arcsine" = "arcsine",
  "Soft-killing (const. barrier)" = "soft_killing_constant_barrier"
)

DIST_PARAMS <- list(
  exponential = list(
    rate = list(label = "Rate", value = 1, min = 0.01, step = 0.1,
                tip = "F(t) = 1 - exp(-rate * t)")
  ),
  weibull = list(
    shape = list(label = "Shape", value = 1, min = 0.01, step = 0.1,
                 tip = "Weibull shape parameter"),
    scale = list(label = "Scale", value = 1, min = 0.01, step = 0.1,
                 tip = "Weibull scale parameter")
  ),
  gamma = list(
    shape = list(label = "Shape", value = 1, min = 0.01, step = 0.1,
                 tip = "Gamma shape parameter"),
    rate = list(label = "Rate", value = 1, min = 0.01, step = 0.1,
                tip = "Gamma rate parameter")
  ),
  uniform = list(
    min_val = list(label = "Min", value = 0, step = 0.1,
                   tip = "Lower bound of Uniform[min, max]"),
    max_val = list(label = "Max", value = 1, min = 0.01, step = 0.1,
                   tip = "Upper bound of Uniform[min, max]")
  ),
  log_logistic = list(
    shape = list(label = "Shape", value = 1, min = 0.01, step = 0.1,
                 tip = "Log-logistic shape parameter"),
    scale = list(label = "Scale", value = 1, min = 0.01, step = 0.1,
                 tip = "Log-logistic scale parameter")
  ),
  frechet = list(
    shape = list(label = "Shape", value = 1, min = 0.01, step = 0.1,
                 tip = "Frechet shape parameter"),
    scale = list(label = "Scale", value = 1, min = 0.01, step = 0.1,
                 tip = "Frechet scale parameter")
  ),
  lomax = list(
    shape = list(label = "Shape", value = 1, min = 0.01, step = 0.1,
                 tip = "Lomax (Pareto II) shape parameter"),
    scale = list(label = "Scale", value = 1, min = 0.01, step = 0.1,
                 tip = "Lomax scale parameter")
  ),
  arcsine = list(),
  soft_killing_constant_barrier = list(
    barrier = list(label = "Barrier", value = 1, step = 0.1,
                   tip = "Constant barrier level c (nonzero)"),
    killing_rate = list(label = "Killing rate", value = 1, min = 0.01,
                        step = 0.1,
                        tip = "Soft-killing rate lambda > 0")
  )
)

PROCESS_CHOICES <- c(
  "Brownian Motion" = "bm",
  "BM + Drift + Poisson" = "bm_drift_poisson",
  "Negative Gamma" = "negative_gamma",
  "Cauchy" = "cauchy",
  "Ornstein-Uhlenbeck" = "ou",
  "CEV" = "cev",
  "Cox-Ingersoll-Ross" = "cir",
  "Bessel" = "bessel",
  "Wright-Fisher" = "wright_fisher"
)

PROCESS_PARAMS <- list(
  bm = list(
    sigma = list(label = "Sigma", value = 1, min = 0.01, step = 0.1,
                 tip = "Volatility of the Brownian motion")
  ),
  bm_drift_poisson = list(
    mu = list(label = "Mu (drift)", value = 0, step = 0.1,
              tip = "Drift coefficient"),
    sigma = list(label = "Sigma", value = 1, min = 0.01, step = 0.1,
                 tip = "Volatility of the BM component"),
    poisson_intensity = list(label = "Poisson Intensity", value = 1, min = 0.01,
                             step = 0.1,
                             tip = "Rate of the Poisson component")
  ),
  negative_gamma = list(
    rate = list(label = "Rate", value = 1, min = 0.01, step = 0.1,
                tip = "Rate parameter of the Gamma process")
  ),
  cauchy = list(
    gamma = list(label = "Gamma (scale)", value = 1, min = 0.01, step = 0.1,
                 tip = "Scale parameter of the Cauchy process")
  ),
  ou = list(
    theta = list(label = "Theta", value = 1, min = 0.01, step = 0.1,
                 tip = "Mean-reversion speed"),
    mu = list(label = "Mu", value = 0, step = 0.1,
              tip = "Long-term mean"),
    sigma = list(label = "Sigma", value = 1, min = 0.01, step = 0.1,
                 tip = "Volatility")
  ),
  cev = list(
    mu = list(label = "Mu (drift)", value = 0, step = 0.1,
              tip = "Drift coefficient"),
    sigma = list(label = "Sigma", value = 1, min = 0.01, step = 0.1,
                 tip = "Volatility"),
    gamma = list(label = "Gamma (elasticity)", value = 1, min = 0.01,
                 step = 0.1,
                 tip = "Elasticity exponent (1 = GBM, 0.5 = square-root)")
  ),
  cir = list(
    kappa = list(label = "Kappa", value = 1, min = 0.01, step = 0.1,
                 tip = "Mean-reversion speed"),
    theta = list(label = "Theta", value = 1, min = 0.01, step = 0.1,
                 tip = "Long-term mean"),
    sigma = list(label = "Sigma", value = 1, min = 0.01, step = 0.1,
                 tip = "Volatility (Feller: 2*kappa*theta >= sigma^2)")
  ),
  bessel = list(
    delta = list(label = "Delta", value = 3, min = 0.01, step = 0.5,
                 tip = "Dimension parameter (Definition 2.15, Klump & Savov 2025)")
  ),
  wright_fisher = list(
    theta1 = list(label = "Theta1", value = 1, min = 0.01, step = 0.1,
                  tip = "Mutation rate toward upper boundary"),
    theta2 = list(label = "Theta2", value = 1, min = 0.01, step = 0.1,
                  tip = "Mutation rate toward lower boundary"),
    lower = list(label = "Lower", value = 0, step = 0.1,
                 tip = "Lower boundary of the interval"),
    upper = list(label = "Upper", value = 1, min = 0.01, step = 0.1,
                 tip = "Upper boundary of the interval")
  )
)

COST_CHOICES <- c(
  "abs (symmetric)" = "abs",
  "identity (one-sided)" = "identity",
  "-x (negative identity)" = "neg"
)

INIT_CHOICES <- c(
  "Fixed point" = "fixed",
  "Uniform[a, b]" = "uniform",
  "Normal(mu, sigma)" = "normal"
)

SIM_TYPE_CHOICES <- c(
  "Anulova Discretization" = "anulova",
  "Quantile Discretization" = "quantile",
  "Soft-Killing IFPT" = "soft_killing",
  "N-BMP" = "nbmp",
  "QTPS" = "qtps"
)

# Algorithm groups for conditional UI panels
NEEDS_DIST <- c("anulova", "quantile", "soft_killing", "qtps")
NEEDS_PATHRES <- c("nbmp", "qtps")
NEEDS_METHOD <- c("quantile", "qtps")
DIST_NO_SAMPLER <- c("soft_killing_constant_barrier")

# Build JS condition for conditionalPanel
js_includes <- function(ns, id, values) {
  sprintf("[%s].includes(input['%s'])",
          paste0("'", values, "'", collapse = ","), ns(id))
}

# -- Helpers ------------------------------------------------------------------

# Label with an info-icon tooltip (bslib::tooltip)
label_tip <- function(label, tip) {
  shiny::tagList(
    label, " ",
    bslib::tooltip(
      shiny::icon("circle-info", class = "text-muted"),
      tip
    )
  )
}

build_param_inputs <- function(ns, prefix, param_list) {
  if (length(param_list) == 0L) return(NULL)
  lapply(names(param_list), function(name) {
    p <- param_list[[name]]
    lbl <- if (!is.null(p$tip)) label_tip(p$label, p$tip) else p$label
    shiny::numericInput(
      inputId = ns(paste0(prefix, "_", name)),
      label = lbl,
      value = p$value,
      min = if (!is.null(p$min)) p$min else NA,
      step = if (!is.null(p$step)) p$step else NA
    )
  })
}

build_dist_code <- function(input) {
  family <- input$dist_family
  switch(family,
    exponential = sprintf("dist_exponential(rate = %s)", input$dist_rate),
    weibull = sprintf("dist_weibull(shape = %s, scale = %s)",
                      input$dist_shape, input$dist_scale),
    gamma = sprintf("dist_gamma(shape = %s, rate = %s)",
                    input$dist_shape, input$dist_rate),
    uniform = sprintf("dist_uniform(min = %s, max = %s)",
                      input$dist_min_val, input$dist_max_val),
    log_logistic = sprintf("dist_log_logistic(shape = %s, scale = %s)",
                           input$dist_shape, input$dist_scale),
    frechet = sprintf("dist_frechet(shape = %s, scale = %s)",
                      input$dist_shape, input$dist_scale),
    lomax = sprintf("dist_lomax(shape = %s, scale = %s)",
                    input$dist_shape, input$dist_scale),
    arcsine = "dist_arcsine()",
    soft_killing_constant_barrier = sprintf(
      "dist_soft_killing_constant_barrier(barrier = %s, killing_rate = %s)",
      input$dist_barrier, input$dist_killing_rate
    )
  )
}

build_process_code <- function(input) {
  ptype <- input$process_type
  switch(ptype,
    bm = sprintf("process_bm(sigma = %s)", input$proc_sigma),
    bm_drift_poisson = sprintf(
      "process_bm_drift_poisson(mu = %s, sigma = %s, poisson_intensity = %s)",
      input$proc_mu, input$proc_sigma, input$proc_poisson_intensity
    ),
    negative_gamma = sprintf("process_negative_gamma(rate = %s)", input$proc_rate),
    cauchy = sprintf("process_cauchy(gamma = %s)", input$proc_gamma),
    ou = sprintf("process_ou(theta = %s, mu = %s, sigma = %s)",
                 input$proc_theta, input$proc_mu, input$proc_sigma),
    cev = sprintf("process_cev(mu = %s, sigma = %s, gamma = %s)",
                  input$proc_mu, input$proc_sigma, input$proc_gamma),
    cir = sprintf("process_cir(kappa = %s, theta = %s, sigma = %s)",
                  input$proc_kappa, input$proc_theta, input$proc_sigma),
    bessel = sprintf("process_bessel(delta = %s)", input$proc_delta),
    wright_fisher = sprintf(
      "process_wright_fisher(theta1 = %s, theta2 = %s, lower = %s, upper = %s)",
      input$proc_theta1, input$proc_theta2, input$proc_lower, input$proc_upper
    )
  )
}

build_init_code <- function(input) {
  switch(input$init_type,
    "fixed" = sprintf("function(n) rep(%s, n)", input$init_fixed_value),
    "uniform" = sprintf("function(n) runif(n, %s, %s)",
                        input$init_uniform_a, input$init_uniform_b),
    "normal" = sprintf("function(n) rnorm(n, mean = %s, sd = %s)",
                       input$init_normal_mu, input$init_normal_sigma)
  )
}

# -- Module UI ----------------------------------------------------------------

config_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(".bslib-sidebar hr { margin: 0.3rem 0; }"),
    shiny::selectInput(ns("sim_type"),
      label_tip("Simulation Type",
                "IFTP algorithms approximate a boundary; particle systems simulate paths"),
      choices = SIM_TYPE_CHOICES),

    # Distribution section (IFTP algorithms + QTPS)
    shiny::conditionalPanel(
      condition = js_includes(ns, "sim_type", NEEDS_DIST),
      shiny::hr(),
      shiny::tags$strong("Target Distribution"),
      shiny::selectInput(ns("dist_family"),
        label_tip("Distribution",
                  "Target distribution for the inverse first-passage time problem"),
        choices = DIST_CHOICES),
      shiny::uiOutput(ns("dist_params_ui"))
    ),

    # Process section (all sim types)
    shiny::hr(),
    shiny::tags$strong("Stochastic Process"),
    shiny::selectInput(ns("process_type"),
      label_tip("Process", "Stochastic process driving particle increments"),
      choices = PROCESS_CHOICES),
    shiny::uiOutput(ns("process_params_ui")),

    shiny::hr(),

    # Algorithm-specific parameters: Anulova
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'anulova'", ns("sim_type")),
      shiny::tags$strong("Anulova Parameters"),
      shiny::numericInput(ns("anulova_time_horizon"),
        label_tip("Time Horizon", "Simulation endpoint T"),
        value = 2, min = 0.01, step = 0.5),
      shiny::numericInput(ns("anulova_time_steps"),
        label_tip("Time Steps", "Number of equidistant time grid points"),
        value = 100, min = 10, step = 10),
      shiny::numericInput(ns("anulova_n_particles"),
        label_tip("Particles", "Monte Carlo sample size (larger = more accurate, slower)"),
        value = 1000, min = 50, step = 100)
    ),

    # Algorithm-specific parameters: Quantile
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'quantile'", ns("sim_type")),
      shiny::tags$strong("Quantile Parameters"),
      shiny::numericInput(ns("quantile_time_horizon"),
        label_tip("Time Horizon", "Simulation endpoint T"),
        value = 2, min = 0.01, step = 0.5),
      shiny::numericInput(ns("quantile_time_steps"),
        label_tip("Time Steps", "Refinement parameter (total particles = time_steps × remove_per_step)"),
        value = 100, min = 2, step = 10),
      shiny::numericInput(ns("quantile_remove_per_step"),
        label_tip("Remove per Step", "Particles removed at each quantile step"),
        value = 10, min = 1, step = 1)
    ),

    # Algorithm-specific parameters: Soft-killing
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'soft_killing'", ns("sim_type")),
      shiny::tags$strong("Soft-Killing Parameters"),
      shiny::numericInput(ns("soft_killing_time_horizon"),
        label_tip("Time Horizon", "Simulation endpoint T"),
        value = 2, min = 0.01, step = 0.5),
      shiny::numericInput(ns("soft_killing_killing_rate"),
        label_tip("Killing Rate", "Soft killing rate from Klump & Kolb (2024)"),
        value = 1, min = 0.01, step = 0.1),
      shiny::numericInput(ns("soft_killing_time_steps"),
        label_tip("Time Steps", "Number of equidistant time steps"),
        value = 100, min = 1, step = 10),
      shiny::numericInput(ns("soft_killing_n_particles"),
        label_tip("Particles", "Number of weighted particles"),
        value = 1000, min = 100, step = 100)
    ),

    # Algorithm-specific parameters: N-BMP
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'nbmp'", ns("sim_type")),
      shiny::tags$strong("N-BMP Parameters"),
      shiny::numericInput(ns("nbmp_n_particles"),
        label_tip("Particles (N)", "Number of particles in the N-BMP system"),
        value = 50, min = 5, step = 5),
      shiny::numericInput(ns("nbmp_time_horizon"),
        label_tip("Time Horizon", "Simulation endpoint T"),
        value = 2, min = 0.01, step = 0.5),
      shiny::numericInput(ns("nbmp_branching_rate"),
        label_tip("Branching Rate",
                  "Per-particle branching rate (total rate = N * branching_rate)"),
        value = 1, min = 0.01, step = 0.1)
    ),

    # Algorithm-specific parameters: QTPS
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'qtps'", ns("sim_type")),
      shiny::tags$strong("QTPS Parameters"),
      shiny::numericInput(ns("qtps_n_particles"),
        label_tip("Particles (N)", "Initial count (decreases by 1 each step)"),
        value = 50, min = 5, step = 5),
      shiny::numericInput(ns("qtps_time_horizon"),
        label_tip("Time Horizon", "Simulation endpoint T"),
        value = 2, min = 0.01, step = 0.5)
    ),

    # Time grid method (quantile and qtps only)
    shiny::conditionalPanel(
      condition = js_includes(ns, "sim_type", NEEDS_METHOD),
      shiny::selectInput(ns("method"),
        label_tip("Time Grid Method",
                  "quantiles: deterministic grid from quantile function; order_statistics: stochastic grid from sorted samples (not available for all distributions)"),
        choices = c("Quantiles" = "quantiles",
                    "Order Statistics" = "order_statistics"))
    ),

    shiny::hr(),

    # Cost function (all types)
    shiny::selectInput(ns("cost_type"),
      label_tip("Cost Function",
                "Particle selection criterion: abs for symmetric, identity for one-sided, -x for soft-killing"),
      choices = COST_CHOICES),

    # Initial particle distribution (all types)
    shiny::selectInput(ns("init_type"),
      label_tip("Initial Distribution", "Starting positions at time 0"),
      choices = INIT_CHOICES),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'fixed'", ns("init_type")),
      shiny::numericInput(ns("init_fixed_value"),
        label_tip("Start Value", "All particles start at this position"),
        value = 0, step = 0.1)
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'uniform'", ns("init_type")),
      shiny::numericInput(ns("init_uniform_a"),
        label_tip("a (lower)", "Lower bound of Uniform[a, b]"),
        value = -1, step = 0.1),
      shiny::numericInput(ns("init_uniform_b"),
        label_tip("b (upper)", "Upper bound of Uniform[a, b]"),
        value = 1, step = 0.1)
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'normal'", ns("init_type")),
      shiny::numericInput(ns("init_normal_mu"),
        label_tip("Mu", "Mean of the normal distribution"),
        value = 0, step = 0.1),
      shiny::numericInput(ns("init_normal_sigma"),
        label_tip("Sigma", "Standard deviation of the normal distribution"),
        value = 0.1, min = 0.01, step = 0.1)
    ),

    # Path resolution (nbmp and qtps only)
    shiny::conditionalPanel(
      condition = js_includes(ns, "sim_type", NEEDS_PATHRES),
      shiny::numericInput(ns("path_resolution"),
        label_tip("Path Resolution",
                  "Path resolution between selection events (smaller = smoother, slower)"),
        value = 0.005, min = 0.001, step = 0.005)
    ),

    # Seed
    shiny::numericInput(ns("seed"),
      label_tip("Seed", "Fix for reproducible results"),
      value = NA, min = 1, step = 1),
    shiny::checkboxInput(ns("capture_seed"),
      label_tip("Capture seed",
                "Auto-generate and store a seed when none is set, for reproducibility"),
      value = FALSE)
  )
}

# -- Module Server ------------------------------------------------------------

config_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Auto-switch cost function when simulation type changes
    shiny::observeEvent(input$sim_type, {
      default_cost <- switch(input$sim_type,
        soft_killing = "neg",
        nbmp = "identity",
        "abs"
      )
      shiny::updateSelectInput(session, "cost_type", selected = default_cost)
    })

    # Dynamic distribution parameter inputs
    output$dist_params_ui <- shiny::renderUI({
      shiny::req(input$dist_family)
      build_param_inputs(ns, "dist", DIST_PARAMS[[input$dist_family]])
    })

    # Dynamic process parameter inputs
    output$process_params_ui <- shiny::renderUI({
      shiny::req(input$process_type)
      build_param_inputs(ns, "proc", PROCESS_PARAMS[[input$process_type]])
    })

    # Build distribution object from inputs
    build_dist <- shiny::reactive({
      shiny::req(input$dist_family)
      family <- input$dist_family
      switch(family,
        exponential = iftp::dist_exponential(
          rate = input$dist_rate %||% 1
        ),
        weibull = iftp::dist_weibull(
          shape = input$dist_shape %||% 1, scale = input$dist_scale %||% 1
        ),
        gamma = iftp::dist_gamma(
          shape = input$dist_shape %||% 1, rate = input$dist_rate %||% 1
        ),
        uniform = iftp::dist_uniform(
          min = input$dist_min_val %||% 0, max = input$dist_max_val %||% 1
        ),
        log_logistic = iftp::dist_log_logistic(
          shape = input$dist_shape %||% 1, scale = input$dist_scale %||% 1
        ),
        frechet = iftp::dist_frechet(
          shape = input$dist_shape %||% 1, scale = input$dist_scale %||% 1
        ),
        lomax = iftp::dist_lomax(
          shape = input$dist_shape %||% 1, scale = input$dist_scale %||% 1
        ),
        arcsine = iftp::dist_arcsine(),
        soft_killing_constant_barrier = iftp::dist_soft_killing_constant_barrier(
          barrier = input$dist_barrier %||% 1,
          killing_rate = input$dist_killing_rate %||% 1
        )
      )
    })

    # Build process object from inputs
    build_process <- shiny::reactive({
      shiny::req(input$process_type)
      ptype <- input$process_type
      switch(ptype,
        bm = iftp::process_bm(
          sigma = input$proc_sigma %||% 1
        ),
        bm_drift_poisson = iftp::process_bm_drift_poisson(
          mu = input$proc_mu %||% 0,
          sigma = input$proc_sigma %||% 1,
          poisson_intensity = input$proc_poisson_intensity %||% 1
        ),
        negative_gamma = iftp::process_negative_gamma(
          rate = input$proc_rate %||% 1
        ),
        cauchy = iftp::process_cauchy(
          gamma = input$proc_gamma %||% 1
        ),
        ou = iftp::process_ou(
          theta = input$proc_theta %||% 1,
          mu = input$proc_mu %||% 0,
          sigma = input$proc_sigma %||% 1
        ),
        cev = iftp::process_cev(
          mu = input$proc_mu %||% 0,
          sigma = input$proc_sigma %||% 1,
          gamma = input$proc_gamma %||% 1
        ),
        cir = iftp::process_cir(
          kappa = input$proc_kappa %||% 1,
          theta = input$proc_theta %||% 1,
          sigma = input$proc_sigma %||% 1
        ),
        bessel = iftp::process_bessel(
          delta = input$proc_delta %||% 3
        ),
        wright_fisher = iftp::process_wright_fisher(
          theta1 = input$proc_theta1 %||% 1,
          theta2 = input$proc_theta2 %||% 1,
          lower = input$proc_lower %||% 0,
          upper = input$proc_upper %||% 1
        )
      )
    })

    # Build init function from selection
    build_init <- shiny::reactive({
      shiny::req(input$init_type)
      switch(input$init_type,
        "fixed" = {
          v <- input$init_fixed_value %||% 0
          function(n) rep(v, n)
        },
        "uniform" = {
          a <- input$init_uniform_a %||% -1
          b <- input$init_uniform_b %||% 1
          function(n) runif(n, a, b)
        },
        "normal" = {
          m <- input$init_normal_mu %||% 0
          s <- input$init_normal_sigma %||% 0.1
          function(n) rnorm(n, mean = m, sd = s)
        }
      )
    })

    # Build cost function from selection
    build_cost <- shiny::reactive({
      shiny::req(input$cost_type)
      switch(input$cost_type,
        "abs" = abs,
        "identity" = identity,
        "neg" = function(x) -x
      )
    })

    # Disable order_statistics for distributions without a sampler
    shiny::observe({
      if (input$dist_family %in% DIST_NO_SAMPLER &&
          !is.null(input$method) && input$method == "order_statistics") {
        shiny::updateSelectInput(session, "method", selected = "quantiles")
        shiny::showNotification(
          "Order statistics method is not available for this distribution.",
          type = "warning", duration = 4
        )
      }
    })

    # Return all parameters as reactive
    shiny::reactive({
      sim_type <- input$sim_type

      needs_dist <- sim_type %in% c("anulova", "quantile", "soft_killing", "qtps")

      specific <- switch(sim_type,
        anulova = list(
          time_horizon = input$anulova_time_horizon,
          time_steps = input$anulova_time_steps,
          n_particles = input$anulova_n_particles
        ),
        quantile = list(
          time_horizon = input$quantile_time_horizon,
          time_steps = input$quantile_time_steps,
          remove_per_step = input$quantile_remove_per_step,
          method = input$method
        ),
        soft_killing = list(
          time_horizon = input$soft_killing_time_horizon,
          killing_rate = input$soft_killing_killing_rate,
          time_steps = input$soft_killing_time_steps,
          n_particles = input$soft_killing_n_particles
        ),
        nbmp = list(
          n_particles = input$nbmp_n_particles,
          time_horizon = input$nbmp_time_horizon,
          branching_rate = input$nbmp_branching_rate
        ),
        qtps = list(
          n_particles = input$qtps_n_particles,
          time_horizon = input$qtps_time_horizon,
          method = input$method
        )
      )

      c(
        list(
          sim_type = sim_type,
          dist = if (needs_dist) build_dist(),
          process = build_process(),
          cost = build_cost(),
          init = build_init(),
          path_resolution = if (!is.na(input$path_resolution)) input$path_resolution,
          capture_seed = isTRUE(input$capture_seed),
          seed = if (!is.na(input$seed)) input$seed else NULL,
          dist_code = if (needs_dist) build_dist_code(input) else NULL,
          process_code = build_process_code(input),
          cost_code = if (input$cost_type == "neg") "function(x) -x" else input$cost_type,
          init_code = build_init_code(input)
        ),
        specific
      )
    })
  })
}
