library(shiny)
library(bslib)
library(ggplot2)
library(iftp)

# Source modules
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# Non-blocking execution: default plan(sequential) works everywhere.
# For true async: call future::plan(future::multisession) before run_app().
if (requireNamespace("future", quietly = TRUE)) {
  if (!inherits(future::plan(), "sequential")) {
    message("iftp: Using ", class(future::plan())[1L], " for async simulation")
  }
}

# -- UI -----------------------------------------------------------------------

ui <- page_sidebar(
  title = "iftp Explorer",
  sidebar = sidebar(
    width = 350,
    config_ui("config"),
    hr(),
    bslib::input_task_button("run", "Run Simulation",
                             class = "btn-primary w-100",
                             icon = icon("play"))
  ),
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Results",
      conditionalPanel(
        condition = "!['nbmp','qtps'].includes(input['config-sim_type'])",
        checkboxInput("cost_transform",
                      "Cost-transformed boundary",
                      value = TRUE)
      ),
      plotOutput("main_plot", height = "500px"),
      verbatimTextOutput("summary")
    ),
    nav_panel(
      "Export",
      layout_columns(
        col_widths = 6,
        card(
          card_header("Download Data"),
          downloadButton("dl_csv", "CSV",
                         class = "btn-outline-secondary w-100 mb-2"),
          conditionalPanel(
            condition = "!['nbmp','qtps'].includes(input['config-sim_type'])",
            downloadButton("dl_json", "JSON",
                           class = "btn-outline-secondary w-100 mb-2")
          ),
          conditionalPanel(
            condition = "['nbmp','qtps'].includes(input['config-sim_type'])",
            downloadButton("dl_json_matrix", "JSON (matrix)",
                           class = "btn-outline-secondary w-100 mb-2"),
            downloadButton("dl_json_segments", "JSON (segments)",
                           class = "btn-outline-secondary w-100 mb-2")
          ),
          downloadButton("dl_rds", "RDS",
                         class = "btn-outline-secondary w-100")
        ),
        card(
          card_header("Download Plot"),
          downloadButton("dl_png", "PNG",
                         class = "btn-outline-secondary w-100 mb-2"),
          downloadButton("dl_pdf", "PDF",
                         class = "btn-outline-secondary w-100")
        )
      ),
      card(
        card_header("Reproducible R Code"),
        verbatimTextOutput("code_snippet"),
        downloadButton("dl_code", "Download R Script",
                       class = "btn-outline-secondary w-100 mt-2")
      )
    )
  )
)

# -- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  config <- config_server("config")

  rv <- reactiveValues(
    result = NULL,
    run_time = NULL,
    error_msg = NULL
  )

  # Async simulation runner via ExtendedTask
  sim_task <- ExtendedTask$new(function(params) {
    promises::future_promise({
      t0 <- proc.time()
      result <- tryCatch(
        iftp::shiny_dispatch_simulation(params),
        error = function(e) {
          structure(conditionMessage(e), class = "sim_error")
        }
      )
      list(result = result,
           elapsed = (proc.time() - t0)[["elapsed"]])
    }, seed = TRUE)
  })

  observeEvent(input$run, {
    rv$error_msg <- NULL
    sim_task$invoke(config())
  })

  # Collect result when task completes
  observe({
    res <- sim_task$result()
    if (inherits(res$result, "sim_error")) {
      rv$error_msg <- as.character(res$result)
      rv$result <- NULL
      rv$run_time <- NULL
    } else {
      rv$error_msg <- NULL
      rv$result <- res$result
      rv$run_time <- res$elapsed
    }
  }) |> bindEvent(sim_task$result())

  # Results tab: plot
  output$main_plot <- renderPlot({
    req(rv$result)
    if (inherits(rv$result, "iftp_result")) {
      plot(rv$result, cost_transform = input$cost_transform %||% TRUE)
    } else {
      plot(rv$result)
    }
  })

  # Results tab: summary
  output$summary <- renderPrint({
    if (!is.null(rv$error_msg)) {
      cat("Error:", rv$error_msg, "\n")
      return(invisible(NULL))
    }
    req(rv$result)
    print(rv$result)
    captured <- rv$result$params$seed
    if (!is.null(captured)) {
      cat(sprintf("Seed: %d\n", captured))
    }
    if (!is.null(rv$run_time)) {
      cat(sprintf("Computation time: %.2f seconds\n", rv$run_time))
    }
  })

  # Export tab: code snippet
  # When capture_seed was active, use the captured seed from the result
  # so the generated code reproduces this exact simulation.
  output$code_snippet <- renderPrint({
    req(rv$result)
    cfg <- config()
    captured <- rv$result$params$seed
    if (!is.null(captured) && is.null(cfg$seed)) {
      cfg$seed <- captured
    }
    cat(shiny_generate_code(cfg))
  })

  # Export tab: download handlers
  dl_filename <- function(ext, suffix = NULL) {
    if (is.null(rv$result)) return(paste0("iftp.", ext))
    suggest_filename(rv$result, ext, suffix = suffix)
  }

  output$dl_csv <- downloadHandler(
    filename = function() dl_filename("csv"),
    content = function(file) {
      req(rv$result)
      export_result(rv$result, file = file)
    }
  )

  output$dl_json <- downloadHandler(
    filename = function() dl_filename("json"),
    content = function(file) {
      req(rv$result)
      export_result(rv$result, file = file)
    }
  )

  output$dl_json_matrix <- downloadHandler(
    filename = function() dl_filename("json", suffix = "matrix"),
    content = function(file) {
      req(rv$result)
      export_result(rv$result, file = file, format = "matrix")
    }
  )

  output$dl_json_segments <- downloadHandler(
    filename = function() dl_filename("json", suffix = "segments"),
    content = function(file) {
      req(rv$result)
      export_result(rv$result, file = file, format = "segments")
    }
  )

  output$dl_rds <- downloadHandler(
    filename = function() dl_filename("rds"),
    content = function(file) {
      req(rv$result)
      save_result(rv$result, file)
    }
  )

  output$dl_png <- downloadHandler(
    filename = function() dl_filename("png"),
    content = function(file) {
      req(rv$result)
      png(file, width = 10, height = 6, units = "in", res = 150)
      on.exit(dev.off())
      if (inherits(rv$result, "iftp_result")) {
        plot(rv$result, cost_transform = input$cost_transform %||% TRUE)
      } else {
        plot(rv$result)
      }
    }
  )

  output$dl_pdf <- downloadHandler(
    filename = function() dl_filename("pdf"),
    content = function(file) {
      req(rv$result)
      pdf(file, width = 10, height = 6)
      on.exit(dev.off())
      if (inherits(rv$result, "iftp_result")) {
        plot(rv$result, cost_transform = input$cost_transform %||% TRUE)
      } else {
        plot(rv$result)
      }
    }
  )

  output$dl_code <- downloadHandler(
    filename = function() dl_filename("R"),
    content = function(file) {
      req(rv$result)
      writeLines(shiny_generate_code(config()), file)
    }
  )
}

# -- Launch -------------------------------------------------------------------

shinyApp(ui, server)
