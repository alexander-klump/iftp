# Internal: pivot a wide DF (time + segment columns) to long format.
wide_to_long <- function(df) {
  mat <- as.matrix(df[, -1, drop = FALSE])
  n_cols <- ncol(mat)
  n_times <- nrow(mat)
  data.frame(
    time = rep(df[[1]], times = n_cols),
    value = as.vector(mat),
    segment = factor(rep(seq_len(n_cols), each = n_times))
  )
}

# Internal: add wide-DF paths to a ggplot as a labelled layer.
add_paths <- function(p, df, label, linewidth = 0.8, alpha = 1) {
  long <- wide_to_long(df)
  long$label <- label

  p + ggplot2::geom_line(
    data = long,
    ggplot2::aes(
      x = .data$time, y = .data$value,
      colour = .data$label, group = .data$segment
    ),
    linewidth = linewidth, alpha = alpha,
    na.rm = TRUE
  )
}

# Internal: add standard overlays (reference line, IFTP boundary, labels, theme).
add_overlays <- function(p, reference_line, iftp_boundary, title,
                         y_label = "Boundary b(t)") {
  if (!is.null(iftp_boundary)) {
    p <- p + ggplot2::geom_line(
      data = iftp_boundary,
      ggplot2::aes(x = .data$time, y = .data$boundary),
      colour = "blue", linewidth = 0.8, linetype = "dashed"
    )
  }

  if (!is.null(reference_line)) {
    p <- p + ggplot2::geom_hline(
      yintercept = reference_line, linetype = "dashed", colour = "red"
    )
  }

  p + ggplot2::labs(title = title, x = "Time", y = y_label, colour = NULL) +
    ggplot2::theme_minimal()
}
