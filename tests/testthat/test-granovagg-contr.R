quiet_contr <- function(...) {
  suppressWarnings(suppressMessages({
    result <- NULL
    capture.output({ result <- granovagg.contr(...) })
    result
  }))
}

test_that("granovagg.contr returns plots and prints weighted means for arousal", {
  data("arousal", package = "granovaGG")
  contrasts22 <- data.frame(
    c(-0.5, -0.5, 0.5, 0.5),
    c(-0.5, 0.5, -0.5, 0.5),
    c(0.5, -0.5, -0.5, 0.5)
  )
  names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")
  plots <- quiet_contr(arousal, contrasts = contrasts22)
  expect_length(plots, 4)
  
  messages <- capture.output(
    granovagg.contr(arousal, contrasts = contrasts22),
    type = "message"
  )
  expect_true(any(grepl("Weighted", messages, fixed = TRUE)))
  
  summary_data <- attr(plots, "summary.data")
  expect_false(is.null(summary_data))
  long_data <- data.frame(
    contrast_number = rep(as.character(seq_len(ncol(arousal))), each = nrow(arousal)),
    score = as.vector(as.matrix(arousal))
  )
  manual_summary <-
    long_data |>
    dplyr::group_by(contrast_number) |>
    dplyr::summarise(
      group.mean = mean(score),
      standard.deviation = sd(score),
      group.size = dplyr::n(),
      .groups = "drop"
    )
  expected_pooled <-
    sqrt(sum((manual_summary$group.size - 1) *
               manual_summary$standard.deviation^2) /
           sum(manual_summary$group.size - 1))
  expect_equal(
    unique(summary_data$pooled.standard.deviation),
    expected_pooled,
    tolerance = 1e-10
  )
})

test_that("granovagg.contr handles default and custom x-axis labels", {
  data("arousal", package = "granovaGG")
  contrasts22 <- data.frame(
    c(-0.5, -0.5, 0.5, 0.5),
    c(-0.5, 0.5, -0.5, 0.5),
    c(0.5, -0.5, -0.5, 0.5)
  )
  names(contrasts22) <- c("Drug.A", "Drug.B", "Drug.A.B")

  default_plots <- quiet_contr(arousal, contrasts = contrasts22)
  expect_identical(default_plots[[1]]$labels$x, "Contrast Drug.A")

  custom_labels <- quiet_contr(
    arousal,
    contrasts = contrasts22,
    xlab = c("Drug A vs rest", "Drug B vs rest", "Interaction")
  )
  expect_identical(custom_labels[[2]]$labels$x, "Drug B vs rest")

  named_override <- quiet_contr(
    arousal,
    contrasts = contrasts22,
    xlab = c(Drug.B = "Custom Drug B label")
  )
  expect_identical(named_override[[2]]$labels$x, "Custom Drug B label")
  expect_identical(named_override[[3]]$labels$x, "Contrast Drug.A.B")

  expect_error(
    granovagg.contr(arousal, contrasts = contrasts22, xlab = c("only two", "labels")),
    "Provide exactly 3 x-axis labels"
  )
  expect_error(
    granovagg.contr(arousal, contrasts = contrasts22, xlab = c(Unknown = "Nope")),
    "x-axis overrides provided for unknown contrasts"
  )
})
