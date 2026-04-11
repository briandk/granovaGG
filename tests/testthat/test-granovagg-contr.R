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
