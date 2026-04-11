test_that("granovagg.1w group summary matches poison dataset", {
  data("poison", package = "granovaGG")
  plot_obj <- suppressWarnings(
    suppressMessages(
      granovagg.1w(
        data = poison$SurvTime,
        group = poison$Group,
        resid = FALSE,
        print.squares = FALSE,
        print.group.summary = FALSE,
        print.model.summary = FALSE
      )
    )
  )
  group_summary <- attr(plot_obj, "group.summary")
  expect_false(is.null(group_summary))
  
  actual_summary <- group_summary |>
    dplyr::arrange(group) |>
    dplyr::select(
      group,
      group.mean,
      trimmed.mean,
      variance,
      standard.deviation,
      group.size
    ) |>
    dplyr::mutate(group = as.integer(as.character(group)))
  
  expected_summary <- poison |>
    dplyr::group_by(Group) |>
    dplyr::summarise(
      group.mean = mean(SurvTime),
      trimmed.mean = mean(SurvTime, trim = 0.2),
      variance = var(SurvTime),
      standard.deviation = sd(SurvTime),
      group.size = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(Group) |>
    dplyr::rename(group = Group)
  
  expect_equal(actual_summary, expected_summary, tolerance = 1e-8)
})

test_that("granovagg.1w F-statistic matches poison reference model", {
  data("poison", package = "granovaGG")
  plot_obj <- suppressWarnings(
    suppressMessages(
      granovagg.1w(
        data = poison$SurvTime,
        group = poison$Group,
        resid = FALSE,
        print.squares = FALSE,
        print.group.summary = FALSE,
        print.model.summary = FALSE
      )
    )
  )
  model_summary <- attr(plot_obj, "model.summary")
  expect_false(is.null(model_summary))
  
  expected_model <- summary(lm(SurvTime ~ factor(Group), data = poison))
  expect_equal(
    unname(model_summary$fstatistic["value"]),
    unname(expected_model$fstatistic["value"]),
    tolerance = 1e-10
  )
})
