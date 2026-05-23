#' Elemental Graphic Display for Contrast Effect of ANOVA
#'
#' Provides graphic displays that shows data and effects for a priori contrasts
#' in ANOVA contexts; also corresponding numerical results.
#'
#' Function provides graphic displays of contrast effects for prespecified
#' contrasts in ANOVA. Data points are displayed as relevant for each contrast
#' based on comparing groups according to the positive and negative contrast
#' coefficients for each contrast on the horizontal axis, against response
#' values on the vertical axis. Data points corresponding to groups not being
#' compared in any contrast (coefficients of zero) are ignored. For each
#' contrast (generally as part of a 2 x 2 panel) a line segment is given that
#' compares the (weighted) mean of the response variable for the negative
#' coefficients versus the positive coefficients. Standardized contrasts are
#' used, wherein the sum of (magnitudes) of negative coefficients is unity; and
#' the same for positive coefficients. If a line is `notably' different from
#' horizontal (i.e. slope of zero), a `notable' effect has been identified;
#' however, the question of statistical significance generally depends on a
#' sound context-based estimate of standard error for the corresponding effect.
#' This means that while summary aov numerical results and test statistics are
#' presented (see below), the appropriateness of the default standard error
#' generally requires the analyst's judgment. The response values are to be
#' input in (a stacked) form, i.e. as a vector, for all cells (cf. arg. ylab).
#' The matrix of contrast vectors \code{contrasts} must have G rows (the number
#' of groups), and a number of columns equal to the number of prespecified
#' contrasts, at most G-1. If the number of columns of \code{contrasts} is G-1,
#' then the number per group, or cell size, is taken to be
#' \code{length(data)/G}, where \code{G = nrow(contrasts)}.
#'
#' If the number of columns of \code{contrasts} is less than G-1 then the user
#' must stipulate \code{npg}, the number in each group or cell.  The function
#' is designed for the case when all cell sizes are the same, and may be most
#' helpful when the a priori contrasts are mutually orthogonal (e.g., in power
#' of 2 designs, or their fractional counterparts; also when specific row or
#' column comparisons, or their interactions (see the example below based on
#' rat weight gain data)). It is not essential that contrasts be mutually
#' orthogonal; but mutual linear independence is required. (When factor levels
#' correspond to some underlying continuum a standard application might use
#' \code{con = contr.poly(G)}, for G the number of groups; consider also
#' \code{contr.helmert(G)}.)  The final plot in each application shows the data
#' for all groups or cells in the design, where groups are simply numbered from
#' 1:G, for G the number of groups, on the horizontal axis, versus the response
#' values on the vertical axis.
#'
#' @param data Vector of scores for all equally sized groups, or a data.fame or
#'   matrix where each column represents a group.
#' @param contrasts Matrix of column contrasts with dimensions (number of
#'   groups [G]) x (number of contrasts) [generally (G x G-1)].
#' @param ylab Character; y axis label. Defaults to a generic granova title.
#' @param xlab Optional character vector giving replacement x axis labels for
#'   each contrast panel. Provide a vector the same length as the number of
#'   contrasts (matched by position) or a named vector keyed by the contrast
#'   matrix column names. When \code{NULL}, labels default to the automatic
#'   \code{"Contrast <name>"} strings derived from \code{contrasts}.
#' @param plot.theme argument indicating a ggplot2 theme to apply to the
#'   graphic; defaults to a customized theme created for the contrast graphic
#' @param print.summary Logical; prints the linear model summary, weighted means,
#'   group statistics, and contrast matrix when \code{TRUE} (default). Set to
#'   \code{FALSE} to suppress all printed output, e.g. inside an RMarkdown chunk.
#' @param jj Numeric; controls \code{\link{jitter}} and allows you to control the
#'   degree of jitter in the contrast plots. When \code{jj = NULL} (the default)
#'   a width of \code{0.01} is used. Values between \code{0} and \code{1} are
#'   treated as the jitter width directly, while values \code{\ge 1} are divided
#'   by \code{100} for backwards compatibility with historical percent-based
#'   inputs. The same setting drives the per-group summary plot (scaled by three
#'   to keep those points legible).
#' @param ... Optional arguments to/from other functions.
#' @return a list of ggplot objects, one element per plot. That allows you to access any individual plot
#'   or plots, then modify them as you wish (with ggplot2 commands, for example).
#'
#'   The function also provides printed output:
#'   \item{Weighted Means}{Table showing the (weighted) means for positive
#'      and negative coefficients for each (row) contrast, and for each row, the
#'      difference between these means, and the standardized effect size in the
#'      final column.}
#'   \item{summary.lm}{Summary results for a linear
#'      model analysis based on the R function \code{lm} (When effects are simple,
#'      as in an equal n's power of 2 design, mean differences will generally
#'      correspond to the linear regression coefficients as seen in the \code{lm}
#'      summary results.)}
#'   \item{Contrasts}{The contrast matrix you specified.}
#'
#' @author Brian A. Danielak \email{brian@@briandk.com}\cr
#'   Robert M. Pruzek \email{RMPruzek@@yahoo.com}
#'
#' with contributions by:\cr
#'   William E. J. Doane \email{wil@@drdoane.com}\cr
#'   James E. Helmreich \email{James.Helmreich@@Marist.edu}\cr
#'   Jason Bryer \email{jason@@bryer.org}
#'
#' @seealso \code{\link{granovagg.1w}},
#'   \code{\link{granovagg.ds}}, \code{\link{granovaGG}}
#' @keywords hplot
#' @example demo/granovagg.contr.R
#' @references Wickham, H. (2009). Ggplot2: Elegant Graphics for Data Analysis. New York: Springer.
#' @references Wilkinson, L. (1999). The Grammar of Graphics. Statistics and computing. New York: Springer.
#' @import ggplot2
#' @import stats
#' @import utils
#' @import assertthat
#' @importFrom rlang .data
#' @export
granovagg.contr <- function(data,
                            contrasts,
                            ylab       = "default_y_label",
                            xlab       = NULL,
                            plot.theme = "theme_granova_contr",
                            jj         = NULL,
                            print.summary = TRUE,
                            ...
                   )
{

  # Plots responses by contrasts.
  # 'data' must be vector of scores for all equal size groups.
  # 'con' must be matrix of column contrasts with dimensions (number of groups) x (number of contrasts)
  # [generally n X n-1].  The number of rows = number 'cells' or groups.
  # Basic lm (regression) results are provided; orthogonal contrasts are ideal (but not essential).
  # 'jj' controls jitter.

  # 'ctr' is shorthand for the ConTRast data object that will hold all the information for plotting
  FormatResponseData <- function(data) {
    is.data.one.dimensional <- is.null(dim(data)[2])
    if (is.data.one.dimensional) {
      return(data)
    }

    return(stack(as.data.frame(data))[, 1])
  }

  GetDegreeOfJitter <- function(jj) {
    assert_that(
      is.null(jj) || (is.numeric(jj) && length(jj) == 1 && !is.na(jj)),
      msg = "`jj` must be NULL or a single numeric value"
    )
    if (is.null(jj)) {
      return(0.01)
    }
    assert_that(jj >= 0, msg = "`jj` must be non-negative")
    if (jj < 1) {
      return(jj)
    }
    return(jj / 100)
  }

  std.contr <- function(contrasts, tolerance = sqrt(.Machine$double.eps)^0.6) {
      if (!is.matrix(contrasts)) {
          contrasts <- as.matrix(contrasts)
      }
      if (sum(abs(colMeans(contrasts))) > tolerance) {
          stop("Input vector/matrix must have mean zero (for each column)")
      }
      if (ncol(contrasts) == 1) {
          contrasts <- matrix(contrasts, ncol = 1)
      }
      dg <- apply(abs(contrasts), 2, sum)
      if (length(dg) == 1) {
          dg <- as.matrix(dg)
      }
      standardized.contrasts <- round(2 * contrasts %*% diag(1/dg), 3)

      return(standardized.contrasts)
  }

  ResolveContrastLabelOverrides <- function(x.labels, contrast.matrix) {
    if (is.null(x.labels)) {
      return(NULL)
    }
    overrides <- unlist(x.labels, use.names = TRUE)
    assert_that(
      length(overrides) > 0,
      msg = "`xlab` overrides must include at least one character value"
    )
    label.names <- names(overrides)
    overrides <- as.character(overrides)
    if (!is.null(label.names)) {
      names(overrides) <- label.names
    }
    has.named.labels <- length(label.names) > 0 && any(nzchar(label.names))
    if (has.named.labels) {
      assert_that(
        all(nzchar(label.names)),
        msg = "Named `xlab` overrides must supply names for every element"
      )
      column.names <- colnames(contrast.matrix)
      assert_that(
        !is.null(column.names),
        msg = "Named `xlab` overrides require column names on the contrast matrix"
      )
      unknown <- setdiff(label.names, column.names)
      assert_that(
        length(unknown) == 0,
        msg = paste(
          "x-axis overrides provided for unknown contrasts:",
          paste(unknown, collapse = ", ")
        )
      )
      resolved <- rep(NA_character_, ncol(contrast.matrix))
      resolved[match(label.names, column.names)] <- overrides
      return(resolved)
    }
    if (length(overrides) == 1 && ncol(contrast.matrix) > 1) {
      overrides <- rep(overrides, ncol(contrast.matrix))
    }
    assert_that(
      length(overrides) == ncol(contrast.matrix),
      msg = sprintf(
        "Provide exactly %d x-axis labels (got %d)",
        ncol(contrast.matrix),
        length(overrides)
      )
    )
    return(overrides)
  }

  BuildContrastLabels <- function(contrast.matrix, x.labels) {
    base.labels <- vapply(
      X = seq_len(ncol(contrast.matrix)),
      FUN = function(index) { GetContrastName(contrast.matrix, index) },
      FUN.VALUE = character(1)
    )
    overrides <- ResolveContrastLabelOverrides(x.labels, contrast.matrix)
    if (is.null(overrides)) {
      return(base.labels)
    }
    replace.indices <- !is.na(overrides)
    base.labels[replace.indices] <- overrides[replace.indices]
    return(base.labels)
  }

  ResolveGroupLabels <- function(original.data, contrast.matrix, number.of.groups) {
    if (!is.null(dim(original.data))) {
      column.names <- colnames(original.data)
      if (!is.null(column.names) && length(column.names) == number.of.groups) {
        return(column.names)
      }
    }
    row.names <- rownames(contrast.matrix)
    if (!is.null(row.names) && length(row.names) == number.of.groups) {
      return(row.names)
    }

    return(as.character(seq_len(number.of.groups)))
  }

  indic <- function(xx) {
             mm <- matrix(0, length(xx), length(unique(xx)))
             indx <- ifelse(xx == col(mm), 1, 0)
             indx
  }

  AdaptVariablesFromGranovaComputations <- function () {

    response            <- FormatResponseData(data)
    contrasts           <- as.matrix(contrasts)
    assert_that(!is.null(dim(contrasts)),
                msg = "contrasts must be a matrix or two-dimensional object")
    number.of.groups    <- nrow(contrasts)
    assert_that(number.of.groups > 0,
                msg = "contrasts must have at least one row")
    assert_that(length(response) %% number.of.groups == 0,
                msg = "Number of responses must be a multiple of the number of groups")
    responses.per.group <- length(response)/number.of.groups
    group.identifiers   <- rep(1:number.of.groups, ea = responses.per.group)
    indicator.matrix    <- indic(group.identifiers)
    indicated.contrasts <- indicator.matrix %*% contrasts
    standardized.contrasts <- std.contr(indicated.contrasts)
    apply(contrasts, 2, function(column) {
      assert_that(abs(sum(column)) < sqrt(.Machine$double.eps),
                  msg = "Each contrast must sum to zero")
    })

    return(
        list(
          response                      = response,
          contrast.matrix               = contrasts,
          scaled.standardized.contrasts = standardized.contrasts * responses.per.group,
          number.of.contrasts           = dim(standardized.contrasts)[2],
          number.of.groups              = number.of.groups,
          responses.per.group           = responses.per.group,
          group.labels                  = ResolveGroupLabels(data, contrasts, number.of.groups),
          contrast.labels               = BuildContrastLabels(contrasts, xlab)
        )
    )
  }

  GetContrastPlotData <- function (ctr) {
    return(
      lapply(
        X         = 1:ctr$number.of.contrasts,
        FUN       = ExtractDataForPlot,
        contrasts = ctr$scaled.standardized.contrasts,
        response  = ctr$response
      )
    )
  }

  GetLinearModel <- function(ctr) {
    Contrast <- ctr$scaled.standardized.contrasts
    Response <- ctr$response

    return(lm(Response ~ Contrast))
  }

  ExtractDataForPlot <- function (contrasts, response, index) {
      non.zero.indicators <- contrasts[, index] != 0
      x.values <- contrasts[, index][non.zero.indicators]
      y.values <- response[non.zero.indicators]
      raw.data <- data.frame(x.values, y.values)

    return(
        list(
          raw.data     = raw.data,
          summary.data = GetSummary(raw.data)
        )
    )
  }

  GetSummary <- function(data) {
    # To appease R CMD check
    x.values <- NULL
    y.values <- NULL
    summary_output <- data |>
      dplyr::group_by(.data$x.values > 0) |>
      dplyr::summarise(
        contrasts = mean(.data$x.values),
        responses = mean(.data$y.values)
      )
    return(summary_output)
    # summary_output <- data %>% dplyr
    #   data, .(x.values > 0), summarise,
    #   contrasts          = mean(x.values),
    #   responses          = mean(y.values)
    # )
    # return(summary_output)
  }

  GetContrastPlots <- function (ctr) {
    return(
      lapply(
        X             = 1:ctr$number.of.contrasts,
        FUN           = ComposeContrastPlot,
        plot.data     = ctr$contrast.plot.data
      )
    )
  }

  ComposeContrastPlot <- function(plot.data, index) {
    p <- ggplot()
    p <- p + MeanResponse(plot.data[[index]]$raw.data$y.values)
    p <- p + JitteredResponsesByContrast(plot.data[[index]]$raw.data)
    p <- p + EffectsOfContrasts(plot.data[[index]]$summary.data)
    p <- p + ConnectEffectMeans(plot.data[[index]]$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + ContrastPlotTitle(ctr, index)
    p <- p + ContrastPlotXLabel(ctr, index)
    p <- p + YLabel()

    return(p)
  }

  MeanResponse <- function(response) {
    return(
      geom_hline(
        aes(yintercept = mean(response)),
        color = brewer.pal(8, "Set1")[1],
        data  = as.data.frame(data),
        alpha = 0.5,
        linewidth = 0.3
      )
    )
  }

  JitteredResponsesByContrast <- function (data) {
    return(
      geom_point(
        aes(
          x = .data$x.values,
          y = .data$y.values
        ),
        data     = data,
        position = position_jitter(height = 0, width = GetDegreeOfJitter(jj))
      )
    )
  }

  EffectsOfContrasts <- function(data) {
    return(
      geom_point(
        aes(
          x = .data$contrasts,
          y = .data$responses
        ),
        data  = data,
        color = brewer.pal(8, "Set1")[2],
        size  = 3,
        alpha = 0.75
      )
    )
  }

  ConnectEffectMeans <- function(data) {
    return(
      geom_line(
        aes(
          x = .data$contrasts,
          y = .data$responses
        ),
        data  = data,
        color = brewer.pal(8, "Set1")[2],
        alpha = 1,
        linewidth = 0.5
      )
    )
  }

  ContrastPlotTitle <- function(ctr, index) {
    plot.title <- paste("Coefficients vs. Response\n", ctr$contrast.labels[[index]])
    return(
        ggtitle(plot.title)
    )
  }

  ContrastPlotXLabel <- function(ctr, index) {
    return(
        ggplot2::xlab(ctr$contrast.labels[[index]])
    )
  }

  YLabel <- function() {
    result <- ylab
    if (ylab == "default_y_label") {
      result <- "Outcome (Response)"
    }

    return(ggplot2::ylab(paste(result)))
  }

  GetSummaryPlotData <- function(ctr) {
    raw.data <- as.data.frame(
                   matrix(ctr$response, ncol = ctr$number.of.groups)
                 )
    raw.data <- RenameSummaryColumnNames(raw.data, ctr$group.labels)
    raw.data <- raw.data |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "contrast_number",
        values_to = "score"
      )
    raw.data$contrast_number <- factor(
      raw.data$contrast_number,
      levels = ctr$group.labels
    )
    summary.data <- GetGroupSummary(raw.data)
    summary.data$contrast_number <- factor(
      summary.data$contrast_number,
      levels = ctr$group.labels
    )

    return(list(
                raw.data     = raw.data,
                summary.data = summary.data
          )
    )
  }

  RenameSummaryColumnNames <- function(data, labels) {
    assert_that(
      length(labels) == ncol(data),
      msg = sprintf(
        "Expected %d group labels but got %d",
        ncol(data),
        length(labels)
      )
    )
    colnames(data) <- labels

    return(data)
  }

  GetGroupSummary <- function(data) {
    summary_data <-
      data |>
      dplyr::group_by(.data$contrast_number) |>
      dplyr::summarise(
        group.mean = mean(.data$score),
        standard.deviation = sd(.data$score),
        group.size = dplyr::n(),
        .groups = "drop"
      )
    degrees_of_freedom <- sum(summary_data$group.size - 1)
    pooled_sd <- NA_real_
    if (degrees_of_freedom > 0) {
      pooled_variance <- sum(
        (summary_data$group.size - 1) * (summary_data$standard.deviation^2),
        na.rm = TRUE
      ) / degrees_of_freedom
      pooled_sd <- sqrt(pooled_variance)
    }
    summary_data$pooled.standard.deviation <- pooled_sd
    return(summary_data)
  }

  ComposeSummaryPlot <- function(plot.data) {
    p <- ggplot()
    p <- p + MeanResponse(plot.data$raw.data$value)
    p <- p + RawScoresByGroup(plot.data$raw.data)
    p <- p + MeansByGroup(plot.data$summary.data)
    p <- p + ConnectGroupResponseMeans(plot.data$summary.data)
    p <- p + Theme(plot.theme)
    p <- p + GroupSummaryPlotTitle(ctr)
    p <- p + GroupSummaryXLabel()
    p <- p + YLabel()
    return(p)
  }

  RawScoresByGroup <- function(data) {
    return(
      geom_point(
        aes(
          x = as.factor(.data$contrast_number),
          y = .data$score
        ),
        data = data,
        position = position_jitter(height = 0, width = 3 * GetDegreeOfJitter(jj))
      )
    )
  }

  MeansByGroup <- function(data) {
    return(
      geom_point(
        aes(
          x = .data$contrast_number,
          y = .data$group.mean
        ),
        data  = data,
        color = brewer.pal(8, "Set1")[2],
        size  = I(3),
        alpha = 1
      )
    )
  }

  ConnectGroupResponseMeans <- function(data) {
    return(
      geom_line(
        aes(
          x = .data$contrast_number,
          y = .data$group.mean
        ),
        data  = data,
        color = brewer.pal(8, "Set1")[2],
        group = 1, # https://stackoverflow.com/a/29019102
        alpha = 1
      )
    )
  }

  GroupSummaryPlotTitle <- function(ctr) {
    plot.title <- paste("Responses for all groups\n", "each n = ", ctr$responses.per.group)
    return(
      ggtitle(plot.title)
    )
  }

  GroupSummaryXLabel <- function() {
    return(xlab("Group Indicator"))
  }

  CollateOutputPlots <- function(ctr) {
    output <- list(NULL)

    for (i in 1:ctr$number.of.contrasts) {
      output[[i]] <- ctr$contrast.plots[[i]]
    }
    output[[ctr$number.of.contrasts + 1]] <- ctr$summary.plot

    return(output)
  }

  PrintOutput <- function() {
    PrintLinearModelSummary(ctr$linear.model)
    PrintSummaryDataByContrast(ctr)
    PrintSummaryDataByGroup(ctr)
    PrintContrasts(ctr)
  }

  PrintLinearModelSummary <- function(model) {
    model.summary <- summary(model)
    message("\nLinear Model Summary")
    print(model.summary)
  }

  GetSummaryDataByContrast <- function(x, pooled.standard.deviation) {
    ExtractData <- function(x, pooled_standard_deviation) {
      summary.data <- x$summary.data
      neg <- summary.data$responses[summary.data$contrasts <= 0]
      pos <- summary.data$responses[summary.data$contrasts > 0]
      diff <- pos - neg
      stEftSze <- (pos - neg) / pooled_standard_deviation
      return(data.frame(neg, pos, diff, stEftSze))
    }
    output <- sapply(
      X = x,
      FUN = ExtractData,
      pooled_standard_deviation = pooled.standard.deviation
    )
    output <- output |>
      t() |>
      as.data.frame() |>
      ForceRowNamesToBeContrastNumbers()
    return(output)
  }

  ForceRowNamesToBeContrastNumbers <- function(x) {
    dimnames(x)[[1]] <- sapply(1:(dim(x)[[1]]), function(x) {paste("Contrast", x, sep="")})

    return(x)
  }

  PrintSummaryDataByContrast <- function(ctr) {
    message("\n(Weighted) means, mean differences, and standardized effect size")
    print(
      GetSummaryDataByContrast(ctr$contrast.plot.data, ctr$summary.plot.data$summary.data$pooled.standard.deviation[1]), digits = 3)
  }

  PrintSummaryDataByGroup <- function(ctr) {
    message("\nSummary statistics by group")
    print(ctr$summary.plot.data$summary.data, digits = 4)
  }

  PrintContrasts <- function(ctr) {
    message("\nThe contrasts you specified")
    print(ctr$contrast.matrix, digits = 3)
  }

  ctr                        <- AdaptVariablesFromGranovaComputations()
  ctr$linear.model           <- GetLinearModel(ctr)
  ctr$contrast.plot.data     <- GetContrastPlotData(ctr)
  ctr$contrast.plots         <- GetContrastPlots(ctr)
  ctr$summary.plot.data      <- GetSummaryPlotData(ctr)
  ctr$summary.plot           <- ComposeSummaryPlot(ctr$summary.plot.data)
  ctr$output                 <- CollateOutputPlots(ctr)

  if (print.summary) PrintOutput()
  output <- ctr$output
  attr(output, "summary.data") <- ctr$summary.plot.data$summary.data
  return(output)
}
