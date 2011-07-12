GeomRugAlt <- proto(
  Geom, {
    draw <- function(., data, scales, coordinates, ...) {  
      rugs <- list()
      data <- coordinates$transform(data, scales)    
      if (!is.null(data$x)) {
        rugs$x <- with(data, segmentsGrob(
          x0 = unit(x, "native"), x1 = unit(x, "native"), 
          y0 = unit(1 - 0.03, "npc"), y1 = unit(1, "npc"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }  

      if (!is.null(data$y)) {
        rugs$y <- with(data, segmentsGrob(
          y0 = unit(y, "native"), y1 = unit(y, "native"), 
          x0 = unit(1, "npc"), x1 = unit(1 - 0.03, "npc"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }  

      gTree(children = do.call("gList", rugs))
    }

    objname <- "rug_alt"

    desc <- "Marginal rug plots"

    default_stat <- function(.) StatIdentity
    default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
    guide_geom <- function(.) "path"

    examples <- function(.) {
      p <- ggplot(mtcars, aes(x=wt, y=mpg))
      p + geom_point()
      p + geom_point() + geom_rug_alt()
      p + geom_point() + geom_rug_alt(position='jitter')
    }
  }
)

geom_rug_alt <- GeomRugAlt$build_accessor()

vplayout <- function(x, y) {
  return(
    viewport(layout.pos.row = x, 
             layout.pos.col = y
    )
  )
}

LayoutFourPlotsPerPage <- function(list.of.plots) {
  total.plots     <- length(list.of.plots)
  holdover.plots  <- total.plots %% 4
  proper.plots    <- total.plots - holdover.plots
  
  if (proper.plots > 0) {
    LayoutProperPlots(list.of.plots, proper.plots)
  }
  LayoutHoldoverPlots(list.of.plots, holdover.plots)
}

LayoutProperPlots <- function(list.of.plots, proper.plots) {
  for (i in 1:proper.plots) {
    grid.arrange(list.of.plots[[i]],
                 list.of.pllots[[i+1]]
                 list.of.pllots[[i+2]]
                 list.of.pllots[[i+3]]
    )
    i <- i + 4
  }
}

SetupFourPlotPage <- function(index) {
  if (index %% 4 == 1) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
  }
}

PrintPlots <- function(list.of.plots, plot.num) {
  row <- ceiling( (plot.num + 2) /2) %% 2 + 1
  col <- ((plot.num + 1) %% 2) + 1
  
  return(
    print(list.of.plots[[plot.num]], vp = vplayout(row, col))
  )
}

DisplayEndOfPageMessage <- function(plot.num) {
  if (plot.num %% 4 == 0 && interactive()) {
    readline("Examine the contrast plots and consider printing. When you're done, press <Return>")
  }
}

Theme <- function(plot.theme) {
    return(
      eval(call(plot.theme))
    )
}

GetContrastName <- function(ctr, index) {
  if (is.null(dimnames(ctr$contrast.matrix))) {
    contrast.name <- paste(index)
  }
  else {
    provided.contrast.name <- dimnames(ctr$contrast.matrix)[[2]][index]
    contrast.name <- paste(provided.contrast.name)
  }
  
  return(paste("Contrast ", contrast.name))
}

ReorderDataByColumn <- function(x, column) {
  return(x[order(x[[column]]), ])
}

OverlapWarning <- function(data, tolerance) {
  first <- FirstElementOverlaps(data, tolerance)
  inner <- InnerElementsOverlap(data, tolerance)
  last  <- LastElementOverlaps(data, tolerance)
  
  return(c(first, inner, last))
}

FirstElementOverlaps <- function(data, tolerance) {
  return(
    abs(data[1] - data[2]) < tolerance
  )
}

InnerElementsOverlap <- function(data, tolerance) {
  return(sapply(
           X         = 2:(length(data) - 1),
           FUN       = NeighborOverlaps,
           data      = data,
           tolerance = tolerance
         )
  )
}

LastElementOverlaps <- function(data, tolerance) {
  last.index <- length(data)
  return(
    abs(data[last.index] - data[(last.index) - 1]) < tolerance
  )
}

NeighborOverlaps <- function(data, tolerance, index) {
  left.overlap  <- abs(data[index] - data[(index - 1)]) < tolerance
  right.overlap <- abs(data[index] - data[(index + 1)]) < tolerance
  
  return(left.overlap | right.overlap)
}