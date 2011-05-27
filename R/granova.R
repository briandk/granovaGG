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
  for (plot.num in 1:length(list.of.plots)) {
    SetupFourPlotPage(plot.num)
    PrintPlots(list.of.plots, plot.num)
    DisplayEndOfPageMessage(plot.num)
  }
  return()
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

# theme_gray <- function(base_size = 12) {
#   structure(list(
#     axis.line         = theme_blank(),
#     axis.text.x       = theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1),
#     axis.text.y       = theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
#     axis.ticks        = theme_segment(colour = "grey50"),
#     axis.title.x      = theme_text(size = base_size, vjust = 0.5),
#     axis.title.y      = theme_text(size = base_size, angle = 90, vjust = 0.5),
#     axis.ticks.length = unit(0.15, "cm"),
#     axis.ticks.margin = unit(0.1, "cm"),
# 
#     legend.background = theme_rect(colour="white"), 
#     legend.key        = theme_rect(fill = "grey95", colour = "white"),
#     legend.key.size   = unit(1.2, "lines"),
#     legend.text       = theme_text(size = base_size * 0.8),
#     legend.title      = theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
#     legend.position   = "right",
# 
#     panel.background = theme_rect(fill = "grey90", colour = NA), 
#     panel.border     = theme_blank(), 
#     panel.grid.major = theme_line(colour = "white"),
#     panel.grid.minor = theme_line(colour = "grey95", size = 0.25),
#     panel.margin     = unit(0.25, "lines"),
# 
#     strip.background = theme_rect(fill = "grey80", colour = NA), 
#     strip.text.x     = theme_text(size = base_size * 0.8),
#     strip.text.y     = theme_text(size = base_size * 0.8, angle = -90),
# 
#     plot.background = theme_rect(colour = NA, fill = "white"),
#     plot.title      = theme_text(size = base_size * 1.2),
#     plot.margin     = unit(c(1, 1, 0.5, 0.5), "lines")
#   ), class          = "options")
# }
