theme_granova_1w <- function(base_size = 12) {
  theme_granova_1w                  <- theme_bw()
  theme_granova_1w$axis.text.x      <- theme_text(size = 8 , lineheight = 0.9, vjust = 1, angle = 90, colour = "grey50")
  theme_granova_1w$axis.text.y      <- theme_text(size = 8 , lineheight = 0.9, hjust = 1, colour = "grey50")
  theme_granova_1w$axis.title.x     <- theme_text(size = 8, vjust = 2, colour = "grey20")
  theme_granova_1w$axis.title.y     <- theme_text(size = 8, angle = 90, vjust = 0.3, hjust = 0.5, colour = "grey20")
  
  theme_granova_1w$legend.text      <- theme_text(size = 8)
  theme_granova_1w$legend.key.size  <- unit(0.5, "lines")
  
  theme_granova_1w$panel.border     <- theme_blank()
  theme_granova_1w$panel.grid.minor <- theme_line(colour = NA, size = 0.25)
  theme_granova_1w$panel.grid.major <- theme_line(colour = "grey90", size = 0.1)
  theme_granova_1w$plot.title       <- theme_text(face = "bold", size = 10, vjust = -1)
  return(theme_granova_1w)
}
  
theme_granova_ds <- function(base_size = 12) {
  theme_granova_ds <- theme_granova_1w()
  
  theme_granova_ds$legend.key.size <- unit(1.2, "lines")
  theme_granova_ds$legend.text     <- theme_text(size = base_size * 0.8)
  
  return(theme_granova_ds)
}
  
CreateRugRightGeom <- function() {
  GeomRugRight <- proto(Geom, {
    draw <- function(., data, scales, coordinates, ...) {  
      rugs <- list()
      data <- coordinates$transform(data, scales)    
      if (!is.null(data$x)) {
        rugs$x <- with(data, segmentsGrob(
          x0 = unit(x, "native"), x1 = unit(x, "native"), 
          y0 = unit(0, "npc"), y1 = unit(0.03, "npc"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }  

      if (!is.null(data$y)) {
        rugs$y <- with(data, segmentsGrob(
          y0 = unit(y, "native"), y1 = unit(y, "native"), 
          x0 = unit(0, "npc"), x1 = unit(0.03, "npc"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }  

      gTree(children = do.call("gList", rugs))
    }

    objname <- "rug_right"

    desc <- "Marginal rug plots on the right"

    default_stat <- function(.) StatIdentity
    default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
    guide_geom <- function(.) "path"
  })
  return(GeomRugRight)
}

  # structure(list(
  #   axis.line         = theme_blank(),
  #   axis.text.x       = theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1, angle = 45),
  #   axis.text.y       = theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
  #   axis.ticks        = theme_segment(colour = "grey50"),
  #   axis.title.x      = theme_text(size = base_size, vjust = 0.5),
  #   axis.title.y      = theme_text(size = base_size, angle = 90, vjust = 0.5),
  #   axis.ticks.length = unit(0.15, "cm"),
  #   axis.ticks.margin = unit(0.1, "cm"),
  # 
  #   legend.background = theme_rect(colour="white"), 
  #   legend.key        = theme_rect(fill = "grey95", colour = "white"),
  #   legend.key.size   = unit(1.2, "lines"),
  #   legend.text       = theme_text(size = base_size * 0.8),
  #   legend.title      = theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
  #   legend.position   = "right",
  # 
  #   panel.background  = theme_rect(fill = "grey90", colour = NA), 
  #   panel.border      = theme_blank(), 
  #   panel.grid.major  = theme_line(colour = "white"),
  #   panel.grid.minor  = theme_line(colour = "grey95", size = 0.25),
  #   panel.margin      = unit(0.25, "lines"),
  # 
  #   strip.background  = theme_rect(fill = "grey80", colour = NA), 
  #   strip.text.x      = theme_text(size = base_size * 0.8),
  #   strip.text.y      = theme_text(size = base_size * 0.8, angle = -90),
  # 
  #   plot.background   = theme_rect(colour = NA, fill = "white"),
  #   plot.title        = theme_text(size = base_size * 1.2),
  #   plot.margin       = unit(c(1, 1, 0.5, 0.5), "lines")
  # ), class            = "options")

