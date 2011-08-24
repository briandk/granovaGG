theme_granova_1w <- function(base_size = 12) {
  theme_granova_1w                  <- theme_bw()
  theme_granova_1w$axis.text.x      <- theme_text(size = 8, lineheight = 0.9, vjust = 0.5, angle = 90, colour = "grey50")
  theme_granova_1w$axis.text.y      <- theme_text(size = 8, lineheight = 0.9, hjust = 1, colour = "grey50")
  theme_granova_1w$axis.title.x     <- theme_text(size = 8, vjust = 0, colour = "grey20")
  theme_granova_1w$axis.title.y     <- theme_text(size = 8, angle = 90, vjust = 0.3, hjust = 0.5, colour = "grey20")
  
  theme_granova_1w$legend.text      <- theme_text(size = 8, lineheight = 8)
  theme_granova_1w$legend.key.size  <- unit(0.5, "lines")
  
  theme_granova_1w$panel.border     <- theme_blank()
  theme_granova_1w$panel.grid.minor <- theme_line(colour = NA, size = 0.25)
  theme_granova_1w$panel.grid.major <- theme_line(colour = "grey90", size = 0.1)
  theme_granova_1w$plot.title       <- theme_text(face = "bold", size = 10, vjust = 1)
  return(theme_granova_1w)
}
  
theme_granova_1w_gray <- function(base_size = 12) {
  theme_granova_1w_gray             <- theme_gray()
  theme_granova_1w_gray$axis.text.x      <- theme_text(size = 8, lineheight = 0.9, vjust = 0.5, angle = 90, colour = "grey50")
  theme_granova_1w_gray$axis.text.y      <- theme_text(size = 8, lineheight = 0.9, hjust = 1, colour = "grey50")
  theme_granova_1w_gray$axis.title.x     <- theme_text(size = 8, vjust = 2, colour = "grey20")
  theme_granova_1w_gray$axis.title.y     <- theme_text(size = 8, angle = 90, vjust = 0.3, hjust = 0.5, colour = "grey20")
  
  theme_granova_1w_gray$legend.text      <- theme_text(size = 8)
  theme_granova_1w_gray$legend.key.size  <- unit(0.5, "lines")
  
  theme_granova_1w_gray$panel.border     <- theme_blank()
  theme_granova_1w_gray$panel.grid.minor <- theme_line(colour = NA, size = 0.25)
  theme_granova_1w_gray$panel.grid.major <- theme_line(colour = "white", size = 0.1)
  theme_granova_1w_gray$plot.title       <- theme_text(face = "bold", size = 10, vjust = -1)
  return(theme_granova_1w_gray)
}

theme_granova_ds <- function(base_size = 12) {
  theme_granova_ds <- theme_bw()
  
  theme_granova_ds$axis.title.x    <- theme_text(size = 10)
  theme_granova_ds$axis.title.y     <- theme_text(size = 10, angle = 90)
  
  theme_granova_ds$legend.key.size <- unit(1, "lines")
  theme_granova_ds$legend.text     <- theme_text(size = 8, lineheight = 8)
  theme_granova_ds$plot.title      <- theme_text(face = "bold", size = base_size)
  
  return(theme_granova_ds)
}

theme_granova_contr <- function(base_size = 12) {
  theme_granova_contr <- theme_bw()
  theme_granova_contr$axis.text.x      <- theme_text()
  theme_granova_contr$axis.text.y      <- theme_text(hjust = 1)
  theme_granova_contr$axis.title.x     <- theme_text(size = 10, vjust = -0.3)
  theme_granova_contr$axis.title.y     <- theme_text(size = 10, angle = 90, vjust = 0.3, hjust = 0.5)
  
  theme_granova_contr$panel.grid.major <- theme_blank()
  theme_granova_contr$panel.grid.minor <- theme_blank()
  
  theme_granova_contr$plot.title       <- theme_text(face = "bold", size = base_size, vjust = 1)
  
  return(theme_granova_contr)
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
