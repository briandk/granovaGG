# Gray theme
# Produce a theme with gray background and white gridlines
# 
# @argument base font size
# @keywords dplot
# @alias theme_grey
theme_granova <- function(base_size = 12) {
  theme_granova                  <- theme_gray()
  theme_granova$panel.background <- theme_rect(fill = "grey95", colour = NA)
  theme_granova$panel.grid.minor <- theme_line(colour = "white", size = 0.25)
  
  return(theme_granova)
}

