Trails_ds <- function(dsp) {
  with(dsp$trails, {
      geom_segment(
        aes(x    = x.trail.start,
            y    = y.trail.start,
            xend = x.trail.end,
            yend = y.trail.end
        ),
        data     = dsp$trails,
        size     = I(1/3),
        color    = "black",
        linetype = 1,
        alpha    = I(1/10)              
      )
    }
  )
}
