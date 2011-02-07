top_rug <- function(data = NULL, xlabs = NULL, breaks = NULL, ...) {

  args <- list(...)
  if ( !is.null(args$angle) ) {
    angle = args$angle
  } else {
    angle = 0
  }
    
  x <- data[, 1]
  y <- data[, 2]
  
  # rename columns of data parameter so I can use it as the data attribute of geom_segment
  names(data)[1] <- "x"
  names(data)[2] <- "y"
  
  # create default labels
  if (is.null(xlabs)) {
    xlabs <- seq(from = floor(min(x)), to = ceiling(max(x)) )
  }

  # create default breaks
  if (is.null(breaks)) {
    breaks <- seq(from = floor(min(x)), to = ceiling(max(x)) )
  }

  if (length(xlabs) != length(breaks)) {
    stop("xlabs and breaks must be the same length")  
  }

  data.labels <- data.frame(xlabs, breaks, maxy=max(y))

  # ignore breaks that are outside the data range
  # ggplot does this automatically, but we need to do it manually
  data.labels <- data.labels[data.labels$breaks >= min(x), ]
  data.labels <- data.labels[data.labels$breaks <= max(x), ]

  result = list(geom_segment(aes(x    = x, 
                                 y    = max(y) + (max(y)/50), 
                                 xend = x, 
                                 yend = max(y) + (max(y)/60)  
                                ),
                             data = data
                            ),     #top-ticks
             
                geom_segment(aes(x    = min(x), 
                                 y    = max(y) + (max(y)/50), 
                                 xend = max(x), 
                                 yend = max(y) + (max(y)/50)  
                                ),
                             data = data
                            )      #top-axis
               )

       
      
  result = c(result,  geom_text(aes(label = xlabs, 
                                    x     = breaks, 
                                    y     = maxy + (maxy/30)
                                   ),
                                size  = 4,
                                angle = angle,
                                data  = data.labels
                               )
            )
  

  return(result)

}








right_rug <- function(data = NULL, ylabs = NULL, breaks = NULL, ...) {

  args <- list(...)
  if ( !is.null(args$angle) ) {
    angle = args$angle
  } else {
    angle = 0
  }

  x <- data[, 1]
  y <- data[, 2]
  
  # rename columns of data parameter so I can use it as the data attribute of geom_segment
  names(data)[1] <- "x"
  names(data)[2] <- "y"
  
  # create default labels
  if (is.null(ylabs)) {
    ylabs <- seq(from = floor(min(y)), to = ceiling(max(y)) )
  }

  # create default breaks
  if (is.null(breaks)) {
    breaks <- seq(from = floor(min(y)), to = ceiling(max(y)) )
  }

  if (length(ylabs) != length(breaks)) {
    stop("ylabs and breaks must be the same length")  
  }

  data.labels <- data.frame(ylabs, breaks, maxx=max(x))

  # ignore breaks that are outside the data range
  # ggplot does this automatically, but we need to do it manually
  data.labels <- data.labels[data.labels$breaks >= min(y), ]
  data.labels <- data.labels[data.labels$breaks <= max(y), ]


  result = list(geom_segment(aes(x    = max(x) + (max(x)/50), 
                                 y    = y, 
                                 xend = max(x) + (max(x)/60), 
                                 yend = y  
                                ),
                             data = data
                            ),     #right-ticks
             
                geom_segment(aes(x    = max(x) + (max(x)/50), 
                                 y    = min(y), 
                                 xend = max(x) + (max(x)/50), 
                                 yend = max(y)
                                ),
                             data = data
                            )      #right-axis
               )

  result = c(result,  geom_text(aes(label = ylabs, 
                                    x     = maxx + (maxx/30), 
                                    y     = breaks
                                   ),        
                                size  = 4,
                                angle = angle,
                                data  = data.labels
                               )
            )
 
  return(result)

}






# # just another example...
# x <- rnorm(20)
# y <- rnorm(20)
# dd <- data.frame(x, y)
# 
# p <- qplot(x,y)
# 
# xxx <- c( min(x), max(x) )
# xxx <- round(xxx, 2)
# 
# yyy <- c( min(y), max(y) )
# yyy <- round(yyy, 2)
# 
# p + top_rug(data=dd, xlabs=xxx, breaks=xxx) + right_rug(data=dd, ylabs=yyy, breaks=yyy)
# 








if ( !interactive() ) {
  library(ggplot2)
}

## Brian's Sample Code
set.seed(seed = 1001)

dollars <- seq(20000, 40000, 2000)
dollar.labels <- paste("$", dollars)

euros <- dollars * 0.775
euro.labels <- round(euros, 0)

pounds <- seq(6, 11, 1)
pound.labels <- paste(pounds, "lbs.")

kilos <- pounds * 0.4536
kilo.labels <- paste( round(kilos, 1), "kg")

d <- data.frame(annual.income = rnorm(n = 25, mean = 30000, sd = 2500), birth.weight = rnorm(n = 25, mean = 8, sd = 1))
p <- qplot(x = annual.income, y = birth.weight, data = d)
p <- p + scale_x_continuous("Annual Income", breaks=dollars, labels = dollar.labels)
p <- p + scale_y_continuous("Birth Weight", breaks = pounds, labels = pound.labels)


p + top_rug(data=d, xlabs=euro.labels, breaks=dollars, angle=45)  + right_rug(data=d, ylabs=kilo.labels, breaks=pounds)











