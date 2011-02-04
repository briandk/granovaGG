top_rug <- function(data = NULL, xlabs = NULL, breaks = NULL) {

  x <- data[, 1]
  y <- data[, 2]
  
  names(data)[1] <- "x"
  names(data)[2] <- "y"
  
  if (is.null(xlabs)) {
    xlabs <- seq(from = floor(min(x)), to = ceiling(max(x)) )
  }

  if (is.null(breaks)) {
    breaks <- seq(from = floor(min(x)), to = ceiling(max(x)) )
  }

  if (length(xlabs) != length(breaks)) {
    stop("xlabs and breaks must be the same length")  
  }


  result = list(geom_segment(aes(x    = x, 
                                        y    = max(y) + 0.15, 
                                        xend = x, 
                                        yend = max(y) + 0.2  
                                       ),
                             data = data
                            ),     #top-ticks
             
                geom_segment(aes(x    = min(x) - 1, 
                                        y    = max(y) + 0.2, 
                                        xend = max(x) + 1, 
                                        yend = max(y) + 0.2  
                                       ),
                             data = data
                            )      #top-axis
               )

  i = 1
  while (i <= length(xlabs)) {
    if ( (breaks[i] >= min(x)) && (breaks[i] <= max(x)) ) {
      result = c(result,  geom_text(aes_string(label = xlabs[i], 
                                               x     = breaks[i], 
                                               y     = max(y) + 0.3),        
                                    size  = 4
                                   )
                )
    }
    i = i + 1
  }

  return(result)

}

# x <- rnorm(20)
# y <- rnorm(20)
# dd <- data.frame(x, y)
# 
# p <- qplot(x,y)
# 
# p + top_rug(dd)
# 
# xxx <- c( min(x), max(x) )
# xxx <- round(xxx, 2)
# 
# p + top_rug(dd, xxx, xxx)
# 
# 
# 
# 
# 








## Brian's Sample Code
set.seed(seed = 1001)

dollars <- seq(20000, 40000, 2000)
dollar.labels <- paste("$", dollars)

euros <- dollars * 0.775
euro.labels <- round(euros, 0)

pounds <- seq(6, 11, 1)
pound.labels <- paste(pounds, "lbs.")

d <- data.frame(annual.income = rnorm(n = 25, mean = 30000, sd = 2500), birth.weight = rnorm(n = 25, mean = 8, sd = 1))
p <- qplot(x = annual.income, y = birth.weight, data = d)
p <- p + scale_x_continuous("Annual Income", breaks=dollars, labels = dollar.labels)
p <- p + scale_y_continuous("Birth Weight", breaks = pounds, labels = pound.labels)


p + top_rug(d, euro.labels, dollars)












