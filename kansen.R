library(plotly)
library(MASS)

# 3D kansen
kd <- kde2d(x=data$decimalLongitude, y=data$decimalLatitude)
plot_ly(x=kd$x, y=kd$y, z=kd$z) %>% add_surface() 


#integralen mbv integrate (1D)
d<- density.default(x=data$decimalLongitude)
plot(d); abline(v=1, col=2)
xx <- d$x
de <- xx[2L] - xx[1L]
yy<- d$y
f<- approxfun(xx, yy)
C <- integrate(f, min(xx), max(xx))$value
C
p.unscaled <- integrate(f, 1, max(xx))$value
p.scaled <- p.unscaled /C
p.scaled



#integralen kde2d mbv prob (2D)
n<- 50000
x<- data$decimalLongitude
y<- data$decimalLatitude
f1 <- kde2d(x=data$decimalLongitude, y=data$decimalLatitude, n=5000)
f2 <-kde2d(x=data$decimalLongitude, y=data$decimalLatitude)
lims <- c(min(x),max(x),min(y),max(y))

filled.contour(f1)

prob <- function(f,xmin,xmax,ymin,ymax,n,lims){
  ixmin <- max( 1, n*(xmin-lims[1])/(lims[2]-lims[1]) )
  ixmax <- min( n, n*(xmax-lims[1])/(lims[2]-lims[1]) )
  iymin <- max( 1, n*(ymin-lims[3])/(lims[4]-lims[3]) ) 
  iymax <- min( n, n*(ymax-lims[3])/(lims[4]-lims[3]) )
  avg <- mean(f$z[ixmin:ixmax,iymin:iymax])
  probval <- (xmax-xmin)*(ymax-ymin)*avg
  return(probval)
}
prob(f1,-75.1,-75.0,32.0,32.1,n,lims) #C[1] 1.774881e-25

lims


#plot GBS$groups op kaart

plot3 <- ggplot(data, aes(x=data$decimalLongitude, y=data$decimalLatitude)) + geom_point()
PD <- plot3 + geom_density_2d()
GB <- ggplot_build(PD)
GBC <- GB$data[[5]] #contouren