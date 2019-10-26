library(caTools)

filepath <- getwd()
filename <- file.path(filepath, "R_DataScience", "dataVisualization", "MyMandelbrot.git")
     
myjet.colors <-  colorRampPalette(c("black", "green","red", "blue", "#007FFF", "cyan", "#7FFF7F",
                                    "yellow", "#FF7F00", "red", "#7F0000"))

xdx <- 4300
ydy <- 2400

xc <- complex(real = rep(seq(-2.6, 1.5, length.out = xdx), each = ydy),
              imag = rep(seq(-1.9, 1.3, length.out = ydy), xdx))


xc <- matrix(xc, ydy, xdx)
zz <- 0
xxx <- array(0, c(ydy, xdx, 40))

for(nk in 1:40){
  
  zz <-  zz^2 + xc
  xxx[, , nk] <- exp(-abs(zz))
  
}



write.gif(xxx, filename, col = myjet.colors, delay = 110)


