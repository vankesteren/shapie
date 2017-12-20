#  Copyright (c) 2017 Erik-Jan van Kesteren
#  
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#    
#  The above copyright notice and this permission notice shall be included in 
#  all copies or substantial portions of the Software.
#  
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

precision <- 400

theta <- cbind(
  seq(0, 2*pi, length.out = precision),
  seq(0, 4*pi, length.out = precision),
  seq(0, 6*pi, length.out = precision),
  seq(0, 8*pi, length.out = precision)
)


nameToNumber <- function(str) {
  hash <- digest(str, algo='md5', serialize = FALSE)
  split <- strsplit(hash, "")[[1]]
  codes <- sapply(split, utf8ToInt)
  return((codes%*%codes) %% 1.9e9) # stay within int range
}

drawShape <- function(theta, seed, alt = "shape", 
                      nlines = 7, complexity = 2, 
                      colfun = rainbow) {
  set.seed(seed)
  cols <- colfun(nlines)
  tmp <- tempfile(fileext = ".svg")
  svg(tmp, width = 7, height = 7)
  par(mar = c(0,0,0,0), bg = "black")
  plot(c(-1, 1), c(-1, 1), type = "n", axes = F, xlab = "", ylab = "")
  for (i in 1:nlines) {
    xparams <- rtruncnorm(complexity, -0.5, 0.5, 0, 0.25)
    yparams <- rtruncnorm(complexity, -0.5, 0.5, 0, 0.25)
    x <- sin(theta[,1:complexity])%*%xparams
    y <- cos(theta[,1:complexity])%*%yparams
    lines(x = x, y = y, col = cols[i])
  }
  dev.off()
  
  return(list(src = tmp, contentType = "image/svg+xml", 
              style = "max-width:100%;max-height:100%;"))
}
