# Finding the Inverse of a Quadratic Equation 

## The Calibration Problem 

I found myself trying to build some R scripts for calculating an unknown 
concentration using an external calibration line.  This is a basic technique 
in science, where you measure some response for a range of known samples 
(your calibrators) and for your unknown samples.  You then construct a 
calibration line of response against known concentration, and read off 
your unknown concentrations against their measured response.  Easy.

This is easy in R (insert R example)


```r
simpleregression <- data.frame(x = 1:10, y = (1:10 * rnorm(10, mean = 0.5, sd = 0.05)))
simpleregression
```

```
##     x      y
## 1   1 0.5309
## 2   2 0.9513
## 3   3 1.2909
## 4   4 2.0230
## 5   5 2.5490
## 6   6 2.9564
## 7   7 3.0592
## 8   8 3.9246
## 9   9 5.5625
## 10 10 4.8947
```

```r

lmreg <- lm(data = simpleregression, x ~ y)

unknowns <- data.frame(y = runif(10, min = 0, max = 5))
unknowns
```

```
##         y
## 1  3.6863
## 2  2.7264
## 3  0.5838
## 4  3.2940
## 5  0.3254
## 6  2.3993
## 7  1.0965
## 8  0.7412
## 9  3.0964
## 10 2.1267
```

```r

unknowns$predx <- (unknowns$y/coef(lmreg)[[2]]) - coef(lmreg)[[1]]
unknowns
```

```
##         y   predx
## 1  3.6863  1.4983
## 2  2.7264  0.9569
## 3  0.5838 -0.2513
## 4  3.2940  1.2771
## 5  0.3254 -0.3970
## 6  2.3993  0.7725
## 7  1.0965  0.0378
## 8  0.7412 -0.1626
## 9  3.0964  1.1656
## 10 2.1267  0.6188
```


It gets slightly more difficult in this case because of two things; 
Quadratic Regressions and Weighting.  My calibration line was possibly quadratic
so I was using a quadratic regression.  I also wanted to weight the responses
in response to their predicted variability, in my case weighting would be 
proportional to 1/Concentration^2.

However, this now requires you to find the inverse of the resulting quadratic 
equation to back calculate the concentrations.  You can't just reverse the regression 
because then the weighting doesn't work.

In addition, a quadratic regression can have two given values for x at a given y, 
and we're only interested in one, so I wrote a small function to find the inverse of
a quadratic equation in a given interval.


```r

#' Calculate the inverse of a quadratic function y=ax^2+bx+c (ie find x when given y)
#' Gives NaN with non real solutions
#'
#' @param a The x coefficient
#' @param b The x^2 coefficient
#' @param c The Intercept
#' @param y The y value at which the roots should be found
#' @param roots To select which roots are required, 'both' will give both, or 'min' or 'max'
#' @param xmin The minimum X value to be considered
#' @param xmax The maximum X value to be considered
#' @param na.rm Should NA's be ignored
#' @returnType
#' @return A Vector containing the calculated x values at the given y, Imaginary values given as NaN
#' @author Paul Hurley
#' @export
#' @examples
#' invquad(1,1,-10,0)
#' invquad(0.1,0.01,-0.5,7,roots='max', xmin=1, xmax=15)
invquad <- function(a, b, c, y, roots = "both", xmin = (-Inf), xmax = (Inf), 
    na.rm = FALSE) {
    root1 <- sqrt((y - (c - b^2/(4 * a)))/a) - (b/(2 * a))
    root2 <- -sqrt((y - (c - b^2/(4 * a)))/a) - (b/(2 * a))
    root1 <- ifelse(root1 < xmin, NA, root1)
    root1 <- ifelse(root1 > xmax, NA, root1)
    root2 <- ifelse(root2 < xmin, NA, root2)
    root2 <- ifelse(root2 > xmax, NA, root2)
    if (roots == "both") {
        result <- c(root1, root2)
        if (na.rm) 
            result <- ifelse(is.na(root1), root2, result)
        if (na.rm) 
            result <- ifelse(is.na(root2), root1, result)
        if (na.rm) 
            result <- ifelse(is.na(root1) & is.na(root2), NA, result)
    }
    if (roots == "min") 
        result <- pmin(root1, root2, na.rm = TRUE)
    if (roots == "max") 
        result <- pmax(root1, root2, na.rm = TRUE)
    return(result)
}

# Some examples
invquad(1, 1, -10, 0)
```

```
## [1]  2.702 -3.702
```

```r

invquad(0.1, 0.01, -0.5, 7, roots = "max", xmin = 1, xmax = 15)
```

```
## [1] 8.61
```

