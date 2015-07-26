# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    message("No Cache")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x <- rbind(c(1,4),c(-3,5))
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    4
## [2,]   -3    5
## > cacheSolve(m)
## No Cache
## [,1]        [,2]
## [1,] 0.2941176 -0.23529412
## [2,] 0.1764706  0.05882353
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]
## [1,] 0.2941176 -0.23529412
## [2,] 0.1764706  0.05882353
## > 