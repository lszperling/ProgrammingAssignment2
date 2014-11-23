## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## get the value of the matrix
## set the value of the matrix
## set the value of the inverse
## get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solution) inverse <<- solution
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This second function, cacheSolve, calculates de inverse of a matrix
## and stores it in order to use it as a cache and avoid recalculating every time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getInverse())){
        print("Calculating...")
        x$setInverse(solve(x$get(), ...))
    }
    else{
        print("It was already calculated")
    }
    x$getInverse()
}