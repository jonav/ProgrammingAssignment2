## These functions are used to generate the inverse of a matrix efficiently.

## This function takes a matrix and turns it into a list that can be used to cache the inverse of the matrix
## along with the matrix itself.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function will return the inverse of a matrix given a list containing the matrix and a cache of the result.
## If the inverse was previously computed, it will return the cached value.  If not, it will execute the inverse
## using the solve function.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
