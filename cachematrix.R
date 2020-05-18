## A pair of functions to cache the inverse of a matrix
## The first function creates a special matrix object that can cache its
## inverse.
## The second function calculates the inverse of a matrix if the inverse
## has not been previously calculated.

## Function to create a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        #Cached inverse of matrix
        inv <- NULL
        #Set and Get functions for matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        #Set and Get functions for inverse
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        #List of functions for matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse of a matrix or returns the cached inverse
## if the inverse has previously been calculated and the matrix hasn't changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #If inverse is already calculated return cached value
        if(!is.null(inv)) {
                message("Using cached data")
                return(inv)
        }
        #If inverse not already calculated, calculate it
        mat <- x$get()
        inv <- solve(mat, ...)
        #Cache inverse for future use
        x$setInverse(inv)
        #Print inverse to console
        inv
}
