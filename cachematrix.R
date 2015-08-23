## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialise the inversed matrix
        xinv <- NULL  
        ## Set a matrix as the object of makeCacheMatrix function
        set <- function (y) {
                x <<- y
                xinv <<- NULL
                
        } 
        ## Return the matrix
        get <- function() x 
        ## Set the inversed matrix
        setinv <- function(inv) xinv <<- inv
        ## Return the inversed matrix
        getinv <- function() xinv
        ## Prepare a list that contains all the above functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return the inversed matrix
        xinv <- x$getinv()
        ## if not null, return xinv data
        if (!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        ## else, prepare matrix to do calculate inverse
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
