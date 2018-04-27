## File contains the pair of functions that cache the inverse of a matrix. 
##

## makeCacheMatrix function creates an R object that stores a matrix and can cache its inverse.
## It builds a set of functions and returns those functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function requires an argument that is returned by makeCacheMatrix () in order to retrieve the inversed matrix from the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inversed matrix")
                
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}