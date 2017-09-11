## Solve problems described here
## https://github.com/rdpeng/ProgrammingAssignment2
## https://github.com/markhowlandphd/ProgrammingAssignment2
## Caching the Inverse of a Matrix

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
        ## initialise m as NULL
        m <- NULL
        set <- function(y) {
        ## refresh cache
                x <<- y
        ## reset m to NULL
                m <<- NULL
        }
        get <- function() x
        ## update object
        setinv <- function(solve) m <<- solve   
        getinv <- function() m                 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

args(makeCacheMatrix)

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        ## determine if inverse exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if inverse does NOT exist, solve function to get inverse and output
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}

args(cacheSolve)