## This cachematrix.R function caches the inverse of a matrix. 
## It consists of two functions : makeCacheMatrix, cacheSolve

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
           x <<- y
           i <-- NULL
        }
        get <- function () x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get= get, setinv = setinv, getinv = getinv) 
}


## cacheSolve retrieves the cached inverse of makeCacheMatrix,
## if the cache does not exist or the matrix has changed then it computes the inverse 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i) { #returns matrix inverse if cached
            message("getting cached inverse")
            return(i)
        }
        mdata <- x$get()
        i <- solve(mdata, ...)
        x$setinv(i)
        i #returns matrix inverse if not cached
}

