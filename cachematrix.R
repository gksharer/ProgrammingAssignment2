## cachematrix.R functioni computes and caches the inverse of a matrix. 
## It consists of two functions : makeCacheMatrix, cacheSolve

## makeCacheMatrix creates a matrix object that can cache its inverse; returns list of functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
           x <<- y      
           i <<- NULL   
        }
        get <- function () x    
        setinv <- function(solve) i <<- solve    
        getinv <- function() i  
        list(set = set, get= get, setinv = setinv, getinv = getinv) 
}


## cacheSolve retrieves the cached inverse of makeCacheMatrix,
## if the cache does not exist then it computes the inverse 

cacheSolve <- function(x, ...) {
        i <- x$getinv()   # does matrix inverse exist in cache?
        if(!is.null(i)) { # if yes, returns inverse
            message("getting cached inverse")
            return(i)
        }
        mdata <- x$get()        # if no, retrieve input matrix x
        i <- solve(mdata, ...)  # compute inverse of x
        x$setinv(i)             # cache inverse for future use
        i                       # return matrix inverse
}

