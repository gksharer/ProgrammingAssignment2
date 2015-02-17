## cachematrix.R function computes and caches the inverse of a matrix. 
## It consists of two functions : makeCacheMatrix, cacheSolve
## example usage:
## xm <- makeCacheMatrix(x=matrix(1:4,2,2))
## cacheSolve(xm)

## makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { 
           x <<- y          ##sets value of matrix x so that get() can return
           i <<- NULL       ##sets value of i to NULL for each new matrix
        }
        get <- function () x    
        setinv <- function(solve) i <<- solve    
        getinv <- function() i  
        list(set = set, get= get, setinv = setinv, getinv = getinv) 
        ## returns list of 4 functions : set, get, setinv, getinv
        ## that are used by cacheSolve
}


## cacheSolve retrieves the cached inverse of makeCacheMatrix,
## if the cache does not exist then it computes the inverse 
cacheSolve <- function(x, ...) {
        i <- x$getinv()         ## does matrix inverse exist in cache?
        if(!is.null(i)) {       ## if yes, returns inverse
            message("getting cached inverse")
            return(i)
        }
        mdata <- x$get()        ## if no, retrieves input matrix x
        i <- solve(mdata, ...)  ## computes inverse of x
        x$setinv(i)             ## caches inverse for future use
        i                       ## returns matrix inverse
}

