## Bellow we have two functions to cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverso <- NULL
    set <- function(y) {
        x <<- y
        inverso <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverso <<- inverse
    getinverse <- function() inverso
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverso <- x$getinverse()
    if(!is.null(inverso)) {
        message("getting cached data.")
        return(inverso)
    }
    data <- x$get()
    inverso <- solve(data)
    x$setinverse(inverso)
    inverso
}
