## Bellow we have two functions to cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initiate the inverse property
    inverso <- NULL
    
    ## Set the matrix
    set <- function(y) {
        x <<- y
        inverso <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Set the matrix' inverse
    setinverse <- function(inverse) inverso <<- inverse
    
    ## Get the inverse of the matrix
    getinverse <- function() inverso
    
    ## Return a list of methods
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inverso <- x$getinverse()
    
    ## Returns the inverse only if it's already set
    if(!is.null(inverso)) {
        message("getting cached data.")
        return(inverso)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse
    inverso <- solve(data)
    
    ## Set the inverse to the object
    x$setinverse(inverso)
    
    ## Return the inverse matrix
    inverso
}
