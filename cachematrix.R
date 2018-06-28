## A pair of functions that cache the inverse of a matrix

## This makeCacheMatrix function creates a special "matrix" object that can cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { ## set the value of the matrix
                x <<- y
                s <<- NULL
        }
        get <- function() x ## get the value of the matrix
        setinverse <- function(solve) s <<- solve ## set the value of the inverse 
        getinverse <- function() s ## get the value of the inverse
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by the above matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
