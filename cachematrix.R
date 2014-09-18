##Basicly this functions goal is to return the given matrix's inverse. So you set the matrix, and in the end you get its inverse. The special characteristic is that it stores the inverse in the cache, so it doesn't have to calculate it everytime it is asked for.

## This function creates a special "matrix", which is a list of functions, and is able to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
	 s <- NULL
        set <- function (y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the makeCacheMatrix, and if it is already computed gives its inverse from the cache.

cacheSolve <- function(x, ...) {
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
