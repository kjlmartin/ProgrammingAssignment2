## These two functions cache the inverse of a matrix. They are meant to be used
## when the inverse needs to be used repeatedly.

## This function creates an object that is a matrix than can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse)i <<-inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## CacheSolve checks to see if the inverse of the CacheMatrix is already defined.
## If it is, it returns the cached value. If the inverse is not defined, it calculates 
## and caches the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message ("getting cached data")
                return(i)
        }
        data <- x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}
