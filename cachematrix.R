## makeCacheMatrix is a function that creates a matrix and sets the inverse of matrix.
## cacheSolve will calculate the inverse of the matrix created from the makeCacheMatrix function. 
## if the value of the inverse exists already in the above cache the function will omit the  computation
## The cacheSolve function will get the cache, or else it will compute the inverse and sets its value to the cache.

makeCacheMatrix <- function(x = matrix()){ ## creating the makeCacheMatrix function
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}
cacheSolve <- function(x, ...){ ## creating the cacheSolve function
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
