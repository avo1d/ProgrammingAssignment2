
## Map functions set, get, setinverse, getinverse to list
makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setinverse <- function(i) invert <<- i
    getinverse <- function() invert
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## If invert already assigned to getinverse then return value
## else solve for inverse matrix
cacheSolve <- function(x, ...) {
    invert <- x$getinverse()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinverse(invert)
    invert
    ## Return a matrix that is the inverse of 'x'
}