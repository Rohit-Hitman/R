
#creating matrix
makeCacheMatrix <- function(x1 = matrix()) {
    j <- NULL
    set <- function(y) {
        x1 <<- y
        j <<- NULL
    }
    get <- function() x1
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#calculating inverse

cacheSolve <- function(x1, ...) {
    ## Return a matrix that is the inverse of 'x1'
    i <- x1$getinverse()
    if(!is.null(j)) {
        message("getting cached data")
        return(j)
    }
    data <- x1$get()
    j <- solve(data, ...)
    x1$setinverse(j)
    j
}

