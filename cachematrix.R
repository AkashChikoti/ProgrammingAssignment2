## Caching inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(inv) s <<- inv
    getinv <- function() s
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Getting inverse of a matrix. If matrix is unchanged then getting the cached result.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
