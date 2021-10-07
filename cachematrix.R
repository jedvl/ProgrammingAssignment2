## A matrix that cache its inverse

makeCacheMatrix <- function(x = y() ) {
        a <- NULL
        set <- function( y ) {
                x <<- y
                a <<- NULL
                }
        get <- function() {
                x
                }
        setInverse <- function(inverse) {
                a <-inverse
                }
        getInverse <- function() {
                a
                }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Compute the inverse of the matrix from the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x <- x$getInverse()
        if ( !is.null(x) ) {
                message("getting inverse data")
                return(x)
                }
        data <- x$get()
        x <- solve(data) %*% data
        x$setInverse(x)
        x
}
