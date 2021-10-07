## The function caches the inverse of the matrix

CacheInverse <- function( x = y() ) {
        a <- NULL
        make <- function( y ) {
                x <<- y
                a <<- NULL
                }
        take <- function( ) {
                x
                }
        makeInverse <- function( inverse ) {
                a <-inverse
                }
        takeInverse <- function( ) {
                a
                }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The function computes the inverse of the matrix from the previous function

cacheMean <- function( x, ... ) {
        ## Return a matrix that is the inverse of 'x'
        x <- x$getMean( )
        if ( !is.null(x) ) {
                message("getting inverse data")
                return( x )
                }
        data <- x$get( )
        x <- solve( data ) %*% data
        x$setMean( x )
        x
}
