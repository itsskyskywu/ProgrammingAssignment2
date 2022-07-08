makeCacheMatrix <- function( mat = matrix() ) {
    a <- NULL
    set <- function( matrix ) {
            mat <<- matrix
            a <<- NULL
    }
    get <- function() {
    	mat
    }
    setInverse <- function(inverse) {
        a <<- inverse
    }
    getInverse <- function() {
        a
    }
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
    mat <- x$getInverse()
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }
    data <- x$get()
    mat <- solve(data) %*% data
    x$setInverse(mat)
    mat
}
