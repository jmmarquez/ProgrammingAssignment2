## Matrix inversion is usually a costly computation and their may be
## some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Following two funcions let cache a matrix and its inverse and only need calculate
## once the inverse

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It provides a list with 4 "methods" for store the original matrix(set),
## get the original matrix (get),store the matrix inverse (setinversa)
## and get the invers matrix (getinversa)

makeCacheMatrix <- function(x = matrix()) {
    # Add local variable inversa inititalize to null
    inversa <- NULL
    # Build special matrix as a list with x + 4 elements (functions: set, get, setinversa,getinversa)
    set <- function(y) {
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    setinversa <- function(m.inversa) inversa <<- m.inversa
    getinversa <- function() inversa
    list(set = set, get = get,
         setinversa = setinversa,
         getinversa = getinversa)
}


## Function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), the the cachedSolve retrieve 
## the inverse from the cache
## This function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    inversa <- x$getinversa()
    # If inversa of x exist (is cached) return inversa
    if (!is.null(inversa)){
        message("getting cached inversa")
        return(inversa)
    }
    # If inversa of x doesn't exist, get cached matrix, computes inverse and cache inverse
    # and returns inverse
    data <-x$get()
    inversa<-solve(data,...)
    x$setinversa(inversa)
    inversa
}
