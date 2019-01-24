## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## The following two functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invers<-NUll
        set<-function(y){
                x<-y
                invers<-Null}
        get<-function() x
        setinverse<-function(inverse) invers<-inverse
        getinverse<-function() invers
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinvers()
        if(!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }
        matr <- x$get()
        invers <- solve(matr, ...)
        x$setinverse(invers)
        invers
}
