## Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly 
##This assignment is to write a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        in_verse <- NULL                     
        set <- function(y) {
                x <<- y
                in_verse <<- NULL  
        }
        get <- function() x
        set_inv <- function(inverse) in_verse <<- inverse
        get_inv <- function() in_verse  
        list(set = set, 
             get = get,
             set_inv = set_inv,
             get_inv = get_inv) 
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        in_verse <- x$get_inv()
        if(!is.null(in_verse)) {
                message("getting cached data")
                return(in_verse)
        }
        data <- x$get()
        in_verse <- solve(data, ...)
        x$set_inv(in_verse)
        in_verse
}
