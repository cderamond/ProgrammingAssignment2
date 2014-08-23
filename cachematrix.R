## this assignment 2 for Coursera r-programing : rprog-006
## makeCacheMatrix creates a square matrix
## by Wikipedia: http://en.wikipedia.org/wiki/Invertible_matrix 
##note : A square matrix is singular if and only if its determinant is 0. 

## this function generates list of functions to operate
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, 
             get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}

## This will generate the invert and cache it, if and only if the matrix's determinant differs from 0

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(is.na(det(data)) || (det(data) == 0) ) {
                message("Matrix is either singular or its determinant doesn't exist")
                return(NA)
        }
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
