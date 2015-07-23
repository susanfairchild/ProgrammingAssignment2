## This pair of functions will cache the inverse of a matrix


## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){  ## Setting the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## Gets the value of the matrix
        setmatrix <- function(solve) m <<- solve ## Sets the value of the matrix
        getmatrix <- function() m                ## Gets the value of the 
        list(set=set, get=get,                   ## matrix
             setmatrix=setmatrix,                
             getmatrix=getmatrix)
}

## Compute the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()      ## Gets existing matrix data
        if(!is.null(m)) {      ## If m has a value, return the cached data   
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()      ## If m does not have value, calculate the 
        m <- solve(matrix, ...)## inverse of the matrix
        x$setmatrix(m)
        m
}
