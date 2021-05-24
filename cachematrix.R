## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Stores the inverted matrix in the cache to save computational time
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## The inverse matrix will be searched in the cache, 
##to save computation time, if it does not exist, 
##the data will be calculated with the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cache data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
