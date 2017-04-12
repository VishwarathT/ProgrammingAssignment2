## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
    # initialize input variables
    m <- NULL
    
    # creates method to reset cached variables
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # creates method to retrieve the value of the input matrix
    get <- function() x
    
    # created method to set the value of cached inversed matrix
    setinverse <- function(inverse) m <<- inverse
    
    #create method to retrieve the cached inversed matrix
    getinverse <- function() m
    
    #create the resulting object containing all the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function return a matrix that is the inverse of 'x'. 
## 
## The argument for the function is an object of type makeCacheMatrix(), and the inversed
## matrix will be retrieved from the cache if it is already calculated. 
## In the case it is a new input matrix, the inversed matrix will be calculated again 
## and the result will be cached.
##
cacheSolve <- function(x, ...) {
    # try to get a cached inversed matrix
    m <- x$getinverse()
    
    # if cached matrix exists, issue a message, returns it, and ends execution
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if cached matrix does not exists
    # retrieves the new input matrix
    data <- x$get()
    
    #calculates its inverse
    m <- solve(data, ...)
    
    #cache the result
    x$setinverse(m)
    
    #returns the new result
    m
}
