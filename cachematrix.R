## There are two functions 'makeCacheMatrix' and 'cacheSolve' in this R file.
## These functions will be used to cache potentially time-consuming computations
## involved in calculating the inverse of a matrix. 

## This function 'makeCacheMatrix' creates a special 'matrix' object that can cache its inverse
## It returns a list containing the following functions that act on the given matrix
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the matrix inverse
##    4. get the value of the matrix inverse 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
  
    get <- function() x
  
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # The argument x is a special matrix that was created using a
    # call to the makeCacheMatrix described above
    
    # try to get the inverse 'i' from the special matrix
    i <- x$getinverse()
    
    # if 'i' is not null, just return the previously calculated
    # value from the cache
    if (! is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # 'i' is null, so inverse was not already calculated
    # it will be calculated nextand placed into the cache
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
