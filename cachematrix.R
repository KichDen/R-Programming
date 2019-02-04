makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                              
    set <- function(y) {                     
        x <<- y                             
        inv <<- NULL                       
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
