## The makeCacheMatrix and cacheSolve functions are used to manage
## finding the inverse of a matrix.  Once the inverse has been calculated,
## it is cached and the cached version will be returned instead of
## re-calculating the inverse.
##
## Example usage:
##    x <- makeCacheMatrix(matrix( c(1,4,4,1), nrow=2, ncol=2))
##    i <- cacheSolve(x) # Returns calculated version
##    j <- cacheSolve(x) # Returns cached version
##    x$set(matrix( c(1,2,2,1), nrow=2, ncol=2)) # This clears the cached version
##    i <- cacheSolve(x) # Returns calculated version for new matrix
##    j <- cacheSolve(x) # Returns cached version


## makeCacheMatrix:  Create a special matrix type that can be used with cacheSolve.
## The supplied matrix is stored in this special instance.  The cacheSolve function 
## will use the extra property (inverse) to retrieve the cached version.  If the
## cached version is NULL, then the inverse will be calculated and stored in the
## special instance.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL       # The inverse matrix instance variable
    set <- function(y) {
        x <<- y     # Set the matrix
        i <<- NULL  # Set the inverse to null so it will be calculated when needed
    }
    get <- function() x                           # Return the matrix
    setinverse <- function(inverse) i <<- inverse # Set the cached inverse
    getinverse <- function() i                    # Get the last set inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: this function is used to create a matrix that is the
## inverse of 'x'.  If the inverse has been cached, the cached version
## is returned.  Otherwise the inverse will be calculated, cached and
## returned.  Note that the 'x' parameter is a special matrix that is
## created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        ## 'x' is a special matrix returned by makeCacheMatrix.
    i <- x$getinverse()                 # Get the cached value
    if(!is.null(i)) {                   # If set, use the cached version
        message("getting cached data")
        return(i)
    }
    data <- x$get()                     # Get the original matrix
    i <- solve(data, ...)               # Create the inverse
    x$setinverse(i)                     # Set the cached value
    i                                   # Return the inverse
}
