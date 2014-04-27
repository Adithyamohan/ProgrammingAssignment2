## makeCacheMatrix: 
#  	creates a special "matrix" object as below
#  	* set the value of the matrix
#  	* get the value of the matrix
#  	* set the value of the inverse
#  	* get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    # Included the below line as a precautionary step as mentioned in the forum
    x <- x 
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setinv_x <- function(inverse) inv_x <<- inverse
    
    # get the value of the inverse
    getinv_x <- function() inv_x
    
    # returns the special matrix
    list(set = set, get = get, setinv_x = setinv_x, getinv_x = getinv_x)
}


## 	cacheSolve: 
# 	Computes and caches the inverse. returns the inverse if it is already cached  
cacheSolve <- function(x, ...) {
    inv_x <- x$getinv_x()
    
    # Verify if inverse is already calculated
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }

    # Calculate inverse as it is not in cache
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinv_x(inv_x)
    inv_x
}