## Code which can be used to create a `cacheMatrix` object (enclosed 
## environment) whose inverse is stored with the object.

## Returns a `cacheMatrix` object created from the argument matrix object.
makeCacheMatrix <- function(x = matrix()) {
        
        # create the environment for performing these operations
        
        # store the cached inverse
        i <- NULL
        
        # used to change or set the underlying matrix
        # it resets the cached inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # returns the underlying matrix
        get <- function() x
        
        # sets the cached inverse value
        setinv <- function(inv) i <<- inv
        
        
        # gets the cached inverse value
        getinv <- function() i
        
        # return the environment
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## Returns the inverse of the underlying matrix inside a `cacheMatrix` object. 
## If the inverse has already been computed, it returns the already-computed 
## value. Otherwise, it computes the value of the inverse and stores it for 
## future use.
cacheSolve <- function(x, ...) {
        
        # see if there is already a cached inverse
        i <- x$getinv()
        
        # if the cached inverse is not NULL, return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # get the underlying matrix object
        data <- x$get()
        
        # calculate its inverse
        i <- solve(data, ...)
        
        # cache inverse in the cacheMatrix object
        x$setinv(i)
        
        # return the calculated inverse
        i
}
