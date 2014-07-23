## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Function that caches inverse matrix
        
        ## Cache variable for the inverse matrix
        inv <- NULL  
        
        ## Function that initializes the original matrix and inverse one (last to NULL)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Function that returns original matrix ( if not assigned, returns NA (1x1 matrix))
        get <- function() x
        
        ## Function that assigns inverse matrix to the one given as parameter
        setinv <- function(matinv) inv <<- matinv
        
        ## Function that returns inverse matrix (NULL if not assigned)
        getinv <- function() inv
        
        ## List of funtions associated to data/object returned by this function
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Initialize variable 'inv' to the value stored on input parameter ('x')
        inv <- x$getinv()
        
        ## If there is a value for inverse matrix in input parameter 'x', it returns cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If this value is NULL (not inverse computed/assigned), we computed inverse for 
        ## original given matrix cached in parameter 'x'
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ## Assign computed inverse matrix to input parameter 'x' and return inverse
        x$setinv(inv)
        inv
}
