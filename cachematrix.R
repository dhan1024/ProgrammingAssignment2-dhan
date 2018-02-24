
## R Programming assignment 3
## Feb 23rd, 2018

## The function makeCacheMatrix "stores" the user-specified matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
        ## initiate object 'inv'
        inv <- NULL 
        
        ## set() may be used to reset 'x' and clear cached inverse
        ## using <<- means the search of 'y' will occur outside the enclosing environment of makeCacheMatrix
        ## if use <-, set() will not do anything because 'y' is missing in the current environment
        ## e.g., try a$set(matrix(5:8, 2)) after makeCacheMatrix(matrix(1:4, 2))
        set <- function(y = matrix()) { 
            x <<- y              
            inv <<- NULL
        }
        
        ## return 'x'
        get <- function() x
        
        ## set 'inv' to a matrix from a different environment 
        ## in this case we'll set it to the inverse of 'x' computed inside cacheSolve's environment
        ## or it will be NULL if none has been computed
        ## this step allows the inverse of 'x' to be cached
        setinv <- function(invmat = matrix()) inv <<- invmat
        
        ## return 'inv' 
        getinv <- function() inv
        
        ## return the list of all four set/get functions
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
    
}


## The function cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated and if the matrix 
## has not been changed, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    
        ## get cached inverse if it's been computed via prior execution of cacheSolve
        ## or NULL if cached data is not available    
        inv <- x$getinv() 
        
        ## if 'inv' is not NULL, return the message and the cached inverse
        if (!is.null(inv)) {
            message("Retrieving cached data")  
            return(inv)
        }
        
        ## if NULL, retrieve the matrix from makeCacheMatrix
        mat <- x$get()    
        
        ## solve the inverse of that matrix
        inv <- solve(mat) 
        
        ## once solved, pass the inverse to setinv as the 'invmat' argument
        x$setinv(inv)     
        
        ## return the inverse from above computation
        inv               
    
}
