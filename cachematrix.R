## The goals of the Coursera R Programming Course, Assignment Wk 3 are to create:
##      makeCacheMatrix: This function creates a special "matrix" object that can cache 
##      its inverse.
##      cacheSolve: This function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been calculated (and the 
##      matrix has not changed), then the cachesolve should retrieve the inverse from the 
##      cache.

## This code was adapted from the Cached Vector Example provided in the Assignment.

## The makeCacheMatrix function returns a list to:
        ## 1. Set the Matrix
        ## 2. Get the Matrix
        ## 3. Set the Inverse
        ## 4. Get the Inverse
## The returned list can be the input for the next function, cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
                
        m = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinv<-function(solve) m <<- solve
        getinv<-function() m
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## cacheSolve returns a matrix that is the inverse of 'x', where 'x' is the output of
##      makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        m = x$getinv()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
         
        data = x$get()
        m = solve(data, ...)
        
        x$setinv(m)
        
        m
}
