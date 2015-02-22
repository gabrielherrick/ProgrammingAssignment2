#Below are two functions that are used to create a
#special object that stores a matrix and caches its inverse.

#The first function, `makeCacheMatrix` creates a list containing 4 functions to

#1.  set the value of a matrix: can be called later to redefine matrix
#2.  get the value of the matrix: can ge used to calculate and cache calcs
#3.  set the value of the inverse: saves calculated results
#4.  get the value of the inverse: recalls inverse so it doesn't have to be 
#calculated twice. 
# If you feed `makeCacheMatrix' a matrix, it will assign the matrix so that when
# `cacheSolve' runs it can `get' the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set=set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the matrix provided to
#the function `makeCacheMatrix'. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverse in the cache via the `setinv`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix fed as an argument
        ## to makeCacheMatrix. The argument 'x' for cacheSolve is the 
        ## list of functions produced by makeCache Matrix. In order for caching 
        ## work properly, you must first run `makeCacheMatrix' and save its 
        ## output, then run cacheSolve' on that saved output. 
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
