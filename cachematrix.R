## This file contains the functions written for the week 3
##  R Programming Assignment

## This function creates a special matrix object that can cache 
##  its inverse using the solve() function

makeCacheMatrix <- function(x = matrix()) {
    inv_result <- NULL
    set <- function(y) {
        x <<- y
        inv_result <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_result <<- inv
    getinv <- function() inv_result
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function gets the inverse of the special matrix object
##  created by makeCacheMatrix. If the inverse is NULL it calculates
##  the invers using solve() otherwise it just retrieves the cached 
##  value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_result <- x$getinv()
    if(!is.null(inv_result)) {
        message("getting cached data")
        return(inv_result)
    }
    data <- x$get()
    inv_result <- solve(data, ...)
    x$setinv(inv_result)
    inv_result
    
}
