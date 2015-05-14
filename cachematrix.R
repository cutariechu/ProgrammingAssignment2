## This R file contains 2 functions for calculating and storing the inverse of a matrix set by the 
## user. If the value of the inverse has already been calculated and the function is called again, 
## instead of computing once more, the function gets the cached value 


## The function makeCacheMatrix inputs a matrix (e.g. "makeCacheMatrix (matrix(1:4,nrow=2)))
## and creates a list of functions for showing or modifying the values of that matrix
## and its (supposedly) inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) inv <<- solve
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## The function cacheSolve inputs the function makeCacheMatrix and gives the inverse of its matrix.
## If the inverse of the matrix in makeCacheMatrix has been calculated or set earlier, "cacheSolve"
## gets the cached value stored in "setinverse" and doesn't compute it. If the inverse hasn't been
##  calculated before,"cacheSolve" computes it, shows the result and stores it in makeCacheMatrix's 
## list under "setinverse"

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

