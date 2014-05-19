## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function which, assigned to a variable, creates
# a list of functions (or "methods") to store and retrieve a matrix.  

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL

     set <- function(y) {
         x <<- y
         m <<- NULL
     }

     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
 }


## Write a short comment describing this function

# cacheSolve takes the object created with "makeCacheMatrix" as an input and 
# calculates the inverse of the matrix stored therein. If the inverse 
# has already been calculated and stored into the variable "m" within 
# "makeCacheMatrix", than it will be retrieved from there by calling upon 
# the "getinv" function.     

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     n <- x$getinv()
     if(!is.null(n)) {
         message("getting cached data")
         return(n)
     }
     data <- x$get()
     n <- solve(data, ...)
     x$setinv(n)
     n
 }


