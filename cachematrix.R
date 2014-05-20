## The following two functions store a matrix as an input, calculate
## the inverse of that matrix and output the inverse upon request. 
## To this end, the functions have to be used consecutively. While 
## the first one caches a matrix, the second one calculates the
## inverse of that matrix and stores the result.


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




# cacheSolve takes the object created with "makeCacheMatrix" as an input and 
# calculates the inverse of the matrix stored therein. If the inverse 
# has already been calculated and stored into the variable "m" within 
# "makeCacheMatrix", than it will be retrieved from there by calling upon 
# the "getinv" function.     

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'

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


