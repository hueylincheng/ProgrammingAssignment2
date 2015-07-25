## Matrix inversion is usually costly, especially when running inside of a loop.
## This program/function is to cache the inversion of a matrix instead of computing it repeatedly.

## makeCacheMatrix creates a special matrix that can cache its inverse
##
## x: a matrix which is sqaure (NxN) and invertable
## Return: a list includes functions to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of inversion of the matrix
##      4. get the value of the inversion of the matrix
## 

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   
   ##
   ## the <<- operation which is used to assign a value to an object in an environment that is dofferent from the current environment
   ## 
   set <- function(y) {
       x <<- y
       m <<- NULL
   }
   get <- function() x
   setinvert <- function(invert) m <<- invert
   getinvert <- function() m
   list(set = set,
        get = get,
        setinvert = setinvert,
        getinvert = getinvert)
}


## 
## CacheSolve returns the inverse of a matrix. 
## If inverse has been computed, it gets the results from the cache.
## Else cacheSolve computes the inverse via solve() function, and set the invserse value in the cache
##
## x: output of makeCacheMatrix()
## Return: a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {

    m <- x$getinvert()

    ## if matrix inverse already exists, return the inverse matrix
    ## else call solve() to inverse the matrix 
    ##      and set the value in the cache by calling setinvert() function
    ##
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    
    ## set the cache
    x$setinvert(m)
    return(m)
}
