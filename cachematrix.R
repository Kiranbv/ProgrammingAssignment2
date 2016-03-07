## Put comments here that give an overall description of what your
## functions do
# This program creates two functions that are used to calculate the inverse of a
# matrix. Calculating inverse of a matrix is time consuming task when the dime
# ension of the matrix is very large. Matrix inverse involves finding the dete
# minant and the adjoint of the matrix. Finding the adjoint of a matrix takes 
# a very long time. 

# But if the contents of the matrix are not changing, then the inverse can be  
# cached so that when the inverse is needed it can be looked up in the cache
# rather than recomputed. 

## Write a short comment describing this function
# This function creates a special type of vector which is list containing 
# functions to set and get the value of the input matrix, calculate the inverse
# using the solve function and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse of the matrix. 
        matinv <- NULL

        ## Set the value of the vector. 
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        ## Getting the value of the vector.
        get <- function() x
        ## Calculating the inverse using the solve function.
        setinverse <- function(solve) matinv <<- solve
        ## Getting the value of the inverse. 
        getinverse <- function() matinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Write a short comment describing this function
## This function calculates the inverse of the matrix returned by the function
## makeCacheMatrix. If the inverse is already in the cache, it retrieves it.If 
## not, it calculates it and puts it in the cache. 

cacheSolve <- function(x, ...) {
   ## Gets the inverse from the cache.
   matinv <- x$getinverse()
        ## If the inverse exists, return it. 
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        ## If the inverse is not in the cache, calculate it using solve functionon.
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinverse(matinv)
        matinv
        ## Return a matrix that is the inverse of 'x'
}
