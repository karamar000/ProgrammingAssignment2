## Overall Description:
#The overall purpose of these functions is to cache a matrix
#so that a matrix inverse may be computed multiple times in an
#efficient manner. The first function, makeCacheMatrix, contains 
#functions that can cache a matrix. The second function, cacheSolve,
#computes the inverse of the matrix, utilizing the inverse from 
#the cache if possible. 

## makeCacheMatrix Description:
#This function creates a special "matrix" object that can 
#cache its inverse. The function input should be an invertible 
#matrix. The return value is a list of functions that
#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the matrix inverse
#4) get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(input) inverse <<- input
        getInverse <- function () inverse
        
        list(set = set, get = get,      #returned list of functions
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve Description:
#This function uses the special "matrix" returned by makeCacheMatrix
#to compute the matrix inverse. cacheSolve will retrieve the
#inverse from the cache if the inverse has been previously cacluated
#and the matrix has not been changed.
cacheSolve <- function(x, ...) {
        #gets cached/NULL inverse from makeCacheMatrix
        inverse <- x$getInverse() 
        
        #if cache is found
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        #if no cache is found, calculate inverse and setInverse
        dataMatrix <- x$get()
        inverse <- solve(dataMatrix)
        x$setInverse(inverse)
        inverse
}