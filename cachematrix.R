## Functions: makeCacheMatrix() and cacheSolve()
## 
## Arguments: 
## 1) makeCacheMatrix():  A square invertible matrix
## 2) cacheSolve(): A list-object generated before by makeCacheMatrix
##
## Description:
## With makeCacheMatrix() and cacheSolve() it is possible to calculate an inverse matrix
## of a given matrix and cache it in a separate R-environment. So it is protected from being overwritten 
## by accident and can be used several times without costly new calculation
##
## Example:
## x <- makeCacheMatrix(matrix(c(4,7,2,6),nrow=2,ncol=2,byrow=TRUE)) #set origin matrix
## x$setmatrix()      #set new origin matrix (overwrites old one)
## x$getmatrix()      #extracts the origin matrix
## x$setinverse()     #stores calculated inverse matrix in cache
## x$getinverse()     #shows calculated inverse matrix
## cacheSolve(x)      #calculates inverse matrix of origin matrix stored in x and prints it out



## makeCacheMatrix()
## This function creates a set of 4 functions stored in a list object 
## setmatrix, getmatrix, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {             ##Create list of functions
    cache <- NULL                                       ##Initialize cache (bind cache-Variable to given environment)
    setmatrix <- function(y) {                          ##Stores given matrix to be calculated
        x <<- y                                         ##in environment-variable x 
        cache <<- NULL                                  ##Delete possible cached inverse matrix, because a new matrix ahs been defined
    }
    getmatrix <- function() x                           ##defines function to print out given matrix stored in environment variable x
    setinverse <- function(inverse) cache <<- inverse   ##defines function to store calculated inverse matrix in environment variable cache
    getinverse <- function() cache                      ##defines function to print out calculated inverse matrix stored in environment variable cache
    list(setmatrix = setmatrix, getmatrix = getmatrix,  ##creates list of functions defined before
         setinverse = setinverse,
         getinverse = getinverse)  
}

## cacheSolve()
## This function takes a given matrix stored in the environment of the 
## functions created by makeCacheMatrix and calculates the inverse of this matrix.
## After that it returns the inverse matrix back in this environment. For this it uses also
## the set of functions created by makeCacheMatrix.

cacheSolve <- function(x, ...) {        ##Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()           ##gets inverse matrix
    
    if(!is.null(inverse)) {             ##if already calculated before
        message("getting cached data")  ##it prints a message and
        return(inverse)                 ##the cached inverse matrix
    }
    
    matrix <- x$getmatrix()             ##if not calculated before, get origin matrix
    inverse <- solve(matrix, ...)       ##calculate inverse matrix and store in "inverse"
    x$setinverse(inverse)               ##store inverse-matrix in cache
    inverse                             ##and print calculated inverse matrix
}

