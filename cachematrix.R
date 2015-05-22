## makeCacheMatrix(x), takes a matrix x and produces a list.
## This list contains functions to get the matrix and its inverse if it has been calculated

## cachesolve(L), takes a list L (produced by makeCacheMatrix) and produces the inverse of
## the matrix



## makeCacheMatrix() create a list with 4 objects which are functions allowing
## to manipulate a matrix and its inverse.
## It is assumed that the matrix computed is invertible

makeCacheMatrix <- function(x = matrix()) {
        
        #inverse will be use as an object of the upper environment       
        inv <- NULL 

        #set() associated with the cache allow to reset the List for a new matrix

        set <- function(y) {
                x <<- y
                inv <<- NULL   # the cache is emptied
        }
        
        #get() restitutes the matrix
        get <- function() x
        
        #setinv(m) 'stores' the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
        
        #getinv() restitute the inverse instead of calculating it
        getinv <- function() inv
        
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve, takes a List contaning the 'infos' of a matrix (cf previous funciton) and give 
## back the inverse of the matrix, by calculation of by restitution if it has been calculated before

cacheSolve <- function(l, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- l$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- l$get()
        inv <- solve(matr, ...)
        l$setinv(inv)
        inv
}