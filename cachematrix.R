## ----------------------------------------------------------------------------
## Function Pair:  makeCacheMatrix & cacheSolve 
## ----------------------------------------------------------------------------
## 
## The makeCacheMatix and cacheSolve functions work together to compute and 
## cache the inverse of a matrix.
## 
## Usage:
##      Where is "mymatrix" is a "square" matrix...
##      ------------------------------------------
##      mycache <- makeCacheMatrix(mymatrix) ### factory creates the function
##      myinversion <- cacheSolve(mycache)   ### solves matrix or returns cache
##                                                

## ----------------------------------------------------------------------------
## Function:  makeCacheMatrix 
## ----------------------------------------------------------------------------
## 
## Purpose: makeCacheMatix is a factory function that returns a set of functions 
##          to get/set both a matrix and its inversion.  It caches the
##          inversion of the matrix in the function enclosing environment to  
##          avoid calculating the inversion unless the original matrix has
##          changed.  Storing the inversion in the "enclosing environment" of 
##          the function, preserves the value across multiple executions.         
##
## Input: x is an invertible matrix (optional) 
##        If no matrix is supplied x defaults to an empty matrix.
##
## Output:  The factory output is an object that is a list of functions 
##
##          getMatrix() Returns "original" matrix (pre inversion).  
##          setMatrix() Sets matrix to be inverted where x is the new matrix. 
##          getInversion() Returns inverted form of the matrix.   
##          setInversion() Sets inverted matrix where x is the inversion.
##           
## ----------------------------------------------------------------------------
        
makeCacheMatrix <- function(x = matrix()) {
        
        ## Function Variables 
        om <- x      # original matrix
        im <- NULL   # inverted matrix
 
        setMatrix <- function(x) {
                om <- x
                im <<- NULL
        }
        
        getMatrix <- function() om 
        
        setInversion <- function(x) im <<- x
        
        getInversion <- function() im
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInversion = setInversion,
             getInversion = getInversion)

}

## ----------------------------------------------------------------------------
## Function:  cacheSolve 
## ----------------------------------------------------------------------------
## 
## Purpose: Computes the inversion of a matrix based upon an instance of the  
##          list of functions created by the makeCacheMatrix function factory.
##          
##          If the inversion has already been computed/cached, then the cached
##          value will be returned.
##
##          If the inversion is not cached, it computes it using solve(), saves
##          the result in the makeCacheMatrix function instance using 
##          setInversion() and return the inverted matrix.
##
## Input:
##      x is the function list instance returned by calling 
##      makeCacheMatrix() with a matrix you can't to solve/cache.
##
##      Note:  Pass-through of other parameters to solve() is enabled 
##             using "..." 
##
## Output:
##      Returns the inversion of the associated matrix
##
## ----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        
        im <- x$getInversion()
        
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        im <- solve(x$getMatrix(), ...)
        
        x$setInversion(im)
        
        im
}
