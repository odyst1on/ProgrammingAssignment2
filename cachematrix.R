## This script contains two functions, makeCacheMatrix and cacheSolve, to
## support the calculating, caching and retrieval of the inverse of a matrix -
## thereby avoiding the need to calculate it repeatedly.
## It is assumed - as a condition of the programming assignment - that the
## matrix supplied is invertible.

## The makeCacheMatrix function creates a special "vector" object which
## is a list containing functions to: (1) set the value of the input matrix;
## (2) get the value of the input matrix;
## (3) set the inverse matrix;
## (4) get the inverse matrix
##
## makeCacheMatrix can be called initially either with an input matrix whose 
## inverse is required or with the argument blank.
## In both cases, the list of functions is initialised (ready for use when the
## cacheSolve function is called).
## After that point, new values for the input matrix can be set simply by
## calling the set function with the new input matrix as the argument.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        ## If makeCacheMatrix has been called previously, the set function
        ## allows the value of the input matrix to be changed without having to
        ## run through the other code in makeCacheMatrix again.
        set <- function(y) {
                
                ## Reset the parent values of x and m to reflect that a
                ## new input matrix has been provided. The get and getInverse
                ## functions will therefore be updated with these values.
                x <<- y
                m <<- NULL
        }
        
        ## Function to return the value of the input matrix
        get <- function() x
        
        ## Function to store the calculated inverse matrix in m
        setInverse <- function(iMatrix) m <<- iMatrix
        
        ## Function to return the value of m - either an inverse matrix
        ## which has been cached or null where a value has not been calculated
        getInverse <- function() m
        
        ## Create the list of functions described in the header comments
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    
}

## The cacheSolve function checks to see if the inverse of the matrix supplied
## has already been calculated. If it has, it retrieves it from the cache. If it
## has not been calculated, then the function computes the value.

## cacheSolve should be run after (1) makeCacheMatrix has been run and (2) the
## input matrix has been provided - either directly in the call to
## makeCacheMatrix or via a call to the set function.

cacheSolve <- function(x, ...) {

                ## Call the getInverse function defined by makeCacheMatrix 
                m <- x$getInverse()
                
                ## If the inverse matrix has already been calculated, m will
                ## hold it and will therefore not be null
                if(!is.null(m)) {
                        message("getting cached data")
                        ## Return the stored value of the inverse matrix
                        return(m)
                }
                
                ## The inverse matrix has not been calculated, so first get
                ## the value of the input matrix
                data <- x$get()
                
                ## Run the solve function to calculate the inverse of the matrix
                m <- solve(data, ...)
                
                ## Call the setInverse function to store the value
                x$setInverse(m)
                
                ## And, finally, return the value of the inverse matrix
                m
        
}
