## Programming Assignment 2. Lexical Scoping
##
## This set of functions sets up an environment where the results of time-consuming
## calculations can be retained in cache between calls to avoid having to carry
## them out repeatedly.

## The makeCacheMatrix function creates the special matrix object that can
## preserve results using R scoping rules.  Its output is a list containing 
## the following functions:

##   set = sets the matrix and makes any previously saved result 'm' NULL
##   get = returns the matrix values originally saved by makeCacheMatrix
##   setInverse = solves the inverse matrix (retained in 'm' for future retrival)
##   getInverse = returns the previous "solved" value (or NULL if not yet solved)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function calls the getInverse function (created by makeCachematrix)
## to check whether a inverse matrix has previously been saved.  If it has, the
## saved data is simply returned again thereby avoiding re-calculation.

## However, if no saved version is found, we "get" the original matrix and calculate
## the inverse using "solve" and then save the result for future use using the 
## setInverse function (created by makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## if not NULL then we can used the cached result 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## There was no previously saved result, so we must "solve" the matrix
        data <- x$get()
        m <- solve(data, ...)
        ##  and now save it for future use
        x$setInverse(m)
        m
}
