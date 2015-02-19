## Put comments here that give an overall description of what your
## functions do


## 
## This function encapsulate the matrix primitive type to make a cache mechanism 
## on calc of your inverse.
## The matrix passed as argument should be square and invertible.
##

makeCacheMatrix <- function(m_matrix = matrix()) 
{
    m_inverse <- NULL
    
    set <- function(m) 
    {
        m_matrix  <<- m
        m_inverse <<- NULL
    }
    
    get <- function() 
    {
        m_matrix
    }

    setInverse <- function(i) 
    {
        m_inverse <<- i
    }
    
    getInverse <- function() 
    {
        m_inverse
    }
    
    list(set        = set,
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##
## This function computes the inverse of encapsulated matrix on 
## makeCacheMatrix function. If the calc of inverse already done, return the 
## cached result.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    message("calculating data")
    
    data <- x$get()
    
    i <- solve(data)
    
    x$setInverse(i)
    
    i
}
