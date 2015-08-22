##Rohit Ram Manohar
##2 Functions makeCacheMatrix and cacheSolve to store a matrix and find the inverse of the matrix if it has not been already found out.

##Function to Create a Matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix())
{
    ##Set value of Inverse to NULL while creating a new Matrix
    i <- NULL
    ##Internal Function to Set the value of the Matrix
    set <- function(y)
    {
        x <<- y
        i <<- NULL  ##When a new matrix is created set inverse to NULL
    }
    ##Internal Function to get the value of the matrix
    get <- function()
    {
        x
    }
    ##Internal Function to set the value of inverse of the Matrix
    setInverse <- function(mean)
    {
        i <<- mean
    }
    ##Internal Function to get the valu of Inverse of the Matr
    getInverse <- function()
    {
        i
    }
    ##List of Internal Functions in teh Function
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Function to Compute the Inverse of a Matrix
cacheSolve <- function(x, ...)
{
    ##Obtain value of Inverse of Matrix from the Function
    i <- x$getInverse()
    ##Check if value of Inverse is NULL if not return cached value
    if(!is.null(i))
    {
        message("Getting Cache Value")
        return (i)
    }
    ##If value is not stored in cache, Calculate value of Inverse and return the value
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    ##Return the inverse.
    i
}