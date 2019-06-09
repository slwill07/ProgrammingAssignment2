## Cache the inverse of a matrix rather than compute it repeatedly.

## Takes a square invertible matrix, and creates a special "matrix".
## Returns a list containing functions to
## Set the matrix
## Get the matrix
## Set the inverse
## Get the inverse

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        
        set <- function(y) 
        {
                
		x <<- y
                
		inv <<- NULL
        
	}
        
	get <- function() x
        
	setInv <- function(inverse) inv <<- inverse
        
	getInv <- function() inv        
	list(set = set, get = get,
 setInv = setInv,
 getInv = getInv)

}


## Takes the output of makeCacheMatrix().
## Returns the inverse of the special "matrix".

cacheSolve <- function(x, ...)
{
        inv <- x$getInv()
        
	if(!is.null(inv)) 
	{
                
		message("Getting cached data.")
                
		return(inv)
        
	}
        
	data <- x$get()
        
	inv <- solve(data, ...)
        
	x$setInv(inv)
        
	return (inv)
}