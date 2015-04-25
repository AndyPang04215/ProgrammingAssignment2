## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes in a matrix as input and return a list as output. 
# The list returned contains four functions: two have to do with setting and 
# returning a matrix, and other two have to do with setting and returning the inverse
# of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	theInverse <- NULL
	set <- function(y)	{
		x <<- y
		theInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(anInverse) theInverse <<- anInverse
	getInverse <- function() theInverse
	list(set = set, get = get, 
		setInverse = setInverse,
		getInverse = getInverse)
}


## Write a short comment describing this function
# This function cacheSolve takes in a list of sub-functions. It calls attemps to 
# determine if the list already contains some data - the inverse of a
# matrix, and if it does, this function cacheSolve would return the such inverse
# matrix. However, if the list does not contain an inversed matrix, this function
# cacheSolve would compute the inverse, store the result to the list and return.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        anInverse <- x$getInverse()
        if (!is.null(anInverse))	{
        	message("getting cached data")
        	return(anInverse)
        } 
        message("no cached data")
        theData <- x$get()
        theInverse <- solve(theData, ...)
        x$setInverse(theInverse)
        theInverse
}
