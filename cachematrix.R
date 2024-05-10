## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets the value of matrix x,
## creates the variable inv to store its inverse, and defines functions to 
## set, get and cache the value of the original matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) { #set the value of x and set its inverse to NULL
		x <<- y 
		inv <<- NULL
	}
	get <- function() x #Anonymous function that returns the value of x
	setinv <- function(inverse) inv <<- inverse #sets inv to the input value
	getinv <- function() inv #return the value of inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve attempts to get the inverse of a matrix "x" from cache. 
## If it doesn't find it, it uses the functions defined above to calculate,
## set and cache it.

cacheSolve <- function(x,...) {
	inv <- x$getinv() 
	if(!is.null(inv)) { #checks if the inverse of x has been calculated
		message("cached inverse: ")
		return(inv) #if it has been calculated, returns its value and exits
	}
	data <- x$get() 
	inv <- solve(data) #calculates the inverse
	x$setinv(inv) #caches the newly calculated value
	inv #returns the inverse
}