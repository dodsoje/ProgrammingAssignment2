##MakeCacheMatrix function takes as argument a matrix, it then uses the functions setinv and getinv to assign the inverse of the matrix 
##and return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL  		# sets the inverse to NULL
	set <- function(y) {	# set function assignsargument to x
		x <<- y
		inv <<- NULL	# Once function is called, the inverse needs to be set to NULL if matrix is redefined
	}
	get <- function() x	# Get returns the matrix 

	setinv <- function(solve) inv <<- solve
	getinv <- function() inv

	list(set = set, get = get, getinv = getinv, setinv = setinv) # Creates a list of functions

}


##cacheSolve function get passed the matrix as argument. It returns the inverse either in cache, if it is there, or the newly calculated inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinv()	# Returns the most recent value of the inverse (cached or not cached)

	if(!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}
	
	message("Geting non cached data")
	data <- x$get()
	inv <- solve(data, ...)	# If inverse is not in cache return new inverse
	x$setinv(inv)
	inv

}
