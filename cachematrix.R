## [makeCacheMatrix creates a special matrix object that can cache its inverse]

makeCacheMatrix <- function(x = matrix()) {					## input x is a square matrix
			inv <- NULL										## inv is the inverse and is set to NULL every time makeCacheMatrix is called	
			set <- function(y) {							## this takes an input value, saves it and resets it to NULL when a new object is generated 
			x <<- y							
			inv <- NULL
}
			get <- function() {x}							    ## gets the value of the original matrix
				setinverse <- function(solve) {inv <<- solve}	## called by cacheSolve during first access, stores the value by using the superassignment
				getinverse <- function() {inv}					## returns the cached value during later accesses
			list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)		## list of internal functions by makeCacheMatrix 
}


## [cacheSolve computes the inverse of the special matrix object, if the inverse 
## has been calculated before it retrieves the value from the cache]

cacheSolve <- function(x, ...) {				## input x is an object created by makeCacheMatrix
			inv <- x$getinverse()				## access to object x and gets the value of the inverse
			if(!is.null(inv)) {					## if the value is not NULL, i.e. has already been cached
			message ("getting cached data")		## this message is send and
			return(inv)							## the value is returned
}
			data <- x$get()						## if there has been no cache, i.e. inv was NULL
			inv <- solve(data,...)				## then the inverse is calculated 		
			x$setinverse(inv)					## and set to the value 
			inv  								## finally returns a matrix that is the inverse of 'x'
}

 