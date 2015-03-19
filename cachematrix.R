## Example run showing creation of a makeCacheMatrix, two runs of cacheSolve illustrating the scaneraio when a 
## cached value is returnd.  A new matrix is the defined for the makeCacheMatrix and cacheSolve is run twice 
## again to show that the old cached version has been deleted, the new inverse is created and cached and used from that
## point on.

## source("cachematrix.R")
## > m = makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(m)
## get
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## returning cached value for inverse of matrx
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > m$set(rbind(c(2, -1/8), c(-1/8, 2)))
## set
## > cacheSolve(m)
## get
##            [,1]       [,2]
## [1,] 0.50196078 0.03137255
## [2,] 0.03137255 0.50196078
## > cacheSolve(m)
## returning cached value for inverse of matrx
##            [,1]       [,2]
## [1,] 0.50196078 0.03137255
## [2,] 0.03137255 0.50196078

# The makeCacheMatrix function creates a list of functions that operate on a matrix stored in a variable called x
## and its inverse stored in a variable called inv.
##
## functions:
##	1. set - allows a new matrix to be defined
##	2. get - returns the matrix
##	3. setinverse - stores a value of the inverse of the matrix
##	4. getinverse - returns a previously stored inverse of the matrix or NULL if one has not been stored.

makeCacheMatrix <- function(x = matrix()) {
	# initially we haven't calculated the inverse so set the variable we will use to store the inverse in to NULL
	inv <- NULL

	# function that allows a new matrix to be supplied.  When a new matrix is supplied delete the old cached
	# version of the inverse if one existsi by setting inv to NULL.
	set <- function(y) {
		message("set")
		x <<- y
		inv <<- NULL
	}

	get <- function() {
		message("get")
		x
	}

	setinverse <- function(inverse) {
		inv <<- inverse
	}

	getinverse <- function() {
		inv
	}

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function takes a makeCacheMatrix and returns its inverse.  It makes use of the fact that the inverse
## of the matrix can be stores in the makeCacheMatrix variable inv.  Using the getinverse function it checks to see if
## the inverse of the matrix has been calcualated previously.  If it has it returns the previously calculated matrix, 
## i.e. the cached value, if not it calculates the inverse of tha matrix, stores in in the makeCacheMatrix and returns
## the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()

	## If the makeCacheMatrix already has a value for inv then return it -- this is a value cached from 
	## a previous invocation
	if (!is.null(inv)) {
		message("returning cached value for inverse of matrx")
		return(inv)
	}
	
	## Otherwise there is no cached value so calculate on and store it int he cache.
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
