# The makeCacheMatrix function caches the inverse of a matrix and provides 
# a list of functions for matrix(inverse) assignment and retrieval.
#
# Args:
#  x: matrix for which the inverse is to be cached.
#
# Returns:
#  special "matrix", which is a list containing functions to :
#  1) reset the cached inverse and store the input matrix  - setMatrix
#  2) return the value of the stored matrix		   - getMatrix
#  3) cache the input inverse matrix for easy retrieval    - setInverse
#  4) return the inverse matrix(default or cached)   	   - getInverse

makeCacheMatrix <- function(x = matrix()) {
	# Initialze matrix inverse to NULL
	# This is the default value of the inverse matrix
	matrixInverse <- NULL;

	# setMatrix strores the user input matrix in the "mat" variable, defined 
	# in the scope of makeCacheMatrix.  This function also resets the value
	# of the cached inverse matrix to its default value(NULL) with the use of 
	# super assignment operator(<<-)
	setMatrix <- function(inputMatrix){
		x <<- inputMatrix
		matrixInverse <<- NULL
	}
	
	# getMatrix returns the matrix 
	getMatrix <- function() {
		x
	}

	# setInverse stores away the input inverse matrix in "matrixInverse",
	# a variable defined in the scope of the makeCacheMatrix() environment
	# (parent environment of setInverse()). 
	# This caches the value of the inverse.
	setInverse <- function(inverse) {
		matrixInverse <<- inverse
	}
	
	# getInverse returns the inverse of the matrix. The function may either
	# return a cached value or default value, in case matrix inverse has not
	# yet been computed and stored.
	getInverse <- function() {
		matrixInverse
	}
	
	# returns the special "matrix"(list of functions)
	list(setMatrix  = setMatrix, 
             getMatrix  = getMatrix,
	     setInverse = setInverse, 
             getInverse = getInverse)
}



# The cacheSolve function calculates the inverse of a special "matrix" created
# by the makeCacheMatrix function
#
# Args:
#  x: The special "matrix"(list containing functions) returned by 
#     makeCacheMatrix function
#
# Returns:
#  The inverse of the special "matrix"

cacheSolve <- function(x, ...) {
	# The execution environment of makeCacheMatrix() is the enclosing/parent
	# environment of the functions in the special "matrix". As this "matrix"
	# stores the environment of makeCacheMatrix, the functions inside the 
	# special "matrix" can readily access the variables defined in the 
	# scope of makeCacheMatrix.
	
	# Retrieve the inverse matrix already stored in the makeCacheMarix.
	matrixInverse <- x$getInverse()
	
	# Checks whether there is already a cached inverse matrix or not. 
	# If the inverse matrix is a not the default(NUL)L value, then it returns 
	# the inverse matrix from the cache.
	if(!is.null(matrixInverse)) {
		message("The cached inverse matrix is : ")
		return(matrixInverse)
	}
	
	# Otherwise, retreive the matrix 
	mat <- x$getMatrix()

	# Calculate the inverse of the matrix using Solve()
	matrixInverse <- solve(mat, ...)
	
	# Store this inverse back in MakeCacheMatrix for caching the value.
	x$setInverse(matrixInverse)

	# Return the inverse of the matrix 
	matrixInverse
}
