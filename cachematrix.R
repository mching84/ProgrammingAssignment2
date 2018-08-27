## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## This function set the value of the matrix, get the value of the matrix, set the value of the inverseMatrix and get the value of the inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL

	##set the value of the matrix
	setMatrix <- function(y){
	x <<- y
	inverseMatrix <<- NULL
	}
	getMatrix <- function() x							##get the value of the matrix
	setInverse <- function(Inverse) inverseMatrix <<- Inverse		##set the value of the invertible matrix
	getInverse <- function() inverseMatrix					##get the value of the invertible matrix
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMtrix above. If the Inverse has already been calculated(and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getInverse()
	if(!is.null(inverseMatrix)){						##if invertible matrix is not NULL
		message("getting cached invertible matrix")		##print "getting cached invertible matrix
		return(inverseMatrix)						##return the invertible matrix
	}
	data <- x$getMatrix()							##get the matrix data
	inverseMatrix <- solve(data, ...)					##use function solve to inverse matrix
	x$setInverse(inverseMatrix)						##set the invertible matrix
	return(inverseMatrix)							##return the invertible matrix
}
