## makeCacheMatrix creates a special "vector" to hold the information of a matrix, 
##  including original matrix and its inverse

makeCacheMatrix <- function(x= matrix()){
	m <- NULL
	set <- function(y){  ## set the value of the matrix
		x <<- y
		m <<- NULL
	}
	get <- function() x ## get the value of the matrix
	setInverse <- function(inverse) m<<- inverse  ## set the value of the inversed matrix
	getInverse <- function() m  ## get the value of the inversed matrix
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve calculates the inverse matrix of x
## It first see if the inverse matrix of x has been calculated before and stored in the cache
## If yes, cacheSolve will retrieve the matrix and return it
## If no, cacheSolve will calculate the inverse matrix and return it to the vector created by makeCacheMatrix

cacheSolve <- function(x, ...){
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cache data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...) ## return a matrix that is inverse of x
	x$setInverse(m)
	m
}
