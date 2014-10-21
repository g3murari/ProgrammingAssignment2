## Overall description of functions:
## makeCacheMatrix: 
##		This function creates an empty CacheMatrix - with two 
## cache variables, one to cache the original data matrix and the other 
## to cache the inverse of the data matrix. It also defines four functions
## to get and set the cache matrices.
## cacheSolve:
##		This function operates on a CacheMatrix to retrieve the inverse matrix
## data. If the inverse is already available in the cache, it is returned 
## immediately. Else, the inverse is computed for first time and cached into
## the CacheMatrix and the inverse matrix is returned. From next time onwards
## for this particular instance of CacheMatrix the inverse is already available
## in the cache inverse variable.

## The below function 'makeCacheMatrix' , simply creates a matrix object
## 1. That is capable of holding two cached variables - the original matrix
## and it's inverse.
## 2. That defines four functions that operate on the two cached variables.
## 3. The four functions are set/get to set and get the original matrix, 
## setinv/getinv to set and get the inveverse matrix

makeCacheMatrix <- function(x = matrix()) {
	## inv is the cache variable to store the inverse of the original matrix
	inv <- NULL
	
	## Below is the definition of the 'set' function. This initializes
	## x to the input y original matrix. And, resets (initializes) the inv matrix.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## Below is the definition of the 'get' function. This retrieves the 
	## original input matrix cached in variable x.
	get <- function() x
	
	## Below is the definition of the 'setinv' function. This sets or caches
	## inv variable to the value of input variable invMatrix.
	setinv <- function(invMatrix) inv <<- invMatrix
	
	## Below is the definition of the 'getinv' function. This gets the 
	## cached value of inv matrix.
	getinv <- function() inv
	
	## Below operation creates a list of the functions associated with
	## the inverse matrix cache operation.
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The below function computes and returns the inverse of the input matrix 
## object x. The matrix object x should have been created prior to using 
## the 'cacheSolve' function by using the above 'makeCacheMatrix' matrix. 

cacheSolve <- function(x, ...) {
     
	## First, get the inverse matrix from the matrix object using function
	## 'getinv' and assign to variable inv.
	inv <- x$getinv()
	
	## If the inverse matrix had previously been computed, the returned
	## matrix stored in 'inv' would not be NULL. Check of this using the 
	## is.null function.
	
	if(!is.null(inv)) {
		## If the 'inv' is not NULL, the execution enters within this 'if'
		## condition. Now simply return the retrieved cached inverse matrix.
		message("getting cached data")
		
		## Return matrix 'inv' that is the inverse of 'x'. The function will
		## exit on return and no further code within this function will
		## be executed.
		return(inv)
	}

	## If the 'inv' is indeed NULL, the execution skips the above 'if'
	## condition and lands here. Now, we need to compute the inverse for
	## the first time. 
	message("computing inverse of data for first time")
	
	## The original matrix is first retrieved using the 'get' function 
	## and stored in variable 'data'.
	data <- x$get()
	
	## The 'solve' inbuilt function is R is used to compute the inverse
	## matrix of the variable data. The inverse is stored in 'inv' 
	## variable.
	inv <- solve(data, ...)
	
	## The value of the 'inv' variable is then set or cached into the 
	## matrix object x.
	x$setinv(inv)
	
	## Return matrix 'inv' that is the inverse of 'x'
	inv
}
