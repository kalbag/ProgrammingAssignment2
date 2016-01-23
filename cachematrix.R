## Put comments here that give an overall description of what your
## functions do
##
## The following code creates a pair of functions:
## 1. makeCacheMatrix is an auxiliary function which takes a matrix and returns a list of
## four functions to set and get the input matrix and its inverse. The key feature of
## this function is that it caches the inverse and returns the cached inverse when possible
## (i.e., as long as the input has not been reset), and thus, saves computational expense.
## 2. cacheSolve takes the list of functions returned by makeCacheMatrix and returns the
## inverse either by solving it for the first time, or returning it from cache.
##
## Usage is as follows:
## foo <- matrix(...)				## creates a matrix
## cacheMatrix <- makeCacheMatrix(foo)		## stores the list of functions in cacheMatrix
## cacheMatrix$set(foo)				## stores the input matrix to invert in cacheMatrix
## cacheSolve(cacheMatrix)			## solves for the inverse the first time and returns it
## cacheSolve(cacheMatrix)			## returns the inverse from the cached value

## Write a short comment describing this function
##
## makeCacheMatrix takes a matrix and returns a list of functions to set and get an input matrix and its inverse
## It uses the <<- assignment operator to assign variables defined in the enclosing scope of makeCacheMatrix() from
## within the nested functions defined therein. This allows us to cache the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##
## cacheSolve takes the output of makeCacheMatrix, i.e., a list of 4 set and get functions, and
## assumes that the set() function has already been used to assign the input matrix to be inverted.
## It then returns the cached inverse, if it exists, but if it is not found, it solves for the inverse,
## caches it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinv(inv)
	inv
}

## Testing
## The following code exemplifies the usage of the above two functions to compute matrix inverses.
##

tester <- function() {
	## create a 3x3 invertible matrix (i.e., with non-zero determinant)
	mat <- matrix(c(1,4,3,2,5,7,9,6,8), 3, 3)
	print("input matrix:")
	print(mat)
	cacheMatrix <- makeCacheMatrix(mat)
	cacheMatrix$set(mat)
	## solve for the matrix inverse
	print("solve for inverse first time...")
	matinv <- cacheSolve(cacheMatrix)
	print("inverse:")
	print(matinv)
	## test return from cached value the second time
	print("test that cached inverse is returned the second time...")
	cacheSolve(cacheMatrix)
	print("notice that it obtained the result from cache")
	## test the inverse returned is valid
	print("testing the product of the matrix and its inverse is 3x3 unit matrix:")
	unit3x3 <- mat %*% matinv
	print(unit3x3)
	print("notice that the product is a 3x3 unit matrix with accumulated computational errors")
	print("resetting the input matrix")
	mat <- matrix(c(1,4,2,3,7,5,9,6,8), 3, 3)
	cacheMatrix$set(mat)
	print(mat)
	print("solve for inverse again, since input matrix was reset...")
	matinv <- cacheSolve(cacheMatrix)
	print("inverse:")
	print(matinv)
	print("notice that it re-solved for the inverse, since the input was reset, instead of getting the result from cache")
	unit3x3 <- mat %*% matinv
	print("proving matrix x inverse = unit matrix")
	print(unit3x3)
	print("notice that the product is a 3x3 unit matrix with accumulated computational errors")
}
