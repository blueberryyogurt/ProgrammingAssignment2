
## These two functions allow the user to access the inverse of a matrix repeatedly without computing it more than once. 

## makeCacheMatrix takes a matrix x as its input and produces a vector containing three functions:
## 		1)getmatrix(): Returns x.
##		2)setinverse(inv): Stores inv as the inverse of x.
## 		3)getinverse(): Retrieves the stored inverse for x if it exists. Returns NULL if no inverse is stored.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
	getmatrix <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(getmatrix = getmatrix,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve takes a vector produced by makeCacheMatrix as its input and returns the inverse of the corresponding matrix. 
## If the inverse of the this matrix has already been computed by cacheSolve, then it retrieves it without addiitonal computation. Otherwise, it computes this inverse and stores it for future use.  

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data)
        x$setinverse(i)
        i
}


