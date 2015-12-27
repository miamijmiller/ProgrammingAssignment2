## This code will inverse a matrix. First it will check to see if the answer is already available (cahced) and if it is, the code will
## print getting cached data and provide the answer. If the answer isn't cached the code will provide the answer.
## functions do

## Set inverse recieves an argument and stores it in 'in'. This isn't computing the inverse, just storing it.


makeCacheMatrix <- function(x = matrix()) {
		in <- NULL
		set <- function(y) {
				 x <<- y
			   in <<- NULL
		 }
	        
	        get<- function() x
	        setinv <- function(inverse) in <<- inverse
                getinv <- function() in
		        
	   list(set = set, get = get ,setinv = setinv ,getinv = getinv)
}


## This function calls the getInverse function, checks if it was already computed inside the cache inverse <- x$getinv(). 
## If the inverse is not null the function will print it. If it is null it will solve it.

cacheSolve<- function(x, ...) {

		inverse <- x$getinv()		 
		if(!is.null(inverse)) {
		   message("getting cached data")
			 return(inverse)
		  }

		data <- x$getMatrix()
		inverse <- solve(data)
			x$setinv(inverse)
		inverse 
		}
