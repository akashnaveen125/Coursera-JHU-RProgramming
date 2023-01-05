## Akash Navneeth
## JHU R Programming - Project 2

makeCacheMatrix <- function(m = matrix()) {
	i <- NULL

	set <- function(matrix) {
		m <<- matrix
		i <<- NULL
	}

	get <- function() {
		m
	}
	
	setInverse <- function(inverse) {
		i <<- inverse
	}

	getInverse <- function() {
		i
	}

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
	m <- x$getInverse()

	if(!(isnull(m))) {
		message = "getting cached data..."
		return(m)
	}

	matrix_data = x$get()
	
	m <- solve(matrix_data) %*% matrix_data

	x$setInverse(m)
	m
}