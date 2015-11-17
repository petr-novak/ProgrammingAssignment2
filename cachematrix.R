## Module for cached calculation of inverse matrix.
## Written by Petr Novak 11/17/2015

## Example usage:
## a <- makeCacheMatrix(inputmatrix)
## cacheSolve(a)

## Module contains two functions:
## - makeCacheMatrix(x)
## - cacheSolve(x, ...)

## Fuction 'makeCacheMatrix' is handling input data and results saved in cache
## after first calculation of the input data.
## Output is list of setter and getter functions.
## Usage:
## a <- makeCacheMatrix()	## initialize object with getters and setters
## a$set(inputmatrix)		## store input matrix data
## a$get()			## return stored input data
## a$setcache(result)		## store result of the calculation to cache
## a$getcache()			## return result of from cache

makeCacheMatrix <- function(x = matrix()) {
	## Return a list of getter and setter functions for input and cached data

	## Initialize cache by setting to NULL
        cache <- NULL

	## Setter function for storing data - matrix in this case
        set <- function(y) {
		## Store matrix given as argument of setter function to 'x'. E.g.
		## makeCacheMatrix(inputmatrix) means 'inputmatrix' is stored in x
                x <<- y
		## cache is set to NULL as we have new matrix defined
                cache <<- NULL
        }

	## Getter function for the stored data
	## Function returns stored matrix
        get <- function() x
	
	## Setter function for storing calculated result to cache
        setcache <- function(result) cache <<- result
        
	## Getter function for cached result
	## Function returns stored result
	getcache <- function() cache

	## Publishing list of above defined functions
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}

## Function 'cacheSolve' is cached version of solving matrix inversion problem.
## If the input matrix is first met, result is calculated by 'solve(inputmatrix)'
## and stored in the cache for later use. Should we need to recalculate inverse of
## the same matrix again we would just retrieve the result from cache.
## This way spending the time on the repeated calculation is avoided.
##
## Usage:
## cacheSolve(a)

cacheSolve <- function(x, ...) {

	## Get result from cache if any
        cache <- x$getcache()

	## Was there any result in cache?
        if(!is.null(cache)) {
		## Cached result found, user informed and result returned
                message("getting cached data")
                return(cache)
        }

	## No previous results in cache found so we need to calculate one!
	## Tell the user nothing was found and new result is computed
	message("computing new result and storing it to cache")

	## Obtain input data for calculation
        data <- x$get()

	## Perform the calculation - in this case it is inverse matrix
        result <- solve(data, ...)

	## Store calculated result in cache
        x$setcache(result)
	
	## Return result of calculation
        result
}
