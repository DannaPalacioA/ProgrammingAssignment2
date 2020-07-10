##this is the programming assignament for the third week of the second course of data science course in coursera

 ##makecachematrix make two things, set a matrix or/and its inverse and store it in the cache 
 #and then you can call it

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversa) inv <<- inversa
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachesolve call the stored inverse of the matrix or
#calculated it if it has not been stored.

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


##Prove that it works

#makecachematrix <- function(x = matrix()) {
#  +     inv <- NULL
#  +     set <- function(y) {
#    +         x <<- y
#    +         inv <<- NULL
#    +     }
#  +     get <- function() x
#  +     setinv <- function(inversa) inv <<- inversa
#  +     getinv <- function() inv
#  +     list(set = set, get = get,
#             +          setinv = setinv,
#             +          getinv = getinv)
#  + }
#> mycachematrix<-makecachematrix()
#> mycachematrix$set(matrix(c(2,0,1,1,1,-4,3,7,-3),nrow=3,ncol=3,byrow=TRUE))
#> mycachematrix$get()
#[,1] [,2] [,3]
#[1,]    2    0    1
#[2,]    1    1   -4
#[3,]    3    7   -3
#> cachesolve <- function(x, ...) {
#  +     inv <- x$getinv()
#  +     if(!is.null(inv)) {
#    +         message("getting cached data")
#    +         return(inv)
#    +     }
#  +     data <- x$get()
#  +     inv <- solve(data, ...)
#  +     x$setinv(inv)
#  +     inv
#  + }
#> cachesolve(mycachematrix)
#[,1]       [,2]        [,3]
#[1,]  0.46296296  0.1296296 -0.01851852
#[2,] -0.16666667 -0.1666667  0.16666667
#[3,]  0.07407407 -0.2592593  0.03703704
