

dots<-function(...)
{
  eval(substitute(alist(...)))
}


testDots<-function(a=1, b=NULL, ...)
{
cat("a:\n"); print(a)
cat("b:\n"); print(b)

d<-dots(...)

print(d)

}

testlist<-function(a=1, b=NULL, ...)
{
cat("a:\n"); print(a)
cat("b:\n"); print(b)

d<-list(...)

print(d)
}
