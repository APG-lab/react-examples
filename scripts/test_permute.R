#!/usr/bin/env Rscript


permutations <- function (n)
{
	if (n==1)
	{
		return (matrix(1))
	}
	else
	{
		sp <- permutations (n-1)
		p <- nrow (sp)
		A <- matrix (nrow=n*p,ncol=n)
		for (i in 1:n)
		{
			A[(i-1)*p+1:p,] <- cbind (i,sp+(sp>=i))
		}
		return(A)
	}
}

perm_comb <- function (items, n)
{
	cm <- combn (items, n)
	return (do.call (rbind, lapply (1:ncol (cm), FUN=function (x,a) { return (matrix (a[,x][permutations (n)],ncol=n)); },a=cm)))
}

sort_m <- function (m)
{
	return (m[do.call (order, lapply (1:ncol(m),FUN=function (x,a) { a[,x]},a=m)),])
}

um <- matrix(letters[permutations (2)],ncol=2)
sort_m (um)

um <- matrix(letters[permutations (3)],ncol=3)
sort_m (um)

um <- matrix(letters[permutations (4)],ncol=4)
sort_m (um)

um <- perm_comb (letters[1:4], 2)
sort_m (um)

