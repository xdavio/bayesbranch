all: tree0.8.10.Rdata tree0.8.100.Rdata tree0.8.1000.Rdata tree0.6.10.Rdata tree0.6.100.Rdata tree0.6.1000.Rdata tree0.4.10.Rdata tree0.4.100.Rdata tree0.4.1000.Rdata tree0.2.10.Rdata tree0.2.100.Rdata tree0.2.1000.Rdata tree0.8.5000.Rdata tree0.6.5000.Rdata tree0.4.5000.Rdata tree0.2.5000.Rdata

tree0.8.10.Rdata: gen.R
	R --vanilla <gen.R theta=0.8 init=10
tree0.8.100.Rdata: gen.R
	R --vanilla <gen.R theta=0.8 init=100
tree0.8.1000.Rdata: gen.R
	R --vanilla <gen.R theta=0.8 init=1000
tree0.8.5000.Rdata: gen.R
	R --vanilla <gen.R theta=0.8 init=5000



tree0.6.10.Rdata: gen.R
	R --vanilla <gen.R theta=0.6 init=10
tree0.6.100.Rdata: gen.R
	R --vanilla <gen.R theta=0.6 init=100
tree0.6.1000.Rdata: gen.R
	R --vanilla <gen.R theta=0.6 init=1000
tree0.6.5000.Rdata: gen.R
	R --vanilla <gen.R theta=0.6 init=5000


tree0.4.10.Rdata: gen.R
	R --vanilla <gen.R theta=0.4 init=10
tree0.4.100.Rdata: gen.R
	R --vanilla <gen.R theta=0.4 init=100
tree0.4.1000.Rdata: gen.R
	R --vanilla <gen.R theta=0.4 init=1000
tree0.4.5000.Rdata: gen.R
	R --vanilla <gen.R theta=0.4 init=5000


tree0.2.10.Rdata: gen.R
	R --vanilla <gen.R theta=0.2 init=10
tree0.2.100.Rdata: gen.R
	R --vanilla <gen.R theta=0.2 init=100
tree0.2.1000.Rdata: gen.R
	R --vanilla <gen.R theta=0.2 init=1000
tree0.2.5000.Rdata: gen.R
	R --vanilla <gen.R theta=0.2 init=5000


cleanall: 
	rm *~

delete:
	rm *Rdata
