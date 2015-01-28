!Euler Program for the differential equation y'=2x on the interval -10 to 10, such that y(0)=100
!Will write the fortran program here and call this program from a MATLAB script, this will be used to plot with the use of MATLAB.


Program RungeKutta
real deltat,start,end,y0,N,currentPos,newSol,k1,k2,k3,k4
integer endN
REAL, DIMENSION(1000) :: y,x,error

!deltat - time step
!start - start of where the approximation of the differential equation is taking place
!end - end of where the approximation of the differential equation is taking place
!N - number of steps taken between start and finish
y0=100
start=-10
end=10
endN=1000
N=1000
deltat=(end-start)/N
currentPos=start
x(1)=start
y(1)=y0
error(1)=0



write(*,*), deltat
	
	write (*,*) "timestep ",deltat
	write (*,*) "time ",x(1)," y ",y(1), "Error between real and approximate", error(1)
	do i=2,endN
		currentPos=start + (i-1)*deltat
		k1=f(currentPos)
		k2=f(currentPos+(deltat/2.0))
		k3=f(currentPos+(deltat/2.0))
		k4=f(currentPos+deltat)
		newSol=y(i-1) + (deltat/6.0)*(k1+2*k2+2*k3+k4)
		x(i)=currentPos
		y(i)=newSol
		error(i)=abs((currentPos*currentPos)- newSol)
		write (*,*) "time ",x(i)," y ",y(i), "Error between real and approximate", error(i)

		
	end do
	open(unit=1,file="rk-error.txt",status="new")
	open(unit=2,file="rk-x.txt",status="new")
	open(unit=3,file="rk-y.txt",status="new")
	do i=1,endN
		write(unit=1, FMT=*) error(i)
		write(unit=2, FMT=*) x(i)
		write(unit=3, FMT=*) y(i)
	end do
	close(unit=1)
	close(unit=2)
	close(unit=3)

	
end Program RungeKutta


real function f(x)
real x
evalFunc=2*x
return
end
