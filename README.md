# DeLaval-Nozzle-Euler
A university project resolving the Euler equation for a De Laval nozzle given ambient and exhaust conditions 
The project was developed during the course of Computational fluid Dynamics at Politecnico di Milano, 2018.

Code is written in Fortran90.
All modules and functions are intended to provide a general guidance on a simple test case for solving the compressible Euler equations in a quasi-2D geometry.

### GEOMETRY OF THE PROBLEM
The module defining the nozzle geometry is given assigning the area of the in, out and throat sections, which are given as function of the x-grid points A(x):
```

   !
   !                         ____
   !    _____          _____/
   !         \___     /
   !             \___/
   !
   !  -- -- -- -- -- -- -- -- -- --
   !    xi         xg           xu
```

To change this, go in the the _nozzle_geometry.f90_ file


### INPUT HANDLING

input conditions are fixed in the _ugello.f90_ file
Temperatures are expressed in absolute K degrees
Pressures are expressed in Pascal

There is an option of which type of solver to use.
At line 132 of _main.f90_ makes you can leave uncommented the method you want
(LWc is lax-wendroff with upflow correction, Roe is the Roe's flux method, UHR is the High resolution method with entropy fix)

```
      ! Flussi numerici
      !CALL LWc_flux (Dt, Dx, W,  F)
      !CALL Roe_flux (W,  F)
      !CALL UHR_flux (Dt, Dx, W,  F, limiter)
      CALL UHR_flux (0d0, Dx, W,  F, limiter) ! vita DURA. L'idea di utilizzare
      !metodo ad alta risoluzione non derivante da Lax_Wendroff crea riflessione di
      !oscillazioni.
```

### COMPILING

Program is self-contained. 
All modules and functions, are included and the compiling procedure is created in the main file.
Ideally, to get an exacutable version on your unix machine, open the _makefile_ , specify which compiler you are using, save, and simply open the terminal, navigate to the project directory and type :

`make`

