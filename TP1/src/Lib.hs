
type Cargo = String
type Horas = Float
type Años = Float
type Sueldo = Float

sueldoPorPuesto :: Cargo->Sueldo
sueldoPorPuesto "titular" = 149000
sueldoPorPuesto "ayudante" = 66000
sueldoPorPuesto "adjunto" = 116000

sueldoPorHora :: Horas->Horas
sueldoPorHora h
        |h>=5 && h<=15 = 1
        |h>15 && h<=25 = 2
        |h>25 && h<=35 = 3
        |h>35 && h<=45 = 4
        |h>45 && h<=50 = 5

sueldoPorAntiguedad :: Años->Float
sueldoPorAntiguedad x
                  | x<3 = 1
                  | x>=3 && x<5 = 1.2
                  | x>=5 && x<10 = 1.3
                  | x>=10 && x<24 = 1.5
                  | x>=24 = 2.2

calcularSueldo :: Cargo->Horas->Años->Sueldo
calcularSueldo x y z = sueldoPorPuesto (x)*sueldoPorHora(y)*sueldoPorAntiguedad (z)