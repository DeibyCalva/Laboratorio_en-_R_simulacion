import numpy as np
import math
class integrador:
    def __init__(self, xMin, xMax, N):
        self.xMin = xMin
        self.xMax = xMax
        self.N = N
        self.suma = 0.0

    def integrate(self):
        deltaX = (self.xMax-self.xMin) / (self.N-1)
        for i in range(200):
            xi = 1+(i*deltaX)
            xCuadrado = math.pow(xi, 2)
            euler = math.exp(-xi)
            senoX = math.sin(xi)
            self.suma += xCuadrado*euler*senoX*deltaX
        print(round(self.suma, 5))
    def show(self):
        return 0        
examp = integrador(1,3,200)
examp.integrate()
examp.show()


