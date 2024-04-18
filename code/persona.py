
#clase persona
class Persona:
	def __init__(self, nombre, trabajo=None,sueldo=0):
		self.nombre=nombre
		self.trabajo=trabajo
		self.sueldo=sueldo
	def apellido(self):
		return self.nombre.split()[-1]
	def trabajo(self):
		return self.trabajo
	def aumento(self,porcentaje):
		self.sueldo=self.sueldo*(1+porcentaje)
	def __repr__(self):
		return '[Persona: %s,%s,%s]' % (self.nombre,self.trabajo,self.sueldo)

class Gerente(Persona):
	def aumentar_sueldo(self, porcentaje, bono=.10):
		Persona.aumento(self, porcentaje+bono)
	def reducir_sueldo(self, porcentaje):
		Persona.aumento(self, -porcentaje)



#funcion central
# if __name__ == '__main__':
# 	dam=Persona('Damariz Gonzalez','PhD student',900)
# 	nat=Persona('Natalia Poblete','PhD student',900)
# 	nai=Persona('Nairo Torres','PhD student',900)
# 	dam.apellido()
# 	print(nat)
# 	print(nai)
# 	alumnos=[Persona('Patricio Morales','Phd student',900),nai]
# 	alumnos.append(dam)
# 	alumnos.append(nat)

# 	for a in alumnos:
# 		print(a)

# 	mauro=Gerente('Mauricio Avendaño','Gerente personas',2000)
# 	mauro.reducir_sueldo(.25)
	
# 	print(mauro)
