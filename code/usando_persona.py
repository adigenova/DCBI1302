from persona import Persona, Gerente

dam=Persona('Damariz Gonzalez','PhD student',900)
nat=Persona('Natalia Poblete','PhD student',900)
nai=Persona('Nairo Torres','PhD student',900)
dam.apellido()
print(nat)
print(nai)
alumnos=[Persona('Patricio Morales','Phd student',900),nai]
alumnos.append(dam)
alumnos.append(nat)

for a in alumnos:
	print(a)


