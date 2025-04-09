# Ctr
Ctr es **EDSL (Embebbed Domain Specific Language)** implementado en Haskell. Sirve para representar contratos financieros permitiendo así llevar 
un historial de dinero entre dos fechas.

Está basado en el paper:

*Composing contracts: an adventure in financial engineering*

publicado el 23 de Agosto del 2000.


# Formato

Todo archivo .ctr debe estar formadas por líneas las cuales definen fechas y/o contratos, separadas por *;* y finalizadas por una palabra clave *skip* 
la cual no lleva *;*

Además, debe comenzar por un comando:

``` 
date d m y; 
```
el cual setea la fecha inicial de las operaciones.

**Cualquier contrato con fecha anterior a esta, es descartado**

Para definir una fecha cualquiera, simplemente se puede hacer:

``` 
d = d m y; 
```

## Definiendo contratos 

Los contratos en ctr tienen diversas formas:

### Zero
Este contrato paga cero en cualquier momento. Realmente no es muy útil.

``` 
c = zero; 
```

### One 
Este contrato paga 1 en la fecha indicada **en tanto dicha fecha sea posterior a la fecha inicial, de lo contrario no da nada**.
``` 
c = one Int Int Int; 
```
o utilizando una variable para mantener una fecha:

``` 
c = one d; 
```

A partir de ahora, **c** denotará este contrato.

### Scale
Este contrato toma un **Int** *i* y un contrato cualquiera y me permite **multiplicar el valor de dicho contrato por i**

``` 
s = scale Int (one d); 
```
Análogamente:

``` 
s = scale Int c; 
```

A partir de ahora, s denotará este contrato.

### Give
Este contrato, en lugar de dar lo que estipula, lo **quita**. Es decir:

``` 
g = give c; 
```
en lugar de dar 1 en la fecha estipulada por c, **quita 1** en la fecha estipulada por c.

### Then
Este contrato esta formado por dos sub-contratos. Su semántica es quedarse con el primero **en caso este sea válido**. De lo contrario, se queda con el segundo.

Siendo la fecha inicial 9/4/2025, definimos:

``` 
cv = one 28 12 2025;
ci = one 5 4 2025;
```

con esto, podemos hacer:


``` 
t = cv then ci;
```

El cual dará **cv**, y 

``` 
t = ci then cv;
```

el cual **también dará cv**.

### Truncate
Truncate me permite cambiar la fecha de un contrato.

``` 
cv = one 28 12 2025;
ci = one 5 4 2025;
t = truncate 7 9 2025 ci
```

además, **da ese contrato en la nueva fecha**.

Cabe aclarar que el cambio de fecha **define un nuevo contrato**, no actualiza uno anterior.

### And
Este contrato toma dos sub-contratos y da ambos.
No importa si la fecha de alguno es inválida ya que contará como que no da nada.

``` 
a = s and c;
```

Esta sentencia dará ambos *s* y *c*.

### Or 
Este contrato toma dos sub-contratos y **se queda con el que mejor paga**.

``` 
o = s or c;
```

En este caso, se quedará con *s*.

## Composición del código

- `app/`
  - `Main.hs`: archivo principal
- `src/`
  - `Def.hs`: AST del lenguaje
  - `Eval.hs`: Evaluador del lenguaje
  - `Parser.hs`: Parser del lenguaje
  - `Lib.hs`: Funciones útiles
  - `...`: Implementaciones para secuencias, arreglos y paralelismo
- `test/`
  - Tests para el lenguaje

Los demás archivos son para la construcción del proyecto.

# Salida

Como salida, el lenguaje produce un archivo *.png* en la carpeta actual. El mismo contiene un gráfico que muestra la evolución del saldo desde la **fecha inicial** hasta la **última fecha válida que aparece en un contrato**.

# Ejecución

Para ejecutarlo, primero se debe hacer:

``` 
stack setup
```

lo cual configurará la herramienta stack.

Luego:

``` 
stack build 
```
lo cual construirá el proyecto.

Para ejecutarlo se debe hacer:

``` 
stack exec Ctr-exe -- PATH/File.ctr -opt;
```
donde ejecutará el archivo llamado *File.ctr* ubicado en la carpeta *PATH*.

Las opciones disponibles son:

> -a: Muestra el AST del programa.

> -e: Evalua el programa.

> -h: Imprime ayuda.

Por ejemplo, para evaluar el archivo **Ej5.ctr** en la carpeta *test*:

``` 
stack exec Ctr-exe -- test/Ej5.ctr -e;
```






