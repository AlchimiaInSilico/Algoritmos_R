# Algoritmos_R
Este es el repositorio que contiene los diversos algoritmos que hemos implementado en R. Cada proyecto esta separado por carpeta

# Contenido del directorio "Calculo de descriptores":

  *algoritmoPolimerizacion.R
  ```
    #Realiza la polimerizacion comptacional de una Unidad Repetitiva (UR).
    #parametros de entrada:
    #unidadRepetitiva: cadena de caracteres correspondiente al codigo SMILES de la Unidad Repetitiva
    #n: numero de Unidades Repetitivas que se desean unir.
   ```
    
  *descriptoresCAMB.R
  ```
    #Realiza el calculo de descriptores utilizando la libreria camb (GeneratePadelDescriptors). Se debe setear la BD de polimeros que se desea
    #dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)
  ```

  *descriptoresRCDK.R
  ```
    #La funcion descriptoresRCDK() realiza el calculo en paralelo de la BD de polimeros que se desea cargar (modificable dentro de la funcion)
    #dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)
    #el calculo de descriptores lo realiza llamando a la funcion calcularDescriptores, la cual utiliza la libreria rcdk
   ```
    
  *descriptoresRCDK_serie.R
  ```
     #Realiza el calculo de descriptores utilizando la libreria rcdk. Se debe setear la BD de polimeros que se desea
     #dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)
   ```
     
  *filtrarDescriptores.R
  ```
    #Filtra una BD de descriptores, seleccionando los descriptores que se desea obtener.
    # Se debe setear la BD que contiene todos los descriptores y la lista de nombres de descriptores que se desea filtrar
```

  *polimerizadorParalelo.R
  ```
    #Realiza la polimerizacion comptacional de una Unidad Repetitiva (UR) aprovechando el calculo en paralelo. Se debe ingresar por parametro
    # la UR a polimerizar y la cantidad de veces (la cantidad de UR que tendra la cadena resultante)
      ```
