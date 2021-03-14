# REDAP UN
REDAP UN es una aplicación que permite la búsqueda, visualización, descarga y análisis de los datos de precipitación con precisión al minuto tomados en la cuenca urbana de la Universidad Nacional de Colombia: Sede Bogotá durante el periodo comprendido entre los años 2007 y 2012.
>  REDAP UN fue desarrollado por el Semillero de Investiación en Hidrología Urbana de la Universidad Nacional de Colombia. [Sitio web](http://gireh.unal.edu.co/sihu)
>  [Manual de usuario](http://gireh.unal.edu.co/images/docs/Manual_REDAP_UN_V1.pdf)

## Instalación
REDAP UN es una aplicación desarrollada totalmente en el lenguaje de programación R, por medio del paquete Shiny, este permite construir aplicaciones web interactivas a partir de los scripts base de R. La interactividad de estas aplicaciones permite manipular los datos sin tener que manipular el código directamente, lo que hace que sea amigable a usuarios que no estén familiarizados con la programación.

Para utilizar la aplicación se debe instalar el lenguaje de programación R y la interfaz de programación RStudio, para ello recomendamos el tutorial de Hansel Mora, el cual ilustra de manera sencilla como instalar ambos programas, accesible a través de [Link]( https://youtu.be/D9Bp11iZssc ).


Los datos de la aplicación cuentan con registros de 14 pluviografos entre 2007 y 2013. Estos datos deben ser descargados en formato .csv y dispuestos en la carpeta data. Para acceder a los datos basta con descargarlos [aquí](https://drive.google.com/drive/folders/18oV5-_Gw5NXT6Duyuhf6_bOOC7dHE_JF)


## Uso
Una ves dentro del entorno RStudio, los datos ubicados en la carpeta data y el codio App.R abierto procedemos correr la apliación. Dentro de esta se pueden realizar los siguientes procesos:
* Exploración y descarga de datos
* Comparación de estaciones
* Interpolación espacial
### Exploración y descarga
La ubicación de cada uno de los pluviógrafos se puede observar en el mapa dispuesto en la pestaña de salida del menú asociado a la búsqueda y descarga. Al asignar valores al campo de pluviografo y la fecha deseada de descarga, en el menú de salida se puede observar el hietograma de precipitación asociado. Este indica cuantos milímetros de agua por minuto hubo en el periodo de tiempo seleccionado por el usuario. 
Adicionalmente se pueden descargar los datos brutos para realizar analisis posteriores. Para ello se oprime el botón **_ Descargar Datos_** , este botón descargara un CSV con los datos solicitados por el usuario.

### Comparación de estaciones
Esta herramienta permite al usuario hacer comparaciones entre los diferentes pluviógrafos localizados en la cuenca de la Universidad Nacional, esta sección permite comparar las curvas de masa y los hietogramas de 3 pluviógrafos en un periodo de tiempo determinado. Inicialmente se ingresan las fechas y los tres pluviógrafos que se desean comparar. Mediante esta seccion se muestran:
* Curvas de masa
* Curvas de masa adimensionales
* Hietogramas
### Interpolación espacial
Esta herramienta permite al usuario obtener los cálculos de los métodos de interpolación espacial más utilizados en hidrología, para la cuenca de la Universidad Nacional. Estos son:
* IDW
* Krigging
* Poligonos de Thiessen

Inicialmente el usuario ingresa la fecha, en la caja disponible para el ingreso de fecha dispuesta en la interfaz de salida. En el momento de asignar un periodo temporal determinado, la aplicación realiza la interpolación espacial por medio de los tres métodos.
