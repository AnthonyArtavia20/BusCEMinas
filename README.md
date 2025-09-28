## BusCEMinas
## 📥 Requisitos
- **DrRacket** (IDE recomendado para ejecutar código Racket)
- **Racket** versión compatible con `racket/gui`
- Sistema operativo con soporte para interfaces gráficas (Windows, Linux o macOS)

### ¿Cómo usar?
##### Se debe descargar el archivo del proyecto desde el [repositorio de GitHub](https://github.com/AnthonyArtavia20/RegistroTECAssembly) y abrir el archivo `Main.rkt` en **DrRacket**.
Posterior a ello, se debe ejecutar el programa presionando el botón **"Run"** en DrRacket.
El sistema mostrará un menú inicial donde se puede seleccionar el tamaño del tablero (filas y columnas) y la dificultad del juego, y a partir de ahí se interactúa usando el **mouse** para navegar y jugar.

---
## ¿Cómo funciona este programa?
##### Este programa está hecho en lenguaje **Racket**, utilizando **programación funcional** y **recursión** para manipular estructuras de datos basadas en listas que representan el tablero de juego.
El proyecto consiste en la implementación del clásico videojuego **Buscaminas**, donde el jugador debe descubrir todas las celdas sin minas mientras marca con banderas las celdas sospechosas de contener minas.
El sistema permite al usuario personalizar las **dimensiones del tablero** y la **dificultad**, revelar celdas mediante clicks, colocar/quitar banderas y recibir notificaciones de **victoria o derrota**.
Este programa refuerza los conceptos fundamentales de **programación funcional**: **uso de listas como estructuras de datos, funciones recursivas, manejo de estado inmutable, interfaz gráfica con racket/gui y algoritmos de expansión automática.**

---
##### Características importantes:
+ **Tablero personalizable:** Permite crear tableros de diferentes tamaños (matriz nxm) con dificultad ajustable que determina la cantidad de minas.
+ **Interfaz gráfica intuitiva:** Sistema de botones que responden a clicks del mouse para revelar celdas o colocar banderas según el modo seleccionado.
+ **Lógica de expansión automática:** Cuando se revela una celda sin minas adyacentes, el algoritmo recursivamente descubre todas las celdas conectadas libres de minas.
+ **Sistema de banderas:** Permite marcar celdas sospechosas con banderas, con contador que muestra las banderas restantes basado en el total de minas.
+ **Detección de victoria/derrota:** El juego termina automáticamente cuando se descubren todas las celdas seguras y se marcan todas las minas, o cuando se presiona una mina.
+ **Manejo de errores y validación:** El sistema previene clicks en celdas ya descubiertas, controla el límite de banderas y maneja adecuadamente los eventos de la interfaz gráfica.

---
## 🎮 Modo de Juego
1. **Selección inicial:** Al ejecutar, aparece un menú donde puedes elegir filas, columnas y dificultad
2. **Modo Descubrir:** Click izquierdo en las celdas para revelarlas
3. **Modo Bandera:** Cambia al modo bandera con el botón correspondiente para marcar celdas sospechosas
4. **Objetivo:** Descubre todas las celdas sin minas y marca todas las minas con banderas para ganar

---
## 🛠️ Estructura del Proyecto
- `Main.rkt` - Archivo principal de ejecución
- `logic.rkt` - Lógica del juego y manipulación del tablero
- `guiSimple.rkt` - Interfaz gráfica y manejo de eventos
- Documentación técnica y manual de usuario incluidos

---
## 👥 Desarrolladores
- Anthony José Artavia Leitón
- Alex Eduardo Feng Wu  
- Ariel Saborio Álvarez

*Proyecto desarrollado para el curso de Paradigmas de Programación CE 1 Semestre 2025 - Instituto Tecnológico de Costa Rica*
