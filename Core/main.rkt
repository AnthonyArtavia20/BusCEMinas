#lang racket

(require racket/gui)

(require "logic.rkt")
(require "guiSimple.rkt")

;;; NIVELES DE DIFICULTAD
(define NIVEL-FACIL 0.1)
(define NIVEL-MEDIO 0.15)
(define NIVEL-DIFICIL 0.2)

;;; FUNCIÓN PARA CONTAR MINAS EN EL TABLERO 
(define (contar-minas-tablero tablero)
  "Cuenta el total de minas en el tablero de forma recursiva"
  (contar-minas-tablero-aux tablero 0))

(define (contar-minas-tablero-aux tablero acum)
  "Auxiliar recursivo para contar minas en el tablero"
  (if (null? tablero)
      acum
      (contar-minas-tablero-aux (cdr tablero) 
                               (+ acum (contar-minas-fila (car tablero))))))

(define (contar-minas-fila fila)
  "Cuenta minas en una fila de forma recursiva"
  (contar-minas-fila-aux fila 0))

(define (contar-minas-fila-aux fila acum)
  "Auxiliar recursivo para contar minas en una fila"
  (if (null? fila)
      acum
      (contar-minas-fila-aux (cdr fila) 
                           (if (es-mina? (car fila)) 
                               (+ acum 1) 
                               acum))))

;;; FUNCIÓN PRINCIPAL
(define (BuscaCE filas columnas nivel)
  "Función principal para iniciar el juego"
  (cond
    [(or (< filas 8) (> filas 15)) 
     (error "El número de filas debe estar entre 8 y 15")]
    [(or (< columnas 8) (> columnas 15))
     (error "El número de columnas debe estar entre 8 y 15")]
    [else
     (define porcentaje
       (cond
         [(equal? nivel "Facil") NIVEL-FACIL]
         [(equal? nivel "Medio") NIVEL-MEDIO]
         [(equal? nivel "Dificil") NIVEL-DIFICIL]
         [else (error "Nivel no válido. Use: Facil, Medio o Dificil")]))
     
     (printf "Creando tablero de ~ax~a con nivel ~a...\n" filas columnas nivel)
     (define tablero (crear-tablero-con-minas filas columnas porcentaje))
     (define total-minas (contar-minas-tablero tablero))
     (printf "Tablero creado exitosamente! Minas: ~a\n" total-minas)
     (printf "Límite de banderas: ~a\n" total-minas)
     
     (iniciar-juego tablero)  ;; Iniciar la interfaz
     tablero]))


; Crear la ventana principal
(define ventana (new frame%
                     [label "BuscaMinas AAA"]
                     [width 500]
                     [height 200]))

; Crear un mensaje (etiqueta de texto)
(define mensaje (new message%
                     [parent ventana]
                     [label "Hola, bienvenido a BuscaMinas AAA"]))

; Crear una selección desplegable (choice)
(define seleccion-filas (new choice%
                        [parent ventana]
                        [label "Elige la cantidad de filas: "]
                        [choices (list "8" "9" "10" "11" "12" "13" "14" "15")]))

(define seleccion-columnas (new choice%
                        [parent ventana]
                        [label "Elige la cantidad de Columnas: "]
                        [choices (list "8" "9" "10" "11" "12" "13" "14" "15")]))

; Función para obtener la opción seleccionada como string
(define (obtener-seleccion choice-widget)
  (define indice (send choice-widget get-selection))
  (if indice
      (send choice-widget get-string indice)
      #f)) ; Retorna #f si no hay selección

; Función que se ejecuta al presionar el botón
(define (boton-presionado btn evt)
  (define filas-seleccionadas (obtener-seleccion seleccion-filas))
  (define columnas-seleccionadas (obtener-seleccion seleccion-columnas))
  (printf "=== BUSCEMINAS ===\n")
  (printf "Iniciando juego...\n")
  (BuscaCE (string->number filas-seleccionadas) (string->number columnas-seleccionadas) "Facil"))

; Crear el botón
(define boton (new button%
                   [parent ventana]
                   [label "Haz clic aquí"]
                   [callback boton-presionado]))

; Mostrar la ventana
(send ventana show #t)

