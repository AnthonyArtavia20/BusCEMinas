#lang racket

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

;; Ejemplo de uso:
;; (BuscaCE 8 8 "Facil")
;; (BuscaCE 10 12 "Medio") 
;; (BuscaCE 12 15 "Dificil")

;; Para probar directamente:
(printf "=== BUSCEMINAS ===\n")
(printf "Iniciando juego...\n")
(BuscaCE 8 8 "Facil")