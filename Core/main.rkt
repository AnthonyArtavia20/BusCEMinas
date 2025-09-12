#lang racket

(require "logic.rkt")
(require "guiSimple.rkt")

;;; NIVELES DE DIFICULTAD
(define NIVEL-FACIL 0.1)
(define NIVEL-MEDIO 0.15)
(define NIVEL-DIFICIL 0.2)

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
     (printf "Tablero creado exitosamente!\n")
     
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