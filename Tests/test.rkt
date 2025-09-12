#lang racket
;;ESTO ES SOLO PARA PRUEBAS, EL CÖDIGO INICIAL SE EJECUTA DESDE MAIN
(require "logic.rkt")

;; Función para imprimir el tablero en consola (solo para testing)
(define (imprimir-tablero tablero)
  (printf "Tablero de Buscaminas (~a x ~a)\n" 
          (length tablero) 
          (length (first tablero)))
  (printf "Leyenda: M=Mina, -=Vacía, número=Minas adyacentes\n\n")
  
  (imprimir-filas tablero 0))

(define (imprimir-filas tablero fila-actual)
  (if (not (empty? tablero))
      (begin
        (printf "Fila ~a: " fila-actual)
        (imprimir-celda (first (first tablero)))
        (imprimir-resto-fila (rest (first tablero)))
        (printf "\n")
        (imprimir-filas (rest tablero) (+ fila-actual 1)))
      (void)))

(define (imprimir-resto-fila fila)
  (if (not (empty? fila))
      (begin
        (printf " ")
        (imprimir-celda (first fila))
        (imprimir-resto-fila (rest fila)))
      (void)))

(define (imprimir-celda celda)
  (if (es-mina? celda)
      (printf "M")
      (if (= (obtener-minas-adyacentes celda) 0)
          (printf "-")
          (printf "~a" (obtener-minas-adyacentes celda)))))

;; Probemos el código
(printf "=== PRUEBA DEL BUSCAMINAS ===\n\n")

;; USAR DIRECTAMENTE LA FUNCIÓN DE logic.rkt EN LUGAR DE BuscaCE
(define mi-tablero (crear-tablero-con-minas 8 8 0.1))  ;; 0.1 = 10% minas (Fácil)
(imprimir-tablero mi-tablero)

(printf "\n=== CELDA ESPECÍFICA ===\n")
(define celda-ejemplo (obtener-celda mi-tablero 3 4))
(printf "Celda (3,4): Mina? ~a, Minas adyacentes: ~a\n"
        (es-mina? celda-ejemplo)
        (obtener-minas-adyacentes celda-ejemplo))